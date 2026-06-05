% ============================================================================
% CONSTRAINT STORY: legitimate_knowledge_boundary__credentialed_expertise_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
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
 *   human_readable: Legitimate Knowledge Boundary (Credentialed Expertise Reading)
 *   domain: epistemology/science_technology_studies/political_theory
 *
 * SUMMARY:
 *   The credentialed expertise reading of the legitimate knowledge boundary
 *   asserts that rigorous knowledge requires methodologically sound inquiry
 *   validated through credentialed peer review. Under this reading, the
 *   boundary between legitimate and illegitimate knowledge is enforced by
 *   disciplinary institutions, credentialing standards, and gatekeeping
 *   mechanisms (journals, professional associations, academic appointments).
 *   This reading has deep roots in the professionalization of science and has
 *   historically improved epistemic reliability by enforcing methodological
 *   standards and enabling error-correction through expert scrutiny. However,
 *   the reading also exhibits structural extraction: it creates barriers to
 *   entry, privileges institutional access over epistemic quality, and
 *   marginalizes non-credentialed knowledge systems that may produce
 *   epistemically adequate results through different mechanisms. The
 *   constraint is a Tangled Rope at the core — it genuinely coordinates
 *   knowledge validation (peer review functions to catch errors, enforce
 *   transparency, enable cumulative knowledge building) while simultaneously
 *   extracting from non-credentialed producers (their work is deemed
 *   illegitimate, appropriated without attribution, or filtered through
 *   credentialed intermediaries). The rising theater_ratio over the
 *   measurement interval (0.35 → 0.65) reflects the increasing gap between
 *   the stated function of credentialing (enforcing rigor) and its actual
 *   operation (metrics substitution, citation gaming, prestige-capture by
 *   established institutions).
 *
 * KEY AGENTS:
 *   - Credentialed Experts and Institutions: Beneficiaries (institutional/arbitrage) — control gatekeeping apparatus; their work is presumed legitimate; they can arbitrage between venues and standards
 *   - Non-Credentialed Knowledge Producers: Victims (powerless/trapped) — excluded from legitimacy recognition; lack institutional access; no pathway to credentialing without substantial resource investment
 *   - Early-Career Credentialed Researchers: Mixed (moderate/constrained) — possess credentials but face high barriers; benefit from legitimacy presumption but extracted through unpaid labor and metrics dependency
 *   - Alternative Knowledge Communities: Organized Victims (organized/constrained) — coordinate genuine knowledge production but structurally marginalized; work appropriated when filtered through credentialed interpreters
 *   - Peer Review Apparatus: Institutional Actor (institutional/arbitrage) — maintains gatekeeping; increasingly performative (piton classification); theater ratio rising as metrics replace epistemic judgment
 *   - Analytical Observer: Civilizational View (analytical/analytical) — risks naturalizing the credentialing boundary as an immutable law of knowledge rather than a contingent institutional arrangement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimate_knowledge_boundary__credentialed_expertise_reading, 0.52).
domain_priors:suppression_score(legitimate_knowledge_boundary__credentialed_expertise_reading, 0.68).
domain_priors:theater_ratio(legitimate_knowledge_boundary__credentialed_expertise_reading, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__credentialed_expertise_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__credentialed_expertise_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__credentialed_expertise_reading, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimate_knowledge_boundary__credentialed_expertise_reading, tangled_rope).
narrative_ontology:human_readable(legitimate_knowledge_boundary__credentialed_expertise_reading, "Legitimate Knowledge Boundary (Credentialed Expertise Reading)").
narrative_ontology:topic_domain(legitimate_knowledge_boundary__credentialed_expertise_reading, "epistemology/science_technology_studies/political_theory").

domain_priors:requires_active_enforcement(legitimate_knowledge_boundary__credentialed_expertise_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimate_knowledge_boundary__credentialed_expertise_reading, '39c25483-38f7-4277-971c-518831c159d3').
narrative_ontology:cs_kernel_codification('39c25483-38f7-4277-971c-518831c159d3', formalized).
narrative_ontology:cs_authority_grounding('39c25483-38f7-4277-971c-518831c159d3', extraction).
narrative_ontology:cs_interpretation_layer_present('39c25483-38f7-4277-971c-518831c159d3').
narrative_ontology:cs_reading_relation('39c25483-38f7-4277-971c-518831c159d3', legitimate_knowledge_boundary__experiential_pluralism_reading, forecloses).
narrative_ontology:cs_reading_relation('39c25483-38f7-4277-971c-518831c159d3', legitimate_knowledge_boundary__hybrid_coproduction_reading, influences).
narrative_ontology:cs_axiom('39c25483-38f7-4277-971c-518831c159d3', foundational, methodological_rigor_requires_formal_training).
narrative_ontology:cs_axiom_status(methodological_rigor_requires_formal_training, holdable).
narrative_ontology:cs_axiom_grounding('39c25483-38f7-4277-971c-518831c159d3', methodological_rigor_requires_formal_training, empirically_contingent).
narrative_ontology:cs_axiom('39c25483-38f7-4277-971c-518831c159d3', foundational, credentialed_peers_are_best_validators).
narrative_ontology:cs_axiom_status(credentialed_peers_are_best_validators, overridden).
narrative_ontology:cs_axiom_grounding('39c25483-38f7-4277-971c-518831c159d3', credentialed_peers_are_best_validators, empirically_contingent).
narrative_ontology:cs_reference_frame('39c25483-38f7-4277-971c-518831c159d3', disciplinary_methodological_standards).
narrative_ontology:cs_drift_state('39c25483-38f7-4277-971c-518831c159d3', contemporary_metrics_driven_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('39c25483-38f7-4277-971c-518831c159d3', '').
narrative_ontology:cs_kernel_id(legitimate_knowledge_boundary__credentialed_expertise_reading, legitimate_knowledge_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__credentialed_expertise_reading, credentialed_experts).
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__credentialed_expertise_reading, established_disciplinary_institutions).
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__credentialed_expertise_reading, peer_review_gatekeepers).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__credentialed_expertise_reading, non_credentialed_knowledge_producers).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__credentialed_expertise_reading, alternative_methodological_traditions).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__credentialed_expertise_reading, lay_expertise_and_experiential_knowledge).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NON-CREDENTIALED PRODUCERS (SNARE) — No institutional affiliation, no access to publishing pipelines, no recognized credentials. Trapped by structural barriers to credentialing (cost, time, gatekeeping requirements). Their knowledge claims are structurally ineligible for legitimacy recognition regardless of epistemic quality. Maximum extraction: their labor (community knowledge, experiential data, methodological innovations) is often appropriated by credentialed researchers without attribution or compensation.
constraint_indexing:constraint_classification(legitimate_knowledge_boundary__credentialed_expertise_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: EARLY-CAREER CREDENTIALED RESEARCHERS (TANGLED ROPE) — Possess credentials but face high barriers within peer review hierarchy. Constrained by journal gatekeeping, acceptance rates ~5%, career dependence on citations. But also benefit from credential legitimacy — their work is presumed methodologically rigorous. Mixed experience: genuine coordination benefit (peer feedback improves work) plus asymmetric extraction (unpaid peer review labor, publish-or-perish pressure, citation metrics capture).
constraint_indexing:constraint_classification(legitimate_knowledge_boundary__credentialed_expertise_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ESTABLISHED INSTITUTIONS (ROPE) — Universities, research institutes, major journals. Arbitrage position: can publish where/when they choose; can shape methodology standards. The peer review system coordinates research communication while benefiting them. They experience the constraint primarily as a coordination mechanism: credentialing standards enable the global research commons, which they dominate.
constraint_indexing:constraint_classification(legitimate_knowledge_boundary__credentialed_expertise_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: SENIOR ESTABLISHED RESEARCHERS (TANGLED ROPE) — High prestige, strong publication record, substantial power within peer review. But constrained by their own field's methodological orthodoxy — heretical methodologies are difficult to publish even from prestigious positions. Also constrained by citation metrics and grant dependency. Experience the constraint as both coordination (their work gets visibility) and extraction (they enforce the very gatekeeping that trapped early-career researchers).
constraint_indexing:constraint_classification(legitimate_knowledge_boundary__credentialed_expertise_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: PEER REVIEW APPARATUS (PITON) — The journal-editor-reviewer system claims to enforce methodological rigor but increasingly operates as performative theater. Metrics substitution (impact factor, h-index) replaced epistemic quality as the gate. Reviewers often cannot evaluate core claims. The apparatus persists through institutional inertia, not because it works — universities still use journal prestige for hiring despite knowing it correlates poorly with actual contribution. Theater ratio high (0.65) reflects this degradation.
constraint_indexing:constraint_classification(legitimate_knowledge_boundary__credentialed_expertise_reading, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ALTERNATIVE KNOWLEDGE COMMUNITIES (TANGLED ROPE) — Community science, citizen science networks, experiential knowledge commons, indigenous knowledge systems, craft practitioners. Organized but structurally constrained: they coordinate genuine knowledge production and validation but lack institutional power to make their epistemology count as legitimate in formal policy or academic contexts. Experience both coordination benefit (peer learning, method refinement) and extraction (their work is marginalized unless filtered through credentialed interpreters).
constraint_indexing:constraint_classification(legitimate_knowledge_boundary__credentialed_expertise_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (MOUNTAIN) — From a civilizational perspective, credentialed peer review might appear to be an immutable feature of how complex knowledge systems stabilize truth claims. Rigorous methodology and credentialing could be presented as natural laws of epistemic integrity. However, this reading is a false summit: the credentialed expertise boundary is a contingent institutional arrangement that benefits identifiable actors (established institutions) and harms others (non-credentialed knowledge producers). The engine will identify this as naturalization of power.
constraint_indexing:constraint_classification(legitimate_knowledge_boundary__credentialed_expertise_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimate_knowledge_boundary__credentialed_expertise_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(legitimate_knowledge_boundary__credentialed_expertise_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(legitimate_knowledge_boundary__credentialed_expertise_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(legitimate_knowledge_boundary__credentialed_expertise_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(legitimate_knowledge_boundary__credentialed_expertise_reading, TR),
    TR >= 0.70.

:- end_tests(legitimate_knowledge_boundary__credentialed_expertise_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The credentialed expertise reading creates documented barriers to entry (cost of education, time investment, gatekeeping success rates ~5% for prestigious journals) that extract benefit from non-credentialed producers. The barrier is partially justified by genuine methodological rigor enforcement, but empirical evidence shows credentialing also enforces methodological orthodoxy (studies using novel methods face higher rejection rates even when rigorous). Suppression (0.68): High. Non-credentialed producers face structural barriers not just to legitimacy but to the basic preconditions of participation — access to journals, institutional affiliation, credentialing pathways. For early-career credentialed researchers, suppression takes the form of citation metrics dependency, unpaid peer review labor, and publish-or-perish pressure. Theater ratio (0.65): Moderate-high. The peer review apparatus increasingly uses proxy metrics (impact factor, h-index) instead of epistemic quality assessment. Reviewers frequently cannot evaluate core technical claims in specialized domains, and their recommendations often reflect disciplinary conformity rather than methodological rigor. The theater has risen from 0.35 (earlier era with stronger emphasis on technical evaluation) to 0.65 (contemporary metrics-driven assessment).
 *
 * PERSPECTIVAL GAP:
 *   The credentialed expertise reading generates a maximal perspectival gap. Established institutions see a Rope (coordination of global knowledge production). Early-career researchers see a Tangled Rope (genuine but asymmetric coordination). Non-credentialed producers see a Snare (pure gatekeeping extraction with no benefit). Alternative knowledge communities see a Tangled Rope (coordination within their own community, but extraction through marginalization). The peer review apparatus sees a Piton (degraded ritual maintained through inertia). The analytical observer risks seeing a Mountain (natural law of knowledge) but the structural data indicates a false summit — identifiable beneficiaries (established institutions) and victims (non-credentialed producers) reveal this as a constructed boundary, not a natural law.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality is derived from the agent's power, exit options, and relationship to the credentialing boundary. Powerless non-credentialed producers (no exit) bear maximum extraction (d ≈ 0.95). Early-career credentialed researchers (constrained exit, mixed benefits) experience moderate extraction (d ≈ 0.60). Established institutions (arbitrage exit, full benefits) experience negative extraction (d ≈ 0.10). Alternative knowledge communities (organized but constrained) experience moderate extraction (d ≈ 0.55). The analytical observer (analytical exit, universal scope) has a standard derivation (d ≈ 0.72). These directionality values feed into the sigmoid f(d) function, producing effective extractiveness chi values that vary by order of magnitude depending on the observer's structural position.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved through the kernel lens: the credentialed expertise reading embodies ONE coherent claim about how knowledge legitimacy should be determined, but this claim coexists with structurally incompatible alternatives (experiential pluralism, hybrid coproduction). The contradiction is not resolved within this reading — the reading maintains that methodological rigor requires credentialing — but the contradiction is real in the world, where non-credentialed systems produce epistemically adequate results and credentialed systems enforce conformity as much as rigor. The mandatrophy is resolved by acknowledging that this reading is a contested position, not a universal truth.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    methodological_rigor_versus_gatekeeping_conflation,
    'Does credentialing enforce methodological rigor, or does it primarily enforce conformity to disciplinary orthodoxy?',
    'Empirical analysis of acceptance rates by methodology type; correlation between peer review outcomes and subsequent epistemic validity (replication, generalizability, real-world impact); comparison of credentialed vs non-credentialed knowledge producers on identical methodological criteria',
    'If credentialing enforces rigor: classification shifts toward Rope (genuine coordination). If credentialing enforces conformity: classification shifts toward Snare (gatekeeping extraction). Current evidence suggests partial conflation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(methodological_rigor_versus_gatekeeping_conflation, empirical, 'Whether credentialing enforces rigor or orthodoxy conformity').

omega_variable(
    alternative_validation_mechanisms_epistemic_adequacy,
    'Do non-credentialed knowledge systems (community science, indigenous methods, craft expertise) produce epistemically adequate validation through mechanisms other than peer review?',
    'Comparative epistemology: evaluate alternative validation systems on criteria of error-correction, replicability, predictive accuracy, and self-awareness of limitations; case studies where non-credentialed and credentialed systems study the same phenomena and compare outcomes',
    'If adequate: credentialing is revealed as arbitrary gatekeeping (Snare from most perspectives). If inadequate: credentialing represents a real epistemic function (Rope/Tangled Rope). Hybrid outcome likely: adequacy varies by domain.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_validation_mechanisms_epistemic_adequacy, conceptual, 'Whether alternative validation systems are epistemically adequate').

omega_variable(
    credentialing_cost_versus_epistemic_gain_tradeoff,
    'What is the ratio of epistemic gain (error prevention, rigor enforcement) to social cost (barriers to entry, exclusion of valuable knowledge, opportunity cost of gatekeeping labor)?',
    'Cost-benefit analysis: quantify false positive/negative rates, publication lag, career opportunity loss for excluded populations, labor hours spent in peer review vs actual error detection. Longitudinal comparison of fields with high vs low credentialing barriers.',
    'If epistemic gain >> cost: justified extraction (Tangled Rope). If cost >> gain: unjustified extraction (Snare). If comparable: mandatrophy unresolved.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(credentialing_cost_versus_epistemic_gain_tradeoff, empirical, 'Cost-benefit ratio of credentialing barriers to epistemic gain').

omega_variable(
    reading_kernel_contestation_natural_law_versus_constructed,
    'Is the credentialed expertise boundary a natural property of how knowledge stabilizes (and thus inherent to any complex knowledge system), or is it a contingent institutional arrangement that benefits specific actors?',
    'Comparative historical analysis: trace how credentialing developed in different disciplines; identify moments when alternative validation systems were displaced by credentialing and examine whether displacement was epistemic or political; examine domains where non-credentialed systems produce equivalent epistemic outcomes',
    'If natural law: mountain classification justified; credentialing is immutable. If constructed: false summit classification justified; credentialing is a choice that distributes benefits asymmetrically. This omega instantiates the kernel contestation between this reading and the experiential_pluralism and hybrid_coproduction readings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_kernel_contestation_natural_law_versus_constructed, conceptual, 'Natural law vs constructed nature of credentialed expertise boundary (reading-level kernel contestation)').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimate_knowledge_boundary__credentialed_expertise_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legit_cred_tr_t0, legitimate_knowledge_boundary__credentialed_expertise_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(legit_cred_tr_t10, legitimate_knowledge_boundary__credentialed_expertise_reading, theater_ratio, 10, 0.5).
narrative_ontology:measurement(legit_cred_tr_t20, legitimate_knowledge_boundary__credentialed_expertise_reading, theater_ratio, 20, 0.65).

% Extraction over time
narrative_ontology:measurement(legit_cred_be_t0, legitimate_knowledge_boundary__credentialed_expertise_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(legit_cred_be_t10, legitimate_knowledge_boundary__credentialed_expertise_reading, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(legit_cred_be_t20, legitimate_knowledge_boundary__credentialed_expertise_reading, base_extractiveness, 20, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(legit_cred_su_t0, legitimate_knowledge_boundary__credentialed_expertise_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(legit_cred_su_t10, legitimate_knowledge_boundary__credentialed_expertise_reading, suppression_requirement, 10, 0.62).
narrative_ontology:measurement(legit_cred_su_t20, legitimate_knowledge_boundary__credentialed_expertise_reading, suppression_requirement, 20, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimate_knowledge_boundary__credentialed_expertise_reading, information_standard).
narrative_ontology:affects_constraint(legitimate_knowledge_boundary__credentialed_expertise_reading, legitimate_knowledge_boundary__experiential_pluralism_reading).
narrative_ontology:affects_constraint(legitimate_knowledge_boundary__credentialed_expertise_reading, legitimate_knowledge_boundary__hybrid_coproduction_reading).
narrative_ontology:affects_constraint(legitimate_knowledge_boundary__credentialed_expertise_reading, journal_prestige_gatekeeping).
narrative_ontology:affects_constraint(legitimate_knowledge_boundary__credentialed_expertise_reading, citation_metrics_substitution).

% DUAL FORMULATION NOTE:
% The legitimate_knowledge_boundary kernel decomposes into three structurally distinct constraints, one per reading. The credentialed_expertise_reading is upstream of journal_prestige_gatekeeping and citation_metrics_substitution; those constraints operationalize the credentialing boundary through specific institutional mechanisms. The experiential_pluralism and hybrid_coproduction readings challenge the entire frame and would produce different downstream constraint structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(legitimate_knowledge_boundary__credentialed_expertise_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
