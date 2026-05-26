% ============================================================================
% CONSTRAINT STORY: husk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_husk_reading, []).

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
 *   constraint_id: husk_reading
 *   human_readable: Preparedness Theater: Ritualized Compliance Substitutes for Competence Retention
 *   domain: infrastructure_governance/disaster_preparedness
 *
 * SUMMARY:
 *   This constraint captures the husk reading of preparedness retention:
 *   memorial practices and routine compliance preserve institutional
 *   legitimacy and bureaucratic authority, but increasingly substitute
 *   performative demonstration of readiness for actual maintenance of
 *   response competence. A dam passes its required inspection. An emergency
 *   services unit completes its annual training drill. A critical
 *   infrastructure facility documents its preventive maintenance protocol.
 *   These rituals ostensibly demonstrate preparedness, but the husk reading
 *   proposes that they increasingly function as theater rather than as tests
 *   of genuine capacity. The constraint is a tangled rope because it
 *   simultaneously coordinates the transfer of standardized knowledge
 *   (genuine benefit) and extracts legitimacy from systems whose actual
 *   competence may have degraded. The divergence between ritual compliance
 *   and lived competence grows as operators rotate, as procedures ossify into
 *   canonical forms, and as institutional memory held in the hands of
 *   experienced practitioners is displaced by credential and documentation.
 *   The extractive beneficiary is institutional legitimacy — governments,
 *   authorities, and bureaucratic structures gain political and budgetary
 *   justification through demonstrated preparedness scores. The victim is
 *   actual response capacity — the embodied knowledge, pattern recognition,
 *   and adaptive decision-making that come from continuous practice in novel
 *   conditions.
 *
 * KEY AGENTS:
 *   - Institutional Legitimacy / Bureaucratic Authority: Primary beneficiary (institutional/arbitrage) — gains political credibility, budget justification, and regulatory license through passed inspections and completed drills
 *   - Actual Response Capacity: Primary victim (powerless/trapped) — cannot exit reliance on systems; bears cost when disasters reveal that ritual compliance did not preserve genuine competence
 *   - Field Operators / Emergency Responders: Secondary actor (moderate/constrained) — constrained by hierarchy and procedure, yet benefit from coordination structures (training, equipment, mutual aid) that routine compliance creates; experience tension between performance and competence
 *   - Generational Knowledge Transfer: Secondary victim (powerless/trapped) — experienced practitioners' tacit knowledge displaced by procedure documentation; younger operators trained on rules rather than patterns
 *   - Inter-Generational Transmission System: Organized agent (organized/constrained) — mentorship and apprenticeship structures both enabled and constrained by compliance framework; asymmetric extraction toward institutional procedure, away from embodied knowledge
 *   - Regulatory Framework: Institutional actor (institutional/arbitrage) — maintains standardized protocols whose verification function has atrophied (piton perspective); persists through inertia
 *   - Analytical Observer: Civilizational context (analytical/analytical) — risks naturalizing contingent institutional choice (prioritizing compliance over competence) as inevitable consequence of scale
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(husk_reading, 0.58).
domain_priors:suppression_score(husk_reading, 0.68).
domain_priors:theater_ratio(husk_reading, 0.81).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(husk_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(husk_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(husk_reading, theater_ratio, 0.81).

% --- Constraint claim ---
narrative_ontology:constraint_claim(husk_reading, tangled_rope).
narrative_ontology:human_readable(husk_reading, "Preparedness Theater: Ritualized Compliance Substitutes for Competence Retention").
narrative_ontology:topic_domain(husk_reading, "infrastructure_governance/disaster_preparedness").

domain_priors:requires_active_enforcement(husk_reading).

% --- Commitment system structure ---
narrative_ontology:cs_kernel_codification(husk_reading, distributed).
narrative_ontology:cs_authority_grounding(husk_reading, extraction).
narrative_ontology:cs_interpretation_layer_present(husk_reading).
narrative_ontology:cs_kernel_id(husk_reading, preparedness_retention).
narrative_ontology:cs_reading_relation(husk_reading, competence_reading, coexists_with).
narrative_ontology:cs_axiom(husk_reading, foundational, compliance_substitutes_for_competence).
narrative_ontology:cs_axiom_status(compliance_substitutes_for_competence, holdable).
narrative_ontology:cs_axiom_grounding(husk_reading, compliance_substitutes_for_competence, empirically_contingent).
narrative_ontology:cs_axiom(husk_reading, foundational, ritual_preserves_institutional_order).
narrative_ontology:cs_axiom_status(ritual_preserves_institutional_order, holdable).
narrative_ontology:cs_axiom_grounding(husk_reading, ritual_preserves_institutional_order, conventional).
narrative_ontology:cs_reference_frame(husk_reading, compliance_as_legitimacy).
narrative_ontology:cs_drift_state(husk_reading, contemporary_infrastructure_governance, gap(practice_drift, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(husk_reading, institutional_legitimacy).
narrative_ontology:constraint_beneficiary(husk_reading, bureaucratic_authority).
narrative_ontology:constraint_victim(husk_reading, actual_response_capacity).
narrative_ontology:constraint_victim(husk_reading, generational_knowledge_transfer).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISASTER-EXPOSED POPULATION (SNARE) — Trapped in dependence on infrastructure systems whose actual maintenance and response capacity may be performative rather than genuine. No exit option; cannot opt out of reliance on dams, levees, emergency services, or critical infrastructure. Bears full cost of the husk constraint when disasters reveal that drill-based legitimacy did not translate to real competence. Maximum extraction through false assurance.
constraint_indexing:constraint_classification(husk_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: FIELD OPERATOR/EMERGENCY RESPONDER (TANGLED ROPE) — Constrained by institutional hierarchy and career dependency, yet also benefits from coordination structures (training programs, equipment allocation, mutual aid networks) that the routine compliance system creates. Experiences tension between performing procedural compliance and maintaining actual competence. Extraction is moderate because some genuine coordination occurs alongside bureaucratic theater.
constraint_indexing:constraint_classification(husk_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: INFRASTRUCTURE AUTHORITY (ROPE) — Experiences the constraint as coordination: drills, inspections, and compliance routines ostensibly enable systemic reliability monitoring and knowledge transfer. The authority benefits from demonstrated preparedness (reputation, funding, licensing). Experiences extraction as asymmetry in favor of the authority — they capture legitimacy benefit from passing drills; field operators bear the labor cost.
constraint_indexing:constraint_classification(husk_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REGULATORY FRAMEWORK (PITON) — Maintains standardized drill and inspection protocols whose primary function (verifying actual response capacity) has atrophied, replaced by a secondary function (demonstrating institutional compliance). The framework persists through regulatory inertia and because no integrated alternative has fully replaced it. High theater ratio reflects that compliance is performed and documented, but actual competence validation is degraded.
constraint_indexing:constraint_classification(husk_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: INTER-GENERATIONAL TRANSMISSION SYSTEM (TANGLED ROPE) — Organized through mentorship, apprenticeship, and embodied knowledge transfer among experienced and novice operators. The routine compliance framework both enables (provides institutional scaffolding for training) and constrains (substitutes procedure documentation for lived experience; senior knowledge-holders deprioritized relative to credential-holders). Asymmetric extraction: institutional memory of how systems actually behave is displaced by rule-based compliance.
constraint_indexing:constraint_classification(husk_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, some erosion of hands-on competence in favor of documented procedures appears as an inherent consequence of scale and specialization: as systems become larger and more complex, documented compliance must substitute for individual mastery. This perspective risks naturalizing the husk dynamic as an unavoidable feature of modern infrastructure governance. The engine's false summit detector will flag this: the 'inherent to complexity' framing obscures the contingent institutional choice to prioritize demonstrable compliance over validated competence.
constraint_indexing:constraint_classification(husk_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(husk_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(husk_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(husk_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(husk_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(husk_reading, TR),
    TR >= 0.70.

:- end_tests(husk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts institutional legitimacy from populations who depend on infrastructure whose actual competence may be performative. The extraction is not maximal (0.80+) because some genuine coordination occurs — drills do transmit standardized knowledge, procedures do coordinate action, and compliance does create institutional scaffolding. But the extraction is substantial because the primary beneficiary (institutional authority) gains credibility disproportionate to actual risk reduction. Suppression (0.68): Moderate-high. Operators cannot easily refuse participation in drills or inspections. Field knowledge workers cannot openly declare that procedures are theater without career and institutional consequences. Populations cannot opt out of dependence on critical infrastructure. Generational knowledge holders cannot transfer embodied competence through formal channels (they are deprioritized relative to credentialed procedure-followers). Theater ratio (0.81): High and rising over the interval. At t=0 (recent establishment of standardized protocols), theater_ratio was 0.52 — procedures were still being tested against lived experience. By t=15-20, theater_ratio reaches 0.81 — compliance is documented and performed, but actual competence validation has atrophied. This trajectory reflects Goodhart drift: as procedures become the measure of preparedness, procedures become decoupled from the underlying competence they were meant to measure.
 *
 * PERSPECTIVAL GAP:
 *   The husk reading reveals deep perspectival divergence. The infrastructure authority and regulatory framework see the constraint as rope or piton (coordination or degraded ritual persisting through inertia). The field operators experience tangled rope — genuine coordination benefits alongside extractive performance pressure. The disaster-exposed population experiences snare — they have no exit and bear full cost if actual competence has degraded. The inter-generational transmission system experiences tangled rope — procedures create institutional scaffolding for training, but displace embodied knowledge transfer. The analytical observer risks mountain classification (seeing the husk as inevitable consequence of scale) — but the structural data reveals this is a false summit: the choice to prioritize compliance documentation over competence validation is contingent, not inherent to infrastructure governance.
 *
 * DIRECTIONALITY LOGIC:
 *   The primary beneficiary (institutional legitimacy/bureaucratic authority) has arbitrage exit options — they can shift funding, redefine compliance metrics, or deemphasize preparedness without material constraint. They experience low d, hence low or negative effective extraction (they gain legitimacy). The primary victim (disaster-exposed population) has trapped exit options — they cannot opt out of infrastructure dependence. They experience high d, hence high effective extraction (they bear cost of false assurance). Field operators have constrained exit (can exit individual roles at career cost but cannot exit the institutional system) — they experience moderate d and moderate extraction. The institutional regulatory framework has arbitrage exit (can redefine protocols) but maintains them through inertia — this produces the piton classification. Inter-generational knowledge holders are trapped in credential systems that devalue their expertise — high d, experiencing extraction despite their moderate nominal power.
 *
 * MANDATROPHY ANALYSIS:
 *   The husk reading resolves the mandatrophy by instantiating one coherent reading of the preparedness_retention kernel: institutional legitimacy through ritualized compliance. This reading is NOT the only possible reading (the competence reading instantiates a different, structurally distinct claim), but it is a complete, internally consistent constraint story with stable epsilon and clear beneficiary/victim structure. The mandatrophy would arise if we tried to force both readings into a single constraint — that would require epsilon to shift (0.58 for husk, different value for competence) and beneficiary/victim to shift (legitimacy is beneficiary in husk; actual capacity is beneficiary in competence). By decomposing into separate constraint stories, each reading gets its own epsilon, its own perspectives, and its own classification. They are linked via network.affects_constraints to show structural kinship.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ritual_competence_divergence,
    'At what point do drill-passing and actual response capacity diverge structurally such that the routine no longer tests real competence?',
    'Post-disaster analysis: correlation between pre-disaster drill performance and actual response success/failure; identification of gaps between standardized scenarios and real-world conditions; interviews with field operators on perception of drill authenticity',
    'If divergence is early and severe (< 3 years of successful drills before competence degrades): the constraint is pure extraction (Snare), theater is primary function. If divergence is gradual (5-10 years): tangled rope classification is stable, coordination genuine but increasingly performative. If divergence is negligible: rope classification is correct, and the husk reading is misclassifying a healthy system.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ritual_competence_divergence, empirical, 'Divergence between drill performance and actual response capacity').

omega_variable(
    tacit_knowledge_retention_mechanism,
    'Can documented procedures preserve the tacit, embodied knowledge required for effective response under novel conditions, or does procedure documentation structurally displace the human pattern-recognition and adaptive capacity that experience builds?',
    'Cognitive load studies comparing novices using documented procedures vs experienced operators using intuition; analysis of failure modes in novel scenarios for procedure-trained vs apprenticeship-trained responders; longitudinal tracking of operator confidence and decision speed',
    'If procedures preserve competence: the system is Rope or Tangled Rope with genuine coordination value. If procedures displace tacit knowledge: the husk reading is correct; institutional legitimacy is built on procedural compliance that cannot handle novel conditions (most disasters are novel relative to drills).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tacit_knowledge_retention_mechanism, empirical, 'Whether procedures preserve or displace tacit operational knowledge').

omega_variable(
    generational_knowledge_loss_acceleration,
    'Does the routine compliance framework accelerate knowledge loss across generational transitions by treating documented procedure as complete, making veteran operators seem redundant or outdated?',
    'Comparison of knowledge transfer effectiveness before and after compliance standardization; analysis of hiring patterns (credential preference vs experience preference); tracking of operator retention and early retirement patterns around policy changes emphasizing standardized procedures',
    'If accelerated: the husk reading''s ''generational knowledge transfer victim'' status is confirmed. If no acceleration: the constraint is less extractive regarding knowledge transfer; the victim category should be reclassified.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(generational_knowledge_loss_acceleration, empirical, 'Whether compliance framework accelerates generational knowledge loss').

omega_variable(
    legitimacy_extraction_mechanism,
    'Who specifically benefits from the institutional legitimacy gained through demonstrated compliance when the compliance may not reflect actual competence?',
    'Stakeholder benefit analysis: funding allocation tied to compliance scores; career advancement patterns of administrators vs field operators; insurance/liability implications of demonstrating compliance; political capital from preparedness narratives',
    'If benefit is diffuse: the constraint is more Rope (coordination). If benefit is concentrated (administrators, budget holders, politicians): the constraint is more Snare or Tangled Rope, extraction is clearer.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(legitimacy_extraction_mechanism, empirical, 'Who benefits from institutional legitimacy through compliance').

omega_variable(
    competence_reading_coexistence,
    'Can the competence reading (maintenance of actual response capacity through continuous live practice) coexist with the husk reading (institutional legitimacy through ritualized compliance) in the same governance framework, or does one reading structurally foreclose the other?',
    'Institutional design analysis: examination of cases where both high compliance performance and high disaster response success are maintained simultaneously; identification of structural features that enable or prevent coexistence; analysis of resource allocation trade-offs',
    'If coexistence is possible: readings coexist_with each other (different parties optimize for different outcomes). If zero-sum (budget allocation, operator time, institutional priority): readings influence or foreclose each other. The relation type (coexists_with vs influences vs forecloses) determines how the engine models the kernel.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competence_reading_coexistence, conceptual, 'Structural coexistence of competence and husk readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(husk_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(husk_tr_t0, husk_reading, theater_ratio, 0, 0.52).
narrative_ontology:measurement(husk_tr_t8, husk_reading, theater_ratio, 8, 0.68).
narrative_ontology:measurement(husk_tr_t15, husk_reading, theater_ratio, 15, 0.81).
narrative_ontology:measurement(husk_tr_t20, husk_reading, theater_ratio, 20, 0.83).

% Extraction over time
narrative_ontology:measurement(husk_be_t0, husk_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(husk_be_t8, husk_reading, base_extractiveness, 8, 0.5).
narrative_ontology:measurement(husk_be_t15, husk_reading, base_extractiveness, 15, 0.58).
narrative_ontology:measurement(husk_be_t20, husk_reading, base_extractiveness, 20, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(husk_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(husk_reading, competence_reading).

% DUAL FORMULATION NOTE:
% The preparedness_retention kernel decomposes into two structurally distinct constraints with different epsilon values and beneficiary structures. The husk_reading (ε=0.58, institutional legitimacy as beneficiary) models preparedness preservation through ritualized compliance. The competence_reading (sibling constraint) models preparedness preservation through continuous validation of actual response capacity. The kernel itself is contested — different institutional actors and governance frameworks weight the readings differently. Network link shows downstream influence: as husk reading dominates (compliance becomes the measure of preparedness), the competence reading's structural conditions become harder to maintain.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
