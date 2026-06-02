% ============================================================================
% CONSTRAINT STORY: anthropological_record__naturalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_anthropological_record__naturalist_reading, []).

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
 *   constraint_id: anthropological_record__naturalist_reading
 *   human_readable: Anthropological Record as Naturalist Epistemology (Materialist Human Origins)
 *   domain: epistemology/philosophy_of_science/anthropology
 *
 * SUMMARY:
 *   The naturalist reading of the anthropological record frames human origins
 *   as materially explicable through evolutionary biology, paleontology, and
 *   archaeological evidence—knowable via the scientific method without
 *   recourse to supernatural causation. This constraint describes the
 *   institutional enforcement of this reading as the sole authoritative
 *   epistemology for understanding human origins in academic and public
 *   discourse. The constraint exhibits tangled rope structure: genuine
 *   coordination occurs through shared naturalist methodology enabling
 *   collaborative research and cumulative knowledge-building, while
 *   simultaneous extraction occurs through credentialing gatekeeping that
 *   suppresses non-conforming interpretations and excludes non-credentialed
 *   alternative frameworks. The measurement data shows increasing suppression
 *   (0.40→0.65) over 100 years as institutional enforcement has matured,
 *   while extractiveness has increased (0.35→0.58) as the natural authority
 *   has expanded. Theater ratio remains relatively low (0.48-0.52),
 *   indicating the naturalist framework sustains legitimacy through genuine
 *   empirical productivity rather than pure performative
 *   theater—distinguishing this from degraded piton constraints.
 *
 * KEY AGENTS:
 *   - Scientific Institutional Authority: Primary beneficiary (institutional/arbitrage) — establishes naturalist framework as the sole legitimate epistemology for human origins; benefits from monopoly authority and credentialing control
 *   - Non-Credentialed Interpreters: Primary victim (powerless/trapped) — systematically excluded from authoritative interpretation through institutional gatekeeping; no pathway to legitimacy within the framework
 *   - Alternative Origin Narratives (Supernatural/Indigenous): Secondary victim (powerless/identity_locked) — cognitively bound to reject naturalist premises; suppression requires abandoning identity itself, not merely changing method
 *   - Scientific Researchers: Mixed position (powerful/mobile) — benefit from career advancement within framework but also constrained by enforcement of naturalist-only legitimacy
 *   - Public Stakeholders: Moderate constraint (moderate/constrained) — benefit from standardized knowledge dissemination but bear suppression cost through delegitimization of alternative narratives in public institutions
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing institutional arrangement as necessary logical boundary of scientific method
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(anthropological_record__naturalist_reading, 0.58).
domain_priors:suppression_score(anthropological_record__naturalist_reading, 0.65).
domain_priors:theater_ratio(anthropological_record__naturalist_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(anthropological_record__naturalist_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(anthropological_record__naturalist_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(anthropological_record__naturalist_reading, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(anthropological_record__naturalist_reading, tangled_rope).
narrative_ontology:human_readable(anthropological_record__naturalist_reading, "Anthropological Record as Naturalist Epistemology (Materialist Human Origins)").
narrative_ontology:topic_domain(anthropological_record__naturalist_reading, "epistemology/philosophy_of_science/anthropology").

domain_priors:requires_active_enforcement(anthropological_record__naturalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(anthropological_record__naturalist_reading, '9080e82f-da0b-4ca9-bb2e-78eb0e61e5ba').
narrative_ontology:cs_kernel_codification('9080e82f-da0b-4ca9-bb2e-78eb0e61e5ba', fixed_text).
narrative_ontology:cs_authority_grounding('9080e82f-da0b-4ca9-bb2e-78eb0e61e5ba', expertise).
narrative_ontology:cs_interpretation_layer_present('9080e82f-da0b-4ca9-bb2e-78eb0e61e5ba').
narrative_ontology:cs_reading_relation('9080e82f-da0b-4ca9-bb2e-78eb0e61e5ba', anthropological_record__creationist_reading, coexists_with).
narrative_ontology:cs_reading_relation('9080e82f-da0b-4ca9-bb2e-78eb0e61e5ba', anthropological_record__indigenous_epistemology_reading, influences).
narrative_ontology:cs_axiom('9080e82f-da0b-4ca9-bb2e-78eb0e61e5ba', foundational, material_causation_completeness).
narrative_ontology:cs_axiom_status(material_causation_completeness, holdable).
narrative_ontology:cs_axiom_grounding('9080e82f-da0b-4ca9-bb2e-78eb0e61e5ba', material_causation_completeness, empirically_contingent).
narrative_ontology:cs_axiom('9080e82f-da0b-4ca9-bb2e-78eb0e61e5ba', foundational, methodological_naturalism_necessity).
narrative_ontology:cs_axiom_status(methodological_naturalism_necessity, holdable).
narrative_ontology:cs_axiom_grounding('9080e82f-da0b-4ca9-bb2e-78eb0e61e5ba', methodological_naturalism_necessity, deontological).
narrative_ontology:cs_reference_frame('9080e82f-da0b-4ca9-bb2e-78eb0e61e5ba', empirical_materialist_epistemology).
narrative_ontology:cs_drift_state('9080e82f-da0b-4ca9-bb2e-78eb0e61e5ba', contemporary_genetic_era, gap(authority_erosion, minor, false)).
narrative_ontology:cs_created_at('9080e82f-da0b-4ca9-bb2e-78eb0e61e5ba', '').
narrative_ontology:cs_kernel_id(anthropological_record__naturalist_reading, anthropological_record).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(anthropological_record__naturalist_reading, credentialed_scientific_establishment).
narrative_ontology:constraint_beneficiary(anthropological_record__naturalist_reading, secular_institutional_authority).
narrative_ontology:constraint_victim(anthropological_record__naturalist_reading, non_credentialed_interpreters).
narrative_ontology:constraint_victim(anthropological_record__naturalist_reading, alternative_origin_narratives).
narrative_ontology:constraint_victim(anthropological_record__naturalist_reading, interpretive_pluralism).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NON-CREDENTIALED INTERPRETER (SNARE) — Systematically excluded from authoritative interpretation of the anthropological record. No institutional pathway to legitimacy; suppressed through credentialing gatekeeping and institutional accreditation requirements. Bears full cost of exclusion from epistemically authoritative discourse.
constraint_indexing:constraint_classification(anthropological_record__naturalist_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ALTERNATIVE ORIGIN NARRATIVE (SNARE, IDENTITY-LOCKED) — Cannot enter the authorized discourse without accepting naturalist premises that contradict its foundational identity. The binding mechanism is cognitive: the narrative's core identity claim (divine/ancestral origin) is constituted through rejection of naturalist materialism. Exit from suppression would require abandoning the identity itself, not merely changing interpretation methods.
constraint_indexing:constraint_classification(anthropological_record__naturalist_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 3: SCIENTIFIC RESEARCHER (TANGLED ROPE) — Benefits from naturalist framework through career advancement, funding, and institutional legitimacy. Also coordinated by it—genuine research collaboration and knowledge-building occur through shared naturalist method. But extraction occurs: researchers face suppression of non-conforming interpretations, pressure to exclude alternative frameworks from public discourse, and gatekeeping of who gets to participate in 'valid' interpretation.
constraint_indexing:constraint_classification(anthropological_record__naturalist_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: SCIENTIFIC INSTITUTIONAL AUTHORITY (ROPE) — Primary beneficiary with arbitrage options. Maintains institutional legitimacy through credentialing gatekeeping and enforcement of naturalist-only epistemology as 'objective' knowledge. Experiences the constraint as coordination: shared methodological standards enable collaborative knowledge production. Net benefit from the constraint structure far exceeds any coordination costs.
constraint_indexing:constraint_classification(anthropological_record__naturalist_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: PUBLIC STAKEHOLDER (TANGLED ROPE) — Constrained by institutional authority's claim to exclusive interpretive legitimacy. Receives coordination benefit from standardized knowledge (schools, textbooks, shared scientific framework). But bears suppression cost: alternative interpretations are systematically delegitimized in public institutions, constraining interpretive pluralism and minority narrative transmission.
constraint_indexing:constraint_classification(anthropological_record__naturalist_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW (MOUNTAIN) — From a universal/civilizational perspective, the constraint appears as an immutable feature of empirical knowledge-building: any effort to establish objective facts about material human origins requires methodological exclusion of untestable supernatural claims. The naturalist framework is not contingent institutional gatekeeping but a necessary boundary condition for the practice of science itself. However, this perspective risks false-summit classification: the 'necessity' may be institutional rather than logical.
constraint_indexing:constraint_classification(anthropological_record__naturalist_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(anthropological_record__naturalist_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(anthropological_record__naturalist_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(anthropological_record__naturalist_reading, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(anthropological_record__naturalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(anthropological_record__naturalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The primary extraction mechanism is credentialing gatekeeping that excludes non-conforming frameworks from institutional legitimacy. The naturalist reading benefits from monopoly authority over human origins discourse—alternative interpreters bear the cost of exclusion. However, the constraint is not pure extraction (Snare) because genuine coordination also occurs: the shared naturalist methodology enables collaborative knowledge production, cumulative evidence-building, and institutional division of research labor. The tangled rope classification captures both the coordination benefit (researchers can work together through standardized methods) and the asymmetric extraction (alternative frameworks are suppressed, non-credentialed interpreters are excluded). Suppression (0.65): High. Multiple suppression mechanisms operate: (a) credentialing requirements that exclude non-institutionally-trained interpreters, (b) publication gatekeeping through peer review that systematically rejects non-naturalist frameworks, (c) educational gatekeeping through institutional control of anthropology and biology curricula, (d) legitimacy denial in public discourse. The measurement trajectory (0.40→0.65) reflects intensification of institutional enforcement over the past century. Theater ratio (0.48): Relatively low. The naturalist framework maintains legitimacy through genuine empirical productivity—fossil records, genetic evidence, archaeological findings—rather than pure performative theater. This low theater distinguishes the constraint from piton degradation. The slight decline in theater (0.52→0.48) may reflect increasing empirical sophistication, though interpretation here is uncertain.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap in this constraint is exceptionally wide, reflecting deep incommensurability between framework assumptions. The scientific institutional authority (Rope) experiences the constraint as unproblematic coordination—shared methodology enables knowledge-building, suppression of supernatural claims is justified gatekeeping of false or non-testable claims, credentialing requirements ensure rigor. The non-credentialed interpreter (Snare) experiences maximal extraction—systematic exclusion from authoritative discourse, no pathway to institutional legitimacy, suppression justified by institutional authority's claim to monopoly on valid interpretation. The alternative narrative holder (identity-locked Snare) experiences the constraint as identity dissolution—accepting naturalist premises would require rejecting the foundational identity claim. The scientific researcher (Tangled Rope) experiences mixed position—career benefits through framework participation but cognitive constraint from enforcement of naturalist-only legitimacy. The public stakeholder (Tangled Rope) experiences the constraint as educational gatekeeping—benefits from standardized knowledge but bears suppression cost through delegitimization of alternative narratives. The analytical observer risks the false-summit error: the constraint appears as immutable natural law (Mountain) at civilizational scope—'science must exclude untestable claims'—but this naturalizes institutional gatekeeping choices as logical necessities. The perspectival gap is not merely about what empirical facts mean, but about whether the constraint is (a) justified exclusion of actually false claims, (b) justified exclusion of non-testable claims from science (while allowing them outside science), or (c) extractive monopoly gatekeeping using methodology as cover.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality for each perspective is derived from agent power level + exit options + beneficiary/victim status, computed through the sigmoid f(d) function. Scientific institutional authority occupies the beneficiary + arbitrage position (d≈0.05, f(d)≈-0.12)—they have optionality to leave (if they abandoned naturalist commitment, they could exit and establish alternative authority system) and benefit from the constraint, yielding negative or near-zero experienced extraction. Non-credentialed interpreters occupy the victim + trapped position (d≈0.95, f(d)≈1.42)—they cannot escape institutional gatekeeping without abandoning hopes for institutional legitimacy, and they bear the suppression cost, yielding maximum experienced extraction. Alternative narrative holders occupy the victim + identity_locked position (d≈0.89, f(d)≈1.28)—they are not structurally trapped (some could in principle exit) but identity-fused with rejection of naturalism, producing high experienced extraction that differs from trapped victims only in that cognitive reframing could theoretically enable exit. Scientific researchers occupy a mixed position: as beneficiaries they have d≈0.15, but the constraint also limits their interpretive options, suggesting moderate upward adjustment. The analytical observer at civilizational scope occupies the canonical analytical position (d≈0.73), experiencing the constraint as potential false summit—natural law from the civilizational perspective, institutional gatekeeping from closer observational positions.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by clarifying that 'tangled rope' is the correct classification precisely when genuinely coordination and genuine extraction coexist. The naturalist framework produces real coordination value (collaborative research, cumulative evidence-building, institutional knowledge transmission) AND real extraction value (gatekeeping of alternative frameworks, suppression of non-credentialed interpreters, enforcement of epistemic monopoly). The temptation to misclassify is high: (a) to classify as Rope (pure coordination) if emphasizing the genuine research collaboration enabled by shared naturalist methodology, (b) to classify as Snare (pure extraction) if emphasizing the gatekeeping and suppression. But tangled rope is correct: both mechanisms are present and structurally necessary to each other. The coordination function requires gatekeeping—you cannot have collaborative naturalist science without excluding supernatural explanations from the collaborative framework. The extraction function depends on the coordination benefit—the suppression would not persist if the framework produced no genuine knowledge value. The constraint cannot be either pure coordination or pure extraction; it is irreducibly hybrid. The mandatrophy is resolved by recognizing that this is exactly what tangled rope classification is for.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    naturalism_as_method_vs_ontology,
    'Does methodological naturalism (using natural causes in scientific explanation) necessarily entail ontological naturalism (only material causes exist in reality)?',
    'Logical analysis of the boundary between methodological constraint and metaphysical claim; examination of whether excluding supernatural claims from science commits science to denying their existence vs. merely declining to adjudicate them',
    'If identical: naturalist reading correctly frames science as incompatible with supernatural origins. If distinct: naturalist reading commits extractive overreach—suppressing alternative ontologies that science cannot actually falsify, not merely excluding them from scientific method.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(naturalism_as_method_vs_ontology, conceptual, 'Whether methodological naturalism entails ontological naturalism').

omega_variable(
    credentialing_boundary_justification,
    'What justifies the boundary between credentialed scientific interpretation and non-credentialed interpretation of the anthropological record? Is it purely methodological rigor or does it include suppression of non-conforming frameworks?',
    'Historical analysis of credentialing standards: do they exclude non-credentialed interpreters based on methodological insufficiency alone, or do they systematically exclude frameworks that reach non-materialist conclusions independent of methodological defect?',
    'If purely methodological: suppression is extraction justified by genuine epistemic standards. If includes framework suppression: suppression is extractive gatekeeping using methodology as cover.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(credentialing_boundary_justification, empirical, 'Whether credentialing boundaries are purely methodological or include framework suppression').

omega_variable(
    alternative_narrative_empirical_status,
    'Are supernatural/indigenous origin narratives empirically falsified by the anthropological record, or merely non-testable via naturalist methods?',
    'Logical analysis: can the anthropological evidence rule out (a) divine guidance of evolutionary process, (b) ancestral creative acts occurring within the natural timeline, (c) metaphorical readings of material evidence? Or does naturalism exclude these frameworks without actual empirical disproof?',
    'If falsified: suppression is justified exclusion of false claims. If merely non-testable: suppression is extractive enforcement of a framework choice masquerading as empirical refutation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_narrative_empirical_status, conceptual, 'Whether alternative narratives are empirically falsified or merely non-testable').

omega_variable(
    institutional_benefit_from_materialist_monopoly,
    'How much of the credentialing gatekeeping and suppression of alternative frameworks is justified by genuine epistemic standards vs. institutional interest in maintaining monopoly authority over human origins discourse?',
    'Comparative institutional analysis: do credentialing standards exclude non-materialist frameworks more strictly than they exclude other non-testable claims? Do institutional actors resist interpretive pluralism even when pluralism would not undermine empirical validity?',
    'If primarily justified: constraint is tangled rope with warranted gatekeeping. If includes institutional monopoly interest: constraint is snare with extraction masked as methodology.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_benefit_from_materialist_monopoly, empirical, 'Degree of institutional benefit from naturalist monopoly on origins discourse').

omega_variable(
    identity_lock_mechanism_for_alternatives,
    'For indigenous and supernatural origin narratives, is the binding mechanism primarily structural (economic/institutional exclusion from discourse) or identity-constituted (the narrative identity cannot coexist with materialist premises)?',
    'Ethnographic and autobiographical analysis: do suppressed interpreters report barriers as external constraints (gatekeeping they could overcome if resources changed) or as internal identity fusions (accepting naturalism would require ceasing to be the person/community they are)?',
    'If structural: exit is possible with institutional change. If identity-locked: institutional change alone is insufficient; cognitive frame shift is required, which may be impossible without identity dissolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_for_alternatives, empirical, 'Whether alternative narrative suppression is structural or identity-constituted').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(anthropological_record__naturalist_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anthrec_nat_tr_t0, anthropological_record__naturalist_reading, theater_ratio, 0, 0.52).
narrative_ontology:measurement(anthrec_nat_tr_t50, anthropological_record__naturalist_reading, theater_ratio, 50, 0.5).
narrative_ontology:measurement(anthrec_nat_tr_t100, anthropological_record__naturalist_reading, theater_ratio, 100, 0.48).

% Extraction over time
narrative_ontology:measurement(anthrec_nat_be_t0, anthropological_record__naturalist_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(anthrec_nat_be_t50, anthropological_record__naturalist_reading, base_extractiveness, 50, 0.48).
narrative_ontology:measurement(anthrec_nat_be_t100, anthropological_record__naturalist_reading, base_extractiveness, 100, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(anthrec_nat_su_t0, anthropological_record__naturalist_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(anthrec_nat_su_t50, anthropological_record__naturalist_reading, suppression_requirement, 50, 0.58).
narrative_ontology:measurement(anthrec_nat_su_t100, anthropological_record__naturalist_reading, suppression_requirement, 100, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(anthropological_record__naturalist_reading, identity_coordination).
narrative_ontology:affects_constraint(anthropological_record__naturalist_reading, anthropological_record__creationist_reading).
narrative_ontology:affects_constraint(anthropological_record__naturalist_reading, anthropological_record__indigenous_epistemology_reading).
narrative_ontology:affects_constraint(anthropological_record__naturalist_reading, scientific_credentialing_gatekeeping).
narrative_ontology:affects_constraint(anthropological_record__naturalist_reading, public_discourse_naturalism_monopoly).

% DUAL FORMULATION NOTE:
% The anthropological record is a contested kernel with three distinct constraint stories corresponding to three readings: naturalist_reading (this story), creationist_reading, and indigenous_epistemology_reading. Each reading interprets the same empirical evidence (skeletal remains, genetic sequences, archaeological artifacts) through a different epistemological framework. Each story has its own extractiveness value reflecting the institutional enforcement costs specific to that reading. These are not three perspectives on one constraint, but three distinct constraints modeling three distinct epistemological regimes. Link them via network.affects_constraints to show that institutional enforcement of naturalist reading affects the viability of sibling readings, and vice versa.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(anthropological_record__naturalist_reading, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
