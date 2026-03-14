% ============================================================================
% CONSTRAINT STORY: intelligent_design_methodology
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_intelligent_design_methodology, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: intelligent_design_methodology
 *   human_readable: Intelligent Design as a Methodological Constraint on Evolutionary Biology
 *   domain: philosophy_of_science/biology/epistemology
 *
 * SUMMARY:
 *   Intelligent Design (ID) methodology presents a structural constraint on
 *   evolutionary biology education and research that exhibits characteristics
 *   of both pure extraction (snare) and genuine coordination (tangled rope).
 *   The constraint operates across multiple institutional levels: curriculum
 *   mandates, textbook publication markets, scientific education policy, and
 *   legal frameworks regulating classroom speech. From the perspective of
 *   evolutionary biologists and educators, ID methodology claims create
 *   asymmetric extraction: biologists bear suppression costs (defending
 *   naturalism, curriculum challenges, legal exposure) while beneficiaries
 *   (religious traditionalist coalitions, politically mobilized groups) gain
 *   institutional access and legitimacy claims without bearing equivalent
 *   epistemic burdens. However, from the perspective of cultural pluralism
 *   advocates and some educators, the constraint represents genuine
 *   coordination: presenting multiple frameworks for understanding life's
 *   diversity acknowledges genuine worldview differences and can improve
 *   science education's cultural legitimacy. The theater_ratio (0.68)
 *   reflects that much institutional engagement with ID methodology debate is
 *   performative — academic conferences, educational panels, and
 *   philosophical discussions continue despite settled empirical questions,
 *   sustained through legitimacy rituals rather than unresolved
 *   methodological problems.
 *
 * KEY AGENTS:
 *   - Evolutionary Biology Field: Primary victim (powerless/trapped) — cannot exit constraint without abandoning teaching legitimacy in contested jurisdictions; bears suppression costs through legal challenges, curriculum mandates, reputational pressure
 *   - Religious Traditionalist Coalitions: Primary beneficiary (institutional/arbitrage) — gain policy access and institutional legitimacy for worldview representation without bearing equivalent epistemic costs; can exit without constraint
 *   - Biology Educators and Textbook Publishers: Secondary victims (moderate/constrained) — constrained by policy mandates and parental pressure; also benefit through market segmentation and pluralistic framing authority
 *   - Scientific Standards Organizations: Organized actors (organized/constrained) — view ID debate as transitional coordination problem; building pathways to sunset through improved naturalistic explanations and scientific literacy
 *   - Philosophy of Science Institutions: Institutional performers (institutional/arbitrage) — maintain performative engagement rituals despite settled core questions; sustain theater through disciplinary legitimacy mechanisms
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent institutional arrangements as inherent methodological boundaries
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(intelligent_design_methodology, 0.58).
domain_priors:suppression_score(intelligent_design_methodology, 0.72).
domain_priors:theater_ratio(intelligent_design_methodology, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(intelligent_design_methodology, extractiveness, 0.58).
narrative_ontology:constraint_metric(intelligent_design_methodology, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(intelligent_design_methodology, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(intelligent_design_methodology, tangled_rope).
narrative_ontology:human_readable(intelligent_design_methodology, "Intelligent Design as a Methodological Constraint on Evolutionary Biology").
narrative_ontology:topic_domain(intelligent_design_methodology, "philosophy_of_science/biology/epistemology").

domain_priors:requires_active_enforcement(intelligent_design_methodology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(intelligent_design_methodology, religious_traditionalist_coalitions).
narrative_ontology:constraint_beneficiary(intelligent_design_methodology, political_movements_opposing_materialist_frameworks).
narrative_ontology:constraint_victim(intelligent_design_methodology, evolutionary_biology_epistemic_integrity).
narrative_ontology:constraint_victim(intelligent_design_methodology, science_education_consensus).
narrative_ontology:constraint_victim(intelligent_design_methodology, naturalistic_explanation_frameworks).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EVOLUTIONARY BIOLOGY EPISTEMIC COMMONS (SNARE) — The field cannot exit the constraint without abandoning teaching legitimacy in contested jurisdictions. Faces extraction through mandatory curriculum inclusion, legal challenges, and reputational pressure. Bears full cost of defending explanatory naturalism against non-naturalistic alternatives presented as methodologically equivalent. No alternative pathway available; suppression operates through legal/political mechanisms and educational policy.
constraint_indexing:constraint_classification(intelligent_design_methodology, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: EDUCATORS AND PUBLISHERS (TANGLED ROPE) — Constrained by curriculum mandates, parental pressure, and legal exposure, but also benefit from the constraint through coordination of pluralistic framing and textbook market segmentation. Some genuine coordination function (presenting scientific uncertainty about mechanisms) coexists with asymmetric extraction (burden of defending naturalism against political challenge while maintaining pedagogical authority).
constraint_indexing:constraint_classification(intelligent_design_methodology, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: RELIGIOUS TRADITIONALIST COALITIONS (ROPE) — Experience the constraint as pure coordination: reconciling theological frameworks with curriculum content. Net beneficiary through political access to education policy and institutional legitimacy claims. Can exit without cost (no binding constraint on their beliefs); the constraint enables their preferred worldview representation in schools.
constraint_indexing:constraint_classification(intelligent_design_methodology, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: SCIENTIFIC STANDARDS ORGANIZATIONS (SCAFFOLD) — Organized bodies (NAS, AAAS, state science standards committees) see ID methodology debates as a temporary coordination problem with an intended sunset: as naturalistic explanations improve in explanatory power and public scientific literacy increases, the pressure to present ID as methodologically equivalent diminishes. The constraint is viewed as a transitional scaffolding for managing cultural pluralism during scientific consensus maturation.
constraint_indexing:constraint_classification(intelligent_design_methodology, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: PHILOSOPHY OF SCIENCE INSTITUTIONS (PITON) — The debate over ID's methodological status has become largely performative. The philosophical and empirical arguments have been extensively litigated; the constraint persists through institutional theater (ongoing interdisciplinary engagement rituals, conference panels, legitimacy performances) despite the field's consensus on the question. The performative aspect sustains the constraint's visibility long after its functional resolution.
constraint_indexing:constraint_classification(intelligent_design_methodology, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, the constraint appears as an inherent feature of scientific methodology itself: science cannot adjudicate theological claims, and the boundary between naturalistic and non-naturalistic explanation is an immutable feature of the scientific epistemic frame. However, the structural data reveals this as a false summit — the constraint is contingent on specific institutional configurations (education policy, textbook markets, legal frameworks), not inherent to science's logical structure.
constraint_indexing:constraint_classification(intelligent_design_methodology, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(intelligent_design_methodology_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(intelligent_design_methodology, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(intelligent_design_methodology, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(intelligent_design_methodology, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(intelligent_design_methodology, TR),
    TR >= 0.70.

:- end_tests(intelligent_design_methodology_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts through policy enforcement, legal exposure, and suppression of naturalistic explanation without compensating beneficiaries' frameworks with equivalent empirical power. However, 0.58 reflects that some genuine coordination function exists — plural framings can serve pluralistic publics. The value is lower than pure snare (which would suggest ~0.72) because real coordination benefits exist alongside extraction. Suppression (0.72): High. Multiple suppression mechanisms operate: legal barriers (curriculum challenges, courtroom precedent), institutional barriers (journal publication standards, professional legitimacy), and epistemic barriers (methodological commitments treating design inference as non-naturalistic). Suppression is not absolute — alternatives are expressed and defended — but significant barriers constrain naturalistic explanation's institutional presence. Theater ratio (0.68): High. The ongoing academic engagement with ID methodology questions occurs despite settled empirical consensus on evolutionary mechanisms' explanatory power. Legitimacy rituals (conference panels, educational debates, philosophical discussions) persist through institutional inertia rather than unresolved theoretical problems. The theater increased over the interval as the debate became more politically institutionalized.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximal perspectival divergence. Religious traditionalists see pure coordination (Rope) — presenting multiple worldviews as methodologically equivalent serves pluralistic inclusion. Scientific standards organizations see transitional scaffolding (Scaffold) — the constraint exists to bridge cultural disagreement while naturalistic explanations improve and scientific consensus matures. Philosophers of science see performative ritual (Piton) — the methodological debate persists through institutional engagement despite epistemic settlement. Educators see mixed coordination and extraction (Tangled Rope) — they coordinate pluralistic curriculum while bearing burden of defending naturalism. The evolutionary biology field sees pure extraction (Snare) — methodological demands without epistemic reciprocity, policy pressure without explanatory justification. The civilizational analytical observer risks seeing an immutable boundary (Mountain) — science's methodological naturalism as intrinsic to what science is — but the structural data reveals this as a false summit: the boundary is sustained by contingent institutional configurations (education law, journal standards, professional norms), not by the logical structure of science itself.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) are derived from structural relationships: who benefits, who bears costs, and at what cost. Religious traditionalist coalitions hold d ≈ 0.10-0.15 (beneficiaries with arbitrage exit options — they can abandon the constraint without material cost). Evolutionary biologists hold d ≈ 0.85-0.90 (victims with trapped exit options — they cannot teach without addressing the constraint in many jurisdictions). Educators hold d ≈ 0.60-0.70 (victims with constrained exit — high cost but possible departure). The analytical observer holds d ≈ 0.72-0.75 (analytical position on contested methodological question). These d values feed the sigmoid f(d) to produce experienced extractiveness chi. Beneficiaries with low d experience negative or near-zero chi (they see coordination, not extraction); victims with high d experience amplified chi (high f(d) multiplier). The identity_locked exit option would apply to biologists whose professional identity is constitutively tied to naturalistic methodology — exit would require reconstituting their scientific self-concept.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by revealing that classification varies legitimately across observation positions because the structural relationships truly differ. From the beneficiary's position (religious traditionalist), the constraint is genuine coordination serving pluralism — classification as Rope is their authentic experience. From the field's position (evolutionary biology), the constraint is pure extraction without epistemic reciprocity — classification as Snare is their authentic experience. The snare classification (primary field perspective) dominates the overall assessment because it identifies the asymmetric extraction: victims bear suppression costs while beneficiaries gain policy access without equivalent explanatory power. The scaffold classification (standards organizations) identifies a real feature: the constraint is intended as transitional, with sunset logic embedded in scientific standards development. The piton classification (philosophy) identifies a real feature: much institutional engagement is now performative. The mountain classification is the false summit: the constraint is not an inherent feature of scientific methodology but a contingent institutional arrangement sustained by specific policy frameworks, legal precedents, and professional norms. The analysis avoids collapsing all perspectives into a single type by showing why each classification is locally valid and what structural feature each illuminates.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    methodological_equivalence_boundary,
    'Is the distinction between ''naturalistic explanation'' and ''design inference'' a boundary between distinct methodological frameworks or a distinction drawn within a single naturalistic methodology?',
    'Comparative analysis of inference mechanisms: identify whether ID claims make predictions distinguishable from naturalistic explanations through novel empirical tests or whether ID claims are empirically indistinguishable from methodological naturalism',
    'If distinct methodologies: framework pluralism is legitimate, and the snare classification dissolves into genuine coordination. If within-methodology: design inference is a naturalistic inference pattern (abduction) misframed as non-naturalistic, and the constraint is pure rhetorical extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(methodological_equivalence_boundary, empirical, 'Whether design inference constitutes a distinct methodology or is internal to naturalism').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the measured suppression (0.72) primarily structural (legal/policy barriers, institutional exclusion) or internalized (biologists'' own epistemological commitments preventing genuine consideration of alternatives)?',
    'Compare suppression dynamics across jurisdictions with different legal/policy frameworks; identify whether loosening policy barriers leads to substantive methodological reassessment or merely reduces surface conflict',
    'If structural: loosening policy constraint should reduce suppression. If internalized: suppression persists despite policy change, indicating the constraint''s binding mechanism is cognitive rather than external. If both: splitting between structural (policy) and internalized (epistemic commitment) components changes the constraint''s character.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Structural vs. internalized suppression mechanism').

omega_variable(
    explanatory_power_differential,
    'Do evolutionary mechanisms incorporating selection, mutation, and inheritance explain the diversity and structure of life at rates comparable to competing design inference frameworks?',
    'Quantitative comparison of predictive success: novel predictions confirmed, retrodictions of known phenomena, integration with independent data streams (molecular biology, paleontology, population genetics). Comparison with ID predictive track record.',
    'If naturalistic mechanisms maintain superior explanatory power: the constraint is asymmetrically extractive (beneficiaries gain policy access without epistemic cost; victims pay suppression cost without epistemic compensation). If explanatory power approaches parity: genuine methodological pluralism is sustainable and the snare classification is incorrect.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(explanatory_power_differential, empirical, 'Comparative explanatory power of evolutionary mechanisms vs. design inference').

omega_variable(
    identity_lock_naturalism,
    'Do evolutionary biologists experience their commitment to naturalistic explanation as an external methodological requirement (constrained) or as constitutive of scientific identity itself (identity_locked)?',
    'Survey and interview data: ask whether biologists view naturalism as a contingent choice they could revise given sufficient evidence, or as intrinsic to what science is. Analyze framing of alternatives as ''not science'' vs. ''empirically inadequate''.',
    'If constrained: exit is materially difficult but conceptually available; the biologist can imagine abandoning naturalism. If identity_locked: exit requires reconstituting what ''science'' means to the agent. Identity lock increases the constraint''s binding force — the agent cannot see exiting from within their self-conception.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_naturalism, conceptual, 'Whether naturalism commitment is externally constrained or identity-constitutive').

omega_variable(
    cultural_pluralism_genuine_need,
    'Is there a genuine coordination problem requiring ID methodology presentation to manage cultural pluralism in education, or is the demand for inclusion primarily extractive (seeking institutional legitimacy without epistemic content)?',
    'Comparative education outcomes: jurisdictions with explicit ID inclusion vs. pure naturalistic presentation; measure student understanding of evolutionary mechanisms, science/theology distinction, and cross-cultural science literacy. Identify whether ID inclusion improves or diminishes these outcomes.',
    'If genuine coordination need: the scaffold and tangled rope classifications are correct; the constraint serves pluralism. If purely extractive: all non-snare classifications are misidentifications; the snare classification should dominate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cultural_pluralism_genuine_need, empirical, 'Whether ID inclusion serves genuine educational coordination or pure extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(intelligent_design_methodology, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(idm_tr_t0, intelligent_design_methodology, theater_ratio, 0, 0.55).
narrative_ontology:measurement(idm_tr_t10, intelligent_design_methodology, theater_ratio, 10, 0.62).
narrative_ontology:measurement(idm_tr_t20, intelligent_design_methodology, theater_ratio, 20, 0.68).

% Extraction over time
narrative_ontology:measurement(idm_be_t0, intelligent_design_methodology, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(idm_be_t10, intelligent_design_methodology, base_extractiveness, 10, 0.53).
narrative_ontology:measurement(idm_be_t20, intelligent_design_methodology, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(intelligent_design_methodology, identity_coordination).
narrative_ontology:affects_constraint(intelligent_design_methodology, science_education_curriculum_standardization).
narrative_ontology:affects_constraint(intelligent_design_methodology, naturalism_methodology_boundary).
narrative_ontology:affects_constraint(intelligent_design_methodology, religious_traditionalism_institutional_power).

% DUAL FORMULATION NOTE:
% ID methodology as a constraint should be decomposed from the specific empirical claims about biological mechanisms. The constraint operates at the meta-level (how scientific and non-scientific explanations are institutionally positioned) independently of the object-level debate about evolutionary theory's explanatory adequacy. A separate constraint story (naturalism_methodology_boundary) addresses whether the naturalism/non-naturalism distinction is a methodological requirement or a contingent institutional choice. The ID methodology constraint is downstream of this boundary constraint but represents distinct structural extraction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(intelligent_design_methodology, analytical, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
