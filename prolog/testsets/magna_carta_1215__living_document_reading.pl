% ============================================================================
% CONSTRAINT STORY: magna_carta_1215__living_document_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_magna_carta_1215__living_document_reading, []).

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
 *   constraint_id: magna_carta_1215__living_document_reading
 *   human_readable: Magna Carta as Living Document: Interpretive Authority and Adaptive Constitutionalism
 *   domain: constitutional_law/legal_history/political_theory
 *
 * SUMMARY:
 *   The 'living document' reading of Magna Carta treats the 1215 baronial
 *   charter as an adaptive constitutional substrate whose original meaning
 *   has been legitimately superseded by an accumulating tradition of
 *   interpretation. Under this reading, the document's authority derives not
 *   from its author's intent (King John and rebel barons) but from its
 *   capacity to be reread across centuries to address new constitutional
 *   problems. The Magna Carta becomes a vessel for evolving understandings of
 *   due process, individual rights, and rule of law — meanings entirely
 *   absent from or contradictory to the 1215 text itself. This reading
 *   instantiates a specific form of constitutional legitimacy: authority
 *   grounded in a fixed historical kernel (the document and its name) while
 *   operating through an interpretive tradition that progressively detaches
 *   meaning from origin. The constraint exhibits the hybrid structure of
 *   Tangled Rope: genuine coordination function (the document provides stable
 *   constitutional language across centuries) combined with asymmetric
 *   extraction (the judiciary and contemporary rights advocates benefit from
 *   the interpretive flexibility; those committed to original meaning are
 *   constrained by having that meaning progressively overridden). The theater
 *   ratio (0.64) reflects that contemporary invocation of Magna Carta is
 *   substantially ceremonial — the document's legitimating power persists
 *   despite the acknowledged irrelevance of the 1215 baronial grievances to
 *   modern constitutional law.
 *
 * KEY AGENTS:
 *   - Judicial Authority: Primary beneficiary (institutional/arbitrage) — gains maximal interpretive flexibility; can reread the document to address new constitutional problems without formal amendment
 *   - Contemporary Rights Advocates: Secondary beneficiary (organized/mobile) — use the living-document reading to secure recognition for rights (gender equality, racial justice) not mentioned in the 1215 text
 *   - Originalist Legal Scholars: Primary victim (powerless/trapped) — their commitment to original baronial meaning is progressively superseded by accumulated precedent; cannot exit without abandoning the document's authority
 *   - Baronial Constraint Framework: Victim (institutional/trapped) — the original meaning (limitation of royal prerogative, protection of baronial property) is explicitly overridden; the framework cannot survive the interpretive tradition
 *   - Constitutional Text Itself: Ambiguous position (analytical/analytical) — the 1215 document is simultaneously the legitimating source and the thing being progressively emptied of original meaning
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing the interpretive tradition as inevitable constitutional development rather than examining the authority structure that enables it
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_1215__living_document_reading, 0.52).
domain_priors:suppression_score(magna_carta_1215__living_document_reading, 0.48).
domain_priors:theater_ratio(magna_carta_1215__living_document_reading, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_1215__living_document_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(magna_carta_1215__living_document_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(magna_carta_1215__living_document_reading, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_1215__living_document_reading, tangled_rope).
narrative_ontology:human_readable(magna_carta_1215__living_document_reading, "Magna Carta as Living Document: Interpretive Authority and Adaptive Constitutionalism").
narrative_ontology:topic_domain(magna_carta_1215__living_document_reading, "constitutional_law/legal_history/political_theory").

domain_priors:requires_active_enforcement(magna_carta_1215__living_document_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_1215__living_document_reading, 'c4af638f-f2ea-4423-8add-ddaa2947fc11').
narrative_ontology:cs_kernel_codification('c4af638f-f2ea-4423-8add-ddaa2947fc11', fixed_text).
narrative_ontology:cs_authority_grounding('c4af638f-f2ea-4423-8add-ddaa2947fc11', lineage).
narrative_ontology:cs_interpretation_layer_present('c4af638f-f2ea-4423-8add-ddaa2947fc11').
narrative_ontology:cs_reading_relation('c4af638f-f2ea-4423-8add-ddaa2947fc11', magna_carta_1215__baronial_privilege_reading, coexists_with).
narrative_ontology:cs_reading_relation('c4af638f-f2ea-4423-8add-ddaa2947fc11', magna_carta_1215__universal_rights_reading, influences).
narrative_ontology:cs_axiom('c4af638f-f2ea-4423-8add-ddaa2947fc11', foundational, interpretive_tradition_legitimates).
narrative_ontology:cs_axiom_status(interpretive_tradition_legitimates, holdable).
narrative_ontology:cs_axiom_grounding('c4af638f-f2ea-4423-8add-ddaa2947fc11', interpretive_tradition_legitimates, deontological).
narrative_ontology:cs_axiom('c4af638f-f2ea-4423-8add-ddaa2947fc11', secondary, adaptive_meaning_constitutive).
narrative_ontology:cs_axiom_status(adaptive_meaning_constitutive, holdable).
narrative_ontology:cs_axiom_grounding('c4af638f-f2ea-4423-8add-ddaa2947fc11', adaptive_meaning_constitutive, instrumental).
narrative_ontology:cs_reference_frame('c4af638f-f2ea-4423-8add-ddaa2947fc11', precedent_accumulation_authority).
narrative_ontology:cs_drift_state('c4af638f-f2ea-4423-8add-ddaa2947fc11', contemporary_legal_pluralism, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('c4af638f-f2ea-4423-8add-ddaa2947fc11', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(magna_carta_1215__living_document_reading, magna_carta_1215).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_1215__living_document_reading, interpretive_judiciary).
narrative_ontology:constraint_beneficiary(magna_carta_1215__living_document_reading, contemporary_rights_advocates).
narrative_ontology:constraint_victim(magna_carta_1215__living_document_reading, baronial_constraint_framework).
narrative_ontology:constraint_victim(magna_carta_1215__living_document_reading, textual_original_meaning).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ORIGINALIST LEGAL SCHOLAR (SNARE) — Trapped by the interpretive tradition that has progressively detached Magna Carta from its 1215 baronial meaning. Cannot exit: accepting the document's legitimacy requires accepting accumulated precedent that contradicts the original meaning. Full extraction: the living-document framework explicitly supersedes and overrides the scholar's textual foundation.
constraint_indexing:constraint_classification(magna_carta_1215__living_document_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PRACTICING LAWYER (TANGLED ROPE) — Constrained by the need to invoke Magna Carta's legitimacy while navigating doctrinal precedent that has transformed its meaning. Genuine coordination function: the document provides a stable foundation for constitutional argumentation across centuries. Asymmetric extraction: lawyers benefit from the document's rhetorical power while bearing the cost of internal doctrinal incoherence. Exit options are constrained by professional requirement to work within established precedent.
constraint_indexing:constraint_classification(magna_carta_1215__living_document_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: JUDICIAL AUTHORITY (ROPE) — Benefits from the living-document reading through maximal interpretive flexibility. Experiences the constraint as pure coordination: the document's adaptability enables the judiciary to address contemporary rights claims while maintaining constitutional legitimacy. No experienced extraction; arbitrage exit option reflects the judiciary's capacity to reinterpret as needed. The constraint solves a coordination problem: how to invoke ancient authority while adapting to modern circumstances.
constraint_indexing:constraint_classification(magna_carta_1215__living_document_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: CONTEMPORARY RIGHTS MOVEMENT (SCAFFOLD) — Organized agents (civil rights coalitions, international human rights advocates) use the living-document reading as a temporary coordinating mechanism to secure recognition for historically marginalized claims (gender equality, racial justice, LGBTQ+ rights). The scaffold has a sunset: as specific rights become formally codified (constitutional amendments, statutory law, international treaties), reliance on Magna Carta's living meaning diminishes. Exit option is mobile because these movements can shift to explicit new legal instruments.
constraint_indexing:constraint_classification(magna_carta_1215__living_document_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: HISTORICAL DOCUMENT RITUAL (PITON) — The ceremonial invocation of Magna Carta in constitutional discourse has become substantially performative. The document's historical authenticity and original meaning are acknowledged to be irrelevant to contemporary legal application, yet ceremonial reference persists for legitimacy theater. The theater ratio is high: invoking 'Magna Carta' provides constitutional legitimacy regardless of what the 1215 document actually required. The constraint persists through institutional inertia — the symbolic power of the name outlasts the functional meaning.
constraint_indexing:constraint_classification(magna_carta_1215__living_document_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a purely logical perspective, living constitutionalism appears to be an irreducible feature of any enduring legal framework: a document's meaning MUST adapt to new circumstances, or it becomes inapplicable and loses authority. This perspective sees interpretive evolution as natural law — the only way a constitution can survive. However, the structural data reveals this as a false summit: the specific beneficiaries (judiciary, contemporary rights advocates) and victims (original-meaning commitments) show that the living-document reading is a contingent institutional arrangement, not an inevitable feature of constitutional logic.
constraint_indexing:constraint_classification(magna_carta_1215__living_document_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(magna_carta_1215__living_document_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(magna_carta_1215__living_document_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(magna_carta_1215__living_document_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(magna_carta_1215__living_document_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(magna_carta_1215__living_document_reading, TR),
    TR >= 0.70.

:- end_tests(magna_carta_1215__living_document_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The living-document reading enables substantive extraction through interpretive flexibility: the judiciary can reread the document to justify new constitutional doctrines without formal amendment. However, the extraction is not maximal because the coordination function is genuine — the document does solve the problem of invoking ancient authority for contemporary governance. The trajectory from 0.18 (1215) to 0.52 (1950) reflects the progressive accumulation of meaning-layers, each layer adding extraction potential as judges gain more interpretive freedom. Suppression (0.48): Moderate. Barriers to rejecting the living-document reading include: the institutional sunk cost of centuries of precedent, the powerful legitimacy of invoking medieval authority, and the difficulty of proposing alternative constitutional foundations. But suppression is not total — originalist legal scholarship actively contests the reading, and constitutional amendment remains formally available. Theater ratio (0.64): High. The contemporary invocation of Magna Carta operates substantially through performative reference: judges and advocates cite the document's name and broad phrases ('due process,' 'liberty') while acknowledging that the 1215 baronial meaning is irrelevant. The 1215 document required the king not to arbitrarily dispossess barons of fiefs; contemporary judges read it to protect privacy, expressive freedom, and equal protection — meanings the 1215 barons never conceived. The theater has increased monotonically (0.35 → 0.50 → 0.64) as the gap between original meaning and contemporary application has widened.
 *
 * PERSPECTIVAL GAP:
 *   The six perspectives reveal the full structural complexity of adaptive constitutionalism. The judicial authority sees Rope (coordination: the document provides stable language for addressing new problems). The contemporary rights movement sees Scaffold (temporary mechanism for securing new rights, sunset as formal amendments are adopted). The originalist scholar sees Snare (trapped by precedent that overrides the meaning they committed to). The institutional ritual observer sees Piton (ceremonial invocation divorced from functional meaning). The practicing lawyer sees Tangled Rope (genuine coordination alongside constrained extraction). The analytical observer risks seeing Mountain (inevitable feature of constitutional systems) — but the structural data reveals this as a false summit: the specific beneficiaries (judiciary, contemporary advocates) and victims (original-meaning commitments) show that the living-document framework is contingent, not inevitable. The central perspectival gap is between the judiciary's experience of Rope and the originalist's experience of Snare — they occupy opposite structural positions relative to the same constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality derivation chain starts with beneficiary/victim declarations: the living-document reading benefits the judiciary (low d), contemporary rights advocates (moderate d), and harms originalist scholars (high d) and the baronial framework (maximum d). The exit options differentiate further: the judiciary has arbitrage options (can reinterpret as needed), so their benefit is amplified; originalist scholars are trapped by precedent, so their victimhood is amplified. The power atoms reflect their structural capacity: the judiciary is institutional (high authority), contemporary advocates are organized (coordinated agency), originalists are powerless within the precedent system, and the baronial framework is also institutional but superseded. These inputs feed the sigmoid directionality function to produce chi values that differentiate the perspectives.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the 'true' classification depends on the observation position, and all six perspectives are structurally valid. The judicial authority genuinely experiences Rope — their constraint is coordination (using the document to address new problems). The originalist genuinely experiences Snare — their constraint is extraction (the tradition overrides their meaning). The analytical observer's Mountain is a false summit — it naturalizes what is actually a contingent institutional arrangement. The contemporary rights movement's Scaffold is real — they are using the framework as a temporary mechanism with a sunset (formal amendments). The ritual observer's Piton is real — the ceremonial invocation is substantially performative. The practicing lawyer's Tangled Rope is real — mixed coordination and extraction. No single type is 'correct'; the presheaf over the observation site is the answer. Resolving this requires understanding that different agent positions occupy different positions in the extraction flow.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimacy_source_ambiguity,
    'Does the living-document reading derive its legitimacy from the 1215 text, from the interpretive tradition that has accumulated meaning over centuries, or from the outcomes (rights protections) it enables?',
    'Doctrinal genealogy: trace which citations and precedents judges actually invoke when justifying living-document interpretation. If citations flow backward to 1215, legitimacy is textual; if citations are to prior judicial decisions, legitimacy is precedential; if citations justify outcomes, legitimacy is consequentialist.',
    'If textual: the living-document reading is self-undermining (it claims fidelity to 1215 while rejecting original meaning). If precedential: the constraint is pure institutional recursion (each decision binds the next). If consequentialist: the constraint has become a rights-outcome engine with historical window-dressing.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(legitimacy_source_ambiguity, empirical, 'Source of legitimacy for living-document interpretation: textual, precedential, or consequentialist').

omega_variable(
    baronial_meaning_supersession_scope,
    'To what degree has the interpretive tradition genuinely superseded the baronial meaning vs. merely layered contemporary readings atop it?',
    'Textual analysis: identify passages from the 1215 document that contemporary legal interpretation explicitly contradicts vs. merely supplements. If contradicted: supersession is real. If supplemented: the original meaning coexists with layers of new meaning.',
    'If supersession: the living-document reading achieves complete override of original meaning — the constraint is transformative, not additive. If coexistence: Magna Carta contains multiple, partially contradictory meanings simultaneously — the constraint is more complex (genuine hermeneutic palimpsest).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(baronial_meaning_supersession_scope, empirical, 'Whether interpretive tradition has superseded or supplemented original baronial meaning').

omega_variable(
    interpretive_ceiling_existence,
    'Does the living-document framework have a ceiling — constraints on how far reinterpretation can diverge from the original text before losing the document''s legitimating power?',
    'Historical case analysis: identify instances where courts rejected an interpretation of Magna Carta as going ''too far'' from the text, and instances where courts accepted radically novel readings. Threshold detection: what textual distance from 1215 meaning triggers the ceiling?',
    'If ceiling exists: the living-document reading is not truly adaptive — it operates within bounds set by residual fidelity to the original. If no ceiling: the framework is purely consequentialist (any reading that produces acceptable outcomes is valid), and the text is a legitimation vessel with no constraining force.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretive_ceiling_existence, empirical, 'Whether living-document interpretation has a textual fidelity ceiling').

omega_variable(
    reading_decomposition_uncertainty,
    'Is this a single constraint (Magna Carta''s adaptive meaning-making across centuries) or does the living-document reading decompose into distinct constraints with different epsilon values?',
    'Observable test: measure epsilon under two measurement bases: (1) the constraint as rights-expansion mechanism (can contemporary rights claims draw legitimacy from Magna Carta?); (2) the constraint as institutional authority structure (can the judiciary use Magna Carta to justify any decision?). If epsilon differs substantially, this is two constraints.',
    'If two constraints: decompose into living_document_rights_expansion (lower epsilon, coordination function) and judicial_authority_flexibility (higher epsilon, extraction function). If one: maintain unified story. Current authoring assumes decomposition is not necessary — the base ε=0.52 represents the hybrid of both functions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_decomposition_uncertainty, conceptual, 'Whether the living-document reading is a unified constraint or decomposes into multiple constraints').

omega_variable(
    committer_reading_status,
    'This story instantiates the ''living-document reading'' of Magna Carta as a contested kernel. Is this reading itself a coherent commitment, or does it fragment into incompatible sub-readings when examined closely?',
    'Doctrinal coherence test: assemble a set of canonical living-document decisions (Marbury v Madison, Lochner era, Warren Court expansion, contemporary originalism debate). Can a single principle explain all of them? If yes: coherent reading. If no: fragmented into sub-readings with different legitimacy claims.',
    'If coherent: the reading_relations and axioms in cs_structure accurately capture what the living-document reading commits to. If fragmented: the reading itself is already contested, and the sibling readings (baronial, universal) might not be the right decomposition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_reading_status, conceptual, 'Internal coherence of the living-document reading as a unified interpretive stance').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_1215__living_document_reading, 0, 800).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mc_living_theater_1215, magna_carta_1215__living_document_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(mc_living_theater_1615, magna_carta_1215__living_document_reading, theater_ratio, 400, 0.5).
narrative_ontology:measurement(mc_living_theater_1950, magna_carta_1215__living_document_reading, theater_ratio, 700, 0.64).

% Extraction over time
narrative_ontology:measurement(mc_living_extract_1215, magna_carta_1215__living_document_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(mc_living_extract_1615, magna_carta_1215__living_document_reading, base_extractiveness, 400, 0.35).
narrative_ontology:measurement(mc_living_extract_1950, magna_carta_1215__living_document_reading, base_extractiveness, 700, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_1215__living_document_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(magna_carta_1215__living_document_reading, 0.12).
narrative_ontology:affects_constraint(magna_carta_1215__living_document_reading, magna_carta_1215__baronial_privilege_reading).
narrative_ontology:affects_constraint(magna_carta_1215__living_document_reading, magna_carta_1215__universal_rights_reading).
narrative_ontology:affects_constraint(magna_carta_1215__living_document_reading, constitutional_amendment_alternative_pathways).
narrative_ontology:affects_constraint(magna_carta_1215__living_document_reading, precedential_authority_constraint).

% DUAL FORMULATION NOTE:
% Magna Carta 1215 decomposes into three constraint stories, one per dominant reading. Each story has its own epsilon, beneficiary/victim structure, and perspectives. The living-document reading (this story, ε=0.52) treats the document as adaptively reinterpreted across centuries. The baronial-privilege reading (ε=0.35) treats the original meaning as the true constraint and subsequent interpretation as deviation. The universal-rights reading (ε=0.48) treats Magna Carta as discovering universal principles. These are not three interpretations of one constraint; they are three structurally distinct constraints that share a kernel. Each story links to the others via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(magna_carta_1215__living_document_reading, analytical, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
