% ============================================================================
% CONSTRAINT STORY: magna_carta_1215__living_document_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_magna_carta_1215_living_document_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: magna_carta_1215__living_document_reading
 *   human_readable: Magna Carta as Living Constitutional Document (Interpretive Tradition Reading)
 *   domain: constitutional_law/legal_history/political_theory
 *
 * SUMMARY:
 *   Magna Carta (1215, reissued 1217, 1225) originated as a feudal compact
 *   settling disputes between the English crown and baronial landowners. Over
 *   800 years, common-law courts reread the document as the foundation of
 *   universal constitutional protections: due process (Clause 39/40), consent
 *   to taxation (Clause 1), protection of ancient liberties. This reading—the
 *   living-document interpretation—treats the text's original baronial
 *   meaning as historically displaced but foundationally legitimate, and
 *   reads the precedential tradition as legitimate constitutional
 *   development. The constraint described here is the interpretive authority
 *   structure that makes this reading possible and stable. It coordinates
 *   constitutional continuity with adaptability; it also concentrates
 *   hermeneutic power in the judiciary and interpretive establishment. The
 *   other readings of this kernel are the baronial_privilege_reading
 *   (original feudal meaning exhausts legitimate scope) and the
 *   universal_rights_reading (Magna Carta emits timelessly universal rights
 *   by structural necessity, not by reinterpretation). This reading sits
 *   between: it accepts the gap between original meaning and contemporary
 *   scope, but treats the precedential bridging of that gap as legitimate
 *   authority, not as breach of textual fidelity.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_1215__living_document_reading, 0.31).
domain_priors:suppression_score(magna_carta_1215__living_document_reading, 0.18).
domain_priors:theater_ratio(magna_carta_1215__living_document_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_1215__living_document_reading, extractiveness, 0.31).
narrative_ontology:constraint_metric(magna_carta_1215__living_document_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(magna_carta_1215__living_document_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_1215__living_document_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(magna_carta_1215__living_document_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_1215__living_document_reading, rope).
narrative_ontology:human_readable(magna_carta_1215__living_document_reading, "Magna Carta as Living Constitutional Document (Interpretive Tradition Reading)").
narrative_ontology:topic_domain(magna_carta_1215__living_document_reading, "constitutional_law/legal_history/political_theory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_1215__living_document_reading, '2f73b8fa-4fe0-4952-982f-f515d090142a').
narrative_ontology:cs_kernel_codification('2f73b8fa-4fe0-4952-982f-f515d090142a', fixed_text).
narrative_ontology:cs_authority_grounding('2f73b8fa-4fe0-4952-982f-f515d090142a', lineage).
narrative_ontology:cs_interpretation_layer_present('2f73b8fa-4fe0-4952-982f-f515d090142a').
narrative_ontology:cs_reading_relation('2f73b8fa-4fe0-4952-982f-f515d090142a', magna_carta_1215__baronial_privilege_reading, coexists_with).
narrative_ontology:cs_reading_relation('2f73b8fa-4fe0-4952-982f-f515d090142a', magna_carta_1215__universal_rights_reading, influences).
narrative_ontology:cs_axiom('2f73b8fa-4fe0-4952-982f-f515d090142a', foundational, interpretive_tradition_constitutes_development).
narrative_ontology:cs_axiom_status(interpretive_tradition_constitutes_development, holdable).
narrative_ontology:cs_axiom_grounding('2f73b8fa-4fe0-4952-982f-f515d090142a', interpretive_tradition_constitutes_development, conventional).
narrative_ontology:cs_axiom('2f73b8fa-4fe0-4952-982f-f515d090142a', foundational, original_meaning_legitimately_superseded).
narrative_ontology:cs_axiom_status(original_meaning_legitimately_superseded, holdable).
narrative_ontology:cs_axiom_grounding('2f73b8fa-4fe0-4952-982f-f515d090142a', original_meaning_legitimately_superseded, deontological).
narrative_ontology:cs_reference_frame('2f73b8fa-4fe0-4952-982f-f515d090142a', living_constitutional_legitimacy_through_precedent).
narrative_ontology:cs_drift_state('2f73b8fa-4fe0-4952-982f-f515d090142a', contemporary_rights_expansion_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('2f73b8fa-4fe0-4952-982f-f515d090142a', '').
narrative_ontology:cs_kernel_id(magna_carta_1215__living_document_reading, magna_carta_1215).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_1215__living_document_reading, common_law_judiciary).
narrative_ontology:constraint_beneficiary(magna_carta_1215__living_document_reading, constitutional_interpreters).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(magna_carta_1215__living_document_reading, legislative_authority).
narrative_ontology:constraint_victim(magna_carta_1215__living_document_reading, dissenting_litigants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets Magna Carta's open language through accumulated case law and precedent. Maintains authority to expand or narrow the document's scope by reading new grievances into old clauses (Clause 39/40 due process, Clause 1 consent principles). Benefits from this interpretive latitude: judicial legitimacy rests partly on the ability to declare law responsive to contemporary concerns without formal amendment.
narrative_ontology:constraint_stakeholder(magna_carta_1215__living_document_reading, common_law_judiciary, agenda_setter,
    institutional, generational, constrained, national).

% Scholars, legal theorists, and courts reading Magna Carta as a living framework rather than fixed baronial compact. They benefit from the interpretive tradition because it validates their authority to recast the document's meaning—their scholarship and opinions shape which precedents carry weight and how new applications emerge. The tradition grants them seats at the table of constitutional legitimacy.
narrative_ontology:constraint_stakeholder(magna_carta_1215__living_document_reading, constitutional_interpreters, beneficiary,
    institutional, generational, constrained, national).

% Subject to judicial interpretation that can expand Magna Carta's constraints on legislative power through precedent and reinterpretation. Parliament is bound by evolving judge-made law reading the old document into new domains (habeas corpus, parliamentary privilege, statutory construction canons derived from Magna Carta). Cannot easily override this through amendment (requires supermajority consensus) and cannot claim the document means only what the barons intended.
narrative_ontology:constraint_stakeholder(magna_carta_1215__living_document_reading, legislative_authority, payer,
    institutional, generational, constrained, national).

% Legal scholars and judges advocating for fixed, historically-grounded meaning argue that legitimate constitutional authority derives from original textual intent, not expanding reinterpretation. They are excluded from this reading's authority structure: the living-document framework treats historical intent as one input among many, not as a limiting principle. Their alternative framing (the baronial_privilege_reading) remains a live position in jurisprudence but does not set the interpretive agenda.
narrative_ontology:constraint_stakeholder(magna_carta_1215__living_document_reading, originalist_interpreters, excluded,
    organized, biographical, constrained, national).

% Individuals whose claims courts reject by reading Magna Carta narrowly, or whose claims succeed because courts read it expansively through precedent. They bear the costs of judicial interpretation either way—they cannot appeal to the document's original text as binding. If the reading changes, their claims and liabilities shift unpredictably.
narrative_ontology:constraint_stakeholder(magna_carta_1215__living_document_reading, dissenting_litigants, payer,
    powerless, immediate, trapped, local).
narrative_ontology:stakeholder_secondary_role(magna_carta_1215__living_document_reading, dissenting_litigants, observer).

% The structural principle that prior judicial decisions carry binding or persuasive weight—not a party, but the mechanism through which the living-document reading operates. Analyzed here as an observer seat to track how precedential authority scaffolds the interpretive tradition.
narrative_ontology:constraint_stakeholder(magna_carta_1215__living_document_reading, authority_of_precedent, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a stable, legitimacy-bearing framework for constitutional interpretation that accommodates both respect for historical text and responsiveness to contemporary legal problems. By treating Magna Carta as a living document whose meaning develops through precedent, courts can anchor their decisions in historical authority while extending protections to new circumstances without requiring formal amendment—solving the coordination problem of constitutional stability versus adaptability.
% TRANSFER_FUNCTION: Transfers hermeneutic power from original textual intent (which would limit interpretation) to the accumulated interpretive tradition (which expands the scope of who can authoritatively speak about the document's meaning). Judges and legal interpreters gain authority to reread the text; legislatures and originalist interpreters lose the power to claim 'the document clearly means X because that is what was drafted'—the document's meaning is now the sum of authoritative readings, not the authors' intent.
% ABSENT_VOICES: Historians who argue the document's original baronial context is historically decisive; originalist judges and scholars who advocate for historical-meaning constraint; the barons themselves (no longer present, their intended meanings discounted). Modern citizens whose constitutional claims depend on how courts reinterpret old clauses have no seat at the table until they litigate—their interests are not consulted in advance.
% DISAPPEARANCE_RATIONALE: If the living-document reading disappeared—if courts reverted to treating Magna Carta as a fixed feudal compact with no capacity for interpretive development—constitutional protections would collapse or have to be rebuilt entirely through new statutes and amendments. Habeas corpus, due process rights, jury protections, parliamentary privilege, and countless other doctrines rest on the precedential chain reading the old text into modern contexts. Loss of interpretive authority would require constitutional replacement, not mere clarification.
% FOUNDING_PROBLEM: How can a medieval aristocratic charter written to settle a feudal dispute with a king serve as the foundation of a modern, universal constitutional order? The document's text is specific to the political moment (barons, crown, feudal homage); its authority claims to transcend time. The living-document reading solves this by treating the text as foundational but open to reinterpretation—the principles (constraint on arbitrary power, due process, consent) are universal; the application develops through precedent.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional historians and judges across multiple jurisdictions (common law nations, US, Canada, Australia) attest that the founding problem persists: Magna Carta remains the symbolic and legal anchor of constitutional authority, yet its original baronial meaning is indefensible as the basis for modern universal rights. The living-document solution—treating precedent as legitimate constitutional development—is endorsed by mainstream constitutional scholarship and practised by courts. The baronial_privilege_reading's corroborators (originalist historians, some legal scholars) argue the founding problem is a false necessity created by the living-document reading itself, but they do not deny the historical gap the reading purports to bridge.
narrative_ontology:disappearance_verdict(magna_carta_1215__living_document_reading, world_rearranges).
narrative_ontology:founding_problem_status(magna_carta_1215__living_document_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_1215__living_document_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(magna_carta_1215__living_document_reading, 'none', 1).
narrative_ontology:epsilon_provenance(magna_carta_1215__living_document_reading, 0.31, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(magna_carta_1215__living_document_reading_tests).
:- end_tests(magna_carta_1215__living_document_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low-to-moderate (0.31 endpoint) because the constraint coordinates genuine constitutional functions: it provides a stable framework for constitutional interpretation that neither original-intent textualism nor pure legislative amendment can match. The interpretive tradition allows courts to extend protections without formal amendment (lower friction than constitutional revision). However, extraction rises from 0.08 to 0.31 over 800 years because the judiciary and interpretive establishment accumulate power to redefine the constraint's scope—what began as a specific feudal contract becomes a universal frame, and the power to reread is concentrated. Suppression is low (0.18) because originalist and historical alternative interpretations remain available in scholarship and dissenting opinions; the living-document reading is the dominant interpretive mode but not enforced by coercion, only by institutional weight. Theater ratio is very low (0.12 endpoint, rising from 0.05) because the interpretive function is genuinely performed: courts do engage in close reading, precedent does constrain new interpretation (stare decisis operates), and the constraint actually solves coordination problems. The slight rise in theater over time reflects increasing ceremonial and rhetorical emphasis on Magna Carta's symbolic status (especially in the 20th century) relative to its functional role in litigation. Measurements are authored on one shared time grid so every metric appears at every examined time point.
 *
 * PERSPECTIVAL GAP:
 *   The engine should compute per-seat classifications that diverge meaningfully. From the judiciary's institutional perspective, the living-document reading is pure coordination: they maintain interpretive authority through precedent, courts can adapt to new problems, the system is stable and respected. Extractiveness and suppression should compute low from this seat. From the legislature's perspective, the same structure is partially constraining: judge-made law limits legislative options, the precedential chain is binding, and Parliament cannot easily redefine the document. Extractiveness should compute moderate from this seat. From originalist interpreters' perspective, the reading is suppressive: their alternative authority base (original historical intent) is systematically delegitimized, their arguments are heard but do not carry institutional weight. From dissenting litigants' perspective, the reading is extractive and suppressive: they bear the costs of judicial discretion without input and cannot exit. The authored metrics (extractiveness=0.31 at the constraint-level) represent a weighted average or institutional perspective; the per-seat computations should show the structural divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The common-law judiciary is the structural beneficiary (gains interpretive authority and legitimacy from the living-document reading; without it, they would be constrained by historical intent or must defer to legislators). Constitutional interpreters (scholars, theorists) similarly benefit by gaining authority to reshape the document's meaning. Legislative authority is partially a payer (constrained by judge-made law that evolves through precedent; Parliament cannot easily claim the document supports a reading courts have rejected). The originalist interpreters are excluded, not payers—they are shut out of the dominant authority structure, not extracted from. Dissenting litigants are payers: they bear the costs of interpretive unpredictability (how courts will read their case into the precedential tradition) without power to shape the interpretation. From the judiciary's seat, the constraint is coordination (provides stable interpretive framework); from the legislative seat, it is mixed (coordination in some domains, constraint in others); from dissenting litigants' seat, it approaches extraction (they pay costs of judicial discretion and have no exit).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem ('How can a feudal compact serve as universal constitutional foundation?') is demonstrably live: contemporary constitutionalism across common-law nations continues to invoke Magna Carta as authority while acknowledging the historical distance between its original meaning and modern application. The living-document reading solves this by legitimating the precedential tradition as the mode of constitutional development. The mandate has not atrophied: courts continue to cite Magna Carta precedent in novel contexts (habeas corpus, due process, parliamentary privilege extensions). However, there is a secondary mandatrophy question: does the constraint persist because the living-document reading remains the best solution to the founding problem, or because institutional inertia and symbolic authority maintain it even as other solutions (formal constitutional amendment, statutory replacement, pure judge-made common law without historical anchoring) become viable? The measurement series show extractiveness rising while theater_ratio stays low, suggesting the functional mandate (providing interpretive stability) is being performed, but the institutional power concentration (the beneficiary side) is increasing. This is not theater—it is genuine extraction layered onto coordination. The classification should remain rope or tangled_rope, not piton, because the underlying coordination function is still being performed and the constraint would genuinely rearrange the world if it disappeared.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    precedent_vs_innovation_boundary,
    'What distinguishes legitimate reinterpretation of Magna Carta through precedent from illegitimate judicial innovation? Where does the line between ''reading the text into new contexts'' and ''rewriting the text'' sit?',
    'Comparative jurisprudence across common-law systems examining cases where courts explicitly refused to extend Magna Carta''s scope as exceeding legitimate reinterpretation. Doctrinal analysis of stare decisis constraints on judicial reading.',
    'If the line is clear and enforced, the living-document reading maintains coordinating function with minimal extractive overhead. If the line is contested or erodes over time, the reading becomes more extractive (judicial power accumulates without constraint), raising the risk of mandatrophy (institutional power maintaining an atrophied mandate).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(precedent_vs_innovation_boundary, conceptual, 'The legitimacy boundary between reinterpretation and rewriting in the living-document framework.').

omega_variable(
    kernel_reading_structural_ambiguity,
    'Is the living-document reading of Magna Carta a defensible interpretation of the same kernel, or does it require a fundamentally different kernel (one that treats the document as an adaptive text rather than a fixed legal instrument)?',
    'Textual analysis of the Charter''s own reissue pattern (1215, 1217, 1225, 1297 confirmatio) and language about perpetual validity. If the Charter itself claims adaptability, the reading is self-authorized; if the original text claims fixity and the reading rejects that claim, the reading may be reframing the kernel itself.',
    'If the reading is self-authorized by the kernel''s own language, it coheres more cleanly with the baronial_privilege_reading and universal_rights_reading as alternative framings of one kernel. If the reading reframes the kernel, it is a meta-constraint on authority rather than a constraint within the kernel''s own terms—the classification may shift from rope to tangled_rope (the reading both coordinates constitutional interpretation AND extracts hermeneutic power by redefining what the kernel is).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structural_ambiguity, conceptual, 'Whether the living-document reading interprets or reframes the Magna Carta kernel itself.').

omega_variable(
    institutional_weight_vs_epistemic_authority,
    'Does the living-document reading persist because it is epistemically superior to originalist alternatives, or because institutional weight (legal education, court precedent, bar associations) has made it the default without actively refuting the alternatives?',
    'Historical analysis of legal education and bar credentialing: how much institutional gatekeeping versus active argumentation supports the living-document dominance? What would it take for a shift in professional epistemic consensus (new historical scholarship, formalist jurisprudence revival, etc.) to change the dominant reading?',
    'If epistemic authority is the driver, the reading''s persistence is robust and non-extractive (it wins on merit). If institutional weight is the primary driver, the reading is more vulnerable to erosion and may be extractive (it persists through entrenchment rather than sustained justification). The threshold for changing the reading should be lower in the latter case.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(institutional_weight_vs_epistemic_authority, empirical, 'What sustains the living-document reading: epistemic justification or institutional entrenchment?').

omega_variable(
    universal_rights_vs_interpretive_tradition,
    'Can the living-document reading coherently accommodate the universal_rights_reading''s claim that Clause 39/40 emits timeless due-process rights, or does treating rights as developed through precedent implicitly deny their universality?',
    'Jurisprudential analysis of whether precedential development is compatible with rights universalism. Some jurists argue precedent reveals universal principles (progressive discovery); others argue it constructs new meanings (construction without universality). The two readings must either foreclose or coexist; this omega identifies which.',
    'If they coexist, both remain live and the constraint is a meta-framework scaffolding contestation. If they foreclose, one must be displaced—the living-document reading would have to either reject universal-rights language or reframe universal rights as emerging through precedent (not as timeless principles). This affects the reading_relations classification in cs_structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(universal_rights_vs_interpretive_tradition, conceptual, 'Compatibility of precedential development with rights universalism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_1215__living_document_reading, 0, 800).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(magn_tr_t0, magna_carta_1215__living_document_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(magn_tr_t100, magna_carta_1215__living_document_reading, theater_ratio, 100, 0.06).
narrative_ontology:measurement(magn_tr_t200, magna_carta_1215__living_document_reading, theater_ratio, 200, 0.08).
narrative_ontology:measurement(magn_tr_t400, magna_carta_1215__living_document_reading, theater_ratio, 400, 0.11).
narrative_ontology:measurement(magn_tr_t600, magna_carta_1215__living_document_reading, theater_ratio, 600, 0.12).
narrative_ontology:measurement(magn_tr_t800, magna_carta_1215__living_document_reading, theater_ratio, 800, 0.12).

% Extraction over time
narrative_ontology:measurement(magn_be_t0, magna_carta_1215__living_document_reading, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(magn_be_t100, magna_carta_1215__living_document_reading, base_extractiveness, 100, 0.12).
narrative_ontology:measurement(magn_be_t200, magna_carta_1215__living_document_reading, base_extractiveness, 200, 0.18).
narrative_ontology:measurement(magn_be_t400, magna_carta_1215__living_document_reading, base_extractiveness, 400, 0.28).
narrative_ontology:measurement(magn_be_t600, magna_carta_1215__living_document_reading, base_extractiveness, 600, 0.29).
narrative_ontology:measurement(magn_be_t800, magna_carta_1215__living_document_reading, base_extractiveness, 800, 0.31).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(magna_carta_1215__living_document_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_1215__living_document_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(magna_carta_1215__living_document_reading, 0.18).
narrative_ontology:affects_constraint(magna_carta_1215__living_document_reading, magna_carta_1215__baronial_privilege_reading).
narrative_ontology:affects_constraint(magna_carta_1215__living_document_reading, magna_carta_1215__universal_rights_reading).

% DUAL FORMULATION NOTE:
% The magna_carta_1215 kernel has three readings, each instantiating a distinct constraint: (1) baronial_privilege_reading treats the Charter as a feudal compact whose scope is limited to the contracting parties and original grievances; (2) living_document_reading (this file) treats the Charter as an adaptive constitutional substrate whose meaning develops through precedent; (3) universal_rights_reading treats the Charter as emitting transhistorical universal rights by structural necessity, independent of reinterpretation. These readings coexist in jurisprudence and constitutional practice but have different structural consequences. The living-document reading influences both siblings by providing the institutional mechanism (precedential authority) through which other readings are either absorbed or marginalized. Decomposition follows DP-001 (ε-invariance): each reading instantiates a different constraint because the reading's core premise (about how the Charter's authority works) determines what counts as legitimate application, hence determining the beneficiary/victim structure, the coordination function, and the extraction pattern.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
