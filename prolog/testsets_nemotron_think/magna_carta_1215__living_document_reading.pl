% ============================================================================
% CONSTRAINT STORY: magna_carta_1215__living_document_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    narrative_ontology:constraint_vindicates/2,
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
 *   constraint_id: magna_carta_1215__living_document_reading
 *   human_readable: Magna Carta as Living Constitutional Substrate (Living Document Reading)
 *   domain: constitutional_law/legal_history/political_theory
 *
 * SUMMARY:
 *   The living document reading of Magna Carta treats the 1215 charter not as
 *   a frozen feudal contract but as the originating node of an adaptive
 *   constitutional substrate. On this reading, the charter's specific
 *   provisions (fish weirs, standard measures, baronial reliefs) were always
 *   meant to be superseded by an interpretive tradition that extracts general
 *   principles (due process, rule of law, constraint on arbitrary power) and
 *   applies them to novel circumstances. The constraint is the precedential
 *   system itself: stare decisis, the hierarchy of courts, and the
 *   professionalized bar constitute the enforcement machinery that makes the
 *   living tradition binding. The reading claims this is pure coordination
 *   (rope) — solving the problem of constitutional continuity amid change —
 *   but originalist claimants and fixed-meaning reliants experience it as
 *   extraction from their reliance interests. The authority structure (common
 *   law courts) scaffolds contestation between this reading, the baronial
 *   privilege reading, and the universal rights reading without resolving it.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_1215__living_document_reading, 0.35).
domain_priors:suppression_score(magna_carta_1215__living_document_reading, 0.25).
domain_priors:theater_ratio(magna_carta_1215__living_document_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_1215__living_document_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(magna_carta_1215__living_document_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(magna_carta_1215__living_document_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_1215__living_document_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(magna_carta_1215__living_document_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_1215__living_document_reading, rope).
narrative_ontology:human_readable(magna_carta_1215__living_document_reading, "Magna Carta as Living Constitutional Substrate (Living Document Reading)").
narrative_ontology:topic_domain(magna_carta_1215__living_document_reading, "constitutional_law/legal_history/political_theory").

domain_priors:requires_active_enforcement(magna_carta_1215__living_document_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_1215__living_document_reading, '3e50e06f-970b-429f-90a6-86a51ef6ec16').
narrative_ontology:cs_kernel_codification('3e50e06f-970b-429f-90a6-86a51ef6ec16', fixed_text).
narrative_ontology:cs_authority_grounding('3e50e06f-970b-429f-90a6-86a51ef6ec16', lineage).
narrative_ontology:cs_interpretation_layer_present('3e50e06f-970b-429f-90a6-86a51ef6ec16').
narrative_ontology:cs_reading_relation('3e50e06f-970b-429f-90a6-86a51ef6ec16', magna_carta_1215__baronial_privilege_reading, influences).
narrative_ontology:cs_reading_relation('3e50e06f-970b-429f-90a6-86a51ef6ec16', magna_carta_1215__universal_rights_reading, coexists_with).
narrative_ontology:cs_axiom('3e50e06f-970b-429f-90a6-86a51ef6ec16', foundational, constitutional_meaning_evolves_through_precedent).
narrative_ontology:cs_axiom_status(constitutional_meaning_evolves_through_precedent, holdable).
narrative_ontology:cs_axiom_grounding('3e50e06f-970b-429f-90a6-86a51ef6ec16', constitutional_meaning_evolves_through_precedent, conventional).
narrative_ontology:cs_axiom('3e50e06f-970b-429f-90a6-86a51ef6ec16', foundational, original_understanding_not_binding).
narrative_ontology:cs_axiom_status(original_understanding_not_binding, holdable).
narrative_ontology:cs_axiom_grounding('3e50e06f-970b-429f-90a6-86a51ef6ec16', original_understanding_not_binding, conventional).
narrative_ontology:cs_reference_frame('3e50e06f-970b-429f-90a6-86a51ef6ec16', original_1215_feudal_charter).
narrative_ontology:cs_drift_state('3e50e06f-970b-429f-90a6-86a51ef6ec16', contemporary_constitutional_practice, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('3e50e06f-970b-429f-90a6-86a51ef6ec16', '').
narrative_ontology:cs_kernel_id(magna_carta_1215__living_document_reading, magna_carta_1215).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_1215__living_document_reading, constitutional_order).
narrative_ontology:constraint_beneficiary(magna_carta_1215__living_document_reading, future_generations).
narrative_ontology:constraint_beneficiary(magna_carta_1215__living_document_reading, adaptive_governance).
narrative_ontology:constraint_beneficiary(magna_carta_1215__living_document_reading, legal_practitioners).
narrative_ontology:constraint_beneficiary(magna_carta_1215__living_document_reading, universal_rights_proponents).
narrative_ontology:constraint_victim(magna_carta_1215__living_document_reading, originalist_claimants).
narrative_ontology:constraint_victim(magna_carta_1215__living_document_reading, fixed_meaning_reliants).
narrative_ontology:constraint_vindicates(magna_carta_1215__living_document_reading, constitutional_adaptation_legitimacy).
narrative_ontology:constraint_vindicates(magna_carta_1215__living_document_reading, precedential_authority).
narrative_ontology:constraint_vindicates(magna_carta_1215__living_document_reading, living_constitutionalism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer the precedential system through stare decisis; their rulings constitute the living interpretive tradition. They justify evolving interpretation as necessary for constitutional continuity. They bear the institutional burden of maintaining legitimacy while adapting doctrine.
narrative_ontology:constraint_stakeholder(magna_carta_1215__living_document_reading, constitutional_courts, agenda_setter,
    institutional, generational, analytical, national).

% Benefit from a predictable but flexible framework for constitutional argument. The living document reading gives them doctrinal tools to advocate for clients in novel circumstances. They can exit to other jurisdictions or practice areas but their professional identity is tied to the precedential system.
narrative_ontology:constraint_stakeholder(magna_carta_1215__living_document_reading, legal_practitioners, beneficiary,
    organized, biographical, mobile, national).

% Inherit a constitutional order that can adapt to unforeseen challenges without requiring constant formal amendment. They have no voice in current interpretive choices but bear the long-run consequences of doctrinal evolution. Their situation is defined by structural dependency on the choices of living interpreters.
narrative_ontology:constraint_stakeholder(magna_carta_1215__living_document_reading, future_generations, beneficiary,
    powerless, civilizational, trapped, national).

% Legislatures and executive branches benefit from a constitutional framework that accommodates new governance problems (administrative state, digital regulation, climate policy) without textual amendment. They participate in shaping the living tradition through appointments and political constitutionalism but are constrained by judicial supremacy in interpretation.
narrative_ontology:constraint_stakeholder(magna_carta_1215__living_document_reading, adaptive_governance, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(magna_carta_1215__living_document_reading, adaptive_governance, agenda_setter).

% Use the living document framework to expand rights protections beyond 1215 categories (due process, equal protection, privacy). Their reading coexists with and is enabled by the living document mechanism. They would lose their primary interpretive pathway if the framework reverted to originalism.
narrative_ontology:constraint_stakeholder(magna_carta_1215__living_document_reading, universal_rights_proponents, beneficiary,
    organized, generational, mobile, global).

% Bear the cost of having their preferred fixed-meaning arguments systematically marginalized in binding precedent. They must litigate within a doctrinal framework they regard as illegitimate. Their exit options are constrained: they can dissent, advocate for appointments, or seek constitutional amendment, but cannot opt out of the precedential system's authority.
narrative_ontology:constraint_stakeholder(magna_carta_1215__living_document_reading, originalist_claimants, payer,
    moderate, biographical, constrained, national).

% Individuals and groups who structured their affairs reliance on specific original understandings (e.g., property rights, federalism boundaries) and find those understandings superseded by evolved doctrine. They have no organized voice and limited exit; the cost of doctrinal change falls on them directly.
narrative_ontology:constraint_stakeholder(magna_carta_1215__living_document_reading, fixed_meaning_reliants, payer,
    powerless, immediate, constrained, local).

% Advocates of the historical feudal-contract reading who argue Magna Carta protects only the specific privileges of 1215 barons. They are excluded from contemporary constitutional conversation because their reading has no institutional uptake; the living document framework treats their position as a historical artifact, not a live option.
narrative_ontology:constraint_stakeholder(magna_carta_1215__living_document_reading, baronial_privilege_proponents, excluded,
    powerless, immediate, trapped, local).

% Analyze the living document framework from outside the adjudicative system. They document the drift from original meaning, evaluate the legitimacy of interpretive methods, and supply the intellectual infrastructure that courts draw on. They neither collect nor pay the constraint's extraction.
narrative_ontology:constraint_stakeholder(magna_carta_1215__living_document_reading, constitutional_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for legitimate constitutional evolution through interpretive tradition and precedential accumulation, enabling the constitutional order to adapt to novel circumstances without formal amendment while maintaining continuity with the founding text.
% TRANSFER_FUNCTION: Moves interpretive authority from the original 1215 understanding to the accumulated body of judicial precedent and scholarly interpretation, transferring the power to define constitutional meaning from historical actors to living institutions and from fixed textual anchors to evolving doctrinal structures.
% ABSENT_VOICES: The 1215 barons themselves (historically excluded by time); contemporary originalist judges and scholars who argue the living document framework lacks democratic legitimacy but operate within the precedential system they critique; populations in former colonies whose constitutional orders inherited the living document framework without consent.
% DISAPPEARANCE_RATIONALE: If the living document framework vanished overnight, constitutional meaning would revert to original 1215 understandings or require constant formal amendment; the adaptive capacity of the constitutional order would collapse, centuries of precedential development would lose their authoritative force, and rights expansions (due process, equal protection, privacy) grounded in evolved interpretation would lose their doctrinal foundation.
% FOUNDING_PROBLEM: The 1215 charter's specific feudal provisions became obsolete as social and political conditions changed; a mechanism was needed to preserve constitutional continuity while allowing meaning to evolve with changing circumstances, avoiding both revolutionary rupture and textual rigidity.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians (Maitland, Pocock, Holt) document the historical supersession of feudal provisions by the 14th century; comparative constitutional scholars (Ackerman, Elkins, Ginsburg) note that all enduring written constitutions develop living interpretive traditions; the correspondence is attested from outside the benefiting parties by historians of English law and comparative constitutionalism.
narrative_ontology:disappearance_verdict(magna_carta_1215__living_document_reading, world_rearranges).
narrative_ontology:founding_problem_status(magna_carta_1215__living_document_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_1215__living_document_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(magna_carta_1215__living_document_reading, 'none', 1).
narrative_ontology:epsilon_provenance(magna_carta_1215__living_document_reading, 0.35, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness is moderate (0.35 at interval end) because the living framework does impose costs on those who rely on fixed original meaning, but these costs are framed as the price of constitutional adaptability rather than rent extraction. Suppression is low (0.25) because originalist arguments remain legally permissible and are regularly heard in court — they are marginalized in outcomes, not silenced in process. Theater ratio is low (0.15) because the precedential system performs genuine doctrinal work; the performative element (ceremonial invocation of Magna Carta in opinions unrelated to its text) is minor. Accessibility collapse is moderate (0.45) because originalism remains a live interpretive option intellectually, though institutionally constrained. Resistance is moderate (0.55) reflecting the persistent originalist counter-movement.
 *
 * PERSPECTIVAL GAP:
 *   From the constitutional court seat, the living document framework appears as genuine coordination (rope) — it enables the legal system to function across centuries. From the originalist claimant seat, the same structure appears as extraction (snare/tangled rope) — their reliance interests are overridden by a precedent they regard as illegitimate. From the fixed-meaning reliant seat, it appears as suppression without representation. The engine computes this divergence from the structural data: same constraint, different directionalities, different effective extractions.
 *
 * DIRECTIONALITY LOGIC:
 *   Constitutional courts are the agenda-setters with analytical exit (they define the framework from within). Legal practitioners and universal rights proponents are beneficiaries with mobile/constrained exit — they gain professional utility and rights-expansion pathways. Future generations are trapped beneficiaries — they inherit the adaptive capacity but cannot consent. Adaptive governance institutions are dual-positioned beneficiaries/agenda-setters. Originalist claimants and fixed-meaning reliants are payers with constrained exit — they bear the cost of doctrinal supersession but cannot opt out of the system's authority. Baronial privilege proponents are excluded — their reading has no institutional purchase. Constitutional scholars are analytical observers.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (constitutional adaptation without rupture) remains live — modern governance challenges (digital rights, climate liability, algorithmic due process) continue to require evolved interpretation. The living document reading resolves mandatrophy by declaring the adaptation mechanism itself as the enduring constitutional function, not any specific 1215 provision. The mandate has not atrophied; it has metastasized into the entire common law constitutional order.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_extraction_boundary,
    'Is the living document framework a genuine coordination mechanism for constitutional continuity, or does it extract from originalist positions by declaring their reliance interests illegitimate?',
    'Comparative analysis of constitutional systems with and without living document traditions: if systems without living traditions (strict originalism/textualism) exhibit higher constitutional rupture rates or amendment failure, the coordination function is vindicated; if they exhibit stable adaptation through formal amendment, the living framework''s extraction from originalist reliance is exposed as unnecessary.',
    'If coordination function is primary, the constraint remains rope; if extraction from originalist reliance is primary and unnecessary, it reclassifies toward tangled_rope or snare for the originalist seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, empirical, 'Whether the living document framework''s adaptive capacity justifies its supersession of original meaning, or whether formal amendment could achieve the same adaptation without doctrinal drift.').

omega_variable(
    legitimacy_source_ambiguity,
    'Does the living document framework''s authority rest on conventional social coordination (the bar''s acceptance of stare decisis) or on a contested empirical claim about democratic legitimacy (that judicial evolution better reflects popular will than formal amendment)?',
    'Genealogical analysis of judicial self-justification: if courts explicitly ground living interpretation in democratic legitimacy claims that are empirically falsifiable (e.g., ''evolving standards of decency'' tracking public opinion), the grounding is empirically contingent and vulnerable to foreclosure; if they ground it in conventional role morality (''this is what judges do''), the grounding is conventional and stable.',
    'If empirically contingent and the empirical claim fails, the axiom ''constitutional_meaning_evolves_through_precedent'' may be overridden, triggering drift toward repudiation_pressure. If conventional, the framework is robust to empirical challenge.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(legitimacy_source_ambiguity, conceptual, 'The epistemic grounding of the living document framework''s authority — conventional vs. empirically contingent — and its vulnerability to axiom_overriding drift.').

omega_variable(
    kernel_reading_structure,
    'Does the living document reading foreclose, coexist with, or influence the baronial privilege and universal rights readings, and is this relationship symmetric?',
    'Institutional mapping: trace whether baronial privilege arguments are formally excluded from courts (foreclosed) or merely lose (coexist); trace whether universal rights expansions cite the living document framework as enabling mechanism (influences) or operate independently.',
    'If forecloses baronial reading, the reading_relations entry must change from influences to forecloses. If universal rights reading operates independently, relation changes from coexists_with to influences (unidirectional). Asymmetry in relations affects contamination propagation in the constraint family.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_structure, conceptual, 'Structural relationships among the three Magna Carta readings — whether the living document reading''s meta-constraint position creates asymmetric influence or symmetric coexistence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_1215__living_document_reading, 0, 809).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(magna_carta_living_doc_tr_t0, magna_carta_1215__living_document_reading, theater_ratio, 0, 0.02).
narrative_ontology:measurement(magna_carta_living_doc_tr_t200, magna_carta_1215__living_document_reading, theater_ratio, 200, 0.05).
narrative_ontology:measurement(magna_carta_living_doc_tr_t400, magna_carta_1215__living_document_reading, theater_ratio, 400, 0.08).
narrative_ontology:measurement(magna_carta_living_doc_tr_t500, magna_carta_1215__living_document_reading, theater_ratio, 500, 0.1).
narrative_ontology:measurement(magna_carta_living_doc_tr_t600, magna_carta_1215__living_document_reading, theater_ratio, 600, 0.12).
narrative_ontology:measurement(magna_carta_living_doc_tr_t700, magna_carta_1215__living_document_reading, theater_ratio, 700, 0.14).
narrative_ontology:measurement(magna_carta_living_doc_tr_t809, magna_carta_1215__living_document_reading, theater_ratio, 809, 0.15).

% Extraction over time
narrative_ontology:measurement(magna_carta_living_doc_be_t0, magna_carta_1215__living_document_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(magna_carta_living_doc_be_t200, magna_carta_1215__living_document_reading, base_extractiveness, 200, 0.12).
narrative_ontology:measurement(magna_carta_living_doc_be_t400, magna_carta_1215__living_document_reading, base_extractiveness, 400, 0.22).
narrative_ontology:measurement(magna_carta_living_doc_be_t500, magna_carta_1215__living_document_reading, base_extractiveness, 500, 0.28).
narrative_ontology:measurement(magna_carta_living_doc_be_t600, magna_carta_1215__living_document_reading, base_extractiveness, 600, 0.31).
narrative_ontology:measurement(magna_carta_living_doc_be_t700, magna_carta_1215__living_document_reading, base_extractiveness, 700, 0.33).
narrative_ontology:measurement(magna_carta_living_doc_be_t809, magna_carta_1215__living_document_reading, base_extractiveness, 809, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(magna_carta_living_doc_su_t0, magna_carta_1215__living_document_reading, suppression_requirement, 0, 0.05).
narrative_ontology:measurement(magna_carta_living_doc_su_t200, magna_carta_1215__living_document_reading, suppression_requirement, 200, 0.1).
narrative_ontology:measurement(magna_carta_living_doc_su_t400, magna_carta_1215__living_document_reading, suppression_requirement, 400, 0.15).
narrative_ontology:measurement(magna_carta_living_doc_su_t500, magna_carta_1215__living_document_reading, suppression_requirement, 500, 0.18).
narrative_ontology:measurement(magna_carta_living_doc_su_t600, magna_carta_1215__living_document_reading, suppression_requirement, 600, 0.2).
narrative_ontology:measurement(magna_carta_living_doc_su_t700, magna_carta_1215__living_document_reading, suppression_requirement, 700, 0.22).
narrative_ontology:measurement(magna_carta_living_doc_su_t809, magna_carta_1215__living_document_reading, suppression_requirement, 809, 0.25).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_1215__living_document_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(magna_carta_1215__living_document_reading, 0.08).
narrative_ontology:affects_constraint(magna_carta_1215__living_document_reading, common_law_precedent_system).
narrative_ontology:affects_constraint(magna_carta_1215__living_document_reading, constitutional_amendment_rules).
narrative_ontology:affects_constraint(magna_carta_1215__living_document_reading, judicial_review_doctrine).
narrative_ontology:affects_constraint(magna_carta_1215__living_document_reading, magna_carta_1215__baronial_privilege_reading).
narrative_ontology:affects_constraint(magna_carta_1215__living_document_reading, magna_carta_1215__universal_rights_reading).

% DUAL FORMULATION NOTE:
% This constraint (living_document_reading) and its siblings (baronial_privilege_reading, universal_rights_reading) form a constraint family decomposing the colloquial label 'Magna Carta.' The living document reading provides the meta-mechanism (precedential accumulation) that processes the other readings' claims. ε differs substantially: baronial_privilege_reading has near-zero extractiveness (historical artifact); universal_rights_reading has moderate extractiveness (rights-expansion claims against states); living_document_reading has moderate extractiveness (supersession of fixed-meaning reliance). They are linked by network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(magna_carta_1215__living_document_reading, moderate, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
