% ============================================================================
% CONSTRAINT STORY: fisa_702_statutory_text__constitutional_floor_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fisa_702_constitutional_floor, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: fisa_702_statutory_text__constitutional_floor_reading
 *   human_readable: Fourth Amendment Warrant Requirement for FISA 702 Queries (Constitutional Floor Reading)
 *   domain: constitutional_law/national_security/surveillance_policy
 *
 * SUMMARY:
 *   The FISA 702 program permits collection of communications where one party
 *   is a non-U.S. person reasonably believed to be outside the U.S. and the
 *   target is a foreign intelligence matter. Incidentally collected U.S.
 *   person communications are retained and can be queried by government
 *   agencies. The constitutional floor reading claims Fourth Amendment
 *   warrant requirement applies to ALL queries of U.S. person communications
 *   content, regardless of whether the original collection was lawful foreign
 *   intelligence targeting. This reading reframes the problem: not 'is
 *   foreign intelligence collection lawful' but 'are domestic person content
 *   searches by executive fiat constitutional.' The reading creates a tangled
 *   rope: genuine coordination function (protecting Fourth Amendment rights
 *   from executive expansion) bundled with extraction cost (intelligence
 *   operations must accept judicial review delays and oversight). The claim
 *   and metrics are independent: claimed_type is tangled_rope (coordination +
 *   enforcement asymmetry); extractiveness is low (0.25) because the
 *   constraint primarily imposes compliance cost rather than pure rent,
 *   shifting from operational speed (executive preference) to constitutional
 *   review (judicial requirement).
 *
 * KEY AGENTS:
 *   - u_s_persons_subject_to_702_collection: trapped powerless agents whose communications may be collected but content cannot be searched without warrant
 *   - executive_intelligence_agencies: institutional payer bearing warrant requirement as compliance cost; agenda_setter that could resist reading via litigation/legislative action
 *   - fisa_court: institutional agenda_setter whose authority expands to warrant review for every query
 *   - congress: excluded party that wrote statutory foreign intelligence exception but is bound by constitutional floor under this reading
 *   - supreme_court: ultimate observer/arbiter whose constitutional interpretation determines if reading binds
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fisa_702_statutory_text__constitutional_floor_reading, 0.25).
domain_priors:suppression_score(fisa_702_statutory_text__constitutional_floor_reading, 0.45).
domain_priors:theater_ratio(fisa_702_statutory_text__constitutional_floor_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fisa_702_statutory_text__constitutional_floor_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(fisa_702_statutory_text__constitutional_floor_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(fisa_702_statutory_text__constitutional_floor_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fisa_702_statutory_text__constitutional_floor_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(fisa_702_statutory_text__constitutional_floor_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fisa_702_statutory_text__constitutional_floor_reading, tangled_rope).
narrative_ontology:human_readable(fisa_702_statutory_text__constitutional_floor_reading, "Fourth Amendment Warrant Requirement for FISA 702 Queries (Constitutional Floor Reading)").
narrative_ontology:topic_domain(fisa_702_statutory_text__constitutional_floor_reading, "constitutional_law/national_security/surveillance_policy").

domain_priors:requires_active_enforcement(fisa_702_statutory_text__constitutional_floor_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fisa_702_statutory_text__constitutional_floor_reading, '0ba7a4f8-ac6e-4947-87ee-7113d9605435').
narrative_ontology:cs_kernel_codification('0ba7a4f8-ac6e-4947-87ee-7113d9605435', fixed_text).
narrative_ontology:cs_authority_grounding('0ba7a4f8-ac6e-4947-87ee-7113d9605435', lineage).
narrative_ontology:cs_interpretation_layer_present('0ba7a4f8-ac6e-4947-87ee-7113d9605435').
narrative_ontology:cs_reading_relation('0ba7a4f8-ac6e-4947-87ee-7113d9605435', fisa_702_statutory_text__incidental_collection_reading, forecloses).
narrative_ontology:cs_reading_relation('0ba7a4f8-ac6e-4947-87ee-7113d9605435', fisa_702_statutory_text__foreign_target_strict_reading, influences).
narrative_ontology:cs_axiom('0ba7a4f8-ac6e-4947-87ee-7113d9605435', foundational, fourth_amendment_absolute_content_protection).
narrative_ontology:cs_axiom_status(fourth_amendment_absolute_content_protection, holdable).
narrative_ontology:cs_axiom_grounding('0ba7a4f8-ac6e-4947-87ee-7113d9605435', fourth_amendment_absolute_content_protection, deontological).
narrative_ontology:cs_axiom('0ba7a4f8-ac6e-4947-87ee-7113d9605435', foundational, warrant_requirement_applies_pre_query).
narrative_ontology:cs_axiom_status(warrant_requirement_applies_pre_query, holdable).
narrative_ontology:cs_axiom_grounding('0ba7a4f8-ac6e-4947-87ee-7113d9605435', warrant_requirement_applies_pre_query, deontological).
narrative_ontology:cs_reference_frame('0ba7a4f8-ac6e-4947-87ee-7113d9605435', constitution_primacy_over_statute).
narrative_ontology:cs_drift_state('0ba7a4f8-ac6e-4947-87ee-7113d9605435', contemporary_post_snowden_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('0ba7a4f8-ac6e-4947-87ee-7113d9605435', '').
narrative_ontology:cs_kernel_id(fisa_702_statutory_text__constitutional_floor_reading, fisa_702_statutory_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fisa_702_statutory_text__constitutional_floor_reading, fourth_amendment_protections).
narrative_ontology:constraint_beneficiary(fisa_702_statutory_text__constitutional_floor_reading, u_s_persons_subject_to_702_collection).
narrative_ontology:constraint_victim(fisa_702_statutory_text__constitutional_floor_reading, executive_intelligence_operations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(fisa_702_statutory_text__constitutional_floor_reading, privacy_advocacy_groups).
narrative_ontology:constraint_victim(fisa_702_statutory_text__constitutional_floor_reading, executive_intelligence_agencies).
narrative_ontology:constraint_vindicates(fisa_702_statutory_text__constitutional_floor_reading, fourth_amendment_as_structural_limit).
narrative_ontology:constraint_vindicates(fisa_702_statutory_text__constitutional_floor_reading, warrant_requirement_content_protection).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% U.S. citizens whose communications are incidentally collected via FISA 702 collection targeting foreign persons abroad, then later queried by U.S. government agencies. Under this reading, their communications content cannot be searched without a warrant supported by probable cause, even when the original collection was lawful foreign intelligence targeting. They have no practical exit: their communications may be collected regardless of their actions, but government querying of content requires the Fourth Amendment warrant standard.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__constitutional_floor_reading, u_s_persons_subject_to_702_collection, beneficiary,
    powerless, biographical, trapped, national).

% Executive branch (NSA, FBI, CIA) operates the 702 collection and query infrastructure. Under this reading they bear a compliance cost: every query of U.S. person content requires FISA Court review of probable cause and individualized justification, not merely a showing of foreign intelligence purpose or incidental-collection legality. This constrains operational speed and expands judicial oversight of executive targeting decisions.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__constitutional_floor_reading, executive_intelligence_agencies, payer,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(fisa_702_statutory_text__constitutional_floor_reading, executive_intelligence_agencies, agenda_setter).

% Judicial body that must conduct individualized probable cause review before any query of incidentally collected U.S. person communications content. Under this reading, the FISA Court's function expands from foreign intelligence oversight to criminal procedure gatekeeper for content searches, creating workload and establishing precedent that warrants may issue on foreign intelligence grounds as probable cause.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__constitutional_floor_reading, fisa_court, agenda_setter,
    institutional, generational, analytical, national).

% Statutory authority (FISA as codified) does not explicitly require warrants for 702 queries; this reading reinterprets existing statutory structure through constitutional floor. Congress could amend FISA to codify warrant requirements, narrowing the constraint's scope, or could resist and establish that statutory foreign intelligence collection overrides Fourth Amendment query requirements—but under this reading Congress cannot do the latter and remain constitutional.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__constitutional_floor_reading, congress, excluded,
    institutional, generational, analytical, national).

% Organizations arguing for Fourth Amendment protections on FISA collection and queries. They benefit from a constitutional floor that raises barriers to U.S. person targeting. Their position has political and legal resources but limited direct leverage over executive operations; they pursue litigation and legislative channels.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__constitutional_floor_reading, privacy_advocacy_groups, beneficiary,
    organized, biographical, mobile, national).

% Non-U.S. persons who are the lawful foreign intelligence targets of 702 collection. They are outside the Fourth Amendment's reach, but their communications may be collected and the incidental U.S. person content within them becomes subject to warrant requirements for U.S. government queries under this reading. They have no standing to invoke or challenge this constraint.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__constitutional_floor_reading, foreign_intelligence_targets_abroad, observer,
    powerless, immediate, analytical, global).

% Final arbiter of constitutional interpretation. This reading claims Fourth Amendment floor regardless of statutory language; if Supreme Court rejects the reading and upholds statutory foreign intelligence exception, the constraint dissolves. If the Court endorses it, the constraint becomes binding constitutional law and overrides statutory ambiguity.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__constitutional_floor_reading, supreme_court, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fisa_702_statutory_text__constitutional_floor_reading, diffuse).
narrative_ontology:fixing_cost_class(fisa_702_statutory_text__constitutional_floor_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a constitutional floor protecting U.S. persons from warrantless searches of their communications content by federal government agencies, irrespective of whether the original collection was lawful foreign intelligence targeting. Coordinates judicial oversight (FISA Court warrant review) with executive access (query authorization), preventing unconstrained executive access to incidentally collected domestic communications.
% TRANSFER_FUNCTION: Transfers authority to conduct content searches from executive unilateral determination (foreign intelligence purpose suffices) to judiciary (FISA Court probable cause review required). Moves operational discretion from speed and secrecy to constitutional compliance review on every query of domestic person content.
% ABSENT_VOICES: Foreign intelligence targets abroad have no Fourth Amendment standing and cannot raise this constraint; they are excluded from the warrant/query regime. Executive branch at the operational level (field agents, collection managers not party to litigation) who argue warrant requirements impair intelligence gathering are also effectively excluded from formal decision-making about this constitutional floor, though their interests structure the execution conflict.
% DISAPPEARANCE_RATIONALE: If this reading disappeared and statutory foreign intelligence exception held: FISA 702 collection and queries would operate without Fourth Amendment warrant requirement for U.S. person content discovered incidentally. The constraint's disappearance would not rearrange the physical world—the statute and operations persist—but would eliminate a constitutional check on executive query authority. The contest is whether that check exists at all, not whether world logistics change; the verdict is 'contested' because executive and legislative branches claim statutory authority eliminates the warrant requirement, while Fourth Amendment absolutists claim the Constitution cannot be overridden by statute.
% FOUNDING_PROBLEM: Post-9/11 foreign intelligence collection exposed U.S. persons to warrantless surveillance when their communications were incidentally collected while targeting foreign persons abroad. The founding problem (from this reading's seat) is: how do we protect domestic persons from executive content searches conducted under foreign intelligence cover, without abolishing legitimate foreign intelligence operations?
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by U.S. Intelligence Community reports (NSA transparency disclosures, Congressional oversight reports) confirming large volumes of U.S. person communications are incidentally collected and later queried under 702. Privacy advocates, civil liberties litigation, and Congressional critics corroborate from outside the executive beneficiary set that the incidental-collection-to-query pathway is an open problem. The executive position that the problem is solved by minimization procedures and foreign intelligence justification is disputed by Fourth Amendment scholars and outside authorities.
narrative_ontology:disappearance_verdict(fisa_702_statutory_text__constitutional_floor_reading, contested).
narrative_ontology:founding_problem_status(fisa_702_statutory_text__constitutional_floor_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fisa_702_statutory_text__constitutional_floor_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(fisa_702_statutory_text__constitutional_floor_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fisa_702_statutory_text__constitutional_floor_reading, 0.25, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fisa_702_statutory_text__constitutional_floor_reading_tests).
:- end_tests(fisa_702_statutory_text__constitutional_floor_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.25) because this reading does not establish pure extraction—it enforces a constitutional protection. The extraction measured is the executive's loss of operational speed and secrecy: executive agencies cannot query incidentally collected U.S. person content without judicial warrant review, imposing delay and transparency costs. These are compliance burdens, not rents to a beneficiary. Suppression is moderate (0.45) because the constraint faces active legal/legislative resistance: executive branch and foreign intelligence hawks argue the reading conflicts with statutory authority and operational necessity. Resistance is high (0.72) because the reading is actively disputed in litigation and Congress; Fourth Amendment absolutists and civil liberties advocates mount sustained legal challenges. Accessibility_collapse is moderate-high (0.68) because once the Fourth Amendment floor is established, the alternative of warrantless query becomes legally inaccessible (collapse is high), but the reading itself is contested—alternative readings remain live political options (collapse is not complete). Theater_ratio is low (0.22) because the constraint's function is not performative: FISA Court warrant review for queries is genuinely gatekeeping, not theater. The small theatrical component is the foreign intelligence justification language (invoking foreign intelligence purpose as probable cause, which may obscure purely domestic surveillance).
 *
 * PERSPECTIVAL GAP:
 *   Divergence centers on whether the Fourth Amendment 'floor' pre-exists statutory interpretation or is a creative constitutional reading imposed despite statutory language favoring executive discretion. Executive and statutory-authority proponents argue the reading extracts from legitimate intelligence operations by imposing constraints Congress rejected. Fourth Amendment absolutists argue the reading simply applies the Constitution's actual floor. Mandate analysis: this reading avoids the false-rope trap (claiming pure coordination when extraction exists) by explicitly acknowledging the enforcement asymmetry: executive compliance cost is paired with judicial oversight expansion and Fourth Amendment protection for u_s_persons. The reading is not a snare because the coordination function (protecting Fourth Amendment rights) is genuine and benefits the protected class; it is tangled because the benefit and cost are bundled in a single mechanism (warrant review) and require active enforcement (FISA Court must review every query).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality for executive_intelligence_agencies: high (d ≈ 0.75), approaching target position. They bear the warrant requirement as a compliance cost; their operational autonomy is constrained; exit options are effectively constrained (they cannot abandon 702 collection but must comply with warrant review, or they must litigate/lobby to overturn the reading). Power is institutional (high), so the cost hits a powerful actor, but the structure targets their discretion, not their existence. Directionality for u_s_persons_subject_to_702_collection: low (d ≈ 0.15), beneficiary position despite being powerless. They receive the Fourth Amendment protection; they have no exit (communications may be collected regardless), but the constraint prevents queries of their content without warrant. The benefit is direct and constitutional. Directionality for FISA_court: symmetric (d ≈ 0.50). The court gains authority and responsibility; the expansion serves its institutional interest in relevance and constitutional role, but it also bears workload and precedent costs. Power is institutional; time_horizon is generational; exit_options are analytical (courts do not exit their jurisdiction).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is 'how do we protect Fourth Amendment rights against executive expansion of foreign intelligence surveillance?' Under this reading the founding problem is LIVE: every query of incidentally collected U.S. person content is a potential Fourth Amendment violation without a warrant. The constraint prevents the violation. However, mandatrophy could emerge if: (1) Supreme Court rejects the reading and holds FISA statute overrides Fourth Amendment for foreign intelligence, or (2) Congress explicitly amends FISA to authorize warrantless queries, formally rejecting the constitutional floor. At that point the mandate (Fourth Amendment protection) would be dead while the constraint (warrant review requirement) persists, creating a zombie constraint maintained by judicial/legislative inertia. Currently the constraint is live—it enforces a live constitutional mandate—but faces high resistance from executive and some legislative quarters.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fourth_amendment_search_definition,
    'Does querying a database of incidentally collected communications constitute a ''search'' under Fourth Amendment doctrine, or is the search the original collection (which was lawful foreign intelligence targeting)?',
    'Supreme Court precedent on digital searches (e.g., Riley v. California, Carpenter v. United States) clarifies whether accessing stored communications content in a database triggers independent Fourth Amendment analysis. If Carpenter-line reasoning applies to government databases, query = search; if original-collection doctrine controls, query is not an independent search.',
    'If query = search, warrant requirement applies to every query (this reading''s position). If query is not independent search, warrant requirement does not apply (statutory foreign intelligence exception controls). The classification hinges on the search concept.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fourth_amendment_search_definition, conceptual, 'What constitutes a Fourth Amendment search in the FISA 702 context—is it the collection or the query?').

omega_variable(
    statutory_authority_vs_constitutional_floor,
    'Can Congressional statutory language authorizing executive action in foreign intelligence override a Fourth Amendment constitutional floor, or is the Fourth Amendment an absolute limit Congress cannot contract around?',
    'Supreme Court ruling on the relationship between FISA statutory authority and Fourth Amendment limits. Precedent: some Fourth Amendment protections are held absolute (e.g., warrant requirement for home searches cannot be overridden by statute), while others admit statutory exceptions (e.g., administrative searches, national security exception to warrant requirement in some contexts).',
    'If Fourth Amendment search warrant requirement is absolute, this reading''s constraint is binding regardless of statutory language. If statutory national security exception to warrant requirement is permissible, the statute can override the constitutional floor and the constraint dissolves.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(statutory_authority_vs_constitutional_floor, empirical, 'Whether Fourth Amendment warrant requirement for searches is absolute or admits statutory exceptions in the national security context.').

omega_variable(
    incidental_collection_doctrine,
    'Does lawful foreign intelligence collection that incidentally captures U.S. person communications authorize later government queries of that U.S. person content, or does incidental capture not confer query authority?',
    'FISA statute interpretation + Fourth Amendment doctrine on derivative searches. If incidental capture confers no authority to search the captured content, each query is a fresh search requiring independent warrant justification. If incidental capture confers authority to access the content for foreign intelligence purpose, warrant may not be required.',
    'If incidental capture does not confer query authority, this reading''s warrant requirement is correct. If it does confer query authority, queries of incidentally collected material fall within the foreign intelligence exception and do not require Fourth Amendment warrant.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incidental_collection_doctrine, conceptual, 'Whether lawful foreign intelligence collection implicitly authorizes queries of incidentally captured U.S. person content.').

omega_variable(
    suppression_mechanism_structural,
    'Is the measured suppression (0.45) primarily structural (executive/operational resistance to judicial review delays) or primarily internalized (legal/doctrinal disagreement about whether Fourth Amendment applies)?',
    'If executive compliance with FISA Court warrant review occurs without litigation when the ruling is formally established, suppression is primarily structural. If persistent legal challenges and legislative resistance continue after formal establishment, suppression is partly internalized (institutional resistance to the reading itself).',
    'If structural, the constraint could persist once administrative machinery stabilizes (warrant review becomes routine). If internalized, the constraint requires continuous active enforcement against doctrinal resistance and faces higher collapse risk if institutional balance shifts.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_structural, empirical, 'Whether suppression of this constraint is structural/operational or internalized/doctrinal.').

omega_variable(
    foreign_intelligence_versus_criminal_procedure,
    'Is FISA fundamentally a foreign intelligence statute (where warrant requirement may not apply) or a criminal procedure statute subject to Fourth Amendment floors (where warrant requirement applies)?',
    'Statutory interpretation of FISA''s preamble, structure, and Congressional intent. Litigation establishing the statute''s primary purpose and the scope of its exception to traditional criminal procedure rules.',
    'If FISA is foreign intelligence-primary, executive has broader authority to query incidentally collected content without warrant. If FISA is criminal-procedure-adjacent, warrant requirement applies. This reading reframes FISA as a criminal procedure question; the sibling reading maintains it as foreign intelligence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(foreign_intelligence_versus_criminal_procedure, conceptual, 'Whether FISA 702 is fundamentally a foreign intelligence statute or a criminal procedure statute.').

omega_variable(
    kernel_reading_contest_structure,
    'This reading and its sibling readings (foreign_target_strict, incidental_collection) all claim the same FISA 702 statute as kernel but assign different readings. Are these readings logically incompatible (forecloses), politically coexistent (coexists_with), or hierarchically ordered (influences)? Which reading''s interpretation of ''search'' and ''probable cause'' is the controlling doctrine?',
    'Supreme Court precedent on FISA interpretation and Fourth Amendment scope establishes which reading the Constitution requires. Absent Supreme Court clarity, all three readings remain contestable institutional positions.',
    'If this reading forecloses the incidental_collection_reading, the warrant requirement is mandatory and the statute is reinterpreted. If readings coexist, different institutional seats (judiciary vs. executive) apply different readings and conflict persists. If this reading influences but does not foreclose, it raises the doctrinal cost of the incidental_collection reading without eliminating it.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest_structure, conceptual, 'The structural relationship among the three sibling readings of the FISA 702 kernel and which reading will establish controlling doctrine.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fisa_702_statutory_text__constitutional_floor_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fisa_tr_t0, fisa_702_statutory_text__constitutional_floor_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(fisa_tr_t0, observed).
narrative_ontology:measurement(fisa_tr_t4, fisa_702_statutory_text__constitutional_floor_reading, theater_ratio, 4, 0.18).
narrative_ontology:measurement_basis(fisa_tr_t4, projected).
narrative_ontology:measurement(fisa_tr_t8, fisa_702_statutory_text__constitutional_floor_reading, theater_ratio, 8, 0.2).
narrative_ontology:measurement_basis(fisa_tr_t8, projected).
narrative_ontology:measurement(fisa_tr_t12, fisa_702_statutory_text__constitutional_floor_reading, theater_ratio, 12, 0.22).
narrative_ontology:measurement_basis(fisa_tr_t12, observed).
narrative_ontology:measurement(fisa_tr_t16, fisa_702_statutory_text__constitutional_floor_reading, theater_ratio, 16, 0.23).
narrative_ontology:measurement_basis(fisa_tr_t16, projected).
narrative_ontology:measurement(fisa_tr_t20, fisa_702_statutory_text__constitutional_floor_reading, theater_ratio, 20, 0.22).
narrative_ontology:measurement_basis(fisa_tr_t20, projected).
narrative_ontology:measurement(fisa_tr_t24, fisa_702_statutory_text__constitutional_floor_reading, theater_ratio, 24, 0.22).
narrative_ontology:measurement_basis(fisa_tr_t24, projected).

% Extraction over time
narrative_ontology:measurement(fisa_be_t0, fisa_702_statutory_text__constitutional_floor_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement_basis(fisa_be_t0, projected).
narrative_ontology:measurement(fisa_be_t4, fisa_702_statutory_text__constitutional_floor_reading, base_extractiveness, 4, 0.21).
narrative_ontology:measurement_basis(fisa_be_t4, projected).
narrative_ontology:measurement(fisa_be_t8, fisa_702_statutory_text__constitutional_floor_reading, base_extractiveness, 8, 0.24).
narrative_ontology:measurement_basis(fisa_be_t8, projected).
narrative_ontology:measurement(fisa_be_t12, fisa_702_statutory_text__constitutional_floor_reading, base_extractiveness, 12, 0.25).
narrative_ontology:measurement_basis(fisa_be_t12, observed).
narrative_ontology:measurement(fisa_be_t16, fisa_702_statutory_text__constitutional_floor_reading, base_extractiveness, 16, 0.26).
narrative_ontology:measurement_basis(fisa_be_t16, projected).
narrative_ontology:measurement(fisa_be_t20, fisa_702_statutory_text__constitutional_floor_reading, base_extractiveness, 20, 0.26).
narrative_ontology:measurement_basis(fisa_be_t20, projected).
narrative_ontology:measurement(fisa_be_t24, fisa_702_statutory_text__constitutional_floor_reading, base_extractiveness, 24, 0.25).
narrative_ontology:measurement_basis(fisa_be_t24, projected).

% Suppression requirement over time
narrative_ontology:measurement(fisa_su_t0, fisa_702_statutory_text__constitutional_floor_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement_basis(fisa_su_t0, observed).
narrative_ontology:measurement(fisa_su_t4, fisa_702_statutory_text__constitutional_floor_reading, suppression_requirement, 4, 0.41).
narrative_ontology:measurement_basis(fisa_su_t4, projected).
narrative_ontology:measurement(fisa_su_t8, fisa_702_statutory_text__constitutional_floor_reading, suppression_requirement, 8, 0.43).
narrative_ontology:measurement_basis(fisa_su_t8, projected).
narrative_ontology:measurement(fisa_su_t12, fisa_702_statutory_text__constitutional_floor_reading, suppression_requirement, 12, 0.45).
narrative_ontology:measurement_basis(fisa_su_t12, observed).
narrative_ontology:measurement(fisa_su_t16, fisa_702_statutory_text__constitutional_floor_reading, suppression_requirement, 16, 0.46).
narrative_ontology:measurement_basis(fisa_su_t16, projected).
narrative_ontology:measurement(fisa_su_t20, fisa_702_statutory_text__constitutional_floor_reading, suppression_requirement, 20, 0.45).
narrative_ontology:measurement_basis(fisa_su_t20, projected).
narrative_ontology:measurement(fisa_su_t24, fisa_702_statutory_text__constitutional_floor_reading, suppression_requirement, 24, 0.45).
narrative_ontology:measurement_basis(fisa_su_t24, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fisa_702_statutory_text__constitutional_floor_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(fisa_702_statutory_text__constitutional_floor_reading, 0.12).
narrative_ontology:affects_constraint(fisa_702_statutory_text__constitutional_floor_reading, fisa_702_statutory_text__foreign_target_strict_reading).
narrative_ontology:affects_constraint(fisa_702_statutory_text__constitutional_floor_reading, fisa_702_statutory_text__incidental_collection_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel fisa_702_statutory_text. Sibling readings foreign_target_strict_reading and incidental_collection_reading instantiate different interpretations of the same statutory text. The three readings form a constraint family: they share the kernel but assign different primacy to Fourth Amendment, statutory foreign intelligence exception, and minimization doctrine. This reading (constitutional_floor) claims Fourth Amendment warrant requirement overrides statutory ambiguity; the foreign_target reading claims statutory text narrowly constrains collection; the incidental_collection reading claims statutory exception permits warrantless queries. All three are live institutional positions linked by network.affects_constraints symmetry (each reading influences the others' legitimacy and doctrinal scope).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(fisa_702_statutory_text__constitutional_floor_reading, institutional, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
