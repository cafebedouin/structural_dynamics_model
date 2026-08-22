% ============================================================================
% CONSTRAINT STORY: refugee_convention_text__procedural_integrity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_refugee_convention_text__procedural_integrity_reading, []).

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
 *   constraint_id: refugee_convention_text__procedural_integrity_reading
 *   human_readable: Refugee Convention Text: Procedural Integrity Reading
 *   domain: international_law/migration/human_rights
 *
 * SUMMARY:
 *   The Refugee Convention's text has become a contested kernel. Three
 *   readings coexist: (1) the expansive-humanitarian reading, treating
 *   'well-founded fear' and 'particular social group' broadly to protect
 *   generalized violence and gender-based persecution; (2) the
 *   restrictive-sovereignty reading, interpreting the Convention as a minimum
 *   floor permitting maximum state discretion and requiring individualized
 *   persecution proof; (3) the procedural-integrity reading (this
 *   constraint), treating the Convention as primarily establishing a fair
 *   assessment procedure—outcome is secondary, but process is mandatory and
 *   internationally reviewable. This story instantiates the
 *   procedural-integrity reading. It does not engage or resolve the contest
 *   between humanitarian and restrictive frames; it operates in parallel to
 *   both. The extracted-from population includes applicants trapped in
 *   procedural gatekeeping and offshore-processed subjects whose procedural
 *   access is degraded. The beneficiary is the institutional role of
 *   independent review bodies whose authority rests on procedural
 *   guardianship. The constraint exhibits moderate extractiveness (0.38)
 *   because procedure-only protection is incomplete: applicants gain standing
 *   and voice but not substantive protection; states gain discretion but only
 *   within procedurally auditable bounds. Theater rises from 0.28 to 0.41
 *   over the interval as states perform procedural compliance while narrowing
 *   definitions—they invest in procedure machinery while using it to process
 *   faster rejections.
 *
 * KEY AGENTS:
 *   - procedurally_compliant_asylum_adjudicators — institutional beneficiary; frames authority as process-protection
 *   - states_with_restrictive_policies — institutional payer; bears cost of robust procedure even with narrow definitions
 *   - applicants_denied_substantive_review — powerless victims; trapped at procedural gatekeeping stages
 *   - offshore_processing_subjects — powerless victims; identity-locked in geographic displacement + procedural barriers
 *   - independent_review_bodies — institutional agenda-setter; operationalize and enforce procedural standards
 *   - restrictive_sovereignty_reading_states — excluded from deliberation; reject international procedural oversight
 *   - humanitarian_advocacy_community — excluded from deliberation; argue procedure is insufficient without broad substantive protection
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(refugee_convention_text__procedural_integrity_reading, 0.38).
domain_priors:suppression_score(refugee_convention_text__procedural_integrity_reading, 0.52).
domain_priors:theater_ratio(refugee_convention_text__procedural_integrity_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(refugee_convention_text__procedural_integrity_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(refugee_convention_text__procedural_integrity_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(refugee_convention_text__procedural_integrity_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(refugee_convention_text__procedural_integrity_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(refugee_convention_text__procedural_integrity_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(refugee_convention_text__procedural_integrity_reading, tangled_rope).
narrative_ontology:human_readable(refugee_convention_text__procedural_integrity_reading, "Refugee Convention Text: Procedural Integrity Reading").
narrative_ontology:topic_domain(refugee_convention_text__procedural_integrity_reading, "international_law/migration/human_rights").

domain_priors:requires_active_enforcement(refugee_convention_text__procedural_integrity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(refugee_convention_text__procedural_integrity_reading, '53c71258-8dbf-4234-88b4-f4713426efc5').
narrative_ontology:cs_kernel_codification('53c71258-8dbf-4234-88b4-f4713426efc5', fixed_text).
narrative_ontology:cs_authority_grounding('53c71258-8dbf-4234-88b4-f4713426efc5', lineage).
narrative_ontology:cs_interpretation_layer_present('53c71258-8dbf-4234-88b4-f4713426efc5').
narrative_ontology:cs_reading_relation('53c71258-8dbf-4234-88b4-f4713426efc5', refugee_convention_text__restrictive_sovereignty_reading, coexists_with).
narrative_ontology:cs_reading_relation('53c71258-8dbf-4234-88b4-f4713426efc5', refugee_convention_text__expansive_humanitarian_reading, coexists_with).
narrative_ontology:cs_axiom('53c71258-8dbf-4234-88b4-f4713426efc5', foundational, fair_individualized_assessment_binding).
narrative_ontology:cs_axiom_status(fair_individualized_assessment_binding, holdable).
narrative_ontology:cs_axiom_grounding('53c71258-8dbf-4234-88b4-f4713426efc5', fair_individualized_assessment_binding, deontological).
narrative_ontology:cs_axiom('53c71258-8dbf-4234-88b4-f4713426efc5', foundational, outcome_secondary_to_procedure).
narrative_ontology:cs_axiom_status(outcome_secondary_to_procedure, holdable).
narrative_ontology:cs_axiom_grounding('53c71258-8dbf-4234-88b4-f4713426efc5', outcome_secondary_to_procedure, conventional).
narrative_ontology:cs_reference_frame('53c71258-8dbf-4234-88b4-f4713426efc5', convention_as_procedural_standard).
narrative_ontology:cs_drift_state('53c71258-8dbf-4234-88b4-f4713426efc5', contemporary_offshore_processing_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('53c71258-8dbf-4234-88b4-f4713426efc5', '').
narrative_ontology:cs_kernel_id(refugee_convention_text__procedural_integrity_reading, refugee_convention_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(refugee_convention_text__procedural_integrity_reading, procedurally_compliant_asylum_adjudicators).
narrative_ontology:constraint_victim(refugee_convention_text__procedural_integrity_reading, applicants_denied_substantive_review).
narrative_ontology:constraint_victim(refugee_convention_text__procedural_integrity_reading, offshore_processing_subjects).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(refugee_convention_text__procedural_integrity_reading, states_with_restrictive_policies).
narrative_ontology:constraint_victim(refugee_convention_text__procedural_integrity_reading, states_with_restrictive_policies).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Courts, administrative tribunals, and review bodies that adopt the procedural-integrity reading frame their authority as *process guardians* rather than substantive policymakers. They benefit from a doctrine that legitimates their role (enforcing fair process), insulates them from policy pressure (the outcome is secondary to the procedure), and creates a class of cases they can credibly adjudicate. Procedural scrutiny is their institutional domain; this reading empowers them to strike down both over-generous and over-restrictive determinations on process grounds alone.
narrative_ontology:constraint_stakeholder(refugee_convention_text__procedural_integrity_reading, procedurally_compliant_asylum_adjudicators, beneficiary,
    institutional, generational, analytical, global).

% Bear the cost of maintaining robust procedural machinery (trained adjudicators, independent review, full evidentiary hearings) even when they wish to narrow the substantive protection threshold. The reading permits them to deny protection on narrow-definition grounds (e.g., 'particular social group' interpreted strictly) but only after exhaustive fair process. They must also absorb the cost of applicants who pass procedure-based challenges, even if rejected on substantive grounds they believe are permissible. The payer aspect: they fund the procedural infrastructure that the reading mandates.
narrative_ontology:constraint_stakeholder(refugee_convention_text__procedural_integrity_reading, states_with_restrictive_policies, payer,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(refugee_convention_text__procedural_integrity_reading, states_with_restrictive_policies, beneficiary).

% Asylum seekers whose applications are rejected at early procedural gatekeeping stages (language access, legal representation, time-bound filing deadlines) before their substantive claims are heard. Under this reading, they are the primary victims of procedural failures: if procedure collapses, their substantive protection claim never reaches adjudication. The reading's strength is that it focuses protective power on ensuring procedure integrity; its weakness is that procedural compliance is necessary but not sufficient—a fair procedure can still produce a lawful rejection.
narrative_ontology:constraint_stakeholder(refugee_convention_text__procedural_integrity_reading, applicants_denied_substantive_review, payer,
    powerless, biographical, trapped, global).

% Applicants routed to processing centers outside the protecting state's territory (Australian offshore processing, EU external borders). Under the procedural-integrity reading, they are victims only if the offshore location undermines procedural access (inadequate legal counsel, language barriers, inability to present evidence, lack of independent review). If offshore processing maintains full procedural guarantees (counsel, hearing, appeal), the reading permits it—outcome is secondary to procedure. Their identity-lock is geographic/legal: they are already displaced; the offshore location adds a second displacement barrier, making exit from the process itself structurally impossible.
narrative_ontology:constraint_stakeholder(refugee_convention_text__procedural_integrity_reading, offshore_processing_subjects, payer,
    powerless, biographical, identity_locked, global).

% Appeals courts, human rights ombudspersons, and international monitoring mechanisms that operationalize the procedural-integrity reading by auditing state conduct for compliance with fair-process standards. They set the procedural agenda—what counts as 'individualized assessment', what evidence access is required, what representation rights are non-waivable. Their power is procedural gatekeeping; their constraint is that they cannot substitute their own substantive judgment for the initial adjudicator's.
narrative_ontology:constraint_stakeholder(refugee_convention_text__procedural_integrity_reading, independent_review_bodies, agenda_setter,
    institutional, generational, analytical, global).

% States adhering to the restrictive-sovereignty reading would argue that procedural-integrity focus subordinates state borders and sovereign discretion to international process oversight. They are excluded from the procedural-integrity reading's deliberative frame—that reading does not engage the sovereignty-first premise. Their objection would be structural: the reading accepts international procedural scrutiny as legitimate, which they contest.
narrative_ontology:constraint_stakeholder(refugee_convention_text__procedural_integrity_reading, restrictive_sovereignty_reading_states, excluded,
    institutional, generational, trapped, global).

% NGOs, refugee councils, and human rights organizations that frame the Convention through the expansive-humanitarian reading. They would argue that procedural focus is insufficiently protective when procedures are applied to narrow definitions—you can have perfectly fair process rejecting someone whose need is real but whose persecution does not fit the 'particular social group' definition the restrictive reading endorses. They are excluded from the deliberative frame of the procedural-integrity reading, which treats outcome as secondary.
narrative_ontology:constraint_stakeholder(refugee_convention_text__procedural_integrity_reading, humanitarian_advocacy_community, excluded,
    organized, biographical, constrained, global).

% Researchers, international legal scholars, and human rights monitors who assess state compliance with procedural-integrity standards. They record whether independent review occurs, whether applicants have counsel and adequate time, whether decisions are reasoned and appealable. They do not adjudicate substance; they measure procedural fidelity.
narrative_ontology:constraint_stakeholder(refugee_convention_text__procedural_integrity_reading, international_court_observers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(refugee_convention_text__procedural_integrity_reading, states_with_restrictive_policies).
narrative_ontology:fixing_cost_class(refugee_convention_text__procedural_integrity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared international procedural standard (fair individualized assessment, reasoned decision-making, appellate access) that all states can adopt without committing to identical substantive outcomes. Solves the coordination problem: how can states with different threat assessments and capacities share a refugee protection framework without either total harmonization (expansive reading) or complete national discretion (restrictive reading). Procedure is the coordinating mechanism—all agree that fair process is legitimate; outcomes may vary.
% TRANSFER_FUNCTION: Moves procedural obligations (legal representation, training, time, evidentiary access) from asylum applicants to state adjudicators. States must absorb the cost of robust process even when they wish to narrowly define protection categories. The constraint transfers the burden of justification: states must explain rejection through documented procedure, not assertion. Applicants gain the right to be heard; states gain the right to narrow definitions, conditional on procedure being sound.
% ABSENT_VOICES: Expansive humanitarian reading holders (who would argue process is subordinate to protection breadth) and restrictive sovereignty reading holders (who would argue process is subordinate to state discretion) are both excluded from the procedural-integrity deliberative frame. Neither group's core premise is engaged—the reading does not contest whether borders matter or whether protection should be broad; it treats both as secondary to fair procedure. Alternative framings would assign different weight to coordination vs. substantive protection.
% DISAPPEARANCE_RATIONALE: If procedural-integrity enforcement vanished, states would adopt ad hoc, asymmetric asylum systems. Some would narrow definitions and eliminate appeal; others would expand protection and formalize it. There would be no shared standard for what counts as 'fair assessment'—and without that coordinating principle, neither interstate negotiation nor refugee advocacy would have a common vocabulary. International refugee governance would bifurcate between humanitarian havens and fortress states with no middle ground. The procedural framework is the structure holding the Convention together across divergent state preferences.
% FOUNDING_PROBLEM: Post-1951 refugee protection faced a crisis of legitimacy: how could states adopt a binding protection norm when they had irreconcilable views about who deserved protection and how much? The procedural-integrity answer: agree on *process*, not outcome. Fair individual assessment, independent review, reasoned denial, and appellate access are procedurally neutral—they do not commit a state to any particular protection threshold. A state can have a narrow definition of 'well-founded fear' and still comply with the Convention, provided the procedure is transparent and reviewable.
% FOUNDING_PROBLEM_CORROBORATION: International Court of Justice and European Court of Human Rights case law from the 1990s and 2000s (Soering, Chahal, Bankovic) established that procedure—access to counsel, reasoned decisions, independent review—was the core enforceable commitment of the Convention, separate from substantive breadth. These courts (external authorities, not beneficiaries of the reading) adopted the procedural-integrity framing when states' substantive commitments diverged. NGO advocacy reports (e.g., Human Rights Watch, Amnesty International) simultaneously criticize the procedural framing as insufficient when procedures are applied to narrow definitions—their critique corroborates that the reading *is* what courts enforce, even if advocates find it inadequate.
narrative_ontology:disappearance_verdict(refugee_convention_text__procedural_integrity_reading, world_rearranges).
narrative_ontology:founding_problem_status(refugee_convention_text__procedural_integrity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(refugee_convention_text__procedural_integrity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(refugee_convention_text__procedural_integrity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(refugee_convention_text__procedural_integrity_reading, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(refugee_convention_text__procedural_integrity_reading_tests).
:- end_tests(refugee_convention_text__procedural_integrity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.38) because the procedural-integrity frame trades substantive protection breadth for procedural fairness. An applicant with a genuine but legally-undefined claim (e.g., gang violence in a context not recognized as 'particular social group' persecution) can have a perfectly fair procedure that still results in lawful rejection. The constraint extracts from such applicants by redefining their access right from 'protection' to 'fair hearing'. Suppression is moderate-high (0.52) because maintaining the procedural frame requires states to eliminate frontier shortcuts and ad hoc rejections—states must invest in independent adjudication and appellate access, which suppresses their freedom to quickly expel. Theater rises from 0.28 to 0.41 because states increasingly perform procedural compliance (training, written reasons, appeal routes) while using the procedure framework to expedite rejections—the machinery looks fair but processes applicants faster. Accessibility collapse is 0.62 because alternatives to the Convention frame exist (discretionary asylum, humanitarian visas, regional protection) but the procedural-integrity reading has become the dominant articulation in international courts, so applicants who want to claim refugee status must enter procedure; they cannot escape it by contesting its framing. Resistance is high (0.71) because humanitarian advocates and some applicants resist the procedure-only focus as inadequate, and restrictive-sovereignty states resist the international review component as overreach. Both visible resistance streams.
 *
 * PERSPECTIVAL GAP:
 *   The divergence between procedural-integrity reading and the humanitarian/restrictive readings is located in what counts as the binding commitment. The procedural-integrity reading says: the Convention binds states to fair process, not to any particular protection threshold. States may redefine 'well-founded fear' and 'particular social group' narrowly, provided they do so via transparent, independent procedure. The expansive reading says: those definitions have been interpreted broadly by practice and principle; narrowing them violates the Convention's humanitarian purpose. The restrictive reading says: those definitions have always been narrow; fair procedure is secondary to state discretion. This story does not resolve which reading is correct. It articulates one reading fully and maps its structural consequences.
 *
 * DIRECTIONALITY LOGIC:
 *   Procedurally-compliant adjudicators and independent review bodies (institutional, powerful, analytical): beneficiary directionality (d near 0.2) because the reading legitimates and expands their institutional role. The procedural-integrity framing gives courts authority to strike down both over-generous and over-restrictive determinations on process grounds, which strengthens institutional judicial review. States with restrictive policies (institutional, powerful, constrained exit): payer directionality (d near 0.65) because they must absorb procedure costs while ceded outcome discretion (they can narrow definitions but only after proving fair process, which is costly). Applicants (powerless, trapped/identity-locked): target directionality (d near 0.85) because the constraint trades substantive protection access for procedural standing—high extraction from a powerless population with no exit. Independent review bodies also sit as agenda-setter, which creates secondary power; but their primary directionality is beneficiary (the reading empowers them). The asymmetry between agenda-setter and applicant is the core tangled-rope structure: the same procedure coordinates adjudication (shared standard for fairness) and extracts from powerless applicants (procedure becomes gatekeeping mechanism).
 *
 * MANDATROPHY ANALYSIS:
 *   The procedural-integrity reading exhibits a live founding problem (fair individual assessment in a system of divergent state preferences) but the constraint shows theater-ratio growth from 0.28 to 0.41, indicating that procedure is increasingly decoupled from substantive protection. States perform procedural compliance while narrowing definitions and accelerating rejections. This is theater accumulation on a background of stable extractiveness—the machinery is maintained but its protective function is attenuated. The constraint should be classified as tangled-rope because it combines genuine coordination (shared procedural standard enabling interstate cooperation) with asymmetric extraction (applicants gain procedure but not substantive access, while states gain discretion). The mandatrophy question is whether the founding-problem (how to coordinate refugee protection across divergent state interests) is still solved by procedural focus, or whether procedure has become a legitimation mechanism for convergent state narrowing. Theater rise suggests the latter: states use procedure to gain legitimacy for rejections, not to resolve genuine coordination dilemmas.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    procedural_sufficiency_for_protection,
    'Is fair procedure sufficient to satisfy the Convention''s protective mandate, or does process without substantive breadth create a legitimacy gap?',
    'Monitor state practice over a decade: if procedure-compliant states narrowing definitions face sustained humanitarian challenge or regional divergence (some states maintaining broad definitions despite procedural convergence pressure), procedure is insufficient. If procedural harmonization leads to substantive convergence, procedure is sufficient.',
    'If procedure is insufficient, the constraint reclassifies toward snare (procedure becomes extraction mechanism). If procedure is sufficient, it remains tangled-rope (genuine coordination + asymmetric outcome distribution).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(procedural_sufficiency_for_protection, empirical, 'Whether procedural fairness alone can sustain protection mandate legitimacy across divergent state preferences.').

omega_variable(
    kernel_reading_foreclosure,
    'Does procedural-integrity reading foreclose the restrictive-sovereignty reading within a single state''s legal framework, or do they coexist as alternative readings a state could adopt?',
    'Examine state conduct: can a state simultaneously endorse the procedural-integrity reading (fair process as binding) and the restrictive-sovereignty reading (narrow definitions as sovereign right)? If yes, they coexist. If state adoption of procedural integrity requires abandoning sovereignty-first framing, they foreclose.',
    'If they foreclose, the readings are logically incompatible; one reading''s adoption eliminates the other''s legitimacy. If they coexist, both can be held by different parties within a shared legal framework, and neither is eliminated by the other.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure, conceptual, 'Whether procedural-integrity and restrictive-sovereignty readings are logically incompatible or simultaneously holdable.').

omega_variable(
    offshore_processing_procedure_equivalence,
    'Can offshore processing facilities maintain procedurally-equivalent access to counsel, evidentiary presentation, and independent review as onshore processing, or are there structural barriers that degraded procedure inevitably?',
    'Comparative audit of offshore vs. onshore processing: examine counsel availability, hearing-decision intervals, appeal success rates, applicant language access. If metrics converge, procedure is equivalence-capable. If offshore metrics lag systematically, equivalence is structurally impossible.',
    'If procedurally equivalence is impossible, offshore processing is an extraction mechanism independent of good-faith procedure. Applicants in offshore locations are structurally victimized. If equivalence is possible, offshore processing is permissible under the procedural-integrity reading, and harm to offshore subjects becomes a state-failure (bad procedure) rather than reading-failure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(offshore_processing_procedure_equivalence, empirical, 'Whether geographic location of processing determines procedural access equivalence.').

omega_variable(
    reading_contention_location,
    'Is the core disagreement between procedural-integrity and other readings about *what the Convention requires* (substantive dispute) or about *how to read an ambiguous text* (interpretive dispute)?',
    'Examine whether the three readings can all claim textual grounding in the Convention''s language and drafting history. If yes, the contention is interpretive (reading an ambiguous text). If one reading requires reading the text against its plain language, the contention is substantive (one reading is wrong).',
    'If interpretive, all readings are legitimately derivative from the kernel; the constraint describes one valid reading among coherent alternatives. If substantive, one reading is false; the kernel does not support all three readings equally.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_contention_location, conceptual, 'Whether the three sibling readings represent different interpretations of an ambiguous kernel or different claims about what the kernel requires.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(refugee_convention_text__procedural_integrity_reading, 1980, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(refu_tr_t1980, refugee_convention_text__procedural_integrity_reading, theater_ratio, 1980, 0.28).
narrative_ontology:measurement_basis(refu_tr_t1980, observed).
narrative_ontology:measurement(refu_tr_t1995, refugee_convention_text__procedural_integrity_reading, theater_ratio, 1995, 0.32).
narrative_ontology:measurement_basis(refu_tr_t1995, observed).
narrative_ontology:measurement(refu_tr_t2005, refugee_convention_text__procedural_integrity_reading, theater_ratio, 2005, 0.38).
narrative_ontology:measurement_basis(refu_tr_t2005, observed).
narrative_ontology:measurement(refu_tr_t2015, refugee_convention_text__procedural_integrity_reading, theater_ratio, 2015, 0.41).
narrative_ontology:measurement_basis(refu_tr_t2015, observed).
narrative_ontology:measurement(refu_tr_t2024, refugee_convention_text__procedural_integrity_reading, theater_ratio, 2024, 0.41).
narrative_ontology:measurement_basis(refu_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(refu_be_t1980, refugee_convention_text__procedural_integrity_reading, base_extractiveness, 1980, 0.32).
narrative_ontology:measurement_basis(refu_be_t1980, observed).
narrative_ontology:measurement(refu_be_t1995, refugee_convention_text__procedural_integrity_reading, base_extractiveness, 1995, 0.36).
narrative_ontology:measurement_basis(refu_be_t1995, observed).
narrative_ontology:measurement(refu_be_t2005, refugee_convention_text__procedural_integrity_reading, base_extractiveness, 2005, 0.39).
narrative_ontology:measurement_basis(refu_be_t2005, observed).
narrative_ontology:measurement(refu_be_t2015, refugee_convention_text__procedural_integrity_reading, base_extractiveness, 2015, 0.37).
narrative_ontology:measurement_basis(refu_be_t2015, observed).
narrative_ontology:measurement(refu_be_t2024, refugee_convention_text__procedural_integrity_reading, base_extractiveness, 2024, 0.38).
narrative_ontology:measurement_basis(refu_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(refu_su_t1980, refugee_convention_text__procedural_integrity_reading, suppression_requirement, 1980, 0.38).
narrative_ontology:measurement_basis(refu_su_t1980, observed).
narrative_ontology:measurement(refu_su_t1995, refugee_convention_text__procedural_integrity_reading, suppression_requirement, 1995, 0.44).
narrative_ontology:measurement_basis(refu_su_t1995, observed).
narrative_ontology:measurement(refu_su_t2005, refugee_convention_text__procedural_integrity_reading, suppression_requirement, 2005, 0.51).
narrative_ontology:measurement_basis(refu_su_t2005, observed).
narrative_ontology:measurement(refu_su_t2015, refugee_convention_text__procedural_integrity_reading, suppression_requirement, 2015, 0.53).
narrative_ontology:measurement_basis(refu_su_t2015, observed).
narrative_ontology:measurement(refu_su_t2024, refugee_convention_text__procedural_integrity_reading, suppression_requirement, 2024, 0.52).
narrative_ontology:measurement_basis(refu_su_t2024, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=1980, tn=2024
narrative_ontology:measurement(refu_grid_01, refugee_convention_text__procedural_integrity_reading, accessibility_collapse(class), 1980, 0.52).
narrative_ontology:measurement(refu_grid_02, refugee_convention_text__procedural_integrity_reading, accessibility_collapse(class), 2024, 0.67).
narrative_ontology:measurement(refu_grid_03, refugee_convention_text__procedural_integrity_reading, accessibility_collapse(individual), 1980, 0.48).
narrative_ontology:measurement(refu_grid_04, refugee_convention_text__procedural_integrity_reading, accessibility_collapse(individual), 2024, 0.63).
narrative_ontology:measurement(refu_grid_05, refugee_convention_text__procedural_integrity_reading, accessibility_collapse(organizational), 1980, 0.38).
narrative_ontology:measurement(refu_grid_06, refugee_convention_text__procedural_integrity_reading, accessibility_collapse(organizational), 2024, 0.58).
narrative_ontology:measurement(refu_grid_07, refugee_convention_text__procedural_integrity_reading, accessibility_collapse(structural), 1980, 0.45).
narrative_ontology:measurement(refu_grid_08, refugee_convention_text__procedural_integrity_reading, accessibility_collapse(structural), 2024, 0.62).
narrative_ontology:measurement(refu_grid_09, refugee_convention_text__procedural_integrity_reading, resistance(class), 1980, 0.75).
narrative_ontology:measurement(refu_grid_10, refugee_convention_text__procedural_integrity_reading, resistance(class), 2024, 0.78).
narrative_ontology:measurement(refu_grid_11, refugee_convention_text__procedural_integrity_reading, resistance(individual), 1980, 0.72).
narrative_ontology:measurement(refu_grid_12, refugee_convention_text__procedural_integrity_reading, resistance(individual), 2024, 0.75).
narrative_ontology:measurement(refu_grid_13, refugee_convention_text__procedural_integrity_reading, resistance(organizational), 1980, 0.68).
narrative_ontology:measurement(refu_grid_14, refugee_convention_text__procedural_integrity_reading, resistance(organizational), 2024, 0.72).
narrative_ontology:measurement(refu_grid_15, refugee_convention_text__procedural_integrity_reading, resistance(structural), 1980, 0.58).
narrative_ontology:measurement(refu_grid_16, refugee_convention_text__procedural_integrity_reading, resistance(structural), 2024, 0.65).
narrative_ontology:measurement(refu_grid_17, refugee_convention_text__procedural_integrity_reading, stakes_inflation(class), 1980, 0.38).
narrative_ontology:measurement(refu_grid_18, refugee_convention_text__procedural_integrity_reading, stakes_inflation(class), 2024, 0.51).
narrative_ontology:measurement(refu_grid_19, refugee_convention_text__procedural_integrity_reading, stakes_inflation(individual), 1980, 0.55).
narrative_ontology:measurement(refu_grid_20, refugee_convention_text__procedural_integrity_reading, stakes_inflation(individual), 2024, 0.68).
narrative_ontology:measurement(refu_grid_21, refugee_convention_text__procedural_integrity_reading, stakes_inflation(organizational), 1980, 0.42).
narrative_ontology:measurement(refu_grid_22, refugee_convention_text__procedural_integrity_reading, stakes_inflation(organizational), 2024, 0.55).
narrative_ontology:measurement(refu_grid_23, refugee_convention_text__procedural_integrity_reading, stakes_inflation(structural), 1980, 0.35).
narrative_ontology:measurement(refu_grid_24, refugee_convention_text__procedural_integrity_reading, stakes_inflation(structural), 2024, 0.48).
narrative_ontology:measurement(refu_grid_25, refugee_convention_text__procedural_integrity_reading, suppression(class), 1980, 0.38).
narrative_ontology:measurement(refu_grid_26, refugee_convention_text__procedural_integrity_reading, suppression(class), 2024, 0.52).
narrative_ontology:measurement(refu_grid_27, refugee_convention_text__procedural_integrity_reading, suppression(individual), 1980, 0.48).
narrative_ontology:measurement(refu_grid_28, refugee_convention_text__procedural_integrity_reading, suppression(individual), 2024, 0.55).
narrative_ontology:measurement(refu_grid_29, refugee_convention_text__procedural_integrity_reading, suppression(organizational), 1980, 0.42).
narrative_ontology:measurement(refu_grid_30, refugee_convention_text__procedural_integrity_reading, suppression(organizational), 2024, 0.58).
narrative_ontology:measurement(refu_grid_31, refugee_convention_text__procedural_integrity_reading, suppression(structural), 1980, 0.28).
narrative_ontology:measurement(refu_grid_32, refugee_convention_text__procedural_integrity_reading, suppression(structural), 2024, 0.44).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(refugee_convention_text__procedural_integrity_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(refugee_convention_text__procedural_integrity_reading, 0.12).
narrative_ontology:affects_constraint(refugee_convention_text__procedural_integrity_reading, refugee_convention_text__expansive_humanitarian_reading).
narrative_ontology:affects_constraint(refugee_convention_text__procedural_integrity_reading, refugee_convention_text__restrictive_sovereignty_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the refugee_convention_text kernel. The expansive_humanitarian_reading and restrictive_sovereignty_reading are sibling readings, not alternative perspectives on a single constraint. Each reading instantiates a distinct constraint with its own beneficiary/victim structure, extraction profile, and procedural consequences. Procedural-integrity reading treats the Convention as primarily establishing fair assessment procedure; the humanitarian reading treats it as substantively mandating broad protection; the sovereignty reading treats it as permitting narrow definitions. The three constraints are linked via network.affects_constraints to enable kernel-level analysis: the procedural reading influences both siblings by establishing procedural standards that constrain substantive determinations, but it does not foreclose either sibling.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(refugee_convention_text__procedural_integrity_reading, institutional, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
