% ============================================================================
% CONSTRAINT STORY: magna_carta_1215__living_document_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-24
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
 *   constraint_id: magna_carta_1215__living_document_reading
 *   human_readable: Magna Carta as Living Constitutional Substrate (Living Document Reading)
 *   domain: constitutional_law/legal_history/political_theory
 *
 * SUMMARY:
 *   The living-document reading of Magna Carta treats the 1215 charter not as
 *   a fixed feudal contract but as an adaptive constitutional substrate whose
 *   meaning evolves through interpretive tradition and precedential
 *   accumulation. Original meaning (baronial privileges, specific feudal
 *   grievances) is legitimately superseded by centuries of common-law
 *   development: Clause 39's 'lawful judgment of peers' becomes due process;
 *   'free men' expands from landowning barons to all persons; the charter's
 *   reissues and confirmations become a meta-constraint on interpretive
 *   authority itself. The constraint coordinates constitutional change by
 *   scaffolding legitimate disagreement — courts, Parliament, and citizens
 *   all invoke Magna Carta, but the living-document reading holds that no
 *   single reading forecloses the others. The authority structure (common law
 *   courts, parliamentary tradition) manages this contestation without
 *   resolving it.
 *
 * KEY AGENTS:
 *   - common_law_courts: agenda_setter (institutional/civilizational/analytical/national) — administers the interpretive tradition, declares what Magna Carta means today
 *   - parliamentary_sovereignty_tradition: agenda_setter (institutional/civilizational/analytical/national) — can override judicial readings by statute, legitimizes the adaptive method
 *   - constitutional_lawyers: beneficiary (organized/generational/arbitrage/national) — professional cadre that operates the interpretive machinery, gains status from mastery
 *   - citizens_claiming_evolved_rights: beneficiary (moderate/biographical/mobile/national) — invoke evolved readings for due process, fair trial, habeas corpus protections
 *   - originalist_critics: excluded (powerful/biographical/constrained/national) — argue the adaptive reading is judicial overreach, would bind meaning to 1215 intent
 *   - legal_historians: observer (analytical/civilizational/analytical/national) — document the interpretive trajectory, provide external corroboration
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_1215__living_document_reading, 0.12).
domain_priors:suppression_score(magna_carta_1215__living_document_reading, 0.08).
domain_priors:theater_ratio(magna_carta_1215__living_document_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_1215__living_document_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(magna_carta_1215__living_document_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(magna_carta_1215__living_document_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_1215__living_document_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(magna_carta_1215__living_document_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_1215__living_document_reading, rope).
narrative_ontology:human_readable(magna_carta_1215__living_document_reading, "Magna Carta as Living Constitutional Substrate (Living Document Reading)").
narrative_ontology:topic_domain(magna_carta_1215__living_document_reading, "constitutional_law/legal_history/political_theory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_1215__living_document_reading, 'c6959e14-a4db-42af-a375-31c8f0835af8').
narrative_ontology:cs_kernel_codification('c6959e14-a4db-42af-a375-31c8f0835af8', fixed_text).
narrative_ontology:cs_authority_grounding('c6959e14-a4db-42af-a375-31c8f0835af8', lineage).
narrative_ontology:cs_interpretation_layer_present('c6959e14-a4db-42af-a375-31c8f0835af8').
narrative_ontology:cs_reading_relation('c6959e14-a4db-42af-a375-31c8f0835af8', magna_carta_1215__baronial_privilege_reading, coexists_with).
narrative_ontology:cs_reading_relation('c6959e14-a4db-42af-a375-31c8f0835af8', magna_carta_1215__universal_rights_reading, coexists_with).
narrative_ontology:cs_axiom('c6959e14-a4db-42af-a375-31c8f0835af8', foundational, interpretive_tradition_supersedes_original_meaning).
narrative_ontology:cs_axiom_status(interpretive_tradition_supersedes_original_meaning, holdable).
narrative_ontology:cs_axiom_grounding('c6959e14-a4db-42af-a375-31c8f0835af8', interpretive_tradition_supersedes_original_meaning, conventional).
narrative_ontology:cs_axiom('c6959e14-a4db-42af-a375-31c8f0835af8', foundational, precedential_accumulation_legitimizes_constitutional_change).
narrative_ontology:cs_axiom_status(precedential_accumulation_legitimizes_constitutional_change, holdable).
narrative_ontology:cs_axiom_grounding('c6959e14-a4db-42af-a375-31c8f0835af8', precedential_accumulation_legitimizes_constitutional_change, conventional).
narrative_ontology:cs_reference_frame('c6959e14-a4db-42af-a375-31c8f0835af8', common_law_interpretive_tradition).
narrative_ontology:cs_drift_state('c6959e14-a4db-42af-a375-31c8f0835af8', contemporary_human_rights_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('c6959e14-a4db-42af-a375-31c8f0835af8', '2026-08-24T14:30:00Z').
narrative_ontology:cs_kernel_id(magna_carta_1215__living_document_reading, magna_carta_1215).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_1215__living_document_reading, common_law_courts).
narrative_ontology:constraint_beneficiary(magna_carta_1215__living_document_reading, parliamentary_sovereignty_tradition).
narrative_ontology:constraint_beneficiary(magna_carta_1215__living_document_reading, constitutional_lawyers).
narrative_ontology:constraint_beneficiary(magna_carta_1215__living_document_reading, citizens_claiming_evolved_rights).
narrative_ontology:constraint_vindicates(magna_carta_1215__living_document_reading, precedential_accumulation_legitimizes_constitutional_change).
narrative_ontology:constraint_vindicates(magna_carta_1215__living_document_reading, interpretive_tradition_supersedes_original_meaning).
narrative_ontology:constraint_vindicates(magna_carta_1215__living_document_reading, common_law_method_as_constitutional_development).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer the interpretive tradition: decide what Magna Carta means in each era, declare which clauses remain binding, distinguish obsolete feudal provisions from living constitutional principles. Their readings become binding precedent unless Parliament overrides. They gain institutional authority from being the designated interpreters.
narrative_ontology:constraint_stakeholder(magna_carta_1215__living_document_reading, common_law_courts, agenda_setter,
    institutional, civilizational, analytical, national).

% Holds ultimate legislative supremacy — can codify, modify, or abrogate any judicial reading of Magna Carta by statute. Legitimizes the adaptive method by repeatedly confirming the charter (1216, 1217, 1225, 1297) while allowing its meaning to evolve. The living-document reading depends on Parliament not freezing the meaning.
narrative_ontology:constraint_stakeholder(magna_carta_1215__living_document_reading, parliamentary_sovereignty_tradition, agenda_setter,
    institutional, civilizational, analytical, national).

% Professional cadre that operates the interpretive machinery — litigates Magna Carta clauses, writes the commentaries, teaches the tradition. Gains status, fees, and intellectual capital from the adaptive reading's complexity. Can move between practice, academia, and judiciary (arbitrage-grade exit).
narrative_ontology:constraint_stakeholder(magna_carta_1215__living_document_reading, constitutional_lawyers, beneficiary,
    organized, generational, arbitrage, national).

% Invoke evolved Magna Carta readings for due process, fair trial, habeas corpus, and protection against arbitrary detention. The living-document reading gives them textual anchors for rights claims that have no other constitutional basis in the UK's uncodified system. Exit is mobile — they can also invoke ECHR, HRA 1998, or common law directly.
narrative_ontology:constraint_stakeholder(magna_carta_1215__living_document_reading, citizens_claiming_evolved_rights, beneficiary,
    moderate, biographical, mobile, national).

% Argue that the living-document reading is judicial overreach — that Magna Carta's meaning was fixed in 1215 and any evolution is illegitimate amendment by courts. They are excluded from the interpretive tradition's internal logic (which treats evolution as legitimate) but not from the public discourse. Their constraint is that the dominant method treats their position as a dissenting view, not a governing one.
narrative_ontology:constraint_stakeholder(magna_carta_1215__living_document_reading, originalist_critics, excluded,
    powerful, biographical, constrained, national).

% Document the interpretive trajectory from 1215 to present, trace how each clause's meaning shifted, provide external corroboration for the living-document reading's claim that precedential accumulation constitutes constitutional development. They neither collect nor pay — they analyze.
narrative_ontology:constraint_stakeholder(magna_carta_1215__living_document_reading, legal_historians, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(magna_carta_1215__living_document_reading, diffuse).
narrative_ontology:fixing_cost_class(magna_carta_1215__living_document_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Structures legitimate constitutional disagreement by providing an authoritative text whose meaning evolves through precedent rather than revolution — courts, Parliament, and citizens all invoke Magna Carta, and the living-document method lets them disagree without denying each other's legitimacy.
% TRANSFER_FUNCTION: Moves interpretive authority from the 1215 baronial framers to the ongoing common-law tradition; moves protective rights claims (due process, fair trial, habeas corpus) from having no textual anchor to having the charter's authority; moves legitimacy from 'original intent' to 'precedential accumulation.' No material resources transfer — the transfer is epistemic and normative.
% ABSENT_VOICES: The 1215 barons themselves (dead, cannot object to evolved readings); colonial subjects whose rights were denied by the same common-law tradition that the living-document reading celebrates; contemporary originalist theorists who argue the adaptive method is a category error but are treated as dissenters within the tradition, not as participants in it.
% DISAPPEARANCE_RATIONALE: If the living-document reading vanished overnight, UK constitutional practice would lose its primary mechanism for legitimizing evolved rights protections without codified amendment. Courts would lose the charter's authority as a scaffold for due process and fair trial readings; Parliament would lose the charter's legitimizing function for statutory rights; citizens would lose a textual anchor for common-law rights claims. The world would rearrange toward either originalism (freezing meaning) or pure statutory rights (abandoning the charter's authority).
% FOUNDING_PROBLEM: The 1215 charter was built to solve a specific baronial peace with King John — a feudal contract that failed within weeks. The living-document reading was not 'built' at a moment; it emerged as the common-law tradition repeatedly reissued and reinterpreted the charter to address new constitutional crises (Petition of Right 1628, Glorious Revolution 1689, reform acts 1832+). The arrangement solved the meta-problem: how to make constitutional change legitimate in a system without a codified constitution or entrenched amendment procedure.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians (Baker, Lobban, Helmholz) corroborate from outside the benefiting parties that the common-law method of precedential development is a genuine coordination mechanism, not a cover for capture. The parliamentary record shows repeated statutory confirmations of Magna Carta while accepting evolved meanings. Even originalist critics (e.g., Scalia's UK analogues) engage the tradition on its own terms rather than denying its existence — their critique presupposes the adaptive method's dominance.
narrative_ontology:disappearance_verdict(magna_carta_1215__living_document_reading, world_rearranges).
narrative_ontology:founding_problem_status(magna_carta_1215__living_document_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_1215__living_document_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(magna_carta_1215__living_document_reading, 'none', 1).
narrative_ontology:epsilon_provenance(magna_carta_1215__living_document_reading, 0.12, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Base extractiveness is low (0.12) because the living-document reading does not extract resources from a captive population — it provides an interpretive framework that multiple parties use. The 1215 charter was extractive (baronial privilege enforced by force); the living-document reading inverts this: the charter's authority is now deployed *against* extraction (due process, fair trial). Theater ratio (0.25) reflects that some invocations are ceremonial (Magna Carta as totem), but the core common-law method is functional. Accessibility collapse (0.35) is moderate — alternative constitutional theories (originalism, textualism) persist and compete. Resistance (0.15) is low — the living-document reading is the dominant mode in UK and Commonwealth constitutional practice. Suppression (0.08) is near-zero: no one is coerced into accepting this reading; originalist critiques are published, taught, and litigated.
 *
 * PERSPECTIVAL GAP:
 *   The agenda_setter seats (courts, Parliament) experience the constraint as a coordination rope — they use it to structure legitimate constitutional argument. The beneficiary seats (lawyers, citizens) experience it as a protective scaffold — it enables rights claims that would otherwise lack textual anchor. The excluded seat (originalist critics) experiences it as a snare — they see the adaptive method as an illegitimate power grab by courts. The engine will compute these as different per-seat types from the same structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Common law courts and parliamentary tradition are agenda_setters — they administer and can override the interpretive tradition (d near 0.0, beneficiary end). Constitutional lawyers are beneficiaries — they gain professional standing from the interpretive machinery but don't control it (d ~0.2). Citizens claiming evolved rights are beneficiaries — they receive protective interpretations (d ~0.15). Originalist critics are excluded — they bear no extraction from this reading but contest its legitimacy (d ~0.5, symmetric contestation). The reading's coordination function (scaffolding legitimate disagreement) means no seat is a full target (d=1.0).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (baronial peace with King John) is dead — the 1215 settlement lasted weeks. But the arrangement persists because it solved a *different* problem: how to make constitutional change legitimate without revolution. The living-document reading converts a failed peace treaty into a coordination mechanism for ongoing constitutional development. This is not mandatrophy — the constraint's function shifted from 'settle baronial grievances' to 'structure legitimate interpretive contestation,' and the new function is live. The mandatrophy_resolved flag is false because the current function is not vestigial.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    living_document_kernel_reading_identity,
    'Is this constraint a distinct reading of the magna_carta_1215 kernel, or a standalone constraint?',
    'Kernel identity is fixed by the SCOPE manifest: this story instantiates the living_document_reading of kernel magna_carta_1215. The sibling readings baronial_privilege_reading and universal_rights_reading are separate constraint stories with their own ε, stakeholders, and classifications.',
    'If misidentified as a standalone constraint, the committer structure (which reading of which kernel) is lost and the cross-reading structural relations cannot be computed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(living_document_kernel_reading_identity, conceptual, 'This constraint is one reading of the contested magna_carta_1215 kernel; original meaning is legitimately superseded by interpretive tradition.').

omega_variable(
    interpretive_authority_vs_legislative_supremacy,
    'Does the living-document reading''s interpretive authority structure concentrate power in courts, or does it remain subject to parliamentary override?',
    'Track instances where courts invoke Magna Carta clauses against statutes and whether Parliament subsequently reverses or codifies the interpretation. The UK''s uncodified constitution makes this an ongoing empirical question.',
    'If courts can entrench interpretations against legislative will, the constraint''s extraction shifts toward agenda_setter capture; if Parliament routinely overrides, the constraint remains a coordination rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretive_authority_vs_legislative_supremacy, empirical, 'Whether the adaptive reading creates judicial supremacy or remains within legislative supremacy.').

omega_variable(
    precedential_drift_capture_risk,
    'Does precedential accumulation in the living-document reading serve genuine constitutional development, or does it mask ideological capture by legal elites?',
    'Longitudinal analysis of which groups benefit from evolved Magna Carta interpretations — compare outcomes for marginalized claimants vs. institutional actors across the interval.',
    'If capture is systematic, the constraint reclassifies toward tangled_rope or snare; if development is broadly distributed, the rope classification holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(precedential_drift_capture_risk, preference, 'Whether the living-document method is a genuine coordination mechanism or a cover for elite capture.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_1215__living_document_reading, 1215, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(magna_carta_living_tr_t1215, magna_carta_1215__living_document_reading, theater_ratio, 1215, 0.6).
narrative_ontology:measurement(magna_carta_living_tr_t1225, magna_carta_1215__living_document_reading, theater_ratio, 1225, 0.5).
narrative_ontology:measurement(magna_carta_living_tr_t1297, magna_carta_1215__living_document_reading, theater_ratio, 1297, 0.4).
narrative_ontology:measurement(magna_carta_living_tr_t1628, magna_carta_1215__living_document_reading, theater_ratio, 1628, 0.3).
narrative_ontology:measurement(magna_carta_living_tr_t1689, magna_carta_1215__living_document_reading, theater_ratio, 1689, 0.25).
narrative_ontology:measurement(magna_carta_living_tr_t1765, magna_carta_1215__living_document_reading, theater_ratio, 1765, 0.25).
narrative_ontology:measurement(magna_carta_living_tr_t1832, magna_carta_1215__living_document_reading, theater_ratio, 1832, 0.25).
narrative_ontology:measurement(magna_carta_living_tr_t1911, magna_carta_1215__living_document_reading, theater_ratio, 1911, 0.25).
narrative_ontology:measurement(magna_carta_living_tr_t1998, magna_carta_1215__living_document_reading, theater_ratio, 1998, 0.25).
narrative_ontology:measurement(magna_carta_living_tr_t2024, magna_carta_1215__living_document_reading, theater_ratio, 2024, 0.25).

% Extraction over time
narrative_ontology:measurement(magna_carta_living_be_t1215, magna_carta_1215__living_document_reading, base_extractiveness, 1215, 0.45).
narrative_ontology:measurement(magna_carta_living_be_t1225, magna_carta_1215__living_document_reading, base_extractiveness, 1225, 0.38).
narrative_ontology:measurement(magna_carta_living_be_t1297, magna_carta_1215__living_document_reading, base_extractiveness, 1297, 0.32).
narrative_ontology:measurement(magna_carta_living_be_t1628, magna_carta_1215__living_document_reading, base_extractiveness, 1628, 0.25).
narrative_ontology:measurement(magna_carta_living_be_t1689, magna_carta_1215__living_document_reading, base_extractiveness, 1689, 0.18).
narrative_ontology:measurement(magna_carta_living_be_t1765, magna_carta_1215__living_document_reading, base_extractiveness, 1765, 0.15).
narrative_ontology:measurement(magna_carta_living_be_t1832, magna_carta_1215__living_document_reading, base_extractiveness, 1832, 0.13).
narrative_ontology:measurement(magna_carta_living_be_t1911, magna_carta_1215__living_document_reading, base_extractiveness, 1911, 0.12).
narrative_ontology:measurement(magna_carta_living_be_t1998, magna_carta_1215__living_document_reading, base_extractiveness, 1998, 0.11).
narrative_ontology:measurement(magna_carta_living_be_t2024, magna_carta_1215__living_document_reading, base_extractiveness, 2024, 0.12).

% Suppression requirement over time
narrative_ontology:measurement(magna_carta_living_su_t1215, magna_carta_1215__living_document_reading, suppression_requirement, 1215, 0.8).
narrative_ontology:measurement(magna_carta_living_su_t1225, magna_carta_1215__living_document_reading, suppression_requirement, 1225, 0.7).
narrative_ontology:measurement(magna_carta_living_su_t1297, magna_carta_1215__living_document_reading, suppression_requirement, 1297, 0.5).
narrative_ontology:measurement(magna_carta_living_su_t1628, magna_carta_1215__living_document_reading, suppression_requirement, 1628, 0.2).
narrative_ontology:measurement(magna_carta_living_su_t1689, magna_carta_1215__living_document_reading, suppression_requirement, 1689, 0.1).
narrative_ontology:measurement(magna_carta_living_su_t1765, magna_carta_1215__living_document_reading, suppression_requirement, 1765, 0.08).
narrative_ontology:measurement(magna_carta_living_su_t1832, magna_carta_1215__living_document_reading, suppression_requirement, 1832, 0.08).
narrative_ontology:measurement(magna_carta_living_su_t1911, magna_carta_1215__living_document_reading, suppression_requirement, 1911, 0.08).
narrative_ontology:measurement(magna_carta_living_su_t1998, magna_carta_1215__living_document_reading, suppression_requirement, 1998, 0.08).
narrative_ontology:measurement(magna_carta_living_su_t2024, magna_carta_1215__living_document_reading, suppression_requirement, 2024, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_1215__living_document_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(magna_carta_1215__living_document_reading, 0.1).
narrative_ontology:affects_constraint(magna_carta_1215__living_document_reading, magna_carta_1215__baronial_privilege_reading).
narrative_ontology:affects_constraint(magna_carta_1215__living_document_reading, magna_carta_1215__universal_rights_reading).
narrative_ontology:affects_constraint(magna_carta_1215__living_document_reading, habeas_corpus_1679).
narrative_ontology:affects_constraint(magna_carta_1215__living_document_reading, bill_of_rights_1689).
narrative_ontology:affects_constraint(magna_carta_1215__living_document_reading, human_rights_act_1998).

% DUAL FORMULATION NOTE:
% Magna Carta 1215 kernel family: three readings with distinct ε and stakeholder structures. baronial_privilege_reading (high extraction, snare/tangled_rope at origin) → living_document_reading (low extraction, rope via precedential development) → universal_rights_reading (moderate extraction, tangled_rope as rights precedent). The living-document reading is the adaptive bridge: it inherits the charter's authority but repurposes it for constitutional coordination.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(magna_carta_1215__living_document_reading, institutional, 0.05).
constraint_indexing:directionality_override(magna_carta_1215__living_document_reading, analytical, 0.0).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
