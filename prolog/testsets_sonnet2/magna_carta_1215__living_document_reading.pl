% ============================================================================
% CONSTRAINT STORY: magna_carta_1215__living_document_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
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
 *   human_readable: Magna Carta as Adaptive Constitutional Substrate (Living Document Reading)
 *   domain: constitutional_law/legal_history/political_theory
 *
 * SUMMARY:
 *   This story instantiates the living-document reading of the Magna Carta
 *   kernel: the claim that the 1215 text's original meaning has been
 *   legitimately superseded by centuries of interpretive tradition, and that
 *   the resulting precedential accumulation constitutes genuine
 *   constitutional development rather than drift or usurpation. Unlike the
 *   baronial-privilege reading (which anchors meaning to the 1215 feudal
 *   settlement) or the universal-rights reading (which treats Clause 39 as
 *   directly emitting a transhistorical due-process guarantee), this reading
 *   locates authority in the interpretive tradition itself — the chain of
 *   judicial elaboration is what makes the constraint operative today, not
 *   the original text. The referent for extractiveness is the standing
 *   arrangement under contest: the current authority of common-law courts to
 *   treat accumulated precedent as constitutionally authoritative, assessed
 *   by this reading's own lights.
 *
 * KEY AGENTS:
 *   - constitutional_courts: institutional agenda-setter administering which precedent counts as legitimate development
 *   - common_law_judiciary: institutional beneficiary whose discretion the reading licenses and insulates
 *   - legal_academy: organized beneficiary producing and validating the interpretive tradition
 *   - originalist_litigants: moderate-power payers whose textualist claims are structurally disfavored
 *   - communities_awaiting_doctrinal_extension: powerless payers dependent on the tradition eventually reaching them
 *   - legal_historians: analytical observers documenting the gap between text and doctrine
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_1215__living_document_reading, 0.42).
domain_priors:suppression_score(magna_carta_1215__living_document_reading, 0.31).
domain_priors:theater_ratio(magna_carta_1215__living_document_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_1215__living_document_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(magna_carta_1215__living_document_reading, suppression_requirement, 0.31).
narrative_ontology:constraint_metric(magna_carta_1215__living_document_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_1215__living_document_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(magna_carta_1215__living_document_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_1215__living_document_reading, tangled_rope).
narrative_ontology:human_readable(magna_carta_1215__living_document_reading, "Magna Carta as Adaptive Constitutional Substrate (Living Document Reading)").
narrative_ontology:topic_domain(magna_carta_1215__living_document_reading, "constitutional_law/legal_history/political_theory").

domain_priors:requires_active_enforcement(magna_carta_1215__living_document_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_1215__living_document_reading, '4d3599ab-9113-45c3-8a38-8d6e2aabc6b8').
narrative_ontology:cs_kernel_codification('4d3599ab-9113-45c3-8a38-8d6e2aabc6b8', fixed_text).
narrative_ontology:cs_authority_grounding('4d3599ab-9113-45c3-8a38-8d6e2aabc6b8', lineage).
narrative_ontology:cs_interpretation_layer_present('4d3599ab-9113-45c3-8a38-8d6e2aabc6b8').
narrative_ontology:cs_reading_relation('4d3599ab-9113-45c3-8a38-8d6e2aabc6b8', magna_carta_1215__baronial_privilege_reading, coexists_with).
narrative_ontology:cs_reading_relation('4d3599ab-9113-45c3-8a38-8d6e2aabc6b8', magna_carta_1215__universal_rights_reading, coexists_with).
narrative_ontology:cs_axiom('4d3599ab-9113-45c3-8a38-8d6e2aabc6b8', foundational, precedential_accumulation_constitutes_legitimate_development).
narrative_ontology:cs_axiom_status(precedential_accumulation_constitutes_legitimate_development, holdable).
narrative_ontology:cs_axiom_grounding('4d3599ab-9113-45c3-8a38-8d6e2aabc6b8', precedential_accumulation_constitutes_legitimate_development, conventional).
narrative_ontology:cs_axiom('4d3599ab-9113-45c3-8a38-8d6e2aabc6b8', foundational, original_meaning_recoverability_not_required_for_legitimacy).
narrative_ontology:cs_axiom_status(original_meaning_recoverability_not_required_for_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('4d3599ab-9113-45c3-8a38-8d6e2aabc6b8', original_meaning_recoverability_not_required_for_legitimacy, instrumental).
narrative_ontology:cs_reference_frame('4d3599ab-9113-45c3-8a38-8d6e2aabc6b8', precedential_continuity_framework).
narrative_ontology:cs_drift_state('4d3599ab-9113-45c3-8a38-8d6e2aabc6b8', contemporary_constitutional_doctrine, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('4d3599ab-9113-45c3-8a38-8d6e2aabc6b8', '').
narrative_ontology:cs_kernel_id(magna_carta_1215__living_document_reading, magna_carta_1215).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_1215__living_document_reading, constitutional_courts).
narrative_ontology:constraint_beneficiary(magna_carta_1215__living_document_reading, common_law_judiciary).
narrative_ontology:constraint_beneficiary(magna_carta_1215__living_document_reading, legal_academy).
narrative_ontology:constraint_victim(magna_carta_1215__living_document_reading, originalist_litigants).
narrative_ontology:constraint_victim(magna_carta_1215__living_document_reading, communities_awaiting_doctrinal_extension).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Courts holding common-law or constitutional jurisdiction treat Magna Carta's clauses (especially Clause 39/40) as a substrate that later precedent has legitimately built upon rather than a fixed text to be recovered. They select which accumulated precedent counts as authoritative development and which is disregarded as drift, effectively administering the interpretive tradition itself.
narrative_ontology:constraint_stakeholder(magna_carta_1215__living_document_reading, constitutional_courts, agenda_setter,
    institutional, civilizational, analytical, national).

% Judges across centuries have layered due-process and rule-of-law doctrine onto the Magna Carta text, treating each layer as legitimate constitutional development rather than departure. This reading licenses their continuing interpretive discretion and insulates prior rulings from originalist challenge, which is also a direct professional benefit — their accumulated case law becomes the operative authority rather than the 1215 text.
narrative_ontology:constraint_stakeholder(magna_carta_1215__living_document_reading, common_law_judiciary, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(magna_carta_1215__living_document_reading, common_law_judiciary, beneficiary).

% Scholars produce the interpretive tradition that the living-document reading treats as constitutive of constitutional meaning. Their doctrinal writing, historical reconstruction, and theory-building are validated as legitimate meaning-making rather than mere commentary, which sustains academic authority and career structures built on tracing doctrinal lineage.
narrative_ontology:constraint_stakeholder(magna_carta_1215__living_document_reading, legal_academy, beneficiary,
    organized, generational, mobile, national).

% Litigants and advocates who argue for recovery of original 1215 meaning (whether baronial-limited or otherwise) find their arguments structurally disfavored: the living-document frame treats the accumulated precedent, not the founding text, as authoritative. They can raise originalist claims but courts operating under this reading treat such claims as historically interesting rather than dispositive.
narrative_ontology:constraint_stakeholder(magna_carta_1215__living_document_reading, originalist_litigants, payer,
    moderate, biographical, constrained, national).

% Groups whose due-process claims depend on courts extending precedent to their circumstance bear the cost of the tradition's incrementalism and unpredictability: relief arrives only if and when the interpretive chain is extended to them, on a timeline and by a logic they do not control. They cannot invoke the 1215 text directly; they must wait on the accumulated doctrine to reach them.
narrative_ontology:constraint_stakeholder(magna_carta_1215__living_document_reading, communities_awaiting_doctrinal_extension, payer,
    powerless, biographical, trapped, national).

% Elected bodies that might prefer to settle due-process content through statute find much of the field already occupied by judicially-developed constitutional doctrine claiming Magna Carta lineage; legislative revision of that doctrine is difficult once it is framed as constitutional development rather than ordinary law.
narrative_ontology:constraint_stakeholder(magna_carta_1215__living_document_reading, legislatures, excluded,
    institutional, generational, constrained, national).

% Study the gap between the 1215 baronial settlement and its modern doctrinal invocations, documenting how each generation's courts have selectively read the text to authorize contemporary development while asserting continuity with it.
narrative_ontology:constraint_stakeholder(magna_carta_1215__living_document_reading, legal_historians, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, continuously-updatable interpretive authority structure so that constitutional due-process doctrine can develop incrementally through precedent without requiring formal textual amendment at each step; allows courts to resolve novel cases by extension of an established lineage rather than starting from first principles.
% TRANSFER_FUNCTION: Moves interpretive authority over the meaning of 'law of the land' and due process from the original text and its drafters to the accumulated body of judicial decisions and the institutions that produce them; moves practical relief for rights claims from immediate textual entitlement to conditional, sequenced doctrinal extension.
% ABSENT_VOICES: The 1215 barons themselves have no standing to object to interpretations that extend their negotiated privileges into universal doctrine or route around them into judicial discretion; contemporary claimants whose situations do not yet fit the precedential chain have no forum to demand immediate recognition — they must wait for the tradition to reach them.
% DISAPPEARANCE_RATIONALE: If the living-document reading were rejected in favor of a fixed-meaning approach, centuries of due-process jurisprudence built through precedential extension would lose their claimed textual anchor; courts would need either to re-derive doctrine from other sources or to formally acknowledge it as judge-made law independent of Magna Carta, materially changing legitimacy arguments in ongoing litigation and the structure of constitutional argument itself.
% FOUNDING_PROBLEM: How to preserve constitutional continuity and legitimacy across centuries of changed circumstance without requiring either wholesale textual replacement or freezing doctrine at 13th-century baronial terms.
% FOUNDING_PROBLEM_CORROBORATION: Common-law judges and constitutional scholars attest this problem remains live and the tradition is the legitimate solution. Originalist scholars and some legal historians, positioned outside the beneficiary set, contest this: they argue the 'problem' of continuity is a post-hoc justification for judicial policymaking that has no organic connection to the 1215 settlement, and that the living-document frame primarily serves the institutions currently empowered to interpret rather than solving any genuine continuity problem.
narrative_ontology:disappearance_verdict(magna_carta_1215__living_document_reading, world_rearranges).
narrative_ontology:founding_problem_status(magna_carta_1215__living_document_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_1215__living_document_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(magna_carta_1215__living_document_reading, 'none', 1).
narrative_ontology:epsilon_provenance(magna_carta_1215__living_document_reading, 0.42, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness (0.42) and theater ratio (0.38) both rise across the measured interval because the interpretive tradition accumulates its own self-referential apparatus over time — each generation's doctrine cites the prior generation's doctrine as its authority chain lengthens away from the 1215 text, and an increasing share of judicial reasoning is devoted to justifying continuity with 'Magna Carta' rather than resolving the substantive due-process question at hand. Suppression is moderate (0.31): originalist and baronial readings are not banned, but courts operating under this frame treat them as historically interesting rather than dispositive, which functions as a soft suppression of alternative interpretive claims. Accessibility collapse (0.45) and resistance (0.55) reflect that the alternative readings remain genuinely live in professional and political discourse — this is a contested kernel, not a settled mountain.
 *
 * PERSPECTIVAL GAP:
 *   From the seat of the common-law judiciary, the accumulated precedent IS the constitution's operative content — continuity is achieved, not fabricated. From the seat of an originalist litigant or a community still waiting for doctrine to extend to their case, the same structure looks like an unaccountable and unpredictable gatekeeping mechanism dressed in the authority of an 800-year-old text it has long since departed from.
 *
 * DIRECTIONALITY LOGIC:
 *   Courts and the judiciary that administer the interpretive tradition sit near the beneficiary end: the living-document reading is precisely the frame that licenses and legitimizes their ongoing discretion, and their institutional authority over meaning-making is what the constraint protects. The legal academy benefits similarly by having its scholarship treated as constitutive rather than merely descriptive. Originalist litigants and communities dependent on doctrinal extension sit toward the target end: their claims are structurally disadvantaged by a frame that privileges precedential accumulation over textual recovery, and their relief is conditioned on a process they do not control.
 *
 * MANDATROPHY ANALYSIS:
 *   The living-document reading is not itself a resolved mandatrophy case — it is a meta-constraint on interpretive authority that keeps the baronial and universalist readings both alive as contestable positions rather than resolving between them. Its coordination function (preserving constitutional continuity without requiring textual re-negotiation) is genuine and long-running; the extraction component (concentrating interpretive authority in courts and elevating academic doctrine to quasi-constitutional status) rides on top of that coordination function rather than replacing it, which is why this reading is authored as tangled_rope rather than snare or rope alone.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    development_vs_drift_boundary,
    'Is there a principled way to distinguish legitimate precedential ''constitutional development'' from illegitimate judicial drift away from any textually grounded meaning, or does the living-document reading collapse that distinction by definitional fiat?',
    'Comparative doctrinal history: track cases where courts explicitly reversed course after acknowledging a precedent had drifted too far from any textual anchor, versus cases where drift was ratified as development. A pattern of ratification-only outcomes would support the fiat-collapse reading.',
    'If no principled boundary exists, the living-document reading functions primarily as a legitimating vocabulary for whatever courts currently choose to do, strengthening its classification toward the extraction pole of tangled_rope. If a boundary is identifiable and enforced, the coordination function is more substantial and the reading is closer to a genuine rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(development_vs_drift_boundary, conceptual, 'Whether ''development'' versus ''drift'' is a principled distinction or a legitimating relabeling.').

omega_variable(
    kernel_reading_selection_bias,
    'Is the choice of the living-document reading itself a product of who currently holds interpretive power (courts, legal academy) rather than an independent assessment of which reading best fits the 1215 text and its history?',
    'Examine whether the living-document reading gained ascendancy independently of, or concurrently with, the professionalization and institutional consolidation of the judiciary and legal academy as authoritative interpreters. Track historical correlation between doctrinal self-authorization claims and expansions of judicial review power.',
    'If the reading''s ascendancy tracks the interpretive power of its primary beneficiaries, this strengthens the case that the reading itself is partly an artifact of the extraction it authorizes — a form of self-legitimation rather than a neutral interpretive stance among three coequal options.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_bias, conceptual, 'Whether the living-document reading''s dominance reflects institutional self-interest of its administering agents.').

omega_variable(
    committer_framing_alternative,
    'Could the kernel be framed instead as the text ITSELF being the commitment (with the living-document reading, baronial reading, and universalist reading all being downstream interpretive strategies), versus framing the interpretive AUTHORITY STRUCTURE as the commitment (with the text as one input among several)? This story adopts the latter framing.',
    'Compare classification outcomes under a text-as-kernel framing (which would likely produce a fixed_text CS structure with drift concentrated in interpretation) versus an authority-as-kernel framing (which treats the interpretive tradition''s legitimacy as the primary contested object, as authored here).',
    'Under a text-as-kernel framing, this reading''s ε might register lower (the text itself is uncontested; only its application drifts). Under the authority-as-kernel framing adopted here, ε is higher because the contested object is precisely who gets to say what the text now means. The choice of framing was guided by the reading''s own self-description as being ABOUT interpretive legitimacy, not textual content.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_framing_alternative, conceptual, 'Alternative kernel framing (text-as-kernel vs. authority-as-kernel) and its effect on classification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_1215__living_document_reading, 1215, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(magn_tr_t1215, magna_carta_1215__living_document_reading, theater_ratio, 1215, 0.1).
narrative_ontology:measurement(magn_tr_t1400, magna_carta_1215__living_document_reading, theater_ratio, 1400, 0.15).
narrative_ontology:measurement(magn_tr_t1689, magna_carta_1215__living_document_reading, theater_ratio, 1689, 0.22).
narrative_ontology:measurement(magn_tr_t1800, magna_carta_1215__living_document_reading, theater_ratio, 1800, 0.28).
narrative_ontology:measurement(magn_tr_t1900, magna_carta_1215__living_document_reading, theater_ratio, 1900, 0.32).
narrative_ontology:measurement(magn_tr_t1950, magna_carta_1215__living_document_reading, theater_ratio, 1950, 0.35).
narrative_ontology:measurement(magn_tr_t2025, magna_carta_1215__living_document_reading, theater_ratio, 2025, 0.38).

% Extraction over time
narrative_ontology:measurement(magn_be_t1215, magna_carta_1215__living_document_reading, base_extractiveness, 1215, 0.15).
narrative_ontology:measurement(magn_be_t1400, magna_carta_1215__living_document_reading, base_extractiveness, 1400, 0.2).
narrative_ontology:measurement(magn_be_t1689, magna_carta_1215__living_document_reading, base_extractiveness, 1689, 0.28).
narrative_ontology:measurement(magn_be_t1800, magna_carta_1215__living_document_reading, base_extractiveness, 1800, 0.32).
narrative_ontology:measurement(magn_be_t1900, magna_carta_1215__living_document_reading, base_extractiveness, 1900, 0.36).
narrative_ontology:measurement(magn_be_t1950, magna_carta_1215__living_document_reading, base_extractiveness, 1950, 0.38).
narrative_ontology:measurement(magn_be_t2025, magna_carta_1215__living_document_reading, base_extractiveness, 2025, 0.42).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(magna_carta_1215__living_document_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_1215__living_document_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(magna_carta_1215__living_document_reading, baronial_privilege_reading).
narrative_ontology:affects_constraint(magna_carta_1215__living_document_reading, universal_rights_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the magna_carta_1215 kernel. baronial_privilege_reading anchors meaning to the 1215 feudal contract (limited to landowning barons); universal_rights_reading treats Clause 39 as directly emitting a transhistorical, universal due-process guarantee; living_document_reading (this story) relocates the contested question to the legitimacy of the interpretive tradition itself, treating precedential accumulation as legitimate constitutional development regardless of which original-scope reading one holds. All three share the same underlying text as their kernel but instantiate structurally distinct constraints with different ε, different beneficiary/victim structures, and different classification (tangled_rope here, versus the sibling readings' own independently-authored types).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
