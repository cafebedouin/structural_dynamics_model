% ============================================================================
% CONSTRAINT STORY: magna_carta_1215__universal_rights_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_magna_carta_1215__universal_rights_reading, []).

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
 *   constraint_id: magna_carta_1215__universal_rights_reading
 *   human_readable: Magna Carta Clause 39 as Universal Due Process Precedent
 *   domain: constitutional_law/legal_history/political_theory
 *
 * SUMMARY:
 *   The universal_rights_reading treats Magna Carta's Clause 39 ('No free man
 *   shall be seized or imprisoned... except by the lawful judgment of his
 *   peers or by the law of the land') as establishing a due-process principle
 *   that applies, in substance, to all persons subject to state power — not
 *   merely to the free tenants-in-chief who negotiated the 1215 charter with
 *   King John. This reading underwrites centuries of Anglo-American
 *   constitutional rhetoric: the U.S. and other due-process clauses, habeas
 *   corpus jurisprudence, and international human rights instruments
 *   frequently cite Magna Carta as ancestral authority. The reading performs
 *   real coordination work — it gives courts and advocates a shared,
 *   prestigious lineage to anchor anti-arbitrary-detention claims — but it
 *   also does extraction-adjacent work: it retroactively universalizes a
 *   document whose contemporaneous scope was narrow and class-bound, and the
 *   mismatch between claimed universal scope and actual historical
 *   application (colonial subjects, enslaved persons, women) is a real cost
 *   borne by populations the reading claims to have always protected.
 *
 * KEY AGENTS:
 *   - criminal_defendants: Primary beneficiary when the reading is enforced (powerless/trapped) — the protection is invoked on their behalf
 *   - detained_persons: Beneficiary in principle, frequently a de facto victim in practice (powerless/trapped) — bears the gap between claimed and actual coverage
 *   - constitutional_rights_advocates: Agenda-setter (organized/mobile) — constructs and transmits the reading
 *   - judicial_review_institutions: Beneficiary and co-agenda-setter (institutional/arbitrage) — draws legitimacy from the lineage claim
 *   - executive_detention_authorities: Payer (institutional/constrained) — bears the constraint when courts enforce it
 *   - colonial_subjects_excluded_in_practice: Payer (powerless/trapped) — historically denied the protection the reading claims was always theirs
 *   - historically_unfree_populations: Payer (powerless/trapped) — categorically excluded from 'free men' at the time
 *   - legal_historians: Analytical observer (analytical) — documents the textual gap between 1215 meaning and later universalist claim
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_1215__universal_rights_reading, 0.42).
domain_priors:suppression_score(magna_carta_1215__universal_rights_reading, 0.35).
domain_priors:theater_ratio(magna_carta_1215__universal_rights_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_1215__universal_rights_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(magna_carta_1215__universal_rights_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(magna_carta_1215__universal_rights_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_1215__universal_rights_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(magna_carta_1215__universal_rights_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_1215__universal_rights_reading, tangled_rope).
narrative_ontology:human_readable(magna_carta_1215__universal_rights_reading, "Magna Carta Clause 39 as Universal Due Process Precedent").
narrative_ontology:topic_domain(magna_carta_1215__universal_rights_reading, "constitutional_law/legal_history/political_theory").

domain_priors:requires_active_enforcement(magna_carta_1215__universal_rights_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_1215__universal_rights_reading, 'e68f4b93-7e62-4749-936d-33a02d731297').
narrative_ontology:cs_kernel_codification('e68f4b93-7e62-4749-936d-33a02d731297', fixed_text).
narrative_ontology:cs_authority_grounding('e68f4b93-7e62-4749-936d-33a02d731297', lineage).
narrative_ontology:cs_interpretation_layer_present('e68f4b93-7e62-4749-936d-33a02d731297').
narrative_ontology:cs_reading_relation('e68f4b93-7e62-4749-936d-33a02d731297', magna_carta_1215__baronial_privilege_reading, coexists_with).
narrative_ontology:cs_reading_relation('e68f4b93-7e62-4749-936d-33a02d731297', magna_carta_1215__living_document_reading, influences).
narrative_ontology:cs_axiom('e68f4b93-7e62-4749-936d-33a02d731297', foundational, liber_homo_denotes_all_persons_subject_to_sovereign_power).
narrative_ontology:cs_axiom_status(liber_homo_denotes_all_persons_subject_to_sovereign_power, holdable).
narrative_ontology:cs_axiom_grounding('e68f4b93-7e62-4749-936d-33a02d731297', liber_homo_denotes_all_persons_subject_to_sovereign_power, conventional).
narrative_ontology:cs_axiom('e68f4b93-7e62-4749-936d-33a02d731297', foundational, due_process_protection_is_a_transhistorical_moral_entitlement_not_a_contractual_grant).
narrative_ontology:cs_axiom_status(due_process_protection_is_a_transhistorical_moral_entitlement_not_a_contractual_grant, holdable).
narrative_ontology:cs_axiom_grounding('e68f4b93-7e62-4749-936d-33a02d731297', due_process_protection_is_a_transhistorical_moral_entitlement_not_a_contractual_grant, deontological).
narrative_ontology:cs_reference_frame('e68f4b93-7e62-4749-936d-33a02d731297', coke_era_universalization_of_clause_39).
narrative_ontology:cs_drift_state('e68f4b93-7e62-4749-936d-33a02d731297', contemporary_human_rights_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e68f4b93-7e62-4749-936d-33a02d731297', '').
narrative_ontology:cs_kernel_id(magna_carta_1215__universal_rights_reading, magna_carta_1215).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_1215__universal_rights_reading, criminal_defendants).
narrative_ontology:constraint_beneficiary(magna_carta_1215__universal_rights_reading, detained_persons).
narrative_ontology:constraint_beneficiary(magna_carta_1215__universal_rights_reading, constitutional_rights_advocates).
narrative_ontology:constraint_beneficiary(magna_carta_1215__universal_rights_reading, judicial_review_institutions).
narrative_ontology:constraint_victim(magna_carta_1215__universal_rights_reading, executive_detention_authorities).
narrative_ontology:constraint_victim(magna_carta_1215__universal_rights_reading, colonial_subjects_excluded_in_practice).
narrative_ontology:constraint_victim(magna_carta_1215__universal_rights_reading, historically_unfree_populations).
narrative_ontology:constraint_vindicates(magna_carta_1215__universal_rights_reading, due_process_universality_doctrine).
narrative_ontology:constraint_vindicates(magna_carta_1215__universal_rights_reading, rule_of_law_supremacy_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Invoke the Clause 39 lineage — via due process clauses, habeas corpus, and judicial review doctrines — to demand that the state may not imprison or punish them without lawful judgment. They cannot exit the state's jurisdiction; the constraint's value to them lies entirely in whether courts actually enforce it against executive power.
narrative_ontology:constraint_stakeholder(magna_carta_1215__universal_rights_reading, criminal_defendants, beneficiary,
    powerless, biographical, trapped, national).

% Held by state authority, they rely on the universalist reading of 'free men' to claim the protection was never meant to be class-limited. Their situation is the sharpest test of whether the universal reading is operative or aspirational — a detainee outside recognized due process channels experiences the reading's failure directly.
narrative_ontology:constraint_stakeholder(magna_carta_1215__universal_rights_reading, detained_persons, beneficiary,
    powerless, immediate, trapped, national).

% Litigators, scholars, and civil liberties organizations actively construct and extend the universalist reading, citing it in briefs, judicial opinions, and human rights instruments. They administer the reading's transmission — deciding which precedents count as legitimate descendants of Clause 39 — and benefit professionally and institutionally from the reading's continued authority.
narrative_ontology:constraint_stakeholder(magna_carta_1215__universal_rights_reading, constitutional_rights_advocates, agenda_setter,
    organized, generational, mobile, global).

% Courts cite the transhistorical universal reading to ground their own authority to check executive detention and punishment powers. The reading grants courts a genealogical claim to legitimacy stretching back eight centuries, which they deploy selectively and interpret according to contemporary doctrine.
narrative_ontology:constraint_stakeholder(magna_carta_1215__universal_rights_reading, judicial_review_institutions, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(magna_carta_1215__universal_rights_reading, judicial_review_institutions, agenda_setter).

% Security services, immigration enforcement, and executive detention regimes are constrained by the universal reading's due process demands whenever courts enforce it. They bear the transaction cost of judicial oversight and periodically seek carve-outs (national security, immigration, wartime) that the universal reading is invoked to resist.
narrative_ontology:constraint_stakeholder(magna_carta_1215__universal_rights_reading, executive_detention_authorities, payer,
    institutional, immediate, constrained, national).

% Populations under colonial and imperial administration by Magna-Carta-descended legal systems were routinely denied the very protections the universal reading claims were always theirs — the historical record shows the universalist claim was applied selectively along racial and colonial lines, which the reading must explain away as a failure of application rather than a limit of the original text.
narrative_ontology:constraint_stakeholder(magna_carta_1215__universal_rights_reading, colonial_subjects_excluded_in_practice, payer,
    powerless, generational, trapped, global).

% Enslaved persons, serfs, women, and other legally unfree or subordinated groups at the time of the 1215 charter and for centuries after were excluded from 'free men' as a matter of contemporaneous legal fact. The universal reading retroactively claims them as always-covered, a claim they could not have exercised in their own lifetimes.
narrative_ontology:constraint_stakeholder(magna_carta_1215__universal_rights_reading, historically_unfree_populations, payer,
    powerless, civilizational, trapped, national).

% Examine the 1215 text, its Latin terms (liber homo), and its immediate application to conclude the document was a feudal settlement between the king and freeholding barons — not a universal charter. They document the gap between what the text meant in 1215 and what later constitutional tradition claims it meant.
narrative_ontology:constraint_stakeholder(magna_carta_1215__universal_rights_reading, legal_historians, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, ancient, cross-jurisdictional textual anchor that lets courts, advocates, and constitutional drafters coordinate around a single lineage claim for due-process and anti-arbitrary-detention norms, rather than each jurisdiction needing to invent legitimacy for judicial review from scratch.
% TRANSFER_FUNCTION: Moves legitimacy and rhetorical authority from an 800-year-old feudal text to contemporary due-process claims; in practice, moves constraint onto executive and detention authorities in favor of individuals subject to state power, when and where courts choose to enforce the lineage claim.
% ABSENT_VOICES: The historically unfree populations and colonial subjects who the universal reading retroactively includes had no voice in either the 1215 settlement or in most of the subsequent interpretive tradition that expanded 'free men' to cover them; legal historians who document the textual gap are marginalized in constitutional rhetoric that treats the universal reading as settled.
% DISAPPEARANCE_RATIONALE: If the Clause 39 lineage claim vanished, courts and advocates argue due-process protection would lose a load-bearing historical anchor and could be more easily narrowed by executive claims of necessity; skeptics argue the actual protective work is done by modern constitutional text, statute, and enforcement capacity, and the medieval citation is largely rhetorical scaffolding that could be replaced without loss of substantive protection.
% FOUNDING_PROBLEM: In its own 1215 context, the founding problem was barons' vulnerability to arbitrary seizure, disseisin, and punishment by King John without lawful judgment of their peers — a specific feudal grievance about the crown's untrammeled power over a specific propertied class.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians (an outside-the-benefiting-parties seat) attest the founding problem as originally scoped was narrow and baronial, and that the universal-persons reading is a much later constitutional and rhetorical construction layered onto the text starting substantially with 17th-century interpreters (Coke) and consolidated in 18th–20th century constitutionalism; constitutional rights advocates and courts, who benefit from the lineage claim, are not independent corroborators of its original scope.
narrative_ontology:disappearance_verdict(magna_carta_1215__universal_rights_reading, contested).
narrative_ontology:founding_problem_status(magna_carta_1215__universal_rights_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_1215__universal_rights_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(magna_carta_1215__universal_rights_reading, 'none', 1).
narrative_ontology:epsilon_provenance(magna_carta_1215__universal_rights_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(magna_carta_1215__universal_rights_reading_tests).
:- end_tests(magna_carta_1215__universal_rights_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) is moderate rather than low or high because the reading does real coordination work (anchoring due process jurisprudence) while also carrying an extraction-adjacent cost: the rhetorical claim of always-already-universal coverage obscures centuries of exclusion and can be invoked selectively by institutions (courts, advocates) whose authority benefits from the lineage claim regardless of whether protection is actually delivered to the powerless. Suppression (0.35) is moderate — the reading is not coercively imposed on dissenting historians, but it does dominate constitutional pedagogy and legal rhetoric to the point that the narrower baronial reading is treated as a fringe historical curiosity rather than the textually dominant reading. Theater ratio is significant and rising (0.05 to 0.40) — a large and growing share of invocations of 'Magna Carta' in political and legal rhetoric are ceremonial or legitimating gestures rather than doctrinally load-bearing citations. Accessibility collapse is low (0.3): the baronial and living-document readings remain live scholarly and jurisprudential alternatives, not suppressed. Resistance is moderate-high (0.55): legal historians and originalist skeptics actively contest the universalist reading's historical accuracy.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (advocates, courts), the universal reading is functioning coordination — a legitimating anchor for due process norms that serves everyone by strengthening judicial review capacity. From the payer seats (colonial subjects, historically unfree populations, and detained persons whose claims are denied), the same reading operates as a retroactive legitimation narrative: it claims their ancestors were always protected while the historical record shows otherwise, and it can function to make current under-enforcement look like an aberration from a supposedly ancient universal norm rather than a continuation of the norm's actual selective history.
 *
 * DIRECTIONALITY LOGIC:
 *   Constitutional rights advocates and judicial review institutions sit near the beneficiary end: they gain rhetorical and institutional authority from the lineage claim regardless of whether it is historically precise. Criminal defendants and detained persons are structurally intended beneficiaries but their d is pulled toward the target end by trapped exit options and by the fact that enforcement is discretionary — when courts decline to apply the due-process lineage (national security exceptions, immigration detention), these populations bear the cost of the reading's non-enforcement while still being nominally 'covered.' Executive detention authorities are payers when the doctrine is enforced against them. Colonial subjects and historically unfree populations are the clearest victims of the gap between the universal claim and actual historical operation — the d for these groups should sit near the full-target end, reflecting that the 'universal' claim was constructed after, and partly to paper over, their historical exclusion.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (baronial protection from arbitrary crown seizure) is genuinely dead in its original scope — no one today defends Clause 39 as protecting only landowning barons against King John's successors. But the universal reading does not simply retire the constraint; it re-founds it on a new, expanded problem (state power over all persons) that the original text did not actually address. This is a case where mandatrophy analysis must distinguish between the instrument (Clause 39's language) and the doctrine built on it (universal due process): the instrument's original function is dead, but the doctrine it now anchors addresses a live problem. The tangled_rope classification captures this — real coordination (anchoring due process norms) plus asymmetric extraction (rhetorical legitimation that benefits legal and judicial institutions more reliably than it protects the powerless, and that requires overlooking centuries of exclusion to sustain the universalist narrative).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_disagreement_locus,
    'This constraint is one reading (universal_rights_reading) of the magna_carta_1215 kernel. The baronial_privilege_reading holds that ''free men'' meant only the landowning barons party to the 1215 settlement, with no claim to broader coverage. The living_document_reading sidesteps the original-meaning question entirely, holding that the text''s authority now rests on accumulated interpretive tradition rather than 1215 intent. Where is the disagreement actually located?',
    'The disagreement is not primarily empirical (the historical scope of liber homo in 1215 is reasonably well-established as narrow) but interpretive/normative: whether transhistorical rights claims can legitimately be grounded in texts whose contemporaneous application excluded the very populations later claimed as covered. A sibling reading would change the beneficiary/victim structure entirely — under baronial_privilege_reading, the modern due-process population is not a party to the constraint at all; under living_document_reading, the 1215 origin is analytically almost irrelevant and the constraint''s legitimacy rests on the interpretive chain itself, not the founding text''s original scope.',
    'If the field converges on baronial_privilege_reading as historically correct, universal_rights_reading''s rhetorical citations of Clause 39 become a legitimation myth with no genealogical warrant, though the underlying due-process norms could still be defended on other (non-Magna-Carta) grounds. If living_document_reading dominates, the debate over 1215 original meaning becomes moot for legitimacy purposes, and this reading''s extraction concern (selective historical application) becomes less relevant since the reading no longer depends on transhistorical original-meaning claims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_locus, conceptual, 'Locates the kernel disagreement: not the historical facts (largely settled) but whether original textual scope constrains legitimate contemporary invocation, and whether interpretive accumulation can substitute for original universal intent.').

omega_variable(
    retroactive_inclusion_legitimacy,
    'Can a constitutional tradition legitimately claim that a document ''always meant'' to include populations who were, at the time of writing and for centuries after, explicitly and legally excluded from its terms?',
    'This is fundamentally a conceptual/preference question about how constitutional legitimacy relates to textual history — resolvable only by adopting a theory of constitutional interpretation (originalist vs. purposive vs. living-constitutionalist), not by additional historical evidence.',
    'If retroactive universal inclusion is illegitimate without new textual authority, the universal_rights_reading''s extraction component (claiming ancient pedigree for a modern normative commitment) is significant and the reading functions largely as legitimating cover. If retroactive inclusion is legitimate as an extension of an underlying principle the text imperfectly expressed, the reading''s coordination function dominates and extraction is lower than authored here.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(retroactive_inclusion_legitimacy, preference, 'Whether transhistorical rights claims require honest acknowledgment of a document''s original exclusionary scope, or can legitimately claim universal original intent.').

omega_variable(
    enforcement_discretion_as_extraction,
    'Is the gap between the universal reading''s claimed coverage and its actual selective enforcement (against colonial subjects, in immigration detention, in national security contexts) evidence that the reading is extractive cover for institutional legitimacy, or simply the ordinary and expected gap between any legal norm and its imperfect enforcement?',
    'Comparative analysis: does enforcement discretion under the Magna Carta lineage claim correlate systematically with the relative powerlessness of the affected population (suggesting extraction) or is it randomly distributed (suggesting ordinary enforcement variance)?',
    'If enforcement gaps correlate with powerlessness, the tangled_rope classification is well-supported — the coordination benefit accrues broadly to institutional legitimacy while the protective benefit is systematically withheld from the least powerful. If enforcement gaps are randomly distributed, the reading is closer to an ordinary rope with imperfect but non-systematic enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_discretion_as_extraction, empirical, 'Whether selective non-enforcement of the universal due-process claim tracks power asymmetry systematically.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_1215__universal_rights_reading, 1215, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(magn_tr_t1215, magna_carta_1215__universal_rights_reading, theater_ratio, 1215, 0.05).
narrative_ontology:measurement(magn_tr_t1628, magna_carta_1215__universal_rights_reading, theater_ratio, 1628, 0.15).
narrative_ontology:measurement(magn_tr_t1789, magna_carta_1215__universal_rights_reading, theater_ratio, 1789, 0.25).
narrative_ontology:measurement(magn_tr_t1865, magna_carta_1215__universal_rights_reading, theater_ratio, 1865, 0.3).
narrative_ontology:measurement(magn_tr_t1948, magna_carta_1215__universal_rights_reading, theater_ratio, 1948, 0.35).
narrative_ontology:measurement(magn_tr_t2001, magna_carta_1215__universal_rights_reading, theater_ratio, 2001, 0.38).
narrative_ontology:measurement(magn_tr_t2024, magna_carta_1215__universal_rights_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(magn_be_t1215, magna_carta_1215__universal_rights_reading, base_extractiveness, 1215, 0.1).
narrative_ontology:measurement(magn_be_t1628, magna_carta_1215__universal_rights_reading, base_extractiveness, 1628, 0.2).
narrative_ontology:measurement(magn_be_t1789, magna_carta_1215__universal_rights_reading, base_extractiveness, 1789, 0.28).
narrative_ontology:measurement(magn_be_t1865, magna_carta_1215__universal_rights_reading, base_extractiveness, 1865, 0.32).
narrative_ontology:measurement(magn_be_t1948, magna_carta_1215__universal_rights_reading, base_extractiveness, 1948, 0.38).
narrative_ontology:measurement(magn_be_t2001, magna_carta_1215__universal_rights_reading, base_extractiveness, 2001, 0.4).
narrative_ontology:measurement(magn_be_t2024, magna_carta_1215__universal_rights_reading, base_extractiveness, 2024, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(magn_su_t1215, magna_carta_1215__universal_rights_reading, suppression_requirement, 1215, 0.1).
narrative_ontology:measurement(magn_su_t1628, magna_carta_1215__universal_rights_reading, suppression_requirement, 1628, 0.15).
narrative_ontology:measurement(magn_su_t1789, magna_carta_1215__universal_rights_reading, suppression_requirement, 1789, 0.2).
narrative_ontology:measurement(magn_su_t1865, magna_carta_1215__universal_rights_reading, suppression_requirement, 1865, 0.25).
narrative_ontology:measurement(magn_su_t1948, magna_carta_1215__universal_rights_reading, suppression_requirement, 1948, 0.3).
narrative_ontology:measurement(magn_su_t2001, magna_carta_1215__universal_rights_reading, suppression_requirement, 2001, 0.33).
narrative_ontology:measurement(magn_su_t2024, magna_carta_1215__universal_rights_reading, suppression_requirement, 2024, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_1215__universal_rights_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(magna_carta_1215__universal_rights_reading, 0.12).
narrative_ontology:affects_constraint(magna_carta_1215__universal_rights_reading, magna_carta_1215__baronial_privilege_reading).
narrative_ontology:affects_constraint(magna_carta_1215__universal_rights_reading, magna_carta_1215__living_document_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the magna_carta_1215 kernel. baronial_privilege_reading treats the 1215 text's scope as narrow and historically fixed (feudal contract among barons), with a correspondingly minimal victim/beneficiary set limited to the contracting class. living_document_reading brackets the original-meaning question and grounds legitimacy in accumulated interpretive tradition rather than 1215 intent. This universal_rights_reading claims the widest beneficiary/victim set of the three — all persons subject to state power — and correspondingly carries the highest extraction risk, because the widest claim requires the largest retroactive-inclusion move relative to the actual 1215 text and its subsequent selective application.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
