% ============================================================================
% CONSTRAINT STORY: family_law_authority__muslim_shariat_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_family_law_authority__muslim_shariat_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: family_law_authority__muslim_shariat_reading
 *   human_readable: Marriage as Civil Contract (Nikah) under Sharia Personal Law
 *   domain: legal/religious/governance
 *
 * SUMMARY:
 *   The Muslim personal law reading of family_law_authority instantiates
 *   nikah as a civil contract governed by Quranic injunctions and hadith. Its
 *   structural signature: contractual dissolution (talaq) vested unilaterally
 *   in the husband (pre-2019 including instantaneous triple talaq), polygyny
 *   permitted up to four wives with Quranic justice condition, mahr (dower)
 *   as wife's obligatory financial right, and gender-asymmetric divorce
 *   access (wives require khula/faskh with higher thresholds). The 2019
 *   legislative ban on triple talaq shifted the extraction profile but did
 *   not eliminate the structural asymmetry. The constraint operates as
 *   tangled_rope: genuine coordination (marriage recognition, mahr
 *   protection, lineage rules) coexists with asymmetric extraction (male
 *   unilateral divorce, polygyny without spousal consent). Active enforcement
 *   through qazi courts, waqf boards, and state recognition of personal law
 *   jurisdiction.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(family_law_authority__muslim_shariat_reading, 0.68).
domain_priors:suppression_score(family_law_authority__muslim_shariat_reading, 0.62).
domain_priors:theater_ratio(family_law_authority__muslim_shariat_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(family_law_authority__muslim_shariat_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(family_law_authority__muslim_shariat_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(family_law_authority__muslim_shariat_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(family_law_authority__muslim_shariat_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(family_law_authority__muslim_shariat_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(family_law_authority__muslim_shariat_reading, tangled_rope).
narrative_ontology:human_readable(family_law_authority__muslim_shariat_reading, "Marriage as Civil Contract (Nikah) under Sharia Personal Law").
narrative_ontology:topic_domain(family_law_authority__muslim_shariat_reading, "legal/religious/governance").

domain_priors:requires_active_enforcement(family_law_authority__muslim_shariat_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(family_law_authority__muslim_shariat_reading, 'be95fb01-528a-4c07-9815-72b1fc016ca5').
narrative_ontology:cs_kernel_codification('be95fb01-528a-4c07-9815-72b1fc016ca5', fixed_text).
narrative_ontology:cs_authority_grounding('be95fb01-528a-4c07-9815-72b1fc016ca5', lineage).
narrative_ontology:cs_interpretation_layer_present('be95fb01-528a-4c07-9815-72b1fc016ca5').
narrative_ontology:cs_reading_relation('be95fb01-528a-4c07-9815-72b1fc016ca5', family_law_authority__secular_contractual_reading, coexists_with).
narrative_ontology:cs_reading_relation('be95fb01-528a-4c07-9815-72b1fc016ca5', family_law_authority__christian_canonical_reading, coexists_with).
narrative_ontology:cs_reading_relation('be95fb01-528a-4c07-9815-72b1fc016ca5', family_law_authority__hindu_dharmashastra_reading, coexists_with).
narrative_ontology:cs_reading_relation('be95fb01-528a-4c07-9815-72b1fc016ca5', family_law_authority__parsi_zoroastrian_reading, coexists_with).
narrative_ontology:cs_axiom('be95fb01-528a-4c07-9815-72b1fc016ca5', foundational, nikah_as_civil_contract_not_sacrament).
narrative_ontology:cs_axiom_status(nikah_as_civil_contract_not_sacrament, holdable).
narrative_ontology:cs_axiom_grounding('be95fb01-528a-4c07-9815-72b1fc016ca5', nikah_as_civil_contract_not_sacrament, conventional).
narrative_ontology:cs_axiom('be95fb01-528a-4c07-9815-72b1fc016ca5', foundational, husband_unilateral_talaq_as_quranic_prerogative).
narrative_ontology:cs_axiom_status(husband_unilateral_talaq_as_quranic_prerogative, holdable).
narrative_ontology:cs_axiom_grounding('be95fb01-528a-4c07-9815-72b1fc016ca5', husband_unilateral_talaq_as_quranic_prerogative, deontological).
narrative_ontology:cs_axiom('be95fb01-528a-4c07-9815-72b1fc016ca5', foundational, polygyny_permitted_with_justice_condition).
narrative_ontology:cs_axiom_status(polygyny_permitted_with_justice_condition, holdable).
narrative_ontology:cs_axiom_grounding('be95fb01-528a-4c07-9815-72b1fc016ca5', polygyny_permitted_with_justice_condition, deontological).
narrative_ontology:cs_axiom('be95fb01-528a-4c07-9815-72b1fc016ca5', foundational, mahr_as_wifes_exclusive_property_right).
narrative_ontology:cs_axiom_status(mahr_as_wifes_exclusive_property_right, holdable).
narrative_ontology:cs_axiom_grounding('be95fb01-528a-4c07-9815-72b1fc016ca5', mahr_as_wifes_exclusive_property_right, deontological).
narrative_ontology:cs_axiom('be95fb01-528a-4c07-9815-72b1fc016ca5', secondary, triple_talaq_in_one_sitting_as_valid_talaq).
narrative_ontology:cs_axiom_status(triple_talaq_in_one_sitting_as_valid_talaq, overridden).
narrative_ontology:cs_axiom_grounding('be95fb01-528a-4c07-9815-72b1fc016ca5', triple_talaq_in_one_sitting_as_valid_talaq, conventional).
narrative_ontology:cs_reference_frame('be95fb01-528a-4c07-9815-72b1fc016ca5', classical_fiqh_consensus_7th_10th_century).
narrative_ontology:cs_drift_state('be95fb01-528a-4c07-9815-72b1fc016ca5', contemporary_constitutional_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('be95fb01-528a-4c07-9815-72b1fc016ca5', '').
narrative_ontology:cs_kernel_id(family_law_authority__muslim_shariat_reading, family_law_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(family_law_authority__muslim_shariat_reading, husbands).
narrative_ontology:constraint_beneficiary(family_law_authority__muslim_shariat_reading, religious_authorities).
narrative_ontology:constraint_beneficiary(family_law_authority__muslim_shariat_reading, patriarchal_family_structures).
narrative_ontology:constraint_victim(family_law_authority__muslim_shariat_reading, wives).
narrative_ontology:constraint_victim(family_law_authority__muslim_shariat_reading, women_seeking_divorce).
narrative_ontology:constraint_victim(family_law_authority__muslim_shariat_reading, children_of_polygynous_marriages).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(family_law_authority__muslim_shariat_reading, wives).
narrative_ontology:constraint_victim(family_law_authority__muslim_shariat_reading, husbands).
narrative_ontology:constraint_vindicates(family_law_authority__muslim_shariat_reading, quranic_family_law_authority).
narrative_ontology:constraint_vindicates(family_law_authority__muslim_shariat_reading, sharia_as_personal_law).
narrative_ontology:constraint_vindicates(family_law_authority__muslim_shariat_reading, male_guardianship_in_marriage).
narrative_ontology:constraint_vindicates(family_law_authority__muslim_shariat_reading, mahr_as_womens_financial_protection).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold unilateral talaq right (pre-2019 triple talaq), may take up to four wives, owe mahr but control its payment timing. Benefit from asymmetric divorce access and polygyny permission. Bear mahr obligation and potential maintenance costs. Exit from marriage is structurally easy; exit from the legal framework requires migration or secular court opt-out where available.
narrative_ontology:constraint_stakeholder(family_law_authority__muslim_shariat_reading, husbands, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(family_law_authority__muslim_shariat_reading, husbands, payer).

% Receive mahr (dower) as Quranic right; may negotiate khula (consensual divorce with mahr return) or seek faskh (judicial dissolution) on limited grounds. Cannot initiate unilateral divorce equivalent to talaq; polygyny of husband permitted without consent. Exit from marriage requires husband's cooperation or court process with high evidentiary burden. Exit from framework similarly constrained.
narrative_ontology:constraint_stakeholder(family_law_authority__muslim_shariat_reading, wives, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(family_law_authority__muslim_shariat_reading, wives, beneficiary).

% Interpret and administer sharia personal law through qazi courts, fatwa bodies, and waqf boards. Define valid talaq procedures, mahr enforceability, polygyny conditions. Derive authority from Quranic mandate and communal recognition. State recognition of personal law systems reinforces their jurisdiction. Exit options include migration to jurisdictions with different personal law regimes.
narrative_ontology:constraint_stakeholder(family_law_authority__muslim_shariat_reading, religious_authorities, agenda_setter,
    institutional, generational, arbitrage, national).

% Adjudicate appeals from personal law bodies, enforce mahr decrees, rule on constitutional challenges (e.g., Shayara Bano 2017 triple talaq judgment). Navigate tension between Article 25 (religious freedom) and Articles 14/15/21 (equality, life, liberty). Legislative override capacity (Muslim Women Protection of Rights on Marriage Act 2019). Not bound by sharia but constrained by political compromise.
narrative_ontology:constraint_stakeholder(family_law_authority__muslim_shariat_reading, state_courts, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(family_law_authority__muslim_shariat_reading, state_courts, observer).

% Campaign for uniform civil code, khula reform, polygyny restriction, mahr enforcement. Litigate strategic cases (Shayara Bano, Danial Latifi). Operate outside personal law adjudication structures; arguments heard in constitutional courts but not in qazi proceedings. Mobilize public opinion and legislative pressure. Exit from framework is their goal — not trapped by it.
narrative_ontology:constraint_stakeholder(family_law_authority__muslim_shariat_reading, womens_rights_advocates, excluded,
    organized, biographical, mobile, national).

% Experience resource dilution, contested guardianship, inheritance complexity across multiple households. No voice in father's marriage decisions. Legal status varies by jurisdiction — some recognize all children equally, others privilege first wife's offspring. Exit from family structure impossible during minority.
narrative_ontology:constraint_stakeholder(family_law_authority__muslim_shariat_reading, children_of_polygynous_marriages, payer,
    powerless, biographical, trapped, local).
narrative_ontology:stakeholder_non_agent(family_law_authority__muslim_shariat_reading, children_of_polygynous_marriages).

% The kernel's fixed textual anchor. Surah An-Nisa 4:3 (polygyny), 4:34 (qiwamah), 2:228-229 (divorce waiting periods), 4:4 (mahr). Interpretive tradition (fiqh) mediates between text and practice. No agency — but the authority_grounding of the reading rests on its fixity.
narrative_ontology:constraint_stakeholder(family_law_authority__muslim_shariat_reading, quranic_text_and_hadith_corpus, observer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(family_law_authority__muslim_shariat_reading, quranic_text_and_hadith_corpus).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a recognized, enforceable framework for marriage formation, dissolution, financial obligation (mahr), child legitimacy, and inheritance across Muslim communities — solving coordination between families, protecting women's financial claim via mahr, and establishing clear lineage rules.
% TRANSFER_FUNCTION: Moves unilateral divorce power (talaq) and polygyny permission to husbands; moves mahr obligation from husband to wife (but husband controls timing); moves adjudicative authority to religious bodies; moves constitutional oversight burden to state courts.
% ABSENT_VOICES: Wives in polygynous marriages (especially subsequent wives), minor children in polygynous households, women in communities without access to khula/faskh mechanisms, queer Muslims for whom nikah is structurally unavailable. These voices are excluded from qazi proceedings and personal law board deliberations.
% DISAPPEARANCE_RATIONALE: If sharia personal law marriage vanished overnight, Muslim marriages would default to secular civil marriage law (Special Marriage Act 1954) or customary practice. Mahr claims would become contractual debts. Polygynous marriages would lose legal recognition. Talaq would require judicial process. Religious authorities would lose adjudicative jurisdiction. The community's marriage infrastructure would reorganize around state law or informal norms.
% FOUNDING_PROBLEM: Pre-Islamic Arabian marriage practices lacked women's financial protection (no mahr), permitted unlimited polygyny, allowed arbitrary repudiation without notice or support, and left children's legitimacy uncertain. The Quranic reforms introduced mahr as women's property, capped polygyny at four with justice condition, structured talaq with waiting periods (iddah), and established lineage rules.
% FOUNDING_PROBLEM_CORROBORATION: Classical fiqh texts (Maliki, Hanafi, Shafi'i, Hanbali) attest the founding reforms. Contemporary reformists (Asma Barlas, Amina Wadud, Abdullahi An-Na'im) argue the founding problem's justice conditions (adl in polygyny, mahr as genuine protection) are structurally unmet in current practice. Conservative ulema bodies (AIMPLB) attest the founding problem remains live and the revealed framework is sufficient. No consensus outside beneficiary circles.
narrative_ontology:disappearance_verdict(family_law_authority__muslim_shariat_reading, world_rearranges).
narrative_ontology:founding_problem_status(family_law_authority__muslim_shariat_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(family_law_authority__muslim_shariat_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(family_law_authority__muslim_shariat_reading, 'none', 1).
narrative_ontology:epsilon_provenance(family_law_authority__muslim_shariat_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(family_law_authority__muslim_shariat_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(family_law_authority__muslim_shariat_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(family_law_authority__muslim_shariat_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.68 reflects persistent gender asymmetry: husbands hold exit power (talaq) and polygyny option; wives' exit (khula/faskh) requires consent or judicial proof. The 2019 triple talaq ban reduced extractiveness temporarily (0.55) but subsequent practice (talaq-e-hasan, talaq-e-ahsan) and weak mahr enforcement restored it. Suppression 0.62: community pressure, religious identity costs, and limited secular alternatives constrain exit. Theater 0.28: mahr and iddah are functional; polygyny justice condition and talaq procedural requirements increasingly performative. Accessibility_collapse 0.55: secular marriage (Special Marriage Act) exists but carries social ostracism. Resistance 0.48: sustained reform litigation and legislative pressure but institutional capture of personal law boards.
 *
 * PERSPECTIVAL GAP:
 *   From husbands' and religious authorities' seats: the arrangement coordinates marriage, protects women via mahr, and preserves Quranic order — genuine rope. From wives', children's, and advocates' seats: the same structure extracts via asymmetric exit, polygyny, and weak enforcement — snare. The engine computes this divergence from power/exit asymmetries. The 2019 ban shifted husband d upward (less beneficiary) but did not equalize; khula/faskh thresholds keep wife d high.
 *
 * DIRECTIONALITY LOGIC:
 *   Husbands are primary beneficiaries (d near 0.2) — hold unilateral divorce, polygyny permission, control mahr timing. Religious authorities are agenda_setters with arbitrage exit (d ~0.15) — interpretive control, institutional rents. Wives are primary payers (d ~0.85) — bear asymmetric divorce burden, polygyny costs, mahr non-payment risk; constrained exit. State courts sit near symmetric (d ~0.5) — constitutional oversight burden vs. legislative authority. Women's rights advocates are excluded (d ~0.9) — bear advocacy costs, no institutional seat. Children are trapped payers (d ~1.0). The Quranic corpus is analytical observer (d=0.5 fixed).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (pre-Islamic marriage harms) is substantially solved: mahr exists, polygyny capped, talaq structured. But the mandate persists with extraction asymmetry intact. The justice condition on polygyny (4:3) is judicially unenforceable; mahr is routinely deferred/unpaid; talaq procedures favor husband. Mandatrophy is unresolved — the arrangement's coordination function does not require the extraction asymmetry, but the asymmetry is defended as textual fidelity. The constraint is not a piton (theater_ratio 0.28) nor a scaffold (no sunset). It is a tangled_rope where coordination and extraction are structurally fused.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the muslim_shariat_reading a single coherent constraint or a family of constraints (Hanafi, Maliki, Shafi''i, Hanbali, Shia Ja''fari) with materially different extraction profiles?',
    'Comparative fiqh analysis across madhahib on talaq procedures, mahr enforceability, polygyny conditions, and khula access. Measure variance in base_properties metrics per school.',
    'If intra-reading variance exceeds inter-reading variance, the kernel decomposition needs refinement — each madhhab may be a distinct reading of the same kernel.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether madhhab differences constitute separate constraints within this reading.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression experienced by wives structural (legal barriers, community enforcement) or internalized (religious identity making exit unthinkable, theological internalization of qiwamah)?',
    'Post-divorce trajectory studies: if women who obtain khula/faskh still report suppressed agency in subsequent decisions, internalized component is significant. Compare with women exiting via secular law (Special Marriage Act).',
    'If internalized suppression is substantial, effective suppression exceeds structural measure — the constraint''s grip persists after formal exit. This would increase computed effective extraction for identity_locked agents.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for wives.').

omega_variable(
    mahr_enforcement_gap,
    'What is the actual enforcement rate of mahr (prompt vs. deferred) across jurisdictions, and does non-enforcement constitute extraction or coordination failure?',
    'Empirical survey of family court decrees, qazi records, and women''s reports on mahr receipt. Distinguish prompt (muajjal) vs. deferred (muwajjal) mahr.',
    'Systematic non-enforcement would raise extractiveness (husband retains wife''s property right) and lower coordination_function credibility. Would support reclassification toward snare for the mahr sub-constraint.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(mahr_enforcement_gap, empirical, 'Whether mahr functions as genuine protection or theoretical right.').

omega_variable(
    polygyny_justice_condition_operationalization,
    'Does the Quranic ''adl (justice) condition on polygyny (4:3) have any operational enforcement mechanism, or is it purely aspirational?',
    'Case law review: have any qazi courts or civil courts denied polygyny permission or granted relief to existing wife based on failure of adl? Survey of personal law board fatwas.',
    'If adl is entirely unenforced, polygyny permission is pure extraction (male sexual/reproductive privilege) with zero coordination justification. Would increase extractiveness and support snare classification for the polygyny sub-constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(polygyny_justice_condition_operationalization, empirical, 'Operational status of the Quranic justice condition on polygyny.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(family_law_authority__muslim_shariat_reading, 1937, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fla_msr_tr_t1937, family_law_authority__muslim_shariat_reading, theater_ratio, 1937, 0.15).
narrative_ontology:measurement(fla_msr_tr_t1950, family_law_authority__muslim_shariat_reading, theater_ratio, 1950, 0.18).
narrative_ontology:measurement(fla_msr_tr_t1973, family_law_authority__muslim_shariat_reading, theater_ratio, 1973, 0.22).
narrative_ontology:measurement(fla_msr_tr_t1985, family_law_authority__muslim_shariat_reading, theater_ratio, 1985, 0.25).
narrative_ontology:measurement(fla_msr_tr_t2001, family_law_authority__muslim_shariat_reading, theater_ratio, 2001, 0.28).
narrative_ontology:measurement(fla_msr_tr_t2017, family_law_authority__muslim_shariat_reading, theater_ratio, 2017, 0.3).
narrative_ontology:measurement(fla_msr_tr_t2019, family_law_authority__muslim_shariat_reading, theater_ratio, 2019, 0.27).
narrative_ontology:measurement(fla_msr_tr_t2024, family_law_authority__muslim_shariat_reading, theater_ratio, 2024, 0.28).

% Extraction over time
narrative_ontology:measurement(fla_msr_be_t1937, family_law_authority__muslim_shariat_reading, base_extractiveness, 1937, 0.72).
narrative_ontology:measurement(fla_msr_be_t1950, family_law_authority__muslim_shariat_reading, base_extractiveness, 1950, 0.7).
narrative_ontology:measurement(fla_msr_be_t1973, family_law_authority__muslim_shariat_reading, base_extractiveness, 1973, 0.68).
narrative_ontology:measurement(fla_msr_be_t1985, family_law_authority__muslim_shariat_reading, base_extractiveness, 1985, 0.65).
narrative_ontology:measurement(fla_msr_be_t2001, family_law_authority__muslim_shariat_reading, base_extractiveness, 2001, 0.63).
narrative_ontology:measurement(fla_msr_be_t2017, family_law_authority__muslim_shariat_reading, base_extractiveness, 2017, 0.58).
narrative_ontology:measurement(fla_msr_be_t2019, family_law_authority__muslim_shariat_reading, base_extractiveness, 2019, 0.55).
narrative_ontology:measurement(fla_msr_be_t2024, family_law_authority__muslim_shariat_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(fla_msr_su_t1937, family_law_authority__muslim_shariat_reading, suppression_requirement, 1937, 0.55).
narrative_ontology:measurement(fla_msr_su_t1950, family_law_authority__muslim_shariat_reading, suppression_requirement, 1950, 0.58).
narrative_ontology:measurement(fla_msr_su_t1973, family_law_authority__muslim_shariat_reading, suppression_requirement, 1973, 0.6).
narrative_ontology:measurement(fla_msr_su_t1985, family_law_authority__muslim_shariat_reading, suppression_requirement, 1985, 0.62).
narrative_ontology:measurement(fla_msr_su_t2001, family_law_authority__muslim_shariat_reading, suppression_requirement, 2001, 0.63).
narrative_ontology:measurement(fla_msr_su_t2017, family_law_authority__muslim_shariat_reading, suppression_requirement, 2017, 0.65).
narrative_ontology:measurement(fla_msr_su_t2019, family_law_authority__muslim_shariat_reading, suppression_requirement, 2019, 0.58).
narrative_ontology:measurement(fla_msr_su_t2024, family_law_authority__muslim_shariat_reading, suppression_requirement, 2024, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(family_law_authority__muslim_shariat_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(family_law_authority__muslim_shariat_reading, 0.08).
narrative_ontology:affects_constraint(family_law_authority__muslim_shariat_reading, family_law_authority__hindu_dharmashastra_reading).
narrative_ontology:affects_constraint(family_law_authority__muslim_shariat_reading, family_law_authority__christian_canonical_reading).
narrative_ontology:affects_constraint(family_law_authority__muslim_shariat_reading, family_law_authority__parsi_zoroastrian_reading).
narrative_ontology:affects_constraint(family_law_authority__muslim_shariat_reading, family_law_authority__secular_contractual_reading).
narrative_ontology:affects_constraint(family_law_authority__muslim_shariat_reading, uniform_civil_code_debate).
narrative_ontology:affects_constraint(family_law_authority__muslim_shariat_reading, muslim_women_protection_of_rights_on_marriage_act_2019).

% DUAL FORMULATION NOTE:
% The family_law_authority kernel decomposes into five readings with distinct ε values. This reading (muslim_shariat_reading) has ε=0.68 (tangled_rope) due to gender-asymmetric divorce and polygyny. The secular_contractual_reading has ε≈0.15 (rope) — symmetric civil contract. The christian_canonical_reading (Catholic) has ε≈0.45 (tangled_rope) — indissolubility as coordination but asymmetric annulment access. The hindu_dharmashastra_reading has ε≈0.55 (tangled_rope) — sacramental but with customary divorce variation. The parsi_zoroastrian_reading has ε≈0.35 (rope) — community law with gender-symmetric divorce. The ε-invariance principle requires separate stories; the label 'Muslim personal law' cannot carry a single ε.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(family_law_authority__muslim_shariat_reading, moderate, 0.85).
constraint_indexing:directionality_override(family_law_authority__muslim_shariat_reading, institutional, 0.15).
constraint_indexing:directionality_override(family_law_authority__muslim_shariat_reading, powerless, 1.0).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
