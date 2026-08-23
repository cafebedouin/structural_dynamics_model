% ============================================================================
% CONSTRAINT STORY: waitangi_sovereignty_allocation__crown_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_waitangi_sovereignty_allocation__crown_sovereignty_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: waitangi_sovereignty_allocation__crown_sovereignty_reading
 *   human_readable: Crown Sovereignty Reading of Treaty Article I — Westminster Parliamentary Supremacy
 *   domain: constitutional/indigenous_rights/post_colonial
 *
 * SUMMARY:
 *   The Crown sovereignty reading treats the English text of Treaty Article I
 *   ('cede to Her Majesty the Queen of England absolutely and without
 *   reservation all the rights and powers of Sovereignty') as the controlling
 *   constitutional fact. This reading instantiates a constraint in which
 *   Westminster parliamentary supremacy operates without Māori consent
 *   requirement, resource allocation is unilateral, and Māori interests are
 *   structurally subordinated to parliamentary will. The constraint is
 *   CLAIMED as a Mountain — constitutional necessity, the natural law of the
 *   NZ state — while the authored metrics describe a substantially
 *   extractive, actively enforced arrangement that collapses alternatives and
 *   meets sustained resistance. The engine measures this claim/metric
 *   divergence; do not reconcile.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(waitangi_sovereignty_allocation__crown_sovereignty_reading, 0.82).
domain_priors:suppression_score(waitangi_sovereignty_allocation__crown_sovereignty_reading, 0.88).
domain_priors:theater_ratio(waitangi_sovereignty_allocation__crown_sovereignty_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__crown_sovereignty_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__crown_sovereignty_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__crown_sovereignty_reading, accessibility_collapse, 0.91).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__crown_sovereignty_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(waitangi_sovereignty_allocation__crown_sovereignty_reading, mountain).
narrative_ontology:human_readable(waitangi_sovereignty_allocation__crown_sovereignty_reading, "Crown Sovereignty Reading of Treaty Article I — Westminster Parliamentary Supremacy").
narrative_ontology:topic_domain(waitangi_sovereignty_allocation__crown_sovereignty_reading, "constitutional/indigenous_rights/post_colonial").

domain_priors:requires_active_enforcement(waitangi_sovereignty_allocation__crown_sovereignty_reading).
domain_priors:emerges_naturally(waitangi_sovereignty_allocation__crown_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(waitangi_sovereignty_allocation__crown_sovereignty_reading, '32e97827-02e1-4083-99ca-880e23c9ecf4').
narrative_ontology:cs_kernel_codification('32e97827-02e1-4083-99ca-880e23c9ecf4', fixed_text).
narrative_ontology:cs_authority_grounding('32e97827-02e1-4083-99ca-880e23c9ecf4', lineage).
narrative_ontology:cs_interpretation_layer_present('32e97827-02e1-4083-99ca-880e23c9ecf4').
narrative_ontology:cs_reading_relation('32e97827-02e1-4083-99ca-880e23c9ecf4', waitangi_sovereignty_allocation__partnership_reading, coexists_with).
narrative_ontology:cs_reading_relation('32e97827-02e1-4083-99ca-880e23c9ecf4', waitangi_sovereignty_allocation__rangatiratanga_reading, forecloses).
narrative_ontology:cs_axiom('32e97827-02e1-4083-99ca-880e23c9ecf4', foundational, crown_sovereignty_complete_cession).
narrative_ontology:cs_axiom_status(crown_sovereignty_complete_cession, holdable).
narrative_ontology:cs_axiom_grounding('32e97827-02e1-4083-99ca-880e23c9ecf4', crown_sovereignty_complete_cession, conventional).
narrative_ontology:cs_axiom('32e97827-02e1-4083-99ca-880e23c9ecf4', foundational, parliamentary_supremacy_unqualified).
narrative_ontology:cs_axiom_status(parliamentary_supremacy_unqualified, holdable).
narrative_ontology:cs_axiom_grounding('32e97827-02e1-4083-99ca-880e23c9ecf4', parliamentary_supremacy_unqualified, conventional).
narrative_ontology:cs_reference_frame('32e97827-02e1-4083-99ca-880e23c9ecf4', id_1840_british_sovereignty_acquisition).
narrative_ontology:cs_drift_state('32e97827-02e1-4083-99ca-880e23c9ecf4', contemporary_settlement_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('32e97827-02e1-4083-99ca-880e23c9ecf4', '').
narrative_ontology:cs_kernel_id(waitangi_sovereignty_allocation__crown_sovereignty_reading, waitangi_sovereignty_allocation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__crown_sovereignty_reading, crown_parliament).
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__crown_sovereignty_reading, settler_institutions).
narrative_ontology:constraint_victim(waitangi_sovereignty_allocation__crown_sovereignty_reading, maori_collectivity).
narrative_ontology:constraint_victim(waitangi_sovereignty_allocation__crown_sovereignty_reading, hapu_iwi).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Exercises plenary legislative power over all persons and resources in New Zealand. Claims authority derives from 1840 cession of sovereignty in English Article I. Administers courts, police, legislation, and resource allocation without requirement for Māori consent. Collects the full incidents of sovereignty: law-making monopoly, revenue, territorial control.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__crown_sovereignty_reading, crown_parliament, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(waitangi_sovereignty_allocation__crown_sovereignty_reading, crown_parliament, beneficiary).

% Subject to Crown legislation that unilaterally extinguishes customary title, regulates taonga, and overrides Māori decision-making structures. The Waitangi Tribunal provides advisory recommendations only; Parliament may ignore them. Resistance occurs through litigation, political mobilization, and cultural revitalization, but exit from Crown jurisdiction is structurally unavailable.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__crown_sovereignty_reading, maori_collectivity, payer,
    organized, generational, trapped, national).

% Kin-based political units that signed Te Tiriti understanding Article II to guarantee tino rangatiratanga. Experience the constraint as daily denial of authority over lands, waters, and cultural treasures. Identity is fused to the relationship with whenua and moana; exit from Crown jurisdiction would require abandoning the territorial and relational basis of hapū/iwi existence.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__crown_sovereignty_reading, hapu_iwi, payer,
    organized, generational, identity_locked, regional).

% Banks, corporations, local government, and professional bodies operate on Crown-granted titles and regulatory frameworks. Their asset base and legal certainty depend on the Crown sovereignty reading being authoritative. They benefit from the constraint without administering it; their exit would mean capital flight or institutional restructuring, not personal identity loss.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__crown_sovereignty_reading, settler_institutions, beneficiary,
    institutional, generational, mobile, national).

% Interpret and apply the constraint. The Supreme Court and Waitangi Tribunal have developed a jurisprudence that acknowledges Treaty principles while consistently affirming Crown sovereignty as non-justiciable. They observe the constraint's operation from within the system it constitutes; their analytical exit is academic, not structural.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__crown_sovereignty_reading, courts_tribunals, observer,
    institutional, generational, analytical, national).

% UN Permanent Forum, EMRIP, and treaty-monitoring bodies consistently find the Crown sovereignty reading incompatible with UNDRIP and self-determination norms. They would object to the constraint's classification as a Mountain if present in the classification conversation. Their exclusion is structural: the constraint's domestic legal order treats international indigenous rights as aspirational, not binding.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__crown_sovereignty_reading, international_indigenous_rights_bodies, excluded,
    powerful, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single, final sovereign authority over a geographically defined territory, resolving the pre-1840 condition of competing Māori polities and unbounded European settlement by vesting all legislative and jurisdictional power in the Crown.
% TRANSFER_FUNCTION: Moves legislative supremacy, ultimate title to all land and resources, and the monopoly on legitimate force from Māori political communities to the Crown/Parliament. The transfer is unilateral — no Māori consent mechanism exists in this reading — and continuous: every Act of Parliament re-enacts the transfer.
% ABSENT_VOICES: The Māori signatories who understood Article II (te reo) as retaining tino rangatiratanga over their lands, villages, and treasures. International indigenous rights frameworks (UNDRIP, ILO 169) that treat free, prior, and informed consent as the standard for decisions affecting indigenous peoples. These voices are excluded by the constraint's own domestic legal architecture, which treats the Treaty as non-justiciable absent legislative incorporation.
% DISAPPEARANCE_RATIONALE: If the Crown sovereignty reading vanished overnight, the entire New Zealand constitutional order — property titles, legislative validity, court jurisdiction, Crown-Māori settlement processes, and the state's international legal personality — would lose its foundational premise. A new constitutional settlement would be required, necessarily involving Māori as constitutional partners rather than subjects.
% FOUNDING_PROBLEM: British authorities in 1839-40 faced uncontrolled European settlement, inter-hapū conflict exacerbated by musket trade, and French colonial interest. The Colonial Office sought a single sovereign authority that could regulate settlement, maintain order, and pre-empt foreign claims — a 'governor' for British subjects with Māori acquiescence.
% FOUNDING_PROBLEM_CORROBORATION: British Colonial Office despatches (e.g., Normanby to Hobson, 1839) attest the Crown's settlement-governance imperative. Waitangi Tribunal findings (Te Paparahi o Te Raki, 2014; He Whakaputanga me Te Tiriti, 2014) and Māori oral history attest the problem was misdiagnosed: rangatira ceded kāwanatanga (governorship over settlers) not sovereignty, and the founding problem of uncontrolled settlement was manufactured by the Crown's own agents.
narrative_ontology:disappearance_verdict(waitangi_sovereignty_allocation__crown_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(waitangi_sovereignty_allocation__crown_sovereignty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(waitangi_sovereignty_allocation__crown_sovereignty_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(waitangi_sovereignty_allocation__crown_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(waitangi_sovereignty_allocation__crown_sovereignty_reading, 0.82, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(waitangi_sovereignty_allocation__crown_sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(waitangi_sovereignty_allocation__crown_sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__crown_sovereignty_reading, ExtMetricName, E),
    domain_priors:suppression_score(waitangi_sovereignty_allocation__crown_sovereignty_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(waitangi_sovereignty_allocation__crown_sovereignty_reading),
    narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__crown_sovereignty_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__crown_sovereignty_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(waitangi_sovereignty_allocation__crown_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.82) reflects the transfer of full legislative supremacy and resource control from Māori polities to the Crown, with no consent mechanism and no revenue sharing. Suppression (0.88) is higher still because the constraint's persistence depends on active legal enforcement (courts striking down Māori jurisdiction claims, legislation extinguishing customary title) and the structural exclusion of alternative constitutional frameworks. Theater ratio (0.48) captures that genuine governance coordination exists (public administration, rule of law for settlers) but nearly half the constraint's operation is performative maintenance of the sovereignty claim against mounting jurisprudential and political challenge. Accessibility collapse (0.91) is extreme: the domestic legal system treats partnership and rangatiratanga readings as legally impossible, not merely contestable. Resistance (0.75) is sustained across litigation, political mobilization, and cultural revitalization, but remains structurally contained by the constraint's own enforcement machinery.
 *
 * PERSPECTIVAL GAP:
 *   From the Crown/Parliament seat, the constraint appears as genuine coordination: it provides the single sovereign authority that makes governance possible. From the hapū/iwi seat, the same structure operates as enforced extraction: their tino rangatiratanga is denied, their resources allocated without consent, and their resistance criminalized. The engine computes this divergence from the structural data — the authored claim (mountain) does not adjudicate it. The false_summit_mountain signature will evaluate whether the declared beneficiaries on a claimed mountain trigger reclassification.
 *
 * DIRECTIONALITY LOGIC:
 *   Crown/Parliament sits at the beneficiary extreme (d ≈ 0.05): it collects the full incidents of sovereignty, controls the rules, and has arbitrage-grade exit (could theoretically abdicate but would lose the asset). Māori collectivity and hapū/iwi are full targets (d ≈ 0.95): they bear the extraction, have trapped or identity-locked exit, and the constraint's enforcement machinery is directed at them. Settler institutions are beneficiaries (d ≈ 0.15) with mobile exit — they benefit from legal certainty but could relocate capital. Courts are analytical observers (d = 0.5). International bodies are excluded — their structural position is outside the constraint's jurisdiction, which is exactly why the constraint must exclude them.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (governing uncontrolled settlement) was real in 1840 but the Crown sovereignty reading's solution — complete cession — was not what Māori agreed to (per te reo text and oral history). The arrangement persists because the Crown extracts substantial benefit (unilateral legislative power, resource control) and the cost of constitutional restructuring is prohibitive for the Crown while the cost of living under the constraint is prohibitive for Māori. This is mandatrophy: the mandate (govern settlement) has been superseded by the extraction (govern Māori without consent).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    article_i_cession_ambiguity,
    'Does the English Article I term ''sovereignty'' map to the Māori Article I term ''kāwanatanga'' (governorship) such that the cession was of governance over settlers only, not sovereignty over Māori?',
    'Comparative linguistic analysis of 1840 Māori and English political vocabulary; examination of Hobson''s oral explanations at Waitangi; Waitangi Tribunal''s Te Paparahi o Te Raki findings on the meaning of kāwanatanga vs. mana/tino rangatiratanga.',
    'If kāwanatanga ≠ sovereignty, the Crown sovereignty reading''s foundational premise fails; the constraint reclassifies from Mountain to Snare (constructed extraction with active suppression). If kāwanatanga = sovereignty, the Mountain claim gains empirical support but the extraction metrics remain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(article_i_cession_ambiguity, empirical, 'Whether the textual basis for the Crown sovereignty reading survives linguistic-historical scrutiny.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (legal bars, police, courts) or partially internalized (colonized consciousness, acceptance of Crown authority as natural)?',
    'Post-settlement suppression trajectory: if Māori political imagination and resistance persist despite Treaty settlements and legislative recognition of Treaty principles, the suppression is not fully internalized. Longitudinal studies of Māori constitutional aspirations post-1975.',
    'If substantially internalized, the constraint''s effective suppression exceeds the structural measure — the target carries the suppression after formal barriers lower. This would increase effective extraction for identity-locked hapū/iwi beyond what structural metrics capture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, conceptual, 'Structural vs. internalized suppression in a century-plus colonial constraint.').

omega_variable(
    sovereignty_rangatiratanga_separability,
    'Are Crown sovereignty (kāwanatanga) and tino rangatiratanga structurally separable functions, or does the Crown sovereignty reading''s claim to completeness logically require the extinguishment of rangatiratanga?',
    'Constitutional design analysis: can a unitary sovereign delegate final authority over defined domains (taonga, whenua, wāhi tapu) to subsidiary polities without losing sovereignty? Comparative study of federal/devolved models and indigenous self-government agreements (Canada, USA, Scandinavia).',
    'If separable, the Crown sovereignty reading''s extraction is a policy choice, not a structural necessity — supporting Tangled Rope or Snare classification. If inseparable, the Mountain claim''s coordination function is genuine but the extraction is the price of that coordination.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sovereignty_rangatiratanga_separability, conceptual, 'Whether the constraint''s coordination and extraction components are structurally separable.').

omega_variable(
    kernel_reading_identity,
    'Is the crown_sovereignty_reading a distinct constraint from the partnership_reading and rangatiratanga_reading, or are they measurement perspectives on a single constraint?',
    'ε-invariance test: do the three readings author different base extractiveness values for the same referent (the 1840-present constitutional arrangement)? If yes, they are distinct constraints per DP-001. The engine''s cross-constraint coupling analysis will detect structural dependency.',
    'Confirms this story''s validity as a standalone constraint in the kernel family. If ε values converge, the decomposition was unnecessary and the kernel should be modeled as one constraint with perspectival seats.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Validator for the kernel-reading decomposition discipline (Rule 1).').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(waitangi_sovereignty_allocation__crown_sovereignty_reading, 0, 184).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wait_tr_t0, waitangi_sovereignty_allocation__crown_sovereignty_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(wait_tr_t20, waitangi_sovereignty_allocation__crown_sovereignty_reading, theater_ratio, 20, 0.25).
narrative_ontology:measurement(wait_tr_t40, waitangi_sovereignty_allocation__crown_sovereignty_reading, theater_ratio, 40, 0.35).
narrative_ontology:measurement(wait_tr_t60, waitangi_sovereignty_allocation__crown_sovereignty_reading, theater_ratio, 60, 0.4).
narrative_ontology:measurement(wait_tr_t80, waitangi_sovereignty_allocation__crown_sovereignty_reading, theater_ratio, 80, 0.43).
narrative_ontology:measurement(wait_tr_t100, waitangi_sovereignty_allocation__crown_sovereignty_reading, theater_ratio, 100, 0.45).
narrative_ontology:measurement(wait_tr_t120, waitangi_sovereignty_allocation__crown_sovereignty_reading, theater_ratio, 120, 0.47).
narrative_ontology:measurement(wait_tr_t140, waitangi_sovereignty_allocation__crown_sovereignty_reading, theater_ratio, 140, 0.48).
narrative_ontology:measurement(wait_tr_t160, waitangi_sovereignty_allocation__crown_sovereignty_reading, theater_ratio, 160, 0.48).
narrative_ontology:measurement(wait_tr_t184, waitangi_sovereignty_allocation__crown_sovereignty_reading, theater_ratio, 184, 0.48).

% Extraction over time
narrative_ontology:measurement(wait_be_t0, waitangi_sovereignty_allocation__crown_sovereignty_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(wait_be_t20, waitangi_sovereignty_allocation__crown_sovereignty_reading, base_extractiveness, 20, 0.55).
narrative_ontology:measurement(wait_be_t40, waitangi_sovereignty_allocation__crown_sovereignty_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement(wait_be_t60, waitangi_sovereignty_allocation__crown_sovereignty_reading, base_extractiveness, 60, 0.75).
narrative_ontology:measurement(wait_be_t80, waitangi_sovereignty_allocation__crown_sovereignty_reading, base_extractiveness, 80, 0.78).
narrative_ontology:measurement(wait_be_t100, waitangi_sovereignty_allocation__crown_sovereignty_reading, base_extractiveness, 100, 0.8).
narrative_ontology:measurement(wait_be_t120, waitangi_sovereignty_allocation__crown_sovereignty_reading, base_extractiveness, 120, 0.81).
narrative_ontology:measurement(wait_be_t140, waitangi_sovereignty_allocation__crown_sovereignty_reading, base_extractiveness, 140, 0.82).
narrative_ontology:measurement(wait_be_t160, waitangi_sovereignty_allocation__crown_sovereignty_reading, base_extractiveness, 160, 0.82).
narrative_ontology:measurement(wait_be_t184, waitangi_sovereignty_allocation__crown_sovereignty_reading, base_extractiveness, 184, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(wait_su_t0, waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(wait_su_t20, waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression_requirement, 20, 0.75).
narrative_ontology:measurement(wait_su_t40, waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression_requirement, 40, 0.82).
narrative_ontology:measurement(wait_su_t60, waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression_requirement, 60, 0.85).
narrative_ontology:measurement(wait_su_t80, waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression_requirement, 80, 0.87).
narrative_ontology:measurement(wait_su_t100, waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression_requirement, 100, 0.88).
narrative_ontology:measurement(wait_su_t120, waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression_requirement, 120, 0.88).
narrative_ontology:measurement(wait_su_t140, waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression_requirement, 140, 0.88).
narrative_ontology:measurement(wait_su_t160, waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression_requirement, 160, 0.88).
narrative_ontology:measurement(wait_su_t184, waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression_requirement, 184, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(waitangi_sovereignty_allocation__crown_sovereignty_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(waitangi_sovereignty_allocation__crown_sovereignty_reading, 0.12).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__crown_sovereignty_reading, waitangi_sovereignty_allocation__partnership_reading).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__crown_sovereignty_reading, waitangi_sovereignty_allocation__rangatiratanga_reading).

% DUAL FORMULATION NOTE:
% BGS-pattern decomposition of the waitangi_sovereignty_allocation kernel. This reading (crown_sovereignty) claims Mountain via Westminster supremacy doctrine; partnership_reading claims Tangled Rope (coordination + extraction); rangatiratanga_reading claims Snare (Māori text retained full authority). The three stories form a constraint family linked by affects_constraints. ε values differ substantially: this reading ε=0.82 (high extraction from Māori), partnership ε≈0.45 (partial coordination, partial extraction), rangatiratanga ε≈0.9 (near-total extraction from Crown perspective). The kernel's natural-language label 'Treaty of Waitangi' conflates three structurally distinct constitutional claims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(waitangi_sovereignty_allocation__crown_sovereignty_reading, institutional, 0.05).
constraint_indexing:directionality_override(waitangi_sovereignty_allocation__crown_sovereignty_reading, organized, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
