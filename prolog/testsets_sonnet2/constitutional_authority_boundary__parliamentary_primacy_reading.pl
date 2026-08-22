% ============================================================================
% CONSTRAINT STORY: constitutional_authority_boundary__parliamentary_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_authority_boundary__parliamentary_primacy_reading, []).

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
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: constitutional_authority_boundary__parliamentary_primacy_reading
 *   human_readable: Parliamentary Primacy Reading of Constitutional Authority Boundary
 *   domain: constitutional_law/political_philosophy/institutional_design
 *
 * SUMMARY:
 *   This story instantiates the parliamentary-primacy reading of the
 *   constitutional_authority_boundary kernel: where constitutional text
 *   exists, it is subordinate to parliamentary sovereignty, and the elected
 *   legislature retains final authority to define constitutional meaning
 *   through ordinary or entrenched legislation, including the power to
 *   override or reverse judicial constitutional rulings. This is one of three
 *   structurally distinct readings of the same kernel (the others being
 *   judicial_supremacy_reading and coordinate_construction_reading, generated
 *   as separate constraint stories); each reading produces a different
 *   beneficiary/victim structure, a different ε, and a different
 *   classification, and none is generated here except this one, per the
 *   ε-invariance principle.
 *
 * KEY AGENTS:
 *   - elected_legislature: primary beneficiary and agenda_setter — holds and exercises final interpretive authority
 *   - electoral_majorities: beneficiary — gains durable policy control through ordinary politics rather than constitutional litigation
 *   - constitutional_courts: payer/excluded — review function persists but is contingent on legislative forbearance
 *   - constitutional_minorities: payer, powerless, trapped — loses a stable judicial backstop for rights claims
 *   - judicial_review_litigants: payer — bears litigation costs for rulings that remain revisable
 *   - executive_branch: beneficiary/payer depending on which body controls the legislature
 *   - constitutional_scholars: analytical observer across the kernel's readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_authority_boundary__parliamentary_primacy_reading, 0.2).
domain_priors:suppression_score(constitutional_authority_boundary__parliamentary_primacy_reading, 0.28).
domain_priors:theater_ratio(constitutional_authority_boundary__parliamentary_primacy_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_authority_boundary__parliamentary_primacy_reading, extractiveness, 0.2).
narrative_ontology:constraint_metric(constitutional_authority_boundary__parliamentary_primacy_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(constitutional_authority_boundary__parliamentary_primacy_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_authority_boundary__parliamentary_primacy_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(constitutional_authority_boundary__parliamentary_primacy_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_authority_boundary__parliamentary_primacy_reading, rope).
narrative_ontology:human_readable(constitutional_authority_boundary__parliamentary_primacy_reading, "Parliamentary Primacy Reading of Constitutional Authority Boundary").
narrative_ontology:topic_domain(constitutional_authority_boundary__parliamentary_primacy_reading, "constitutional_law/political_philosophy/institutional_design").

domain_priors:requires_active_enforcement(constitutional_authority_boundary__parliamentary_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_authority_boundary__parliamentary_primacy_reading, '3e7e02f8-02d2-468f-bb74-ae750d6d8cef').
narrative_ontology:cs_kernel_codification('3e7e02f8-02d2-468f-bb74-ae750d6d8cef', distributed).
narrative_ontology:cs_authority_grounding('3e7e02f8-02d2-468f-bb74-ae750d6d8cef', practice).
narrative_ontology:cs_interpretation_layer_present('3e7e02f8-02d2-468f-bb74-ae750d6d8cef').
narrative_ontology:cs_reading_relation('3e7e02f8-02d2-468f-bb74-ae750d6d8cef', constitutional_authority_boundary__judicial_supremacy_reading, forecloses).
narrative_ontology:cs_reading_relation('3e7e02f8-02d2-468f-bb74-ae750d6d8cef', constitutional_authority_boundary__coordinate_construction_reading, influences).
narrative_ontology:cs_axiom('3e7e02f8-02d2-468f-bb74-ae750d6d8cef', foundational, electoral_accountability_is_final_legitimacy_test).
narrative_ontology:cs_axiom_status(electoral_accountability_is_final_legitimacy_test, holdable).
narrative_ontology:cs_axiom_grounding('3e7e02f8-02d2-468f-bb74-ae750d6d8cef', electoral_accountability_is_final_legitimacy_test, conventional).
narrative_ontology:cs_axiom('3e7e02f8-02d2-468f-bb74-ae750d6d8cef', foundational, unelected_judicial_override_of_legislative_will_is_democratically_illegitimate).
narrative_ontology:cs_axiom_status(unelected_judicial_override_of_legislative_will_is_democratically_illegitimate, holdable).
narrative_ontology:cs_axiom_grounding('3e7e02f8-02d2-468f-bb74-ae750d6d8cef', unelected_judicial_override_of_legislative_will_is_democratically_illegitimate, instrumental).
narrative_ontology:cs_reference_frame('3e7e02f8-02d2-468f-bb74-ae750d6d8cef', westminster_parliamentary_sovereignty_tradition).
narrative_ontology:cs_drift_state('3e7e02f8-02d2-468f-bb74-ae750d6d8cef', post_rights_charter_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('3e7e02f8-02d2-468f-bb74-ae750d6d8cef', '').
narrative_ontology:cs_kernel_id(constitutional_authority_boundary__parliamentary_primacy_reading, constitutional_authority_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__parliamentary_primacy_reading, elected_legislature).
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__parliamentary_primacy_reading, electoral_majorities).
narrative_ontology:constraint_victim(constitutional_authority_boundary__parliamentary_primacy_reading, constitutional_minorities).
narrative_ontology:constraint_victim(constitutional_authority_boundary__parliamentary_primacy_reading, judicial_review_litigants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__parliamentary_primacy_reading, executive_branch).
narrative_ontology:constraint_victim(constitutional_authority_boundary__parliamentary_primacy_reading, constitutional_courts).
narrative_ontology:constraint_victim(constitutional_authority_boundary__parliamentary_primacy_reading, executive_branch).
narrative_ontology:constraint_vindicates(constitutional_authority_boundary__parliamentary_primacy_reading, democratic_self_governance_doctrine).
narrative_ontology:constraint_vindicates(constitutional_authority_boundary__parliamentary_primacy_reading, electoral_accountability_supremacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds final authority to define constitutional meaning through ordinary or entrenched statute, and can override or reverse judicial constitutional rulings through subsequent legislation. Justifies this as the necessary consequence of electoral accountability: unelected judges should not have the last word over a democratically elected body's understanding of fundamental law. Collects durable policy control as a direct product of this arrangement.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__parliamentary_primacy_reading, elected_legislature, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(constitutional_authority_boundary__parliamentary_primacy_reading, elected_legislature, beneficiary).

% Benefit from the assurance that a temporary majority's preferred policies cannot be permanently blocked by judicial interpretation of constitutional text; can eventually change outcomes they dislike through ordinary electoral and legislative processes rather than constitutional litigation or amendment supermajorities.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__parliamentary_primacy_reading, electoral_majorities, beneficiary,
    organized, biographical, mobile, national).

% Retain a review function but their constitutional rulings can be legislatively overridden, reversed, or rendered advisory by the very body whose acts they review. Cannot enforce a constitutional reading against a determined legislative majority; their interpretive authority is contingent on legislative forbearance rather than structurally protected.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__parliamentary_primacy_reading, constitutional_courts, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(constitutional_authority_boundary__parliamentary_primacy_reading, constitutional_courts, excluded).

% Groups whose rights claims depend on durable judicial protection against majoritarian legislative action bear the cost when that protection is revisable by ordinary or entrenched statute. Where the legislature enjoys final interpretive authority, a rights claim that a court would recognize can be legislatively neutralized by a sufficiently determined and empowered majority, leaving the minority without a stable forum of last resort.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__parliamentary_primacy_reading, constitutional_minorities, payer,
    powerless, biographical, trapped, national).

% Bring constitutional challenges expecting an authoritative judicial remedy, but any favorable ruling remains subject to legislative reversal. Litigation costs and time are borne regardless of whether the ultimate outcome holds, because the legislature can respond to an adverse ruling with corrective or entrenching legislation.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__parliamentary_primacy_reading, judicial_review_litigants, payer,
    moderate, biographical, constrained, national).

% Benefits when it commands a legislative majority, since executive-backed legislation faces no durable judicial ceiling; pays when out of power, since a hostile legislature can use the same interpretive supremacy to entrench limits on executive action that courts cannot lift.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__parliamentary_primacy_reading, executive_branch, beneficiary,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(constitutional_authority_boundary__parliamentary_primacy_reading, executive_branch, payer).

% Study the tradeoffs of parliamentary sovereignty against judicial supremacy and coordinate construction across jurisdictions, documenting where legislative override power protects democratic responsiveness and where it erodes minority rights protection and rule-of-law stability.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__parliamentary_primacy_reading, constitutional_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(constitutional_authority_boundary__parliamentary_primacy_reading, elected_legislature).
narrative_ontology:fixing_cost_class(constitutional_authority_boundary__parliamentary_primacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves the question of final constitutional interpretive authority by locating it in the body most directly and repeatedly accountable to the electorate, avoiding a standing conflict between an unelected judiciary and an elected legislature over which institution's reading of contested constitutional text ultimately governs.
% TRANSFER_FUNCTION: Moves final interpretive authority over constitutional meaning from courts to the elected legislature, and correspondingly moves the durability of constitutional protections away from constitutional minorities and litigants (who lose a stable judicial backstop) toward electoral majorities (who gain assurance that judicial rulings cannot permanently override their legislative preferences).
% ABSENT_VOICES: Constitutional minorities and future litigants whose rights claims would be recognized by a court but are not represented in the legislative majority that can override or entrench past that recognition; they are structurally present as a category but have no seat with power in the legislative process that ultimately settles the question.
% DISAPPEARANCE_RATIONALE: If parliamentary primacy over constitutional meaning were replaced overnight by judicial supremacy, courts would gain the power to invalidate legislation with no legislative override path, fundamentally altering which body has final say over rights claims, election law, and the boundaries of executive power; legislatures would lose the ability to correct or entrench their constitutional readings through ordinary politics.
% FOUNDING_PROBLEM: In parliamentary systems descending from Westminster tradition, the historical problem was curbing unaccountable royal or judicial authority by vesting ultimate lawmaking power in an elected assembly, on the premise that only the electorate's representatives should have the final word on the content and limits of fundamental law.
% FOUNDING_PROBLEM_CORROBORATION: Legislatures and their supporting constitutional theorists attest the founding problem remains live: unelected judges interpreting ambiguous constitutional text still risk substituting their policy preferences for those of an accountable majority. Comparative constitutional scholars and human-rights bodies operating outside the legislature attest that in practice the arrangement is increasingly used to insulate majoritarian policy from minority-rights review rather than to guard against judicial overreach, citing legislative override episodes targeting electoral, criminal-procedure, and minority-protection rulings specifically.
narrative_ontology:disappearance_verdict(constitutional_authority_boundary__parliamentary_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_authority_boundary__parliamentary_primacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_authority_boundary__parliamentary_primacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(constitutional_authority_boundary__parliamentary_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_authority_boundary__parliamentary_primacy_reading, 0.2, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_authority_boundary__parliamentary_primacy_reading_tests).
:- end_tests(constitutional_authority_boundary__parliamentary_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.20 at interval end) consistent with the expected structural delta for this reading: locating final interpretive authority in an elected, periodically accountable body is a comparatively low-coercion arrangement relative to judicial supremacy, because the population subject to the constraint retains an ordinary electoral and legislative remedy rather than depending on constitutional amendment supermajorities or judicial appointment politics. Suppression is likewise modest (0.28) and rises slowly — the coercive element is real (legislative override does foreclose a judicial remedy path for a specific class of minority claims) but is bounded by periodic elections rather than sustained by standing enforcement infrastructure. Theater ratio stays low throughout (0.15 at T=40): the arrangement's coordination function — resolving who has final say — is genuinely exercised, not performed; there is little indication of a maintained fiction disguising an atrophied function.
 *
 * PERSPECTIVAL GAP:
 *   From the legislature's seat, this is a rope: a genuine coordination solution to the problem of final authority, keeping ultimate constitutional meaning tethered to electoral accountability. From a constitutional minority's seat with no realistic path to legislative majority, the same structural arrangement can compute as extractive despite its low authored ε, because the specific protection they need — a stable forum immune to majoritarian override — is exactly what this reading forecloses. This divergence is the point: the engine should compute different effective types for the agenda_setter/beneficiary seats versus the powerless payer seats from the same structural data, without either seat's experience overriding the other in the story's authored claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Elected legislature and electoral majorities sit near the beneficiary end of directionality: the arrangement subsidizes their policy durability by removing a standing judicial veto. Constitutional courts sit closer to the target end on this reading specifically because their formal review authority is rendered advisory or reversible — the constraint extracts institutional authority from them relative to the judicial-supremacy reading, even though courts retain nominal review power. Constitutional minorities and judicial review litigants sit furthest toward the target end: they are structurally trapped or constrained, lacking the electoral leverage that gives majorities their remedy, and their rights claims are the specific class of claim this reading's transfer function moves away from durable protection.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — curbing unaccountable authority by vesting final constitutional say in an elected body — remains partially live (elected accountability is a genuine ongoing value) but is contested as to whether the current operation of legislative override still serves that problem or has drifted into a tool for insulating majoritarian policy, including policy that touches minority rights and electoral procedure, from judicial correction. The status is authored as contested rather than dead specifically because corroboration diverges: the legislature's own theorists see the problem as live, while comparative and rights-focused external observers see the arrangement increasingly used past its original guard-against-judicial-overreach function. This mismatch (status=contested, disappearance_verdict=world_rearranges) is exactly the signal the R5 consumer is built to flag for review rather than resolve unilaterally in the story.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    electoral_accountability_vs_majoritarian_capture,
    'Does locating final constitutional interpretive authority in the elected legislature function as a genuine democratic accountability mechanism, or does it function as a majoritarian capture mechanism that strips durable protection from minorities who cannot assemble legislative power?',
    'Comparative study of jurisdictions operating under parliamentary primacy (e.g. historical Westminster systems, notwithstanding-clause regimes) tracking the frequency and target-class of legislative overrides of judicial rulings: overrides concentrated on procedural or economic policy versus overrides concentrated on minority-rights or electoral-fairness rulings would support different readings of the mechanism''s actual function.',
    'If overrides are concentrated on minority-rights rulings, the arrangement''s effective extraction on constitutional_minorities is higher than the authored ε (0.20) suggests, and this reading''s classification should be re-examined toward tangled_rope; if overrides are broadly distributed and rare, the low-ε rope reading is supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(electoral_accountability_vs_majoritarian_capture, empirical, 'Whether parliamentary override power is used as accountability or as majoritarian capture.').

omega_variable(
    reading_selection_under_determination,
    'Is parliamentary primacy the framing this constitution''s own text and drafting history actually supports, or is it one contestable reading among the three (parliamentary_primacy, judicial_supremacy, coordinate_construction) selected here because the source material foregrounds legislative sovereignty language rather than judicial-review or separation-of-powers language present elsewhere in the same constitutional order?',
    'Textual and originalist analysis of the specific constitutional order''s founding debates, entrenchment clauses, and judicial review provisions (if any) to determine whether the text itself resolves the kernel or leaves it genuinely contested across all three readings.',
    'If the text clearly supports judicial_supremacy_reading or coordinate_construction_reading instead, this story''s classification of parliamentary primacy as the operative arrangement is a misreading of the kernel rather than a legitimate alternative reading, and the sibling readings would need to be treated as primary rather than co-equal.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_selection_under_determination, conceptual, 'Whether parliamentary primacy is the textually supported reading or one of several defensible framings of an underdetermined kernel.').

omega_variable(
    entrenchment_asymmetry,
    'When the legislature uses entrenched (supermajority-protected) legislation rather than ordinary legislation to fix constitutional meaning, does that entrenchment meaningfully convert parliamentary primacy into something closer to a judicially-unreviewable but legislatively-self-imposed constitutional text, blurring the line with coordinate_construction_reading?',
    'Case analysis of jurisdictions where legislatures have entrenched constitutional interpretations via supermajority mechanisms, examining whether courts retain any interpretive role over the entrenched text or whether entrenchment fully forecloses judicial input.',
    'If entrenchment fully forecloses judicial input, this reading''s ε should be treated as its own sub-case with higher suppression than ordinary-legislation parliamentary primacy; if courts retain interpretive latitude over entrenched text, the boundary with coordinate_construction_reading is closer than the story''s expected structural delta assumes.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(entrenchment_asymmetry, conceptual, 'Whether entrenched legislative constitutional definition collapses the distinction between parliamentary primacy and coordinate construction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_authority_boundary__parliamentary_primacy_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_authority_boundary__parliamentary_primacy_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(cons_tr_t8, constitutional_authority_boundary__parliamentary_primacy_reading, theater_ratio, 8, 0.09).
narrative_ontology:measurement(cons_tr_t16, constitutional_authority_boundary__parliamentary_primacy_reading, theater_ratio, 16, 0.1).
narrative_ontology:measurement(cons_tr_t24, constitutional_authority_boundary__parliamentary_primacy_reading, theater_ratio, 24, 0.12).
narrative_ontology:measurement(cons_tr_t32, constitutional_authority_boundary__parliamentary_primacy_reading, theater_ratio, 32, 0.14).
narrative_ontology:measurement(cons_tr_t40, constitutional_authority_boundary__parliamentary_primacy_reading, theater_ratio, 40, 0.15).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_authority_boundary__parliamentary_primacy_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(cons_be_t8, constitutional_authority_boundary__parliamentary_primacy_reading, base_extractiveness, 8, 0.14).
narrative_ontology:measurement(cons_be_t16, constitutional_authority_boundary__parliamentary_primacy_reading, base_extractiveness, 16, 0.16).
narrative_ontology:measurement(cons_be_t24, constitutional_authority_boundary__parliamentary_primacy_reading, base_extractiveness, 24, 0.18).
narrative_ontology:measurement(cons_be_t32, constitutional_authority_boundary__parliamentary_primacy_reading, base_extractiveness, 32, 0.19).
narrative_ontology:measurement(cons_be_t40, constitutional_authority_boundary__parliamentary_primacy_reading, base_extractiveness, 40, 0.2).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_authority_boundary__parliamentary_primacy_reading, suppression_requirement, 0, 0.18).
narrative_ontology:measurement(cons_su_t8, constitutional_authority_boundary__parliamentary_primacy_reading, suppression_requirement, 8, 0.2).
narrative_ontology:measurement(cons_su_t16, constitutional_authority_boundary__parliamentary_primacy_reading, suppression_requirement, 16, 0.22).
narrative_ontology:measurement(cons_su_t24, constitutional_authority_boundary__parliamentary_primacy_reading, suppression_requirement, 24, 0.24).
narrative_ontology:measurement(cons_su_t32, constitutional_authority_boundary__parliamentary_primacy_reading, suppression_requirement, 32, 0.26).
narrative_ontology:measurement(cons_su_t40, constitutional_authority_boundary__parliamentary_primacy_reading, suppression_requirement, 40, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_authority_boundary__parliamentary_primacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(constitutional_authority_boundary__parliamentary_primacy_reading, judicial_supremacy_reading).
narrative_ontology:affects_constraint(constitutional_authority_boundary__parliamentary_primacy_reading, coordinate_construction_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraints decomposed from the single natural-language 'constitutional supremacy' kernel (constitutional_authority_boundary), per the ε-invariance principle: parliamentary_primacy_reading (this file, ε≈0.20, rope-leaning), judicial_supremacy_reading (expected higher ε from unreviewable judicial power with no override remedy), and coordinate_construction_reading (expected intermediate ε from unresolved inter-branch contest). Each reading has its own beneficiary/victim structure and classification; they are linked here via affects_constraints because the readings compete for the same institutional space and legislative or judicial action instantiating one reading structurally forecloses or pressures the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
