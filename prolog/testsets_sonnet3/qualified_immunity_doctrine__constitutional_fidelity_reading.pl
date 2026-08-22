% ============================================================================
% CONSTRAINT STORY: qualified_immunity_doctrine__constitutional_fidelity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_qualified_immunity_doctrine__constitutional_fidelity_reading, []).

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
 *   constraint_id: qualified_immunity_doctrine__constitutional_fidelity_reading
 *   human_readable: Qualified Immunity as Ultra Vires Judicial Fabrication
 *   domain: constitutional_law/civil_rights/law_enforcement_policy
 *
 * SUMMARY:
 *   Qualified immunity shields government officials, principally police
 *   officers, from civil suits under 42 U.S.C. § 1983 unless the right
 *   violated was 'clearly established' at the time of the conduct — a
 *   standard invented by the Supreme Court in Pierson v. Ray (1967) and
 *   substantially reshaped in Harlow v. Fitzgerald (1982), with no textual
 *   basis in the 1871 statute Congress actually passed. Under this reading,
 *   the central wrong is not that the doctrine produces bad outcomes (though
 *   it may) but that federal courts arrogated to themselves a lawmaking
 *   function belonging to Congress, and have maintained and expanded that
 *   arrogation for over five decades without legislative ratification.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qualified_immunity_doctrine__constitutional_fidelity_reading, 0.58).
domain_priors:suppression_score(qualified_immunity_doctrine__constitutional_fidelity_reading, 0.62).
domain_priors:theater_ratio(qualified_immunity_doctrine__constitutional_fidelity_reading, 0.71).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qualified_immunity_doctrine__constitutional_fidelity_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(qualified_immunity_doctrine__constitutional_fidelity_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(qualified_immunity_doctrine__constitutional_fidelity_reading, theater_ratio, 0.71).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qualified_immunity_doctrine__constitutional_fidelity_reading, accessibility_collapse, 0.66).
narrative_ontology:constraint_metric(qualified_immunity_doctrine__constitutional_fidelity_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qualified_immunity_doctrine__constitutional_fidelity_reading, snare).
narrative_ontology:human_readable(qualified_immunity_doctrine__constitutional_fidelity_reading, "Qualified Immunity as Ultra Vires Judicial Fabrication").
narrative_ontology:topic_domain(qualified_immunity_doctrine__constitutional_fidelity_reading, "constitutional_law/civil_rights/law_enforcement_policy").

domain_priors:requires_active_enforcement(qualified_immunity_doctrine__constitutional_fidelity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qualified_immunity_doctrine__constitutional_fidelity_reading, 'f0085afa-d4bb-4687-ab46-ebf4e995ef4a').
narrative_ontology:cs_kernel_codification('f0085afa-d4bb-4687-ab46-ebf4e995ef4a', distributed).
narrative_ontology:cs_authority_grounding('f0085afa-d4bb-4687-ab46-ebf4e995ef4a', practice).
narrative_ontology:cs_interpretation_layer_present('f0085afa-d4bb-4687-ab46-ebf4e995ef4a').
narrative_ontology:cs_reading_relation('f0085afa-d4bb-4687-ab46-ebf4e995ef4a', qualified_immunity_doctrine__protective_scaffold_reading, coexists_with).
narrative_ontology:cs_reading_relation('f0085afa-d4bb-4687-ab46-ebf4e995ef4a', qualified_immunity_doctrine__accountability_void_reading, influences).
narrative_ontology:cs_axiom('f0085afa-d4bb-4687-ab46-ebf4e995ef4a', foundational, judicial_lawmaking_requires_textual_or_constitutional_warrant).
narrative_ontology:cs_axiom_status(judicial_lawmaking_requires_textual_or_constitutional_warrant, holdable).
narrative_ontology:cs_axiom_grounding('f0085afa-d4bb-4687-ab46-ebf4e995ef4a', judicial_lawmaking_requires_textual_or_constitutional_warrant, conventional).
narrative_ontology:cs_axiom('f0085afa-d4bb-4687-ab46-ebf4e995ef4a', foundational, legitimacy_is_severable_from_policy_outcome).
narrative_ontology:cs_axiom_status(legitimacy_is_severable_from_policy_outcome, holdable).
narrative_ontology:cs_axiom_grounding('f0085afa-d4bb-4687-ab46-ebf4e995ef4a', legitimacy_is_severable_from_policy_outcome, deontological).
narrative_ontology:cs_reference_frame('f0085afa-d4bb-4687-ab46-ebf4e995ef4a', statutory_text_supremacy).
narrative_ontology:cs_drift_state('f0085afa-d4bb-4687-ab46-ebf4e995ef4a', post_harlow_qualified_immunity_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('f0085afa-d4bb-4687-ab46-ebf4e995ef4a', '').
narrative_ontology:cs_kernel_id(qualified_immunity_doctrine__constitutional_fidelity_reading, qualified_immunity_doctrine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(qualified_immunity_doctrine__constitutional_fidelity_reading, federal_judiciary).
narrative_ontology:constraint_beneficiary(qualified_immunity_doctrine__constitutional_fidelity_reading, appellate_courts_shaping_precedent).
narrative_ontology:constraint_victim(qualified_immunity_doctrine__constitutional_fidelity_reading, section_1983_plaintiffs).
narrative_ontology:constraint_victim(qualified_immunity_doctrine__constitutional_fidelity_reading, law_enforcement_officers_denied_statutory_text).
narrative_ontology:constraint_victim(qualified_immunity_doctrine__constitutional_fidelity_reading, congress_as_institutional_lawmaker).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(qualified_immunity_doctrine__constitutional_fidelity_reading, law_enforcement_officers_denied_statutory_text).
narrative_ontology:constraint_beneficiary(qualified_immunity_doctrine__constitutional_fidelity_reading, municipal_and_state_governments).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Created and repeatedly re-engineered the 'clearly established law' standard through case law never authorized by the text of 42 U.S.C. § 1983, which contains no immunity language. Controls how the doctrine is applied, narrowed, or expanded in each new opinion, and answers to no external check on this lawmaking function short of a statutory override Congress has not passed.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__constitutional_fidelity_reading, federal_judiciary, agenda_setter,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(qualified_immunity_doctrine__constitutional_fidelity_reading, federal_judiciary, beneficiary).

% Bring constitutional tort claims against officers and are dismissed at summary judgment because no prior case with materially identical facts is found, regardless of whether a constitutional violation occurred. Because the doctrine has no textual basis, plaintiffs have no legislative body to petition for a fix that would reliably survive judicial reinterpretation — the same body that created the doctrine adjudicates challenges to it.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__constitutional_fidelity_reading, section_1983_plaintiffs, payer,
    powerless, biographical, trapped, national).

% Receive de facto protection from suit under the doctrine but have no statutory guarantee of that protection — it can be narrowed or eliminated by the same judicial process that manufactured it, with no legislative vote required either way. Their reliance interest rests on judge-made law rather than a legislative bargain, leaving their position as legally unstable as it is currently favorable.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__constitutional_fidelity_reading, law_enforcement_officers_denied_statutory_text, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(qualified_immunity_doctrine__constitutional_fidelity_reading, law_enforcement_officers_denied_statutory_text, beneficiary).

% Wrote § 1983 with no immunity provision and has never enacted qualified immunity into statute despite decades of opportunity. Its lawmaking authority over the scope of civil rights remedies has been effectively displaced by judicial doctrine it did not author and has only occasionally attempted, unsuccessfully, to override.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__constitutional_fidelity_reading, congress_as_institutional_lawmaker, excluded,
    institutional, generational, constrained, national).

% Benefit indirectly from reduced payout exposure and litigation costs when officer immunity holds, though they are not the doctrine's authors and did not lobby it into existence through legislative process; their gain is incidental to the judiciary's institutional lawmaking rather than a bargained-for exchange.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__constitutional_fidelity_reading, municipal_and_state_governments, beneficiary,
    organized, generational, mobile, national).

% Document the doctrine's absence from the 1871 statutory text and its purely judicial origin in 1967's Pierson v. Ray and its progeny, treating the legitimacy question as separate from and prior to any policy assessment of the doctrine's effects.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__constitutional_fidelity_reading, constitutional_originalist_scholars, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(qualified_immunity_doctrine__constitutional_fidelity_reading, federal_judiciary).
narrative_ontology:fixing_cost_class(qualified_immunity_doctrine__constitutional_fidelity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None recognized under this reading at the level of legitimate lawmaking: whatever administrative convenience the doctrine provides for courts managing dockets is not a coordination function courts were authorized to create by inventing an immunity absent from the governing statute.
% TRANSFER_FUNCTION: Moves adjudicative and lawmaking authority from Congress, the textually authorized body, to the federal judiciary, which exercises it through evolving case law; downstream, this shifts litigation risk and remedy availability from officers and municipalities to injured plaintiffs, but the primary transfer under this reading is institutional — power from legislature to courts.
% ABSENT_VOICES: Congress, as the body whose statutory text was overridden by judicial gloss it never enacted, is structurally absent from the doctrine's ongoing maintenance and revision; its occasional reform bills have died without a floor vote, leaving the judiciary as sole author and sole editor of the rule.
% DISAPPEARANCE_RATIONALE: If the doctrine were abolished by judicial fiat or superseded by statute tomorrow, § 1983 litigation would proceed on the text as written — plaintiffs would need to prove a constitutional violation and causation, nothing more. Damages exposure for officers and municipalities would rise, but the legal framework governing civil rights suits would simply be the one Congress actually wrote; the world of statutory civil rights enforcement would functionally reorganize around the text.
% FOUNDING_PROBLEM: The doctrine was constructed to shield good-faith officials from suits over constitutional standards that were unsettled or unknowable at the time of the conduct — an equitable concern about fair notice, addressed by judicial invention rather than by asking Congress to amend the statute.
% FOUNDING_PROBLEM_CORROBORATION: Originalist and textualist legal scholars outside the judiciary — including sitting appellate judges writing in dissent (e.g., Judge Willett's critiques) — attest that the doctrine's textual foundation was never established and that its 'fair notice' justification has drifted into a categorical shield disconnected from the original equitable concern; this corroboration comes from within the legal profession but explicitly outside the judiciary's own self-justifying opinions, and includes voices with no stake in expanding either plaintiff or officer recovery.
narrative_ontology:disappearance_verdict(qualified_immunity_doctrine__constitutional_fidelity_reading, world_rearranges).
narrative_ontology:founding_problem_status(qualified_immunity_doctrine__constitutional_fidelity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qualified_immunity_doctrine__constitutional_fidelity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(qualified_immunity_doctrine__constitutional_fidelity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(qualified_immunity_doctrine__constitutional_fidelity_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(qualified_immunity_doctrine__constitutional_fidelity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(qualified_immunity_doctrine__constitutional_fidelity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(qualified_immunity_doctrine__constitutional_fidelity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) and theater_ratio (0.71) are authored moderate-to-high under this reading not because officers profit from the doctrine (the accountability_void reading's claim) but because the judiciary extracts institutional lawmaking authority it was never granted, and an increasing share of appellate activity is spent elaborating a 'clearly established law' jurisprudence that performs legal reasoning while functioning as a self-authored gatekeeping mechanism insulated from legislative correction. Suppression (0.62) reflects that the doctrine forecloses the ordinary corrective mechanism (legislative override) by being framed as constitutional common law that only the courts that made it can unmake. Resistance (0.72) is high because originalist scholars, some sitting judges, and civil rights litigants have persistently challenged the doctrine's legitimacy on textual grounds for decades without success.
 *
 * PERSPECTIVAL GAP:
 *   From the judiciary's own seat, the doctrine is continuous common-law elaboration well within Article III's traditional scope. From the constitutional_fidelity reading's seat, that self-assessment is exactly the problem: an institution cannot legitimately certify its own lawmaking authority. The engine should compute institutional-power-holder seats (judiciary) as structurally distinct from powerless-payer seats (plaintiffs) even though both nominally operate within 'the same' § 1983 litigation system.
 *
 * DIRECTIONALITY LOGIC:
 *   The federal judiciary sits as the primary beneficiary because the doctrine's persistence is itself an exercise and vindication of judicial lawmaking authority — the institution that created the rule is also the sole body empowered to revise or abolish it, an unusually complete self-referential loop. Section 1983 plaintiffs and, under this specific reading, Congress itself are treated as bearing costs: plaintiffs lose access to a remedy the statutory text does not withhold, and Congress loses control over the scope of a statute it wrote. Officers are cross-listed as payer/beneficiary because they receive a real practical shield but hold no durable legal entitlement to it — their position is as vulnerable to unilateral judicial revision as plaintiffs' claims are to unilateral judicial denial.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resists collapsing the doctrine into either pure coordination (the protective_scaffold reading) or pure extraction-for-officers (the accountability_void reading) by identifying a third failure mode: institutional overreach unmoored from either function. The founding_problem (fair notice for officials facing unsettled constitutional standards) may have been genuinely live in 1967, but this reading holds that even a live founding problem does not license judicial fabrication of a remedial rule absent statutory text — legitimacy and utility are treated as separate axes, preventing the mandatrophy question from being resolved merely by asking whether the doctrine currently 'still does something useful.'
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_dependent_epsilon,
    'Is qualified immunity better modeled as (a) a genuine coordination mechanism protecting good-faith official action (protective_scaffold_reading), (b) a targeted extraction mechanism benefiting officers and municipalities at plaintiffs'' expense (accountability_void_reading), or (c) an ultra vires judicial lawmaking exercise whose primary beneficiary is the judiciary''s own institutional authority, independent of distributive outcome (this reading)?',
    'These are not competing measurements of one constraint but three structurally distinct constraints sharing a kernel (the doctrine''s text and history). Each reading is authored as its own story with its own epsilon and beneficiary/victim set; this omega records that the choice of reading is a framing decision, not an empirical one resolvable within any single story.',
    'Adopting the constitutional_fidelity reading routes the entire doctrine into ''illegitimate'' regardless of policy performance, denies both officers and victims a legitimate legal framework (since the rule itself lacks authorization), and names the judiciary rather than law enforcement as primary beneficiary — a materially different classification outcome than either sibling reading would produce for the identical underlying case law.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_dependent_epsilon, conceptual, 'Multiple non-reconcilable readings of the same doctrinal kernel, each yielding a different beneficiary set and type.').

omega_variable(
    institutional_authority_vs_outcome_severability,
    'Can the legitimacy of the doctrine''s SOURCE (judicial fabrication absent statutory text) be assessed independently of its DISTRIBUTIVE outcome (who wins and loses in litigation), or does the source question collapse into the outcome question once one asks why the source defect matters?',
    'Comparative analysis of judicially-fabricated doctrines that produced broadly favorable outcomes (e.g., some due process incorporation doctrine) versus this one, to test whether ''illegitimate regardless of outcome'' is coherently maintainable or whether outcome assessment inevitably reenters the legitimacy analysis.',
    'If the source/outcome distinction cannot be maintained, this reading collapses into a version of the accountability_void_reading (illegitimacy demonstrated BY the bad outcome) or the protective_scaffold_reading (legitimacy demonstrated BY the good outcome), undermining this reading''s claimed independence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_authority_vs_outcome_severability, conceptual, 'Whether institutional-authority illegitimacy can be assessed independently of policy consequence.').

omega_variable(
    congressional_acquiescence_as_ratification,
    'Does Congress''s five-decade failure to statutorily override qualified immunity, despite clear opportunity and repeated proposed legislation, constitute a form of implicit ratification that undermines the ''ultra vires, never authorized'' framing?',
    'Legislative history analysis of failed reform bills (e.g., the George Floyd Justice in Policing Act''s qualified immunity provisions) to determine whether failure to pass reflects acquiescence to the judicial doctrine or failure for unrelated political reasons (filibuster dynamics, unrelated bill provisions, lobbying).',
    'If failure to override reflects genuine acquiescence, the doctrine''s legitimacy deficit narrows since the coordinate branch has had continuous opportunity to correct it and declined; if failure reflects unrelated procedural obstruction, the illegitimacy claim under this reading is undiminished.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(congressional_acquiescence_as_ratification, empirical, 'Whether congressional inaction operates as tacit ratification of judicially fabricated doctrine.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qualified_immunity_doctrine__constitutional_fidelity_reading, 0, 55).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qual_tr_t0, qualified_immunity_doctrine__constitutional_fidelity_reading, theater_ratio, 0, 0.4).
narrative_ontology:measurement(qual_tr_t11, qualified_immunity_doctrine__constitutional_fidelity_reading, theater_ratio, 11, 0.48).
narrative_ontology:measurement(qual_tr_t22, qualified_immunity_doctrine__constitutional_fidelity_reading, theater_ratio, 22, 0.55).
narrative_ontology:measurement(qual_tr_t33, qualified_immunity_doctrine__constitutional_fidelity_reading, theater_ratio, 33, 0.62).
narrative_ontology:measurement(qual_tr_t44, qualified_immunity_doctrine__constitutional_fidelity_reading, theater_ratio, 44, 0.67).
narrative_ontology:measurement(qual_tr_t55, qualified_immunity_doctrine__constitutional_fidelity_reading, theater_ratio, 55, 0.71).

% Extraction over time
narrative_ontology:measurement(qual_be_t0, qualified_immunity_doctrine__constitutional_fidelity_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(qual_be_t11, qualified_immunity_doctrine__constitutional_fidelity_reading, base_extractiveness, 11, 0.42).
narrative_ontology:measurement(qual_be_t22, qualified_immunity_doctrine__constitutional_fidelity_reading, base_extractiveness, 22, 0.48).
narrative_ontology:measurement(qual_be_t33, qualified_immunity_doctrine__constitutional_fidelity_reading, base_extractiveness, 33, 0.53).
narrative_ontology:measurement(qual_be_t44, qualified_immunity_doctrine__constitutional_fidelity_reading, base_extractiveness, 44, 0.56).
narrative_ontology:measurement(qual_be_t55, qualified_immunity_doctrine__constitutional_fidelity_reading, base_extractiveness, 55, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(qual_su_t0, qualified_immunity_doctrine__constitutional_fidelity_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(qual_su_t11, qualified_immunity_doctrine__constitutional_fidelity_reading, suppression_requirement, 11, 0.38).
narrative_ontology:measurement(qual_su_t22, qualified_immunity_doctrine__constitutional_fidelity_reading, suppression_requirement, 22, 0.46).
narrative_ontology:measurement(qual_su_t33, qualified_immunity_doctrine__constitutional_fidelity_reading, suppression_requirement, 33, 0.52).
narrative_ontology:measurement(qual_su_t44, qualified_immunity_doctrine__constitutional_fidelity_reading, suppression_requirement, 44, 0.58).
narrative_ontology:measurement(qual_su_t55, qualified_immunity_doctrine__constitutional_fidelity_reading, suppression_requirement, 55, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qualified_immunity_doctrine__constitutional_fidelity_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(qualified_immunity_doctrine__constitutional_fidelity_reading, 0.05).
narrative_ontology:affects_constraint(qualified_immunity_doctrine__constitutional_fidelity_reading, qualified_immunity_doctrine__accountability_void_reading).
narrative_ontology:affects_constraint(qualified_immunity_doctrine__constitutional_fidelity_reading, qualified_immunity_doctrine__protective_scaffold_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked constraints sharing the qualified_immunity_doctrine kernel. constitutional_fidelity_reading (this file) locates the defect in institutional lawmaking authority and names the judiciary as beneficiary; accountability_void_reading locates the defect in distributive extraction and names officers/municipalities as beneficiaries; protective_scaffold_reading treats the arrangement as legitimate coordination with officers as intended beneficiaries of a genuine policy good. All three read the identical case law and statutory text; they differ in what they treat as the operative wrong, and therefore in claimed_type, beneficiary set, and epsilon. Per the epsilon-invariance principle, this divergence is why the readings are authored as separate stories rather than as one story with a measurement parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(qualified_immunity_doctrine__constitutional_fidelity_reading, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
