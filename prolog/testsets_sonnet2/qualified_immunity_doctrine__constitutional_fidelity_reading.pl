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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   human_readable: Qualified Immunity as Ultra Vires Judicial Fabrication (Constitutional Fidelity Reading)
 *   domain: constitutional_law/civil_rights/judicial_authority
 *
 * SUMMARY:
 *   This story instantiates the constitutional_fidelity_reading of the
 *   qualified immunity kernel: the doctrine is illegitimate not because of
 *   who it protects or who it fails, but because the federal judiciary
 *   invented a defense to a federal statute that contains no such defense in
 *   its text, and has continued to build out and calibrate that invented
 *   defense for over five decades without legislative authorization. This
 *   reading brackets the policy question entirely (whether immunity produces
 *   good or bad law-enforcement outcomes) and evaluates only the
 *   institutional-authority question: did anyone with the power to create
 *   this rule actually create it? Under this reading, the answer is no, and
 *   the doctrine is a Snare not because officers are unfairly shielded or
 *   victims unfairly denied, but because the shielding and denial both flow
 *   from an ultra vires act of judicial rule-making that displaced Congress's
 *   own textual choice. This reading sits alongside, but is structurally
 *   distinct from, the accountability_void_reading (which treats immunity as
 *   extraction from victims specifically) and the protective_scaffold_reading
 *   (which treats the same doctrine as legitimate coordination). All three
 *   share the same doctrinal object but author different beneficiary sets,
 *   different ε referents, and different classifications because they are
 *   different constraints, not different measurements of one constraint.
 *
 * KEY AGENTS:
 *   - federal_judiciary: institutional agenda-setter and structural beneficiary — created and administers the doctrine without statutory authorization
 *   - section_1983_plaintiffs: powerless, trapped payers — denied a textually-authorized remedy by a judge-made overlay
 *   - law_enforcement_officers_denied_genuine_legal_clarity: moderate-power payer/beneficiary — receives practical protection but not a legitimate, stable legal framework
 *   - congress: institutional actor structurally excluded from the doctrine's actual construction despite holding the constitutional authority to define the statute
 *   - legal_historians_and_textualist_scholars: analytical observers attesting to the doctrine's textual absence from the 1871 statute
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qualified_immunity_doctrine__constitutional_fidelity_reading, 0.58).
domain_priors:suppression_score(qualified_immunity_doctrine__constitutional_fidelity_reading, 0.72).
domain_priors:theater_ratio(qualified_immunity_doctrine__constitutional_fidelity_reading, 0.61).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qualified_immunity_doctrine__constitutional_fidelity_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(qualified_immunity_doctrine__constitutional_fidelity_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(qualified_immunity_doctrine__constitutional_fidelity_reading, theater_ratio, 0.61).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qualified_immunity_doctrine__constitutional_fidelity_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(qualified_immunity_doctrine__constitutional_fidelity_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qualified_immunity_doctrine__constitutional_fidelity_reading, snare).
narrative_ontology:human_readable(qualified_immunity_doctrine__constitutional_fidelity_reading, "Qualified Immunity as Ultra Vires Judicial Fabrication (Constitutional Fidelity Reading)").
narrative_ontology:topic_domain(qualified_immunity_doctrine__constitutional_fidelity_reading, "constitutional_law/civil_rights/judicial_authority").

domain_priors:requires_active_enforcement(qualified_immunity_doctrine__constitutional_fidelity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qualified_immunity_doctrine__constitutional_fidelity_reading, '22c8c7ed-d8ab-4094-bc7e-43ff07b65b1e').
narrative_ontology:cs_kernel_codification('22c8c7ed-d8ab-4094-bc7e-43ff07b65b1e', distributed).
narrative_ontology:cs_authority_grounding('22c8c7ed-d8ab-4094-bc7e-43ff07b65b1e', extraction).
narrative_ontology:cs_interpretation_layer_present('22c8c7ed-d8ab-4094-bc7e-43ff07b65b1e').
narrative_ontology:cs_reading_relation('22c8c7ed-d8ab-4094-bc7e-43ff07b65b1e', qualified_immunity_doctrine__accountability_void_reading, coexists_with).
narrative_ontology:cs_reading_relation('22c8c7ed-d8ab-4094-bc7e-43ff07b65b1e', qualified_immunity_doctrine__protective_scaffold_reading, forecloses).
narrative_ontology:cs_axiom('22c8c7ed-d8ab-4094-bc7e-43ff07b65b1e', foundational, judicial_lawmaking_absent_textual_authorization_is_illegitimate).
narrative_ontology:cs_axiom_status(judicial_lawmaking_absent_textual_authorization_is_illegitimate, holdable).
narrative_ontology:cs_axiom_grounding('22c8c7ed-d8ab-4094-bc7e-43ff07b65b1e', judicial_lawmaking_absent_textual_authorization_is_illegitimate, conventional).
narrative_ontology:cs_axiom('22c8c7ed-d8ab-4094-bc7e-43ff07b65b1e', foundational, policy_benefit_cannot_cure_authorization_defect).
narrative_ontology:cs_axiom_status(policy_benefit_cannot_cure_authorization_defect, holdable).
narrative_ontology:cs_axiom_grounding('22c8c7ed-d8ab-4094-bc7e-43ff07b65b1e', policy_benefit_cannot_cure_authorization_defect, deontological).
narrative_ontology:cs_reference_frame('22c8c7ed-d8ab-4094-bc7e-43ff07b65b1e', textualist_statutory_supremacy).
narrative_ontology:cs_drift_state('22c8c7ed-d8ab-4094-bc7e-43ff07b65b1e', post_harlow_clearly_established_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('22c8c7ed-d8ab-4094-bc7e-43ff07b65b1e', '').
narrative_ontology:cs_kernel_id(qualified_immunity_doctrine__constitutional_fidelity_reading, qualified_immunity_doctrine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(qualified_immunity_doctrine__constitutional_fidelity_reading, federal_judiciary).
narrative_ontology:constraint_beneficiary(qualified_immunity_doctrine__constitutional_fidelity_reading, supreme_court_institutional_authority).
narrative_ontology:constraint_victim(qualified_immunity_doctrine__constitutional_fidelity_reading, section_1983_plaintiffs).
narrative_ontology:constraint_victim(qualified_immunity_doctrine__constitutional_fidelity_reading, constitutional_text_and_statutory_scheme).
narrative_ontology:constraint_victim(qualified_immunity_doctrine__constitutional_fidelity_reading, law_enforcement_officers_denied_genuine_legal_clarity).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(qualified_immunity_doctrine__constitutional_fidelity_reading, law_enforcement_officers_denied_genuine_legal_clarity).
narrative_ontology:constraint_vindicates(qualified_immunity_doctrine__constitutional_fidelity_reading, separation_of_powers_doctrine).
narrative_ontology:constraint_vindicates(qualified_immunity_doctrine__constitutional_fidelity_reading, textualist_interpretive_method).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Created the 'clearly established law' standard through a line of cases (Pierson v. Ray, Harlow v. Fitzgerald, and successors) without textual grounding in 42 U.S.C. Section 1983's statutory language, which contains no immunity provision. Continues to administer, refine, and extend the doctrine case by case, effectively legislating the scope of a federal remedy Congress wrote to be broad and unqualified. Retains full discretion to narrow, expand, or abolish the doctrine and bears none of its costs directly.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__constitutional_fidelity_reading, federal_judiciary, agenda_setter,
    institutional, civilizational, analytical, national).
narrative_ontology:stakeholder_secondary_role(qualified_immunity_doctrine__constitutional_fidelity_reading, federal_judiciary, beneficiary).

% Bring civil rights claims under a statute written by Congress to provide a remedy for constitutional violations by state actors, and are denied that remedy not because the statute contains an exception but because judges added one. Cannot appeal to the statutory text for relief because the doctrine operates as judge-made overlay; their only path is petitioning the same judiciary that fabricated the barrier to reconsider it.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__constitutional_fidelity_reading, section_1983_plaintiffs, payer,
    powerless, biographical, trapped, national).

% Receive de facto immunity from most suits but not genuine legal clarity about the boundaries of lawful conduct, because a doctrine built on ad hoc 'clearly established' findings rather than settled statutory rules produces unpredictable case-by-case outcomes. Individual officers benefit financially and litigation-wise in the near term but the profession as a whole is denied a legitimate, legislatively-grounded framework that could have been designed to serve both accountability and operational certainty.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__constitutional_fidelity_reading, law_enforcement_officers_denied_genuine_legal_clarity, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(qualified_immunity_doctrine__constitutional_fidelity_reading, law_enforcement_officers_denied_genuine_legal_clarity, beneficiary).

% Wrote Section 1983 in 1871 with no immunity language and has not authorized the judicially-created defense. Has the formal power to codify, modify, or abolish qualified immunity by statute (as some bills have proposed) but has not exercised it, leaving a judicially-authored regime to govern a statute Congress alone is constitutionally empowered to define. Its silence is read by this reading as passivity in the face of usurpation, not ratification.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__constitutional_fidelity_reading, congress, excluded,
    institutional, generational, constrained, national).

% The statutory scheme itself is treated as bearing a cost: the text of Section 1983 and the constitutional separation of legislative and judicial power are diminished each time the doctrine's application diverges from what the enacted text authorizes. Not an actor, but named for completeness as the entity whose integrity this reading holds is at stake.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__constitutional_fidelity_reading, constitutional_text_and_statutory_scheme, payer,
    analytical, civilizational, analytical, national).
narrative_ontology:stakeholder_non_agent(qualified_immunity_doctrine__constitutional_fidelity_reading, constitutional_text_and_statutory_scheme).

% Document the doctrine's absence from the 1871 statutory text and its post-hoc judicial construction across the twentieth century, providing the historical record this reading relies on. Have no stake in the outcome of any particular case but attest to the doctrine's textual and historical illegitimacy independent of whether its policy effects are good or bad.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__constitutional_fidelity_reading, legal_historians_and_textualist_scholars, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(qualified_immunity_doctrine__constitutional_fidelity_reading, federal_judiciary).
narrative_ontology:fixing_cost_class(qualified_immunity_doctrine__constitutional_fidelity_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None cognizable under this reading at the level of legitimate authority: whatever practical coordination the doctrine achieves (screening litigation, giving officers predictability) is achieved through a mechanism the judiciary was not authorized to create, so the coordination-function question is answered negatively as a matter of institutional legitimacy rather than assessed for its policy value.
% TRANSFER_FUNCTION: Moves adjudicative authority over the scope of a congressionally-created remedy from Congress (the constitutionally proper author of federal statutory rights) to the federal judiciary, and correspondingly moves litigation risk and case outcomes away from plaintiffs and toward officers, but the primary transfer this reading names is the transfer of law-making authority itself.
% ABSENT_VOICES: Congress is formally present as an institution but functionally absent from the doctrine's actual construction — no statute was passed authorizing immunity, so the body constitutionally empowered to define the scope of Section 1983 never spoke on this specific question; the doctrine developed entirely within the judiciary's own case law without legislative deliberation or a floor vote on the immunity question itself.
% DISAPPEARANCE_RATIONALE: If the judicially-created doctrine were abolished by the judiciary that made it (or superseded by statute), Section 1983 would revert to something closer to its enacted text: a remedy for deprivation of constitutional rights without a judge-made immunity screen. Litigation patterns, settlement dynamics, and municipal liability insurance markets would all reorganize around the statute as written; the fact that so much reorganizes confirms the doctrine is doing real structural work despite lacking textual authorization.
% FOUNDING_PROBLEM: Officers and municipalities argued in the 1960s-80s that unqualified liability under Section 1983 would deter good-faith law enforcement and expose officials to liability for conduct that was lawful when performed; the Supreme Court fashioned qualified immunity to address that policy concern.
% FOUNDING_PROBLEM_CORROBORATION: Originalist and textualist legal scholars (including some sitting appellate judges, e.g. in dissents and concurrences questioning the doctrine's textual basis) attest from outside law enforcement's beneficiary interest that no statutory or constitutional text authorizes the doctrine regardless of whether the underlying policy concern was real; this reading treats that scholarly and judicial dissent as corroboration that the founding problem, even if once live, was addressed through an institutionally illegitimate mechanism.
narrative_ontology:disappearance_verdict(qualified_immunity_doctrine__constitutional_fidelity_reading, world_rearranges).
narrative_ontology:founding_problem_status(qualified_immunity_doctrine__constitutional_fidelity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qualified_immunity_doctrine__constitutional_fidelity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
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
 *   Extractiveness (0.58) is authored moderate-high: this reading holds that the judiciary extracts institutional law-making authority from Congress, not merely that officers extract impunity from victims — the extraction is jurisdictional/structural, which this reading treats as real but harder to quantify in dollar or case-outcome terms than the accountability_void_reading's ε would be. Suppression (0.72) is authored high because the doctrine forecloses the ordinary avenue for correcting judicial overreach — plaintiffs cannot challenge the doctrine's legitimacy within the same litigation it governs, and Congress has not exercised its remedial power despite having it. Theater ratio (0.61) is authored high and rising over the measured interval: as the doctrine's 'clearly established law' test has grown more elaborate and precedent-specific across the 1967-2024 interval, an increasing share of judicial effort goes into the performance of principled line-drawing (distinguishing near-identical facts as 'clearly established' or not) rather than function tied to any textual standard, which is exactly what an ultra vires doctrine looks like as it entrenches. Accessibility collapse (0.68) reflects that within existing case law, alternatives to the doctrine are nearly foreclosed for litigants and lower courts, though not so completely as a true mountain — the doctrine remains, in this reading's own terms, reversible by the body that made it or by Congress.
 *
 * DIRECTIONALITY LOGIC:
 *   The federal judiciary is the structural beneficiary under this reading: not because judges collect settlement dollars, but because the doctrine's persistence vindicates and expands judicial law-making authority relative to Congress, an institutional-power benefit distinct from any financial one. Section_1983_plaintiffs and the statutory scheme itself are the targets: plaintiffs bear the direct litigation cost, and the statutory scheme bears the more abstract but, under this reading, equally real cost of having its enacted text displaced by non-textual judicial gloss. Officers occupy an unusual position — they are named as payers because this reading holds that even they are denied something (a legitimate, legislatively-considered framework) even though they benefit financially in the near term; this dual role captures that the reading's objection is to the *mechanism*, not to who wins under it.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (deterring frivolous or unpredictable Section 1983 liability against good-faith officers) may or may not remain live as a policy matter, but this reading holds that mandatrophy analysis is almost beside the point: even if the founding problem is fully live and fully justifies some immunity-like rule, the judiciary was never the body authorized to create that rule via case law rather than Congress via statute. This is the reading's central move — it refuses to let a sympathetic policy rationale launder an illegitimate institutional mechanism. Classification as snare (not tangled_rope) follows because this reading does not credit the doctrine with a genuine coordination function it was authorized to perform; the coordination story is, under this reading, cover for an institutional-authority transfer.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textualist_illegitimacy_vs_policy_outcome_independence,
    'Can a judicially-created legal doctrine be simultaneously illegitimate in its institutional origin and beneficial (or harmful) in its policy effects, and does this reading''s framework have any mechanism for weighing those independently rather than treating illegitimacy as dispositive?',
    'Comparative institutional analysis of other judicially-created doctrines (e.g., the exclusionary rule, Miranda warnings) that share a similar authorization gap, tracking whether subsequent congressional codification or judicial abandonment tracked policy assessment or pure legitimacy assessment.',
    'If legitimacy and policy outcome are shown to be entangled in practice (Congress ratifies doctrines it judges beneficial regardless of origin), this reading''s claim of illegitimacy ''regardless of policy outcomes'' becomes harder to sustain as a standalone classificatory ground; if they are shown to be separable, the reading is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textualist_illegitimacy_vs_policy_outcome_independence, conceptual, 'Whether institutional illegitimacy and policy benefit can be coherently separated as independent axes.').

omega_variable(
    congressional_silence_as_ratification_or_passivity,
    'Does Congress''s decades-long failure to codify or abolish qualified immunity by statute constitute implicit ratification of the doctrine, or mere institutional passivity that does not cure the original authorization defect?',
    'Legislative history analysis of failed and pending qualified immunity reform bills, floor statements, and committee reports assessing whether Congress has treated the doctrine as settled law it endorses or as a judicial usurpation it has simply failed to correct.',
    'If silence constitutes ratification, the beneficiary set shifts to include Congress alongside the judiciary and the illegitimacy claim weakens considerably; if silence is mere passivity, the constitutional_fidelity_reading''s core claim is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(congressional_silence_as_ratification_or_passivity, empirical, 'Whether legislative inaction cures a judicially-created authorization gap.').

omega_variable(
    ambiguous_framing_kernel_or_authority_layer,
    'Is the correct kernel-level object for this reading ''qualified immunity as a legal rule'' or ''the judiciary''s authority to create common-law-style defenses to federal statutes'' — the doctrine itself, or the interpretive methodology that licensed it?',
    'Trace whether abolishing qualified immunity specifically (by statute or by Court reversal) would resolve this reading''s objection, or whether the objection would simply relocate to the next judicially-created gloss on a different statute, which would indicate the true kernel is the interpretive methodology rather than this specific doctrine.',
    'If the true kernel is the methodology, this story is one instance of a broader family and should be linked to sibling stories about other judicially-created doctrines (e.g., implied causes of action, sovereign immunity extensions) rather than treated as sui generis; if the doctrine itself is the kernel, this story stands alone as authored.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ambiguous_framing_kernel_or_authority_layer, conceptual, 'Whether the contested kernel is the specific doctrine or the underlying interpretive authority question.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qualified_immunity_doctrine__constitutional_fidelity_reading, 1967, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qual_tr_t1967, qualified_immunity_doctrine__constitutional_fidelity_reading, theater_ratio, 1967, 0.2).
narrative_ontology:measurement_basis(qual_tr_t1967, observed).
narrative_ontology:measurement(qual_tr_t1982, qualified_immunity_doctrine__constitutional_fidelity_reading, theater_ratio, 1982, 0.32).
narrative_ontology:measurement_basis(qual_tr_t1982, observed).
narrative_ontology:measurement(qual_tr_t1995, qualified_immunity_doctrine__constitutional_fidelity_reading, theater_ratio, 1995, 0.42).
narrative_ontology:measurement_basis(qual_tr_t1995, observed).
narrative_ontology:measurement(qual_tr_t2009, qualified_immunity_doctrine__constitutional_fidelity_reading, theater_ratio, 2009, 0.51).
narrative_ontology:measurement_basis(qual_tr_t2009, observed).
narrative_ontology:measurement(qual_tr_t2017, qualified_immunity_doctrine__constitutional_fidelity_reading, theater_ratio, 2017, 0.57).
narrative_ontology:measurement_basis(qual_tr_t2017, observed).
narrative_ontology:measurement(qual_tr_t2024, qualified_immunity_doctrine__constitutional_fidelity_reading, theater_ratio, 2024, 0.61).
narrative_ontology:measurement_basis(qual_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(qual_be_t1967, qualified_immunity_doctrine__constitutional_fidelity_reading, base_extractiveness, 1967, 0.25).
narrative_ontology:measurement_basis(qual_be_t1967, observed).
narrative_ontology:measurement(qual_be_t1982, qualified_immunity_doctrine__constitutional_fidelity_reading, base_extractiveness, 1982, 0.38).
narrative_ontology:measurement_basis(qual_be_t1982, observed).
narrative_ontology:measurement(qual_be_t1995, qualified_immunity_doctrine__constitutional_fidelity_reading, base_extractiveness, 1995, 0.46).
narrative_ontology:measurement_basis(qual_be_t1995, observed).
narrative_ontology:measurement(qual_be_t2009, qualified_immunity_doctrine__constitutional_fidelity_reading, base_extractiveness, 2009, 0.53).
narrative_ontology:measurement_basis(qual_be_t2009, observed).
narrative_ontology:measurement(qual_be_t2017, qualified_immunity_doctrine__constitutional_fidelity_reading, base_extractiveness, 2017, 0.56).
narrative_ontology:measurement_basis(qual_be_t2017, observed).
narrative_ontology:measurement(qual_be_t2024, qualified_immunity_doctrine__constitutional_fidelity_reading, base_extractiveness, 2024, 0.58).
narrative_ontology:measurement_basis(qual_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(qual_su_t1967, qualified_immunity_doctrine__constitutional_fidelity_reading, suppression_requirement, 1967, 0.4).
narrative_ontology:measurement_basis(qual_su_t1967, observed).
narrative_ontology:measurement(qual_su_t1982, qualified_immunity_doctrine__constitutional_fidelity_reading, suppression_requirement, 1982, 0.5).
narrative_ontology:measurement_basis(qual_su_t1982, observed).
narrative_ontology:measurement(qual_su_t1995, qualified_immunity_doctrine__constitutional_fidelity_reading, suppression_requirement, 1995, 0.58).
narrative_ontology:measurement_basis(qual_su_t1995, observed).
narrative_ontology:measurement(qual_su_t2009, qualified_immunity_doctrine__constitutional_fidelity_reading, suppression_requirement, 2009, 0.65).
narrative_ontology:measurement_basis(qual_su_t2009, observed).
narrative_ontology:measurement(qual_su_t2017, qualified_immunity_doctrine__constitutional_fidelity_reading, suppression_requirement, 2017, 0.69).
narrative_ontology:measurement_basis(qual_su_t2017, observed).
narrative_ontology:measurement(qual_su_t2024, qualified_immunity_doctrine__constitutional_fidelity_reading, suppression_requirement, 2024, 0.72).
narrative_ontology:measurement_basis(qual_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qualified_immunity_doctrine__constitutional_fidelity_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(qualified_immunity_doctrine__constitutional_fidelity_reading, qualified_immunity_doctrine__accountability_void_reading).
narrative_ontology:affects_constraint(qualified_immunity_doctrine__constitutional_fidelity_reading, qualified_immunity_doctrine__protective_scaffold_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the qualified_immunity_doctrine kernel. The accountability_void_reading treats the doctrine as victim-centered extraction (concrete ε, high suppression, snare/tangled_rope depending on enforcement framing). The protective_scaffold_reading treats it as legitimate coordination (low-moderate ε, genuine beneficiary/coordination structure, rope or scaffold). This constitutional_fidelity_reading differs from both by locating its ε referent in institutional authorization rather than distributive outcome — its beneficiary set (the judiciary, as an institution gaining law-making latitude) is structurally distinct from either sibling's beneficiary set (officers/municipalities in the scaffold reading; no legitimate beneficiary in the accountability_void reading). All three stories share the doctrinal surface but are authored as three separate constraints per the ε-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
