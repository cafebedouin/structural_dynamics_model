% ============================================================================
% CONSTRAINT STORY: equality_clause_scope__expansive_universalist
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equality_clause_scope__expansive_universalist, []).

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
 *   constraint_id: equality_clause_scope__expansive_universalist
 *   human_readable: Equality Clause — Expansive Universalist Reading
 *   domain: constitutional_law/political_philosophy/civil_rights
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested
 *   equality_clause_scope kernel: the expansive universalist reading, which
 *   holds that the equality principle is self-evidently universal and that
 *   historical exclusions (slavery, coverture, property qualifications) were
 *   hypocritical departures from the text's true meaning rather than
 *   definitional limits on its scope. Under this reading, courts are licensed
 *   to extend equal protection to classes and claims the framers did not
 *   contemplate, using a comparatively low threshold for finding that an
 *   exclusion is illegitimate rather than intended. This is a clean, single-ε
 *   constraint: it does not average across the restrictive originalist or
 *   progressive textualist siblings, and it does not describe the contest
 *   between them. Those are separate constraints, linked by network edges.
 *
 * KEY AGENTS:
 *   - historically_excluded_groups: primary beneficiary (powerless/trapped) — depends on the reading for legal recognition
 *   - civil_rights_litigants: organized beneficiary/agenda_setter (organized/constrained) — drives doctrinal expansion through litigation
 *   - rights_expansion_judiciary: agenda_setter (institutional/analytical) — administers the interpretive discretion the reading depends on
 *   - originalist_legal_traditionalists: payer (organized/constrained) — bears reputational and doctrinal cost of being cast as complicit with exclusion
 *   - settled_expectation_holders: payer (moderate/trapped) — bears retroactive disruption of arrangements built on narrower readings
 *   - constitutional_theorists: analytical observer — studies the reading against its siblings without being bound by outcomes
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equality_clause_scope__expansive_universalist, 0.42).
domain_priors:suppression_score(equality_clause_scope__expansive_universalist, 0.38).
domain_priors:theater_ratio(equality_clause_scope__expansive_universalist, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equality_clause_scope__expansive_universalist, extractiveness, 0.42).
narrative_ontology:constraint_metric(equality_clause_scope__expansive_universalist, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(equality_clause_scope__expansive_universalist, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equality_clause_scope__expansive_universalist, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(equality_clause_scope__expansive_universalist, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equality_clause_scope__expansive_universalist, tangled_rope).
narrative_ontology:human_readable(equality_clause_scope__expansive_universalist, "Equality Clause — Expansive Universalist Reading").
narrative_ontology:topic_domain(equality_clause_scope__expansive_universalist, "constitutional_law/political_philosophy/civil_rights").

domain_priors:requires_active_enforcement(equality_clause_scope__expansive_universalist).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equality_clause_scope__expansive_universalist, 'ada92716-49d6-450c-a198-0d621a74c48a').
narrative_ontology:cs_kernel_codification('ada92716-49d6-450c-a198-0d621a74c48a', fixed_text).
narrative_ontology:cs_authority_grounding('ada92716-49d6-450c-a198-0d621a74c48a', lineage).
narrative_ontology:cs_interpretation_layer_present('ada92716-49d6-450c-a198-0d621a74c48a').
narrative_ontology:cs_reading_relation('ada92716-49d6-450c-a198-0d621a74c48a', equality_clause_scope__restrictive_originalist, forecloses).
narrative_ontology:cs_reading_relation('ada92716-49d6-450c-a198-0d621a74c48a', equality_clause_scope__progressive_textualist, influences).
narrative_ontology:cs_axiom('ada92716-49d6-450c-a198-0d621a74c48a', foundational, equality_principle_is_universal_and_self_evident).
narrative_ontology:cs_axiom_status(equality_principle_is_universal_and_self_evident, holdable).
narrative_ontology:cs_axiom_grounding('ada92716-49d6-450c-a198-0d621a74c48a', equality_principle_is_universal_and_self_evident, deontological).
narrative_ontology:cs_axiom('ada92716-49d6-450c-a198-0d621a74c48a', foundational, historical_exclusion_is_hypocrisy_not_precedent).
narrative_ontology:cs_axiom_status(historical_exclusion_is_hypocrisy_not_precedent, holdable).
narrative_ontology:cs_axiom_grounding('ada92716-49d6-450c-a198-0d621a74c48a', historical_exclusion_is_hypocrisy_not_precedent, deontological).
narrative_ontology:cs_axiom('ada92716-49d6-450c-a198-0d621a74c48a', secondary, judicial_interpretation_is_legitimate_expansion_mechanism).
narrative_ontology:cs_axiom_status(judicial_interpretation_is_legitimate_expansion_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('ada92716-49d6-450c-a198-0d621a74c48a', judicial_interpretation_is_legitimate_expansion_mechanism, conventional).
narrative_ontology:cs_reference_frame('ada92716-49d6-450c-a198-0d621a74c48a', post_founding_universalist_ideal).
narrative_ontology:cs_drift_state('ada92716-49d6-450c-a198-0d621a74c48a', contemporary_civil_rights_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('ada92716-49d6-450c-a198-0d621a74c48a', '').
narrative_ontology:cs_kernel_id(equality_clause_scope__expansive_universalist, equality_clause_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equality_clause_scope__expansive_universalist, historically_excluded_groups).
narrative_ontology:constraint_beneficiary(equality_clause_scope__expansive_universalist, civil_rights_litigants).
narrative_ontology:constraint_beneficiary(equality_clause_scope__expansive_universalist, rights_expansion_judiciary).
narrative_ontology:constraint_victim(equality_clause_scope__expansive_universalist, originalist_legal_traditionalists).
narrative_ontology:constraint_victim(equality_clause_scope__expansive_universalist, settled_expectation_holders).
narrative_ontology:constraint_victim(equality_clause_scope__expansive_universalist, legislative_majoritarian_process).
narrative_ontology:constraint_vindicates(equality_clause_scope__expansive_universalist, universal_human_equality_principle).
narrative_ontology:constraint_vindicates(equality_clause_scope__expansive_universalist, text_transcends_drafters_intent).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Groups denied full legal personhood or equal treatment at the founding (enslaved people, women, non-property-holders, later racial and sexual minorities) whose claims to equal protection depend entirely on the clause being read to reach them despite not being contemplated by its drafters. Their exit option is nonexistent — they cannot opt out of the constitutional order and must win inclusion through it.
narrative_ontology:constraint_stakeholder(equality_clause_scope__expansive_universalist, historically_excluded_groups, beneficiary,
    powerless, generational, trapped, national).

% Advocacy organizations and litigants who bring test cases urging courts to extend equal protection to previously unrecognized classes or claims. They actively shape doctrine by choosing plaintiffs, framing arguments, and building precedent chains; their leverage is entirely judicial, since legislative majorities have often been unwilling or unable to act.
narrative_ontology:constraint_stakeholder(equality_clause_scope__expansive_universalist, civil_rights_litigants, beneficiary,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(equality_clause_scope__expansive_universalist, civil_rights_litigants, agenda_setter).

% Judges who read the equality clause as expressing a self-evident universal principle temporarily betrayed by its authors' compromises. They administer the doctrine by deciding which historical exclusions count as correctable hypocrisy versus binding original meaning, and their interpretive discretion is the mechanism through which the reading operates.
narrative_ontology:constraint_stakeholder(equality_clause_scope__expansive_universalist, rights_expansion_judiciary, agenda_setter,
    institutional, civilizational, analytical, national).

% Judges, scholars, and litigants committed to reading constitutional text as fixed by its ratification-era meaning. Under this reading their interpretive method is treated as complicit with exclusion rather than as a neutral constraint on judicial power; they bear the reputational and doctrinal cost of being cast as defenders of a discredited framework, and cannot exit the debate without abandoning their jurisprudential commitments.
narrative_ontology:constraint_stakeholder(equality_clause_scope__expansive_universalist, originalist_legal_traditionalists, payer,
    organized, generational, constrained, national).

% Parties who arranged their affairs — property, contracts, licensing, institutional membership — around prior narrower readings of equal protection. When courts expand the clause's scope, these arrangements can be invalidated retroactively or made legally untenable; they had no opportunity to anticipate the doctrinal shift and no forum in which to contest it before it occurs.
narrative_ontology:constraint_stakeholder(equality_clause_scope__expansive_universalist, settled_expectation_holders, payer,
    moderate, biographical, trapped, national).

% The constitutional amendment and ordinary lawmaking pathway that this reading substantially bypasses. Its legitimacy as the preferred mechanism for expanding rights is diminished whenever courts reach the same substantive result through interpretation, since sustained political mobilization becomes optional rather than necessary.
narrative_ontology:constraint_stakeholder(equality_clause_scope__expansive_universalist, legislative_majoritarian_process, payer,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_non_agent(equality_clause_scope__expansive_universalist, legislative_majoritarian_process).

% Historians and originalist scholars who would object that treating the framers' actual, documented, exclusionary intent as mere 'hypocrisy to be corrected' erases what the clause's authors meant to enact and substitutes present-day moral commitments for historical fact. Their objection is acknowledged in academic literature but rarely determines case outcomes.
narrative_ontology:constraint_stakeholder(equality_clause_scope__expansive_universalist, framers_intent_scholarship, excluded,
    organized, civilizational, analytical, national).

% Scholars who study how the equality clause's meaning has shifted, comparing this reading's universalist premise against the restrictive originalist and progressive textualist alternatives without being personally bound by the outcome.
narrative_ontology:constraint_stakeholder(equality_clause_scope__expansive_universalist, constitutional_theorists, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(equality_clause_scope__expansive_universalist, diffuse).
narrative_ontology:fixing_cost_class(equality_clause_scope__expansive_universalist, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable textual anchor — 'equal protection' — around which successive generations can coordinate claims for inclusion without needing to rewrite the constitutional text each time, allowing incremental moral progress to be absorbed into existing legal structure.
% TRANSFER_FUNCTION: Moves legal recognition, standing, and remedy from groups and doctrines that benefited from narrower readings (traditionalist jurisprudence, settled arrangements built on exclusion) to groups previously outside the clause's protection, mediated through judicial reinterpretation rather than legislative reallocation.
% ABSENT_VOICES: Framers' intent scholarship and originalist jurists object that this reading treats documented historical meaning as an embarrassment to be interpreted away rather than as authoritative; legislative majoritarian process itself has no voice in a change accomplished substantially through courts. Both types of objection appear in dissents and academic critique but are structurally sidelined by the doctrine's own operating logic.
% DISAPPEARANCE_RATIONALE: If courts abandoned the expansive universalist reading, decades of civil-rights doctrine resting on it (equal protection extended beyond the framers' contemplated scope) would lose its interpretive foundation; excluded groups would need to rebuild protections through amendment or statute, and traditionalist legal frameworks would regain doctrinal primacy. The rearrangement would be substantial and would touch settled law across many domains.
% FOUNDING_PROBLEM: The equality clause's text promises a universal principle while its drafters' own practice — chattel slavery, property qualifications, exclusion of women — flatly contradicted it. The founding problem this reading solves is how to reconcile a document's stated ideal with its authors' contradicted conduct without concluding the ideal itself is false or inapplicable.
% FOUNDING_PROBLEM_CORROBORATION: Historians of the ratification debates (including originalist-sympathetic scholars who nonetheless document the gap between stated principle and enacted practice) attest that the contradiction between text and founding-era practice is real and unresolved by drafting history alone; this corroboration comes from scholarship outside the civil-rights litigation community that most directly benefits from the expansive reading.
narrative_ontology:disappearance_verdict(equality_clause_scope__expansive_universalist, world_rearranges).
narrative_ontology:founding_problem_status(equality_clause_scope__expansive_universalist, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equality_clause_scope__expansive_universalist, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(equality_clause_scope__expansive_universalist, 'none', 1).
narrative_ontology:epsilon_provenance(equality_clause_scope__expansive_universalist, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equality_clause_scope__expansive_universalist_tests).
:- end_tests(equality_clause_scope__expansive_universalist_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42 at interval end) because the reading redistributes legal standing and remedy rather than material resources directly, but does so by substantially discounting settled expectations and traditionalist jurisprudential authority. Suppression is moderate (0.38): the reading does not physically coerce dissenting jurists, but it does structurally discount the interpretive method (originalism) that would resist it, and it operates through judicial fiat with limited direct accountability to the legislative process it substantially bypasses. Theater ratio is low-moderate (0.22) — the coordination function (a stable text anchoring evolving claims) is real, not merely performative, though some invocations of 'self-evident truth' function rhetorically to preempt debate about whether the exclusion in question is actually illegitimate versus originally intended.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary and agenda-setter seats, this reading is corrective justice completing an always-true principle. From the payer seats (originalist scholars, settled-expectation holders), the same operation is judicial extraction of legitimacy from a competing interpretive method and from reliance interests that had no voice in the change. The engine computes this divergence from the structural data; the claimed_type here is authored as tangled_rope to reflect the analytical judgment that both functions are genuinely present, independent of how any single seat would self-describe it.
 *
 * DIRECTIONALITY LOGIC:
 *   Historically excluded groups and civil rights litigants sit near the full-beneficiary end: the reading exists specifically to extend legal recognition to them, and their exit options are minimal (they cannot leave the constitutional order). The judiciary that administers the reading is an agenda_setter with maximal interpretive discretion. Originalist traditionalists and settled-expectation holders sit near the target end: the reading's operation directly discounts their interpretive method or invalidates their prior arrangements, and their exit options are constrained or trapped — they cannot simply opt out of a constitutional regime that has moved past their preferred reading.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (reconciling a universalist text with a contradicted founding practice) remains live rather than resolved, which cuts against treating this as pure inertial extraction — the coordination function of extending a stable principle to newly recognized claims is a genuine, ongoing service, not a vestige. But the reading also requires active enforcement (judicial willingness to override originalist objections and disturb settled arrangements) and identifiably discounts a payer class (originalists, settled-expectation holders) through the same mechanism that benefits excluded groups — hence tangled_rope rather than rope: both a genuine coordination function and asymmetric extraction operate through the same structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    self_evidence_vs_construction,
    'Is the equality clause''s universal scope genuinely self-evident and merely obscured by founding-era hypocrisy, or is ''self-evidence'' itself a rhetorical construction that licenses judges to import contemporary moral commitments into the text?',
    'Comparative analysis of ratification-era debates and subsequent amendment history: if contemporaneous actors explicitly understood the principle as bounded (not universal) and later generations changed the text''s scope through amendment rather than pure interpretation, that would weigh against pure self-evidence; if founding-era dissenters (abolitionists, women''s rights advocates of the era) argued the universal principle was already latent in the text, that would support it.',
    'If genuinely self-evident and merely suppressed, the expansive reading is uncovering rather than constructing meaning, weakening the tangled_rope classification toward rope. If constructed, the reading''s low legitimacy threshold for judicial expansion is doing more independent work than the text itself, strengthening the extraction reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(self_evidence_vs_construction, conceptual, 'Whether the universal scope is discovered or judicially constructed.').

omega_variable(
    kernel_framing_committer_note,
    'Which of the three declared readings of equality_clause_scope should govern a given case, and where is the disagreement actually located?',
    'Not resolvable by data internal to this story — resolution requires comparing this story''s structural data against the sibling stories (restrictive_originalist, progressive_textualist) and observing which institutional actors (courts, legislatures, constitutional conventions) actually prevail in a given historical period.',
    'The disagreement is located specifically at: (1) whether historical exclusion is binding precedent or corrigible hypocrisy, (2) whether the beneficiary set is fixed at ratification or open-ended, and (3) whether judicial interpretation or democratic amendment is the legitimate mechanism for scope change. A sibling reading would change the beneficiary/victim declarations and the requires_active_enforcement structure substantially — the restrictive_originalist reading would likely classify as a mountain or rope from the perspective of its own adherents (fixed, natural boundary), while this reading classifies as tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_committer_note, conceptual, 'Committer-frame note: this is one reading among three of a contested kernel; the sibling readings are separate constraint stories.').

omega_variable(
    reliance_interest_weighting,
    'How much normative weight should settled expectations built on narrower prior readings receive when a court expands the clause''s scope?',
    'Doctrinal survey of how courts applying the expansive reading have actually treated retroactivity and reliance-interest doctrines in equal protection expansions versus other constitutional contexts.',
    'Heavy reliance-interest weighting would reduce the effective extraction from settled_expectation_holders (transitions would be prospective and compensated); light weighting would sustain or increase the measured extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reliance_interest_weighting, empirical, 'Whether reliance interests are protected or discounted in practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equality_clause_scope__expansive_universalist, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t0, equality_clause_scope__expansive_universalist, theater_ratio, 0, 0.1).
narrative_ontology:measurement(equa_tr_t10, equality_clause_scope__expansive_universalist, theater_ratio, 10, 0.12).
narrative_ontology:measurement(equa_tr_t20, equality_clause_scope__expansive_universalist, theater_ratio, 20, 0.15).
narrative_ontology:measurement(equa_tr_t30, equality_clause_scope__expansive_universalist, theater_ratio, 30, 0.17).
narrative_ontology:measurement(equa_tr_t40, equality_clause_scope__expansive_universalist, theater_ratio, 40, 0.19).
narrative_ontology:measurement(equa_tr_t50, equality_clause_scope__expansive_universalist, theater_ratio, 50, 0.21).
narrative_ontology:measurement(equa_tr_t60, equality_clause_scope__expansive_universalist, theater_ratio, 60, 0.22).

% Extraction over time
narrative_ontology:measurement(equa_be_t0, equality_clause_scope__expansive_universalist, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(equa_be_t10, equality_clause_scope__expansive_universalist, base_extractiveness, 10, 0.32).
narrative_ontology:measurement(equa_be_t20, equality_clause_scope__expansive_universalist, base_extractiveness, 20, 0.36).
narrative_ontology:measurement(equa_be_t30, equality_clause_scope__expansive_universalist, base_extractiveness, 30, 0.38).
narrative_ontology:measurement(equa_be_t40, equality_clause_scope__expansive_universalist, base_extractiveness, 40, 0.4).
narrative_ontology:measurement(equa_be_t50, equality_clause_scope__expansive_universalist, base_extractiveness, 50, 0.41).
narrative_ontology:measurement(equa_be_t60, equality_clause_scope__expansive_universalist, base_extractiveness, 60, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t0, equality_clause_scope__expansive_universalist, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(equa_su_t10, equality_clause_scope__expansive_universalist, suppression_requirement, 10, 0.25).
narrative_ontology:measurement(equa_su_t20, equality_clause_scope__expansive_universalist, suppression_requirement, 20, 0.3).
narrative_ontology:measurement(equa_su_t30, equality_clause_scope__expansive_universalist, suppression_requirement, 30, 0.33).
narrative_ontology:measurement(equa_su_t40, equality_clause_scope__expansive_universalist, suppression_requirement, 40, 0.35).
narrative_ontology:measurement(equa_su_t50, equality_clause_scope__expansive_universalist, suppression_requirement, 50, 0.37).
narrative_ontology:measurement(equa_su_t60, equality_clause_scope__expansive_universalist, suppression_requirement, 60, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(equality_clause_scope__expansive_universalist, equality_clause_scope__restrictive_originalist).
narrative_ontology:affects_constraint(equality_clause_scope__expansive_universalist, equality_clause_scope__progressive_textualist).

% DUAL FORMULATION NOTE:
% This is one of three sibling constraints decomposing the natural-language concept 'the equality clause's scope' per the ε-invariance principle. Each reading (expansive_universalist, restrictive_originalist, progressive_textualist) instantiates a structurally distinct constraint with its own beneficiary set, victim set, and classification, because the readings disagree about who is protected and by what mechanism — not merely about how to describe a shared arrangement. They are linked here rather than merged into a single story with a framing parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
