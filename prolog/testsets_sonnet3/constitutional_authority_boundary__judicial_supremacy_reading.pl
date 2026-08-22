% ============================================================================
% CONSTRAINT STORY: constitutional_authority_boundary__judicial_supremacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_authority_boundary__judicial_supremacy_reading, []).

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
 *   constraint_id: constitutional_authority_boundary__judicial_supremacy_reading
 *   human_readable: Judicial Supremacy Reading of Constitutional Authority
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   This story instantiates the judicial supremacy reading of the
 *   constitutional authority boundary kernel: the constitutional text is read
 *   as vesting courts with final, unchallengeable interpretive authority,
 *   able to invalidate legislative and executive acts with no institutional
 *   remedy available. This is a distinct constraint from the
 *   coordinate-construction reading (distributed interpretive authority
 *   across three co-equal branches) and the parliamentary-primacy reading
 *   (legislative sovereignty over constitutional meaning) — each reading is
 *   authored as its own file with its own ε, beneficiaries, and victims, per
 *   the ε-invariance principle. Under this reading, ε is high (0.66) because
 *   the judiciary's veto is genuinely counter-majoritarian and structurally
 *   irreversible short of formal amendment; the sibling readings would author
 *   much lower ε for the same underlying text because they read the kernel as
 *   distributing or subordinating that authority rather than concentrating
 *   it.
 *
 * KEY AGENTS:
 *   - apex_judiciary: Primary agenda-setter and beneficiary (institutional/arbitrage) — sets and enforces final interpretive authority
 *   - elected_legislature: Primary target (powerful/constrained) — bears permanent policy-space narrowing with no override channel
 *   - executive_branch: Secondary target (powerful/constrained) — subject to unreviewable invalidation of executive action
 *   - electoral_majorities: Diffuse target (moderate/trapped) — enacted preferences overridden with no practical recourse
 *   - constitutional_scholars: Analytical observer — compares this reading's costs against coordinate-construction and parliamentary-primacy alternatives
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_authority_boundary__judicial_supremacy_reading, 0.66).
domain_priors:suppression_score(constitutional_authority_boundary__judicial_supremacy_reading, 0.71).
domain_priors:theater_ratio(constitutional_authority_boundary__judicial_supremacy_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_authority_boundary__judicial_supremacy_reading, extractiveness, 0.66).
narrative_ontology:constraint_metric(constitutional_authority_boundary__judicial_supremacy_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(constitutional_authority_boundary__judicial_supremacy_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_authority_boundary__judicial_supremacy_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(constitutional_authority_boundary__judicial_supremacy_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_authority_boundary__judicial_supremacy_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_authority_boundary__judicial_supremacy_reading, "Judicial Supremacy Reading of Constitutional Authority").
narrative_ontology:topic_domain(constitutional_authority_boundary__judicial_supremacy_reading, "constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(constitutional_authority_boundary__judicial_supremacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_authority_boundary__judicial_supremacy_reading, '539e2858-7cbc-41bf-9d1c-0ced3d6a9e4b').
narrative_ontology:cs_kernel_codification('539e2858-7cbc-41bf-9d1c-0ced3d6a9e4b', fixed_text).
narrative_ontology:cs_authority_grounding('539e2858-7cbc-41bf-9d1c-0ced3d6a9e4b', lineage).
narrative_ontology:cs_interpretation_layer_present('539e2858-7cbc-41bf-9d1c-0ced3d6a9e4b').
narrative_ontology:cs_reading_relation('539e2858-7cbc-41bf-9d1c-0ced3d6a9e4b', constitutional_authority_boundary__coordinate_construction_reading, forecloses).
narrative_ontology:cs_reading_relation('539e2858-7cbc-41bf-9d1c-0ced3d6a9e4b', constitutional_authority_boundary__parliamentary_primacy_reading, forecloses).
narrative_ontology:cs_axiom('539e2858-7cbc-41bf-9d1c-0ced3d6a9e4b', foundational, judicial_finality_is_unchallengeable).
narrative_ontology:cs_axiom_status(judicial_finality_is_unchallengeable, holdable).
narrative_ontology:cs_axiom_grounding('539e2858-7cbc-41bf-9d1c-0ced3d6a9e4b', judicial_finality_is_unchallengeable, conventional).
narrative_ontology:cs_axiom('539e2858-7cbc-41bf-9d1c-0ced3d6a9e4b', secondary, counter_majoritarian_veto_requires_no_remedy).
narrative_ontology:cs_axiom_status(counter_majoritarian_veto_requires_no_remedy, holdable).
narrative_ontology:cs_axiom_grounding('539e2858-7cbc-41bf-9d1c-0ced3d6a9e4b', counter_majoritarian_veto_requires_no_remedy, instrumental).
narrative_ontology:cs_reference_frame('539e2858-7cbc-41bf-9d1c-0ced3d6a9e4b', textually_silent_finality_default).
narrative_ontology:cs_drift_state('539e2858-7cbc-41bf-9d1c-0ced3d6a9e4b', post_judicial_review_consolidation, gap(authority_erosion, minor, false)).
narrative_ontology:cs_created_at('539e2858-7cbc-41bf-9d1c-0ced3d6a9e4b', '').
narrative_ontology:cs_kernel_id(constitutional_authority_boundary__judicial_supremacy_reading, constitutional_authority_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__judicial_supremacy_reading, apex_judiciary).
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__judicial_supremacy_reading, constitutional_bar_specialists).
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__judicial_supremacy_reading, entrenched_minority_interests).
narrative_ontology:constraint_victim(constitutional_authority_boundary__judicial_supremacy_reading, elected_legislature).
narrative_ontology:constraint_victim(constitutional_authority_boundary__judicial_supremacy_reading, executive_branch).
narrative_ontology:constraint_victim(constitutional_authority_boundary__judicial_supremacy_reading, electoral_majorities).
narrative_ontology:constraint_vindicates(constitutional_authority_boundary__judicial_supremacy_reading, judicial_review_doctrine).
narrative_ontology:constraint_vindicates(constitutional_authority_boundary__judicial_supremacy_reading, constitutional_supremacy_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds final, unreviewable authority to declare what the constitution means and to invalidate legislative and executive acts with no remedy available to the other branches. Sets its own interpretive doctrine, expands or contracts the scope of what counts as a constitutional question, and answers to no appellate body. Its rulings cannot be overridden by ordinary legislative majorities.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__judicial_supremacy_reading, apex_judiciary, agenda_setter,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(constitutional_authority_boundary__judicial_supremacy_reading, apex_judiciary, beneficiary).

% A professional class whose livelihood depends on the complexity and finality of constitutional litigation before the apex court. The more the court is the exclusive venue for resolving constitutional disputes, the more valuable their specialized expertise becomes. They benefit from the interpretive monopoly without bearing its costs.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__judicial_supremacy_reading, constitutional_bar_specialists, beneficiary,
    organized, biographical, mobile, national).

% Groups unable to win durable protections through ordinary electoral politics use constitutional litigation to lock in outcomes that a shifting legislative majority could not easily reverse. They benefit from a venue immune to majoritarian correction, converting one favorable ruling into a permanent policy floor or ceiling.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__judicial_supremacy_reading, entrenched_minority_interests, beneficiary,
    powerful, generational, arbitrage, national).

% Passes statutes reflecting current electoral mandates, only to have them invalidated by judicial constitutional interpretation with no override mechanism available — no supermajority re-passage, no legislative response clause, no sunset review of the ruling. Its policy space is permanently narrowed by precedent it cannot revisit through ordinary democratic process.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__judicial_supremacy_reading, elected_legislature, payer,
    powerful, biographical, constrained, national).

% Implements policy subject to judicial invalidation of executive action on constitutional grounds, with the same absence of remedy. Executive orders and administrative action can be struck down and the executive has no formal channel to contest the interpretive finality of the ruling.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__judicial_supremacy_reading, executive_branch, payer,
    powerful, biographical, constrained, national).

% Vote for representatives who enact a policy agenda, only to see that agenda constitutionally invalidated by unelected judges applying interpretive methods the electorate did not choose and cannot revise except through the slow, high-threshold process of formal constitutional amendment. Their preferences translate into law only within boundaries the judiciary alone draws.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__judicial_supremacy_reading, electoral_majorities, payer,
    moderate, biographical, trapped, national).

% Study comparative constitutional design across jurisdictions with judicial supremacy, coordinate construction, and parliamentary primacy models, documenting the tradeoffs between counter-majoritarian rights protection and democratic self-governance without being party to any specific ruling.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__judicial_supremacy_reading, constitutional_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, predictable final answer to constitutional disputes, preventing the chaos of each branch asserting final interpretive authority over the same question and enabling durable protection of rights and structural limits that a transient legislative majority might otherwise erode.
% TRANSFER_FUNCTION: Moves final interpretive authority — and the policy discretion that comes with it — from elected, electorally-accountable branches to an appointed, life-tenured judiciary; moves durable policy lock-in capacity to whichever litigants can win before that court.
% ABSENT_VOICES: Electoral majorities whose enacted preferences are invalidated have no seat in the interpretive process itself — they participate only by electing legislators who then have no recourse once a ruling issues. Future legislatures bound by precedent they had no hand in making are also structurally absent from the ruling's formation.
% DISAPPEARANCE_RATIONALE: If judicial supremacy vanished overnight, legislatures would regain capacity to override or work around constitutional rulings through ordinary or entrenched legislation, executive action would face fewer unreviewable constitutional vetoes, and the constitutional bar's practice would reorient around advisory and negotiated resolution rather than final adjudication — the balance of power between branches would materially shift toward the elected branches.
% FOUNDING_PROBLEM: Early constitutional designers sought a mechanism to prevent transient legislative or executive majorities from violating entrenched rights or exceeding structural limits, and to provide a stable, principled means of resolving genuine disputes about what the constitutional text requires.
% FOUNDING_PROBLEM_CORROBORATION: The judiciary and constitutional bar attest the problem remains live, citing ongoing majoritarian threats to minority rights. Legislative scholars and comparative constitutionalists studying parliamentary-primacy systems attest that the problem is addressable through other mechanisms (proportionality review with override clauses, entrenched supermajority requirements) and that unreviewable judicial finality specifically has independently documented costs — reduced legislative responsiveness, judicialization of ordinary politics — that the founding problem does not require solving this way.
narrative_ontology:disappearance_verdict(constitutional_authority_boundary__judicial_supremacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_authority_boundary__judicial_supremacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_authority_boundary__judicial_supremacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(constitutional_authority_boundary__judicial_supremacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_authority_boundary__judicial_supremacy_reading, 0.66, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_authority_boundary__judicial_supremacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_authority_boundary__judicial_supremacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(constitutional_authority_boundary__judicial_supremacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.66) reflects the genuine counter-majoritarian transfer: policy discretion moves from electorally accountable branches to a life-tenured body no ordinary majority can correct. Suppression (0.71) is high because the arrangement's persistence depends on the absence of any legislative override mechanism — this is a structural feature of the reading, not a contingent enforcement choice, and it is unscaled by power or scope per the framework's rule. Theater ratio is comparatively low (0.28) because the interpretive function is largely genuine adjudication rather than performance, though it rises over the measured interval as precedent accumulates and doctrinal expansion increasingly serves to entrench the judiciary's own interpretive monopoly rather than resolve novel disputes. Accessibility collapse (0.62) reflects that once a constitutional ruling issues under this reading, essentially no institutional path exists to revisit it short of a new case reaching the same court or the near-insurmountable threshold of formal amendment. Resistance (0.58) reflects sustained legislative and executive pushback — court-packing proposals, jurisdiction-stripping bills, non-compliance episodes — that never fully dislodges the arrangement.
 *
 * PERSPECTIVAL GAP:
 *   From the judiciary's seat, this is principled, coordinating constitutional stewardship — the tangled_rope's coordination face. From the legislature's and electoral majorities' seats, the same structure is an unreviewable veto with no remedy — the tangled_rope's extraction face. Both are computed from the same structural data; the divergence is exactly what the tangled_rope classification is built to hold rather than force into a single verdict.
 *
 * DIRECTIONALITY LOGIC:
 *   The judiciary sits at the low-d, beneficiary end: it collects interpretive monopoly rents (professional prestige, institutional power, doctrinal control) and its arbitrage-grade exit (it always adjudicates from the position of final authority) locks in the subsidy. The constitutional bar and entrenched minority interests are secondary beneficiaries who profit from the monopoly without administering it. The legislature and executive sit at the high-d, target end: their exit is constrained (no override channel exists by construction of this reading), and the constraint's entire operation runs through invalidating their acts. Electoral majorities are the most trapped: their only channel is electing new representatives who face the same structural ceiling, so their exit option is trapped rather than merely constrained.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — preventing majoritarian violation of entrenched rights — was and remains partially live, which is why this reading is not simply mislabeled as pure extraction: there is a genuine coordination function (durable rights protection, resolving genuine interpretive disputes) that the tangled_rope classification preserves rather than erasing. But the *unchallengeable, remedy-free* character of the authority is what pushes this specific reading toward high extraction: a reading that retained final judicial review but preserved a legislative override mechanism (as in some coordinate-construction or notwithstanding-clause systems) would coordinate the same function at much lower ε. The mandatrophy question here is whether the founding problem still requires *this specific, remedy-free* form of finality, or whether the arrangement has outlived the narrower version of the problem it was built to solve.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_underdetermination_of_finality,
    'Does the constitutional text itself compel the judicial supremacy reading, or is textual finality-of-interpretation an inference the judiciary has drawn from ambiguous structural silence about which branch has the last word?',
    'Comparative textual analysis against constitutions that explicitly assign or deny final interpretive authority (e.g., explicit notwithstanding clauses, explicit judicial review grants, explicit parliamentary sovereignty clauses) to determine whether this text''s silence is more consistent with implied judicial finality or implied coordinate/legislative primacy.',
    'If the text is genuinely silent and the judiciary''s finality claim is self-assigned, this reading''s beneficiary structure (the judiciary benefiting from its own interpretive expansion) becomes a stronger case for reclassifying toward snare; if the text affirmatively compels judicial finality, the tangled_rope''s coordination component is better grounded.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_underdetermination_of_finality, conceptual, 'Whether judicial finality is textually compelled or judicially self-assigned.').

omega_variable(
    kernel_framing_choice_and_reading_selection,
    'Given that all three readings (judicial supremacy, coordinate construction, parliamentary primacy) are defensible on the same text, what evidentiary or interpretive signal justifies treating THIS reading as the one under analysis rather than treating the coordinate-construction reading as the more textually conservative default?',
    'This omega documents the CS-framing under-determination directly: the choice of which reading to author as ''the'' constitutional_authority_boundary constraint for a given jurisdiction is itself a contested interpretive act. Resolution would require jurisdiction-specific doctrinal history (e.g., Marbury-style self-assertion of judicial review versus explicit constitutional text granting it) establishing which reading the actual practice of the relevant constitutional order has settled into.',
    'If the coordinate-construction reading better describes actual practice, this story''s high ε and tangled_rope classification would not apply to that jurisdiction; the choice of reading is a live conceptual fork with direct classification consequences, not a neutral scoping decision.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_choice_and_reading_selection, conceptual, 'Which reading of the kernel a given jurisdiction''s practice actually instantiates.').

omega_variable(
    rights_protection_offset,
    'Does the counter-majoritarian rights protection this reading enables offset its extraction from elected branches, or does the same rights protection function survive under lower-ε alternative readings (e.g., proportionality review with legislative override)?',
    'Cross-jurisdictional comparison of rights-protection outcomes in judicial-supremacy systems versus notwithstanding-clause / override-capable systems, controlling for underlying political culture.',
    'If rights outcomes are comparable under override-capable systems, the remedy-free character of this reading''s extraction is not doing protective work proportional to its cost, strengthening the case that the current ε reflects avoidable extraction rather than necessary coordination cost.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(rights_protection_offset, empirical, 'Whether remedy-free finality is necessary for the rights-protection function or merely one costly means to it.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_authority_boundary__judicial_supremacy_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_authority_boundary__judicial_supremacy_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(cons_tr_t12, constitutional_authority_boundary__judicial_supremacy_reading, theater_ratio, 12, 0.16).
narrative_ontology:measurement(cons_tr_t24, constitutional_authority_boundary__judicial_supremacy_reading, theater_ratio, 24, 0.19).
narrative_ontology:measurement(cons_tr_t36, constitutional_authority_boundary__judicial_supremacy_reading, theater_ratio, 36, 0.22).
narrative_ontology:measurement(cons_tr_t48, constitutional_authority_boundary__judicial_supremacy_reading, theater_ratio, 48, 0.25).
narrative_ontology:measurement(cons_tr_t60, constitutional_authority_boundary__judicial_supremacy_reading, theater_ratio, 60, 0.28).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_authority_boundary__judicial_supremacy_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(cons_be_t12, constitutional_authority_boundary__judicial_supremacy_reading, base_extractiveness, 12, 0.49).
narrative_ontology:measurement(cons_be_t24, constitutional_authority_boundary__judicial_supremacy_reading, base_extractiveness, 24, 0.55).
narrative_ontology:measurement(cons_be_t36, constitutional_authority_boundary__judicial_supremacy_reading, base_extractiveness, 36, 0.59).
narrative_ontology:measurement(cons_be_t48, constitutional_authority_boundary__judicial_supremacy_reading, base_extractiveness, 48, 0.63).
narrative_ontology:measurement(cons_be_t60, constitutional_authority_boundary__judicial_supremacy_reading, base_extractiveness, 60, 0.66).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_authority_boundary__judicial_supremacy_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(cons_su_t12, constitutional_authority_boundary__judicial_supremacy_reading, suppression_requirement, 12, 0.56).
narrative_ontology:measurement(cons_su_t24, constitutional_authority_boundary__judicial_supremacy_reading, suppression_requirement, 24, 0.61).
narrative_ontology:measurement(cons_su_t36, constitutional_authority_boundary__judicial_supremacy_reading, suppression_requirement, 36, 0.65).
narrative_ontology:measurement(cons_su_t48, constitutional_authority_boundary__judicial_supremacy_reading, suppression_requirement, 48, 0.68).
narrative_ontology:measurement(cons_su_t60, constitutional_authority_boundary__judicial_supremacy_reading, suppression_requirement, 60, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_authority_boundary__judicial_supremacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(constitutional_authority_boundary__judicial_supremacy_reading, coordinate_construction_reading).
narrative_ontology:affects_constraint(constitutional_authority_boundary__judicial_supremacy_reading, parliamentary_primacy_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraint files decomposing the natural-language concept 'who has final constitutional interpretive authority.' judicial_supremacy_reading (this file, high ε ~0.66, tangled_rope), coordinate_construction_reading (distributed authority, expected lower ε, closer to rope), and parliamentary_primacy_reading (legislative sovereignty, expected lower ε, closer to scaffold/rope depending on entrenchment) share the same underlying textual kernel but instantiate structurally distinct constraints with different beneficiary/victim sets and different ε. They are linked via network edges rather than merged into one story with a measurement parameter, per the ε-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
