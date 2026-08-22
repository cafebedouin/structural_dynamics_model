% ============================================================================
% CONSTRAINT STORY: constitutional_interpretive_authority__judicial_supremacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_interpretive_authority__judicial_supremacy_reading, []).

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
 *   constraint_id: constitutional_interpretive_authority__judicial_supremacy_reading
 *   human_readable: Judicial Supremacy Reading of Constitutional Interpretive Authority
 *   domain: constitutional_law/political_theory/jurisprudence
 *
 * SUMMARY:
 *   This story instantiates the judicial supremacy reading of the
 *   constitutional interpretive authority kernel: courts hold final say over
 *   whether legislative acts comply with entrenched
 *   constitutional/fundamental rights provisions, and can nullify statutes
 *   duly passed by elected majorities. This is a reading of a contested
 *   kernel, not a description of the kernel itself — the parliamentary
 *   supremacy reading (legislature has final say, no judicial nullification
 *   power) and the coordinate construction reading (no single branch has
 *   final say; meaning is settled through inter-branch dialogue) are separate
 *   constraints with their own ε values, beneficiary sets, and stakeholder
 *   structures, linked here only through network.affects_constraints and
 *   cs_structure.reading_relations. Under this reading, the judiciary and the
 *   litigators who work through it enter the beneficiary set for interpretive
 *   authority; the legislature and the electoral majorities it represents are
 *   subordinated, and the coercive force of judicial nullification is
 *   legitimated through the language of rights-compliance rather than
 *   democratic will.
 *
 * KEY AGENTS:
 *   - apex_judiciary: institutional/arbitrage — sets and administers final interpretive authority, beneficiary of its own doctrine
 *   - constitutional_litigators: organized/mobile — professional beneficiary class whose leverage depends on judicial supremacy persisting
 *   - rights_bearing_minorities_when_courts_side_with_them: powerless/trapped — contingent beneficiary, gains reversible by court composition change
 *   - electoral_majorities: organized/constrained — bears the cost of having enacted statutes nullified
 *   - legislative_branch: institutional/constrained — subordinated branch, high-cost remedies only
 *   - policy_experimentation_capacity: powerless/trapped (non-agent) — diffuse systemic capacity foreclosed by locked-in doctrine
 *   - constitutional_scholars: analytical/analytical — observes the countermajoritarian difficulty from outside
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_interpretive_authority__judicial_supremacy_reading, 0.52).
domain_priors:suppression_score(constitutional_interpretive_authority__judicial_supremacy_reading, 0.58).
domain_priors:theater_ratio(constitutional_interpretive_authority__judicial_supremacy_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_interpretive_authority__judicial_supremacy_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(constitutional_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_interpretive_authority__judicial_supremacy_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(constitutional_interpretive_authority__judicial_supremacy_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_interpretive_authority__judicial_supremacy_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_interpretive_authority__judicial_supremacy_reading, "Judicial Supremacy Reading of Constitutional Interpretive Authority").
narrative_ontology:topic_domain(constitutional_interpretive_authority__judicial_supremacy_reading, "constitutional_law/political_theory/jurisprudence").

domain_priors:requires_active_enforcement(constitutional_interpretive_authority__judicial_supremacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_interpretive_authority__judicial_supremacy_reading, '0aaa88ff-c65c-4d2b-ac12-3a0d83c75302').
narrative_ontology:cs_kernel_codification('0aaa88ff-c65c-4d2b-ac12-3a0d83c75302', formalized).
narrative_ontology:cs_authority_grounding('0aaa88ff-c65c-4d2b-ac12-3a0d83c75302', lineage).
narrative_ontology:cs_interpretation_layer_present('0aaa88ff-c65c-4d2b-ac12-3a0d83c75302').
narrative_ontology:cs_reading_relation('0aaa88ff-c65c-4d2b-ac12-3a0d83c75302', constitutional_interpretive_authority__parliamentary_supremacy_reading, forecloses).
narrative_ontology:cs_reading_relation('0aaa88ff-c65c-4d2b-ac12-3a0d83c75302', constitutional_interpretive_authority__coordinate_construction_reading, coexists_with).
narrative_ontology:cs_axiom('0aaa88ff-c65c-4d2b-ac12-3a0d83c75302', foundational, judicial_review_is_necessary_rights_backstop).
narrative_ontology:cs_axiom_status(judicial_review_is_necessary_rights_backstop, holdable).
narrative_ontology:cs_axiom_grounding('0aaa88ff-c65c-4d2b-ac12-3a0d83c75302', judicial_review_is_necessary_rights_backstop, deontological).
narrative_ontology:cs_axiom('0aaa88ff-c65c-4d2b-ac12-3a0d83c75302', secondary, entrenched_text_requires_nonmajoritarian_final_arbiter).
narrative_ontology:cs_axiom_status(entrenched_text_requires_nonmajoritarian_final_arbiter, holdable).
narrative_ontology:cs_axiom_grounding('0aaa88ff-c65c-4d2b-ac12-3a0d83c75302', entrenched_text_requires_nonmajoritarian_final_arbiter, instrumental).
narrative_ontology:cs_reference_frame('0aaa88ff-c65c-4d2b-ac12-3a0d83c75302', written_constitution_as_higher_law_supremacy).
narrative_ontology:cs_drift_state('0aaa88ff-c65c-4d2b-ac12-3a0d83c75302', contemporary_judicial_review_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('0aaa88ff-c65c-4d2b-ac12-3a0d83c75302', '').
narrative_ontology:cs_kernel_id(constitutional_interpretive_authority__judicial_supremacy_reading, constitutional_interpretive_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_interpretive_authority__judicial_supremacy_reading, apex_judiciary).
narrative_ontology:constraint_beneficiary(constitutional_interpretive_authority__judicial_supremacy_reading, constitutional_litigators).
narrative_ontology:constraint_beneficiary(constitutional_interpretive_authority__judicial_supremacy_reading, rights_bearing_minorities_when_courts_side_with_them).
narrative_ontology:constraint_victim(constitutional_interpretive_authority__judicial_supremacy_reading, electoral_majorities).
narrative_ontology:constraint_victim(constitutional_interpretive_authority__judicial_supremacy_reading, legislative_branch).
narrative_ontology:constraint_victim(constitutional_interpretive_authority__judicial_supremacy_reading, policy_experimentation_capacity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds final say on whether legislative acts survive constitutional review, can strike down statutes passed by elected majorities, and sets the interpretive doctrine that governs when it will do so. Its authority is self-reinforcing: it is the body that determines the scope of its own review power, subject only to formal amendment processes it does not control but which it can also interpret.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__judicial_supremacy_reading, apex_judiciary, agenda_setter,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(constitutional_interpretive_authority__judicial_supremacy_reading, apex_judiciary, beneficiary).

% A professional class whose careers and institutional relevance depend on litigation being the primary venue for resolving contested rights and policy questions. Benefits from every expansion of judicially cognizable claims; has strong incentive to defend and extend the reach of judicial review.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__judicial_supremacy_reading, constitutional_litigators, beneficiary,
    organized, biographical, mobile, national).

% Groups without electoral numbers to prevail in ordinary legislative politics who obtain protection when courts strike down majoritarian legislation on rights grounds. Their gains under this reading are entirely contingent on judicial sympathy — a different court composition can reverse the benefit without any change in their political power.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__judicial_supremacy_reading, rights_bearing_minorities_when_courts_side_with_them, beneficiary,
    powerless, biographical, trapped, national).

% Assemble political coalitions, win elections, and pass legislation through the processes the constitution formally authorizes — only to have enacted statutes nullified by unelected judges applying interpretive methods the majority never voted for and cannot directly overturn except through the much harder path of constitutional amendment or generational court appointment turnover.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__judicial_supremacy_reading, electoral_majorities, payer,
    organized, biographical, constrained, national).

% The body formally vested with lawmaking power finds its enactments subordinated to judicial constitutional interpretation. It can attempt to rewrite statutes to survive review, pursue court-packing or jurisdiction-stripping, or seek amendment — all high-cost, slow, and uncertain compared to the speed with which a single ruling can void its work.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__judicial_supremacy_reading, legislative_branch, payer,
    institutional, generational, constrained, national).

% The systemic capacity of jurisdictions to try, fail, and iterate on policy is constrained wherever a doctrine forecloses categories of legislative experimentation in advance. This is not an actor but a diffuse capacity that shrinks whenever a constitutional ruling locks in one policy answer as the only permissible one.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__judicial_supremacy_reading, policy_experimentation_capacity, payer,
    powerless, generational, trapped, national).
narrative_ontology:stakeholder_non_agent(constitutional_interpretive_authority__judicial_supremacy_reading, policy_experimentation_capacity).

% Groups who might benefit from a different balance of power between courts and legislatures are not consulted on which reading of interpretive authority governs; the reading is settled by past judicial appointments and precedent, not by any process in which they participate.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__judicial_supremacy_reading, future_political_minorities, excluded,
    powerless, generational, trapped, national).

% Study the pattern of judicial review outcomes, document the countermajoritarian difficulty, and assess whether judicial supremacy protects rights more reliably than it entrenches judicial preference over democratic outcomes.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__judicial_supremacy_reading, constitutional_scholars, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(constitutional_interpretive_authority__judicial_supremacy_reading, apex_judiciary).
narrative_ontology:fixing_cost_class(constitutional_interpretive_authority__judicial_supremacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, stable, final-word mechanism for resolving disputes about the meaning of entrenched constitutional text, preventing every legislative majority from having to relitigate fundamental rights questions from scratch, and offers a check against transient majorities using ordinary legislation to strip protections from minorities.
% TRANSFER_FUNCTION: Moves final interpretive authority from elected legislatures to appointed or life-tenured judges; moves the practical cost of overturning a disfavored policy outcome from ordinary electoral politics (win the next election) to extraordinary constitutional politics (amend the constitution or wait out judicial appointments) — a transfer of political leverage from majorities to courts and to whichever litigants can frame their claims in constitutional terms.
% ABSENT_VOICES: Future political minorities who might prefer a different institutional balance are not in the room when the reading is settled; the reading is fixed by precedent and appointment history rather than by any live democratic choice, and dissenting legislatures have no direct channel to contest the interpretive framework itself, only individual rulings within it.
% DISAPPEARANCE_RATIONALE: If judicial supremacy vanished overnight, legislatures could enact and retain statutes currently vulnerable to constitutional nullification, litigators would lose their highest-leverage forum, and rights protections currently secured by courts against majoritarian legislation would depend entirely on legislative goodwill or entrenched political norms rather than judicial backstop — a substantial reallocation of power between branches and between majorities and minorities.
% FOUNDING_PROBLEM: Written constitutions with entrenched rights provisions need some mechanism to prevent ordinary legislative majorities from nullifying those provisions through simple statute; early constitutional designers worried that a legislature could vote away the very rights the constitution purported to guarantee.
% FOUNDING_PROBLEM_CORROBORATION: The judiciary and constitutional litigators attest the problem remains live — legislatures still pass rights-infringing statutes that require judicial correction. Legislative-branch representatives and democratic theorists outside the judiciary attest the problem has been substantially overcorrected: judicial review now nullifies broad swaths of ordinary economic and social policy far beyond the narrow rights-entrenchment problem it was built to solve, per comparative constitutional scholarship documenting the expansion of judicially cognizable claims across the 20th century.
narrative_ontology:disappearance_verdict(constitutional_interpretive_authority__judicial_supremacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_interpretive_authority__judicial_supremacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_interpretive_authority__judicial_supremacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(constitutional_interpretive_authority__judicial_supremacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_interpretive_authority__judicial_supremacy_reading, 0.52, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_interpretive_authority__judicial_supremacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_interpretive_authority__judicial_supremacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(constitutional_interpretive_authority__judicial_supremacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.52 at interval end) because judicial supremacy genuinely does perform a coordination function — providing a stable, final-word mechanism for resolving constitutional disputes and protecting entrenched rights against transient majorities — while also asymmetrically shifting political leverage from elected legislatures to appointed courts. Suppression (0.58) reflects that legislative attempts to override adverse rulings face categorically higher costs (constitutional amendment, generational appointment turnover) than the ordinary political cost of losing an election, and this asymmetry is actively enforced through the doctrine of judicial finality itself. Theater ratio is comparatively low (0.28) because the interpretive function is substantively exercised, not merely performed, though it rises modestly over the measured interval as doctrine hardens into settled precedent that increasingly forecloses re-litigation of the underlying institutional question.
 *
 * PERSPECTIVAL GAP:
 *   The judiciary's own seat experiences this arrangement as principled rights guardianship — a coordination mechanism preventing majoritarian overreach. The legislative branch and electoral majorities experience the identical structure as an extraction of their formally granted lawmaking authority, redirected through a body they did not elect and cannot readily reverse. The engine computes these as structurally different seat classifications from the same authored data; the divergence is not an error to be reconciled but the object of analysis.
 *
 * DIRECTIONALITY LOGIC:
 *   The apex judiciary sits near the full-beneficiary end: it both administers and benefits from the interpretive authority it exercises, with arbitrage-grade exit (it can reshape its own doctrine). Constitutional litigators similarly benefit from every expansion of judicially cognizable claims. Electoral majorities and the legislative branch sit toward the target end: they bear the transfer (nullified statutes, subordinated lawmaking power) with only constrained exit (amendment, appointment politics — both slow and uncertain). Rights-bearing minorities occupy an unusual position: real beneficiaries when courts side with them, but on a d value that should NOT be read as stably low, because their benefit is entirely contingent on judicial composition and could reverse with no change in their own political power — this is noted in a directionality-adjacent omega rather than an override, since the derivation from beneficiary declaration alone is not wrong, just incomplete without that caveat.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — preventing legislative majorities from voting away entrenched rights via ordinary statute — remains genuinely live in narrow cases (e.g., direct majoritarian attempts to strip minority protections). But the founding_problem_status is authored as contested because the scope of judicial nullification has, per outside corroboration, expanded well beyond that narrow rights-entrenchment function into broad review of ordinary economic and social policy. Classifying this as tangled_rope rather than snare or mountain prevents two mislabeling errors: treating the entire apparatus as pure extraction would erase the genuine, still-live rights-protection function; treating it as a natural, beneficiary-free mountain would erase the judiciary's and litigators' real structural stake in the doctrine's persistence and scope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    rights_protection_vs_preference_entrenchment,
    'Does judicial supremacy in practice function primarily to protect fundamental rights against majoritarian overreach, or has it become a vehicle for entrenching judicial policy preferences that lack independent constitutional grounding?',
    'Longitudinal analysis of judicial review outcomes distinguishing narrow rights-entrenchment rulings (striking down direct attacks on named constitutional rights) from broad policy-substitution rulings (striking down ordinary economic or social legislation on contested interpretive grounds), correlated with the specificity of constitutional text invoked.',
    'A finding that most nullifications track narrow, textually-grounded rights claims would support the coordination framing; a finding that most nullifications substitute judicial for legislative policy judgment on contested, textually underdetermined questions would support classifying the excess as extraction riding on the rights-protection justification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rights_protection_vs_preference_entrenchment, empirical, 'Whether judicial review''s actual exercise matches its rights-protection justification.').

omega_variable(
    minority_beneficiary_contingency,
    'Is the benefit that rights-bearing minorities receive under judicial supremacy stable, or is it entirely contingent on the current composition of the court and therefore reversible without any change in the minority''s own political power?',
    'Track reversal rates of minority-protective constitutional rulings across changes in court composition, compared to the durability of minority protections secured through ordinary legislation or constitutional amendment.',
    'If minority protections under judicial supremacy prove highly reversible on court composition change alone, the directionality classification of rights-bearing minorities as stable low-d beneficiaries should be revisited toward a more conditional or symmetric position, since the same body that protects them today can unprotect them tomorrow without any exit option available to the minority.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(minority_beneficiary_contingency, conceptual, 'Whether minority benefit under judicial supremacy is a stable structural position or a contingent, reversible one.').

omega_variable(
    kernel_framing_alternative_dissolves_beneficiary,
    'If the kernel were framed not as ''who holds final interpretive authority'' but as ''what mechanism best protects entrenched rights against transient majorities,'' would the judiciary still register as a beneficiary, or would the beneficiary set collapse to just the protected rights-holders with the judiciary as a pure instrumental administrator?',
    'Compare institutional design counterfactuals (e.g., a rights-protection mechanism administered by a rotating citizen panel or sortition body rather than a permanent judiciary) to see whether the same rights-protection function can be delivered without creating an institutional beneficiary with persistent stakes in retaining interpretive finality.',
    'If the rights-protection function is separable from a permanent judicial beneficiary, the extraction component attributable to judicial self-interest (career security, institutional prestige, doctrinal control) is larger than the coordination framing suggests. If inseparable, the judiciary''s beneficiary status is closer to an unavoidable cost of the coordination function itself.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_alternative_dissolves_beneficiary, conceptual, 'Alternative framing of the kernel and its effect on whether the judiciary is a genuine structural beneficiary.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_interpretive_authority__judicial_supremacy_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(cons_tr_t10, constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement(cons_tr_t20, constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 20, 0.2).
narrative_ontology:measurement(cons_tr_t30, constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 30, 0.22).
narrative_ontology:measurement(cons_tr_t40, constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 40, 0.24).
narrative_ontology:measurement(cons_tr_t50, constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 50, 0.26).
narrative_ontology:measurement(cons_tr_t60, constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 60, 0.28).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(cons_be_t10, constitutional_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 10, 0.38).
narrative_ontology:measurement(cons_be_t20, constitutional_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 20, 0.42).
narrative_ontology:measurement(cons_be_t30, constitutional_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 30, 0.46).
narrative_ontology:measurement(cons_be_t40, constitutional_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 40, 0.48).
narrative_ontology:measurement(cons_be_t50, constitutional_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 50, 0.5).
narrative_ontology:measurement(cons_be_t60, constitutional_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 60, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(cons_su_t10, constitutional_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 10, 0.45).
narrative_ontology:measurement(cons_su_t20, constitutional_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 20, 0.49).
narrative_ontology:measurement(cons_su_t30, constitutional_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 30, 0.52).
narrative_ontology:measurement(cons_su_t40, constitutional_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 40, 0.55).
narrative_ontology:measurement(cons_su_t50, constitutional_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 50, 0.57).
narrative_ontology:measurement(cons_su_t60, constitutional_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 60, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_interpretive_authority__judicial_supremacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(constitutional_interpretive_authority__judicial_supremacy_reading, constitutional_interpretive_authority__parliamentary_supremacy_reading).
narrative_ontology:affects_constraint(constitutional_interpretive_authority__judicial_supremacy_reading, constitutional_interpretive_authority__coordinate_construction_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the constitutional_interpretive_authority kernel. constitutional_interpretive_authority__parliamentary_supremacy_reading places final interpretive authority in the elected legislature with no judicial nullification power (beneficiary set: legislature/electoral majorities; victim set: rights-bearing minorities without majoritarian support). constitutional_interpretive_authority__coordinate_construction_reading diffuses final authority across branches via ongoing dialogue, with no single institutional beneficiary of finality. Each reading has a distinct ε, distinct beneficiary/victim declarations, and its own claimed_type — they are not the same constraint measured three ways; per the ε-invariance principle they are three constraints sharing a contested kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
