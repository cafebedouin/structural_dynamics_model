% ============================================================================
% CONSTRAINT STORY: article_27_veto_power__sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_27_veto_power__sovereignty_reading, []).

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
 *   constraint_id: article_27_veto_power__sovereignty_reading
 *   human_readable: UN Charter Article 27(3) Veto — Sovereignty/Consent Reading
 *   domain: international_relations/institutional_design/constitutional_law
 *
 * SUMMARY:
 *   Article 27(3) of the UN Charter requires the concurring vote of all five
 *   permanent Security Council members for non-procedural resolutions. This
 *   story instantiates the sovereignty reading of that provision: the veto is
 *   not a special privilege invented for five states, but the Westphalian
 *   consent principle — no sovereign state can be legally bound without its
 *   consent — applied to the subset of states whose enforcement capacity (and
 *   after 1949, nuclear arsenals) makes non-consensual binding action against
 *   them either impossible to execute or catastrophically risky to attempt.
 *   Under this reading, the veto is a description of an underlying physical
 *   and political fact (great powers cannot be coerced by an institution that
 *   has no independent enforcement capacity exceeding theirs) rather than a
 *   policy choice extracting rent or a safety mechanism against a specific
 *   war-risk scenario. This is one of three readings of the same kernel
 *   (article_27_veto_power): coordination_reading treats the veto as a
 *   war-prevention mechanism; oligopoly_reading treats it as entrenched
 *   extraction. This story is deliberately narrow — it authors ε for the
 *   sovereignty reading's own account of the standing arrangement, not for
 *   what a rights-redistributing alternative institution would look like.
 *
 * KEY AGENTS:
 *   - p5_states: Primary structural referent — hold enforcement capacity that makes non-consensual binding action against them physically and politically unworkable
 *   - non_p5_un_members: Bound by Security Council resolutions the P5 permit, but cannot themselves block resolutions targeting non-P5 states
 *   - un_secretariat: Administers the institution the veto structurally constrains; cannot compel P5 compliance
 *   - international_law_scholars: Analytical observers debating whether the veto is natural fact or constructed privilege
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_27_veto_power__sovereignty_reading, 0.06).
domain_priors:suppression_score(article_27_veto_power__sovereignty_reading, 0.12).
domain_priors:theater_ratio(article_27_veto_power__sovereignty_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_27_veto_power__sovereignty_reading, extractiveness, 0.06).
narrative_ontology:constraint_metric(article_27_veto_power__sovereignty_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(article_27_veto_power__sovereignty_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_27_veto_power__sovereignty_reading, accessibility_collapse, 0.82).
narrative_ontology:constraint_metric(article_27_veto_power__sovereignty_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_27_veto_power__sovereignty_reading, mountain).
narrative_ontology:human_readable(article_27_veto_power__sovereignty_reading, "UN Charter Article 27(3) Veto — Sovereignty/Consent Reading").
narrative_ontology:topic_domain(article_27_veto_power__sovereignty_reading, "international_relations/institutional_design/constitutional_law").

domain_priors:emerges_naturally(article_27_veto_power__sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_27_veto_power__sovereignty_reading, '7603daf9-9a85-41be-a1f4-9147fd6bfbf8').
narrative_ontology:cs_kernel_codification('7603daf9-9a85-41be-a1f4-9147fd6bfbf8', fixed_text).
narrative_ontology:cs_authority_grounding('7603daf9-9a85-41be-a1f4-9147fd6bfbf8', practice).
narrative_ontology:cs_interpretation_layer_present('7603daf9-9a85-41be-a1f4-9147fd6bfbf8').
narrative_ontology:cs_reading_relation('7603daf9-9a85-41be-a1f4-9147fd6bfbf8', article_27_veto_power__coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('7603daf9-9a85-41be-a1f4-9147fd6bfbf8', article_27_veto_power__oligopoly_reading, influences).
narrative_ontology:cs_axiom('7603daf9-9a85-41be-a1f4-9147fd6bfbf8', foundational, consent_is_precondition_of_binding_obligation).
narrative_ontology:cs_axiom_status(consent_is_precondition_of_binding_obligation, holdable).
narrative_ontology:cs_axiom_grounding('7603daf9-9a85-41be-a1f4-9147fd6bfbf8', consent_is_precondition_of_binding_obligation, conventional).
narrative_ontology:cs_axiom('7603daf9-9a85-41be-a1f4-9147fd6bfbf8', secondary, veto_set_should_track_enforcement_capacity_not_1945_outcome).
narrative_ontology:cs_axiom_status(veto_set_should_track_enforcement_capacity_not_1945_outcome, holdable).
narrative_ontology:cs_axiom_grounding('7603daf9-9a85-41be-a1f4-9147fd6bfbf8', veto_set_should_track_enforcement_capacity_not_1945_outcome, empirically_contingent).
narrative_ontology:cs_reference_frame('7603daf9-9a85-41be-a1f4-9147fd6bfbf8', concert_of_great_powers_non_compellability).
narrative_ontology:cs_drift_state('7603daf9-9a85-41be-a1f4-9147fd6bfbf8', contemporary_multipolar_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('7603daf9-9a85-41be-a1f4-9147fd6bfbf8', '').
narrative_ontology:cs_kernel_id(article_27_veto_power__sovereignty_reading, article_27_veto_power).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_27_veto_power__sovereignty_reading, p5_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(article_27_veto_power__sovereignty_reading, non_p5_un_members).
narrative_ontology:constraint_victim(article_27_veto_power__sovereignty_reading, non_p5_un_members).
narrative_ontology:constraint_vindicates(article_27_veto_power__sovereignty_reading, westphalian_consent_principle).
narrative_ontology:constraint_vindicates(article_27_veto_power__sovereignty_reading, non_compellability_of_sovereign_states_absent_consent).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold nuclear arsenals and/or globally deployable enforcement capacity that makes non-consensual binding action against them either physically impossible or civilizationally catastrophic to attempt. Under this reading they occupy this position structurally, not by rule-manipulation — any institution attempting to bind them without consent would face the same underlying capacity gap the Charter merely acknowledges. They participate in Council deliberation, cast vetoes rarely relative to total resolutions, and bear no cost from the arrangement beyond reputational friction when a veto is publicly exercised against wide international sentiment.
narrative_ontology:constraint_stakeholder(article_27_veto_power__sovereignty_reading, p5_states, beneficiary,
    institutional, civilizational, arbitrage, global).

% Bound by Security Council resolutions once adopted, but cannot themselves block a resolution targeting a fellow non-P5 state, and cannot compel the Council to act against a P5 state's interests. Under this reading their situation reflects the same enforcement-capacity asymmetry that binds the whole system — they were never structurally positioned to compel a nuclear power's compliance regardless of voting rules, so the story does not frame them as victims of a rule but as participants in an institution whose founding premise (universal binding authority) was always bounded by that fact. They benefit from the Council's ability to act at all in the many cases where P5 interests are not directly engaged.
narrative_ontology:constraint_stakeholder(article_27_veto_power__sovereignty_reading, non_p5_un_members, payer,
    moderate, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(article_27_veto_power__sovereignty_reading, non_p5_un_members, beneficiary).

% Administers Council procedure, convenes votes, and implements resolutions once passed, but has no independent enforcement capacity to compel a P5 veto-holder; the Secretariat's authority is wholly derivative of member-state consent and cannot be exited from the arrangement it administers without dissolving the institution itself.
narrative_ontology:constraint_stakeholder(article_27_veto_power__sovereignty_reading, un_secretariat_and_council_organs, agenda_setter,
    institutional, civilizational, trapped, global).
narrative_ontology:stakeholder_secondary_role(article_27_veto_power__sovereignty_reading, un_secretariat_and_council_organs, observer).

% Repeatedly advances Uniting-for-Peace mechanisms, veto-initiative resolutions (e.g. requiring a P5 vetoing state to justify itself before the General Assembly), and Charter reform proposals to expand or abolish the veto. Under this reading these efforts are not treated as illegitimate, but as unable to change the underlying capacity asymmetry the veto reflects — reform of the voting rule would not create the enforcement capacity needed to bind a nuclear power without its consent.
narrative_ontology:constraint_stakeholder(article_27_veto_power__sovereignty_reading, general_assembly_reform_coalition, excluded,
    organized, generational, constrained, global).

% Debate whether the veto is best understood as a natural-law-adjacent structural fact about sovereign consent and enforcement capacity, or as a constructed and contingent privilege that could in principle be redesigned. Hold no stake in the outcome beyond scholarly and doctrinal influence.
narrative_ontology:constraint_stakeholder(article_27_veto_power__sovereignty_reading, international_law_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article_27_veto_power__sovereignty_reading, diffuse).
narrative_ontology:fixing_cost_class(article_27_veto_power__sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents the Security Council from issuing binding resolutions that a great power would refuse to comply with, thereby avoiding formal institutional demands the Council has no independent capacity to enforce and that a nuclear-armed target could refuse without practical consequence beyond diplomatic cost.
% TRANSFER_FUNCTION: Under this reading, nothing is transferred that would not already be unavailable: the veto does not move resources or authority from non-P5 states to P5 states, because non-P5 states were never in a position to compel P5 compliance regardless of the formal voting rule. What the arrangement does move is legitimacy — it converts an enforcement-capacity fact into a legally codified consent requirement, which is itself the sovereignty reading's central claim rather than a side effect.
% ABSENT_VOICES: States excluded from permanent membership despite comparable or greater present-day enforcement or economic capacity than some P5 members (Germany, Japan, India, Brazil) would object that a genuinely consistent sovereignty-and-capacity principle should track capacity dynamically rather than freeze it at 1945; they are not seated at the table that could revise Article 108's amendment procedure, since that procedure itself requires P5 concurrence.
% DISAPPEARANCE_RATIONALE: Under the sovereignty reading, formal removal of the veto would not by itself rearrange the underlying enforcement-capacity distribution — a P5 state would still be practically non-compellable without its consent, so the world would substantially resemble its current shape with informal veto-equivalents re-emerging (as in pre-Charter concert-of-power systems). The oligopoly reading disputes this and holds the world would rearrange significantly (redistribution of formal authority, altered Council composition dynamics); the coordination reading holds an intermediate position (removal would raise war risk in specific confrontation scenarios). This story records the dispute rather than resolving it.
% FOUNDING_PROBLEM: How to build a collective security institution capable of binding member states to peace-preserving decisions, while member states — especially those capable of independently prosecuting or resisting large-scale war — retain the sovereign prerogative not to be bound without their own consent.
% FOUNDING_PROBLEM_CORROBORATION: Independent public international law scholarship (outside P5 governments) — including work tracing the veto's lineage to Congress-of-Vienna-era great-power concert practice predating the UN by over a century — attests that non-consensual binding of militarily dominant states has never been durably achieved by any prior collective security architecture (the League of Nations' unanimity requirement and its collapse are cited as the negative case). This corroboration comes from historians and international lawyers with no direct stake in P5 veto retention, though it is contested by scholars in the oligopoly tradition who attest the problem was substantially resolved by mid-20th-century nuclear deterrence architecture independent of the Charter, making the veto's persistence now serve entrenchment rather than the original problem.
narrative_ontology:disappearance_verdict(article_27_veto_power__sovereignty_reading, contested).
narrative_ontology:founding_problem_status(article_27_veto_power__sovereignty_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_27_veto_power__sovereignty_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(article_27_veto_power__sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_27_veto_power__sovereignty_reading, 0.06, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_27_veto_power__sovereignty_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(article_27_veto_power__sovereignty_reading, ExtMetricName, E),
    domain_priors:suppression_score(article_27_veto_power__sovereignty_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(article_27_veto_power__sovereignty_reading),
    narrative_ontology:constraint_metric(article_27_veto_power__sovereignty_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(article_27_veto_power__sovereignty_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(article_27_veto_power__sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored near-zero (0.06) because, under this reading, no rent is being extracted from a genuine coordination surplus — the constraint describes what would be true of ANY global institution attempting to bind an enforcement-capable state without its consent, veto or no veto. Suppression is low (0.12): the veto suppresses nothing that a non-vetoed Council could actually deliver against a P5 state, since enforcement capacity, not voting rule, is the binding constraint. Accessibility collapse is high (0.82): once the underlying enforcement-capacity asymmetry is understood, there is no real alternative institutional design that escapes it — removing the formal veto would not create the capacity to compel a nuclear power, it would only remove the honesty of saying so. Resistance is low (0.2): sustained challenge exists (Uniting for Peace resolutions, General Assembly veto-initiative debates, ICJ advisory opinions) but has never produced a binding alternative, consistent with the mountain reading's claim that the resistance is real but structurally unable to change the underlying fact.
 *
 * DIRECTIONALITY LOGIC:
 *   P5 states are declared as beneficiaries to trigger honest FSM evaluation — the sovereignty reading's own account holds they are not extracting a rent but simply occupying the position any enforcement-capable state would occupy; the beneficiary declaration exists so the engine can test whether that self-account survives contact with the metrics, not to smuggle in an extraction claim under a different name. No victims are declared under this reading: the sovereignty reading's structural claim is precisely that there is no victim, because no one is owed compliance they are not receiving — a state that cannot compel a nuclear power was never entitled to compel it. The vindicated propositions (Westphalian consent, non-compellability absent consent) are doctrines the constraint's operation supports, not actors who collect anything.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — how to have a collective security body when member states retain sovereign non-compellability — is read here as still live rather than resolved-and-outlived: enforcement capacity asymmetry between P5 and non-P5 states has not disappeared (if anything nuclear proliferation among P5 states has hardened it), so under this reading there is no mandatrophy to resolve. This is precisely why the reading is contested: the oligopoly reading would say the founding problem (post-1945 great-power reconstruction) is dead while the arrangement persists — the sibling readings disagree with this story's own genealogy answer, and that disagreement is preserved as an omega rather than adjudicated here.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_entrenchment,
    'Is the veto a structural inevitability that any global institution would face given nuclear-armed, globally-capable great powers — or is it a constructed constitutional choice (1945 drafting decisions) that happens to also track a real power distribution, and that benefits the P5 as identifiable agents regardless of its naturalness?',
    'Counterfactual institutional design analysis: would a Security Council built without a formal veto but facing the same underlying power asymmetry converge on informal veto-equivalent behavior (as concert-of-powers systems historically have), or would formal removal of the veto actually change great-power compliance behavior? Comparative study of pre-Charter concert systems (Congress of Vienna, League Council unanimity) that lacked codified veto language but exhibited functionally identical non-compellability.',
    'If informal veto-equivalents reliably re-emerge whenever enforcement-capable states are asked to submit to binding collective action without consent, this supports the mountain reading — the codification is downstream of a physical fact, not upstream of it. If formal removal would in fact change behavior (because enforcement capacity is itself partly constituted by the legitimacy the Charter confers), the beneficiary declaration signals a constructed entrenchment and the sovereignty reading is doing cover-story work for the oligopoly reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_entrenchment, conceptual, 'Whether the veto''s non-compellability tracks physical power distribution (mountain) or is a constructed rule that happens to coincide with power distribution while also benefiting P5 states as agents.').

omega_variable(
    kernel_reading_location_of_disagreement,
    'Where exactly do the sovereignty, coordination, and oligopoly readings of Article 27(3) diverge structurally, given they describe the same textual provision?',
    'The three readings disagree not about what the veto does (block non-consensual binding action against a P5 state) but about WHY that fact is normatively and structurally significant: sovereignty_reading treats it as the Westphalian consent principle universally applied (any state, scaled to enforcement capacity); coordination_reading treats it as a war-prevention safety valve specific to nuclear confrontation risk; oligopoly_reading treats it as rent-extracting entrenchment specific to five historically-contingent winners of 1945. Resolution requires historical-counterfactual work: would a sovereignty-only principle generate exactly five vetoes, or would it generate a veto proportional to enforcement capacity that changes membership over time (Germany, Japan, India, Brazil excluded despite comparable or greater present capacity than some P5 members)?',
    'If the veto set tracked enforcement capacity dynamically, sovereignty_reading would be strongly supported and oligopoly_reading weakened. Because the veto set is instead fixed to 1945 victors regardless of subsequent capacity shifts (China''s early weakness, France''s postwar diminishment, the exclusion of later-capable states), the sovereignty reading''s own logic implies the CURRENT veto holder set should have drifted with capacity — its fixedness is evidence for the oligopoly reading''s entrenchment claim, not decisive against the sovereignty reading''s core principle.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_location_of_disagreement, conceptual, 'Documents where the three kernel readings of Article 27(3) actually diverge — not on the mechanism, but on whether the mechanism is universal-principled, safety-functional, or historically-entrenched.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_27_veto_power__sovereignty_reading, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_27_veto_power__sovereignty_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(article_27_veto_power__sovereignty_reading, article_27_veto_power__coordination_reading).
narrative_ontology:affects_constraint(article_27_veto_power__sovereignty_reading, article_27_veto_power__oligopoly_reading).

% DUAL FORMULATION NOTE:
% This story is one of three readings of the article_27_veto_power kernel, decomposed per the ε-invariance principle rather than authored as a single observable-dependent constraint. sovereignty_reading (this file) claims mountain with near-zero ε — the veto as a structural fact about consent and enforcement capacity. coordination_reading claims a war-prevention safety-valve function with low-moderate ε concentrated on nuclear-confrontation scenarios. oligopoly_reading claims tangled_rope or snare with substantial ε — the veto as entrenched rent extraction by 1945's victors against later-capable excluded states and against Council efficacy generally. All three describe the identical textual provision and voting mechanism; they diverge entirely on the normative and causal significance of that mechanism's persistence, which is why they are authored as three linked constraints rather than one constraint with a hedged ε.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
