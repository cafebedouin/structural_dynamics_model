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
    narrative_ontology:suppression_profile/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: article_27_veto_power__sovereignty_reading
 *   human_readable: P5 Veto as Westphalian Non-Consent Principle Applied to Enforcement-Capable States
 *   domain: international_relations/institutional_design/constitutional_law
 *
 * SUMMARY:
 *   This story instantiates the sovereignty_reading of the
 *   article_27_veto_power kernel: the P5 veto read as a formal expression of
 *   the Westphalian principle that no sovereign state can be bound by
 *   international law without its consent, applied to the specific class of
 *   states possessing independent global-reach enforcement capacity
 *   (historically, and now including nuclear deterrent capacity). Under this
 *   reading the veto is not a grant of privilege by the Charter but a
 *   recognition of an enforcement asymmetry that exists independent of any
 *   institutional design — any global body empowered to compel a
 *   nuclear-armed great power into action it rejected would face the
 *   identical coordination failure the Charter's drafters faced in 1945,
 *   because the underlying fact (no external actor can reliably compel such a
 *   state without war) is a fact about the distribution of physical and
 *   military capacity, not about Charter text. This is a DIFFERENT constraint
 *   from the sibling coordination_reading (P5 veto as the negotiated
 *   mechanism specifically preventing great-power war by blocking involuntary
 *   military entanglement) and the sibling oligopoly_reading (P5 veto as
 *   entrenched extraction of authority rents via Charter immutability) —
 *   those readings share the label 'the veto' but author different
 *   beneficiary structures, different ε, and different persistence
 *   mechanisms. Per the ε-invariance principle they are separate constraint
 *   files linked via network edges, not measurement bases on one constraint.
 *
 * KEY AGENTS:
 *   - permanent_five_states: beneficiary seat under this reading, but the reading's claim is that the 'benefit' is simply a correct description of pre-existing enforcement asymmetry, not an extracted advantage
 *   - elected_council_members: observer — their limited coercive leverage over P5 states reflects the same underlying enforcement-capacity fact, not an artificial institutional exclusion, on this reading
 *   - un_member_states_generally: observer — bound within ordinary consent-based international law; the general condition, not a special deprivation
 *   - international_law_scholars_sovereigntist_tradition: analytical observer situating the veto within Westphalian doctrine going back before the UN
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_27_veto_power__sovereignty_reading, 0.08).
domain_priors:suppression_score(article_27_veto_power__sovereignty_reading, 0.15).
domain_priors:theater_ratio(article_27_veto_power__sovereignty_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_27_veto_power__sovereignty_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(article_27_veto_power__sovereignty_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(article_27_veto_power__sovereignty_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_27_veto_power__sovereignty_reading, accessibility_collapse, 0.82).
narrative_ontology:constraint_metric(article_27_veto_power__sovereignty_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_27_veto_power__sovereignty_reading, mountain).
narrative_ontology:human_readable(article_27_veto_power__sovereignty_reading, "P5 Veto as Westphalian Non-Consent Principle Applied to Enforcement-Capable States").
narrative_ontology:topic_domain(article_27_veto_power__sovereignty_reading, "international_relations/institutional_design/constitutional_law").

domain_priors:emerges_naturally(article_27_veto_power__sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_27_veto_power__sovereignty_reading, 'a142584b-1135-4743-a718-65412a3cb95c').
narrative_ontology:cs_kernel_codification('a142584b-1135-4743-a718-65412a3cb95c', fixed_text).
narrative_ontology:cs_authority_grounding('a142584b-1135-4743-a718-65412a3cb95c', lineage).
narrative_ontology:cs_interpretation_layer_present('a142584b-1135-4743-a718-65412a3cb95c').
narrative_ontology:cs_reading_relation('a142584b-1135-4743-a718-65412a3cb95c', article_27_veto_power__coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('a142584b-1135-4743-a718-65412a3cb95c', article_27_veto_power__oligopoly_reading, influences).
narrative_ontology:cs_axiom('a142584b-1135-4743-a718-65412a3cb95c', foundational, no_binding_obligation_without_state_consent).
narrative_ontology:cs_axiom_status(no_binding_obligation_without_state_consent, holdable).
narrative_ontology:cs_axiom_grounding('a142584b-1135-4743-a718-65412a3cb95c', no_binding_obligation_without_state_consent, conventional).
narrative_ontology:cs_axiom('a142584b-1135-4743-a718-65412a3cb95c', foundational, enforcement_capacity_asymmetry_is_physical_not_institutional).
narrative_ontology:cs_axiom_status(enforcement_capacity_asymmetry_is_physical_not_institutional, holdable).
narrative_ontology:cs_axiom_grounding('a142584b-1135-4743-a718-65412a3cb95c', enforcement_capacity_asymmetry_is_physical_not_institutional, empirically_contingent).
narrative_ontology:cs_reference_frame('a142584b-1135-4743-a718-65412a3cb95c', westphalian_consent_baseline).
narrative_ontology:cs_drift_state('a142584b-1135-4743-a718-65412a3cb95c', contemporary_multipolar_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('a142584b-1135-4743-a718-65412a3cb95c', '').
narrative_ontology:cs_kernel_id(article_27_veto_power__sovereignty_reading, article_27_veto_power).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_27_veto_power__sovereignty_reading, permanent_five_states).
narrative_ontology:constraint_vindicates(article_27_veto_power__sovereignty_reading, state_consent_doctrine).
narrative_ontology:constraint_vindicates(article_27_veto_power__sovereignty_reading, sovereign_equality_of_enforcement_incapacity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Each holds independent capacity to defeat any resolution that would purport to bind it without its consent. From the sovereignty reading, this is not a privilege granted by the Charter but a formal recognition that no external body can compel a state possessing independent nuclear deterrent and global enforcement reach to act against its will — the veto names a pre-existing fact about what can be enforced, it does not create the fact.
narrative_ontology:constraint_stakeholder(article_27_veto_power__sovereignty_reading, permanent_five_states, beneficiary,
    institutional, civilizational, arbitrage, global).

% Participate in Council deliberation and can shape agenda and pressure but cannot compel a P5 state to accept a binding obligation it has not consented to. Under the sovereignty reading their limited leverage reflects their own more limited independent enforcement capacity, not an artificial exclusion — no amount of institutional redesign changes what they can physically compel.
narrative_ontology:constraint_stakeholder(article_27_veto_power__sovereignty_reading, elected_council_members, observer,
    moderate, biographical, constrained, global).

% Bound by Council resolutions that a P5 state does not veto, but cannot bind a P5 state that objects. Under this reading their position mirrors the general condition of international law: obligations arise from consent, and consent cannot be manufactured by majority vote against a state with the material capacity to refuse compliance and survive the refusal.
narrative_ontology:constraint_stakeholder(article_27_veto_power__sovereignty_reading, un_member_states_generally, observer,
    powerless, generational, analytical, global).

% Analyze the veto as the Charter-era codification of a principle traceable to Westphalia: no sovereign is bound without consent. They read Article 27 as an honest acknowledgment of an enforcement asymmetry that predates and would survive the UN Charter itself, rather than as an institutional design choice that could have been otherwise.
narrative_ontology:constraint_stakeholder(article_27_veto_power__sovereignty_reading, international_law_scholars_sovereigntist_tradition, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Formally distinguishes resolutions that carry binding force from those that do not, based on whether every state with independent great-power enforcement capacity has consented — preventing the Council from issuing paper obligations that cannot be enforced and would therefore corrode the credibility of everything else it does.
% TRANSFER_FUNCTION: Under this reading, nothing is transferred that was not already the case: the veto does not move authority from anyone to the P5, it registers the pre-existing fact that only states with independent enforcement capacity can be compelled to act by consent-based international law in the first place.
% ABSENT_VOICES: Smaller and middle-power states experience the veto as exclusion from binding-decision authority; the sovereignty reading holds that their exclusion from unilateral coercive capacity is a physical and historical fact the Charter records rather than manufactures, though this claim is precisely what the sibling oligopoly reading disputes.
% DISAPPEARANCE_RATIONALE: If Article 27's veto text were struck from the Charter tomorrow, the underlying fact it names would not change: no coalition could compel a nuclear-armed, globally enforcement-capable state to comply with a resolution it rejected, because compliance would still require either its consent or war with it. A Council resolution 'binding' such a state without its consent would be unenforceable on the same terms it is unenforceable today — the formal veto right is a name for a standing physical constraint, not the constraint's source.
% FOUNDING_PROBLEM: In 1945 the drafters needed a Charter that major military powers would actually join and remain inside of, given that none of them could be forced to comply with a body that could bind them against their will while they retained independent means to defy it.
% FOUNDING_PROBLEM_CORROBORATION: Realist international-relations scholarship outside the P5's own diplomatic services (e.g., analyses of why the League of Nations collapsed without a comparable consent-safeguard for great powers) corroborates that great-power enforcement asymmetry, not merely P5 preference, has continued to make consent-based limits on great-power action a live design constraint for every subsequent international security arrangement, including bodies the P5 did not design.
narrative_ontology:disappearance_verdict(article_27_veto_power__sovereignty_reading, world_unchanged).
narrative_ontology:founding_problem_status(article_27_veto_power__sovereignty_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_27_veto_power__sovereignty_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(article_27_veto_power__sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_27_veto_power__sovereignty_reading, 0.08, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored near-zero (0.08) because, on this reading, no rent is being collected — the P5's capacity to defeat binding action against their will exists whether or not Article 27 is written down; the Charter provision names a physical fact rather than manufacturing an artificial advantage. Suppression is low (0.15) because the constraint's persistence does not depend on coercing anyone into accepting it — states join and remain in the UN system with this feature understood, and no alternative arrangement could compel a nuclear-armed enforcement-capable state without recourse to war, which the drafters and successors alike have avoided. Accessibility collapse is high (0.82): once the enforcement-capacity asymmetry is understood, no institutional alternative that would bind P5 states without consent is genuinely available short of a change in the underlying military and economic power distribution — this is exactly the mountain signature. Resistance is low (0.2): sustained resistance to the veto principle itself (as opposed to resistance to its exercise in particular cases) has not produced any viable alternative structure, consistent with the claim that removing the textual veto would not remove the underlying constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Under this reading the P5 are declared as beneficiaries for FSM-detection purposes — the schema requires this when a mountain claim carries beneficiaries — but the reading's own position is that this is a false positive for the FSM signature: the P5 are not extracting rent from a constructed rule, they are simply the class of agents for whom the underlying physical fact (independent enforcement capacity) already holds. No victim group is declared because, on this reading, no one is being deprived of anything they could otherwise have received — the coordination failure a global compulsion mechanism would face is symmetric and would recur under any institutional alternative.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (get major military powers to join and remain inside a Charter body without fear of being bound against their will) is authored as still live: nuclear deterrence and global enforcement-capacity asymmetry have not disappeared since 1945, and no successor security architecture (including regional and coalition-based ones) has found a mechanism for binding a great power without either its consent or war. This blocks a mandatrophy verdict on this reading — the arrangement has not outlived its function, because the function (naming a persistent physical limit on compellability) is not decoupled from present conditions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    physical_necessity_vs_constructed_privilege,
    'Is the P5 veto a naming of a physical/enforcement-capacity fact that would hold regardless of Charter text (the sovereignty reading), or is it a constructed institutional privilege that could in principle be redesigned away without changing the underlying distribution of power (as the oligopoly reading holds)?',
    'Examine counterfactual cases where enforcement-capable states operated under binding multilateral obligations without an explicit consent-veto (e.g., regional collective-security arrangements involving nuclear states) to test whether compliance without veto-equivalent protection has ever been sustained absent war or threat of war.',
    'If sustained binding compliance without a consent-veto has occurred among comparably enforcement-capable states, the sovereignty reading''s mountain claim weakens substantially and the oligopoly reading''s constructed-privilege claim gains support; if no such case exists, the mountain claim is corroborated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(physical_necessity_vs_constructed_privilege, conceptual, 'Whether veto persistence reflects physical necessity or constructed entrenchment — the core committer disagreement across the kernel''s readings.').

omega_variable(
    beneficiary_declaration_fsm_tension,
    'Does declaring permanent_five_states as a beneficiary on a mountain-claimed constraint correctly trigger FSM scrutiny, or does the sovereignty reading''s claim that ''benefit'' here just means ''accurately described by the rule'' defeat the FSM signature''s premise?',
    'Compare the P5''s position under this reading to a genuine natural-law case with no beneficiary (e.g., gravity) versus a false-summit case (e.g., a corporation benefiting from a labor dynamic framed as natural): assess whether the P5 could exercise the same effective non-compellability in a counterfactual Charter that named a different formal mechanism (e.g., supermajority thresholds calibrated to exclude no single state) — if so, the ''natural fact'' framing is doing more work than it can bear.',
    'If the FSM signature fires and the engine reclassifies this reading toward tangled_rope, that reclassification is exactly the measurement the corpus exists to take — a claimed mountain that computes as extractive when beneficiaries are honestly declared. This does not falsify the reading; it documents where the reading''s claim and the metrics diverge.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_declaration_fsm_tension, conceptual, 'Whether declaring the P5 as beneficiaries is compatible with the sovereignty reading''s mountain claim or is itself evidence against it.').

omega_variable(
    cs_framing_kernel_vs_legitimacy_layer,
    'Is the relevant kernel the Charter text (Article 27) itself, or the deeper legitimacy claim layered above it — the Westphalian consent doctrine that the Charter is read as merely codifying? Under the first framing, authority_grounding is naturally ''formalized text administered by UN organs''; under the second, it is ''a pre-Charter customary-international-law doctrine the Charter cannot override.''',
    'Examine whether states or tribunals have ever treated the consent principle as binding on the Council independent of Charter text (e.g., in disputes over Charter amendment procedure or ICJ advisory opinions referencing sovereign equality) — if the doctrine is invoked as freestanding customary law, the second framing is operative.',
    'Under the Charter-text framing, cs_structure.kernel_codification is fixed_text with authority_grounding lineage (UN organs administering Charter text); under the customary-doctrine framing, kernel_codification would be closer to distributed/implicit with authority_grounding practice (state practice constituting the doctrine). This story adopts the fixed_text/lineage framing as the more tractable and better-evidenced one, but the alternative would shift the reading''s classification pathway.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cs_framing_kernel_vs_legitimacy_layer, conceptual, 'Alternative framings of the kernel — Charter text versus the customary sovereignty doctrine it is read as codifying — and which the author selected.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_27_veto_power__sovereignty_reading, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t1945, article_27_veto_power__sovereignty_reading, theater_ratio, 1945, 0.08).
narrative_ontology:measurement(arti_tr_t1960, article_27_veto_power__sovereignty_reading, theater_ratio, 1960, 0.09).
narrative_ontology:measurement(arti_tr_t1975, article_27_veto_power__sovereignty_reading, theater_ratio, 1975, 0.09).
narrative_ontology:measurement(arti_tr_t1990, article_27_veto_power__sovereignty_reading, theater_ratio, 1990, 0.1).
narrative_ontology:measurement(arti_tr_t2005, article_27_veto_power__sovereignty_reading, theater_ratio, 2005, 0.1).
narrative_ontology:measurement(arti_tr_t2025, article_27_veto_power__sovereignty_reading, theater_ratio, 2025, 0.1).

% Extraction over time
narrative_ontology:measurement(arti_be_t1945, article_27_veto_power__sovereignty_reading, base_extractiveness, 1945, 0.06).
narrative_ontology:measurement(arti_be_t1960, article_27_veto_power__sovereignty_reading, base_extractiveness, 1960, 0.07).
narrative_ontology:measurement(arti_be_t1975, article_27_veto_power__sovereignty_reading, base_extractiveness, 1975, 0.07).
narrative_ontology:measurement(arti_be_t1990, article_27_veto_power__sovereignty_reading, base_extractiveness, 1990, 0.08).
narrative_ontology:measurement(arti_be_t2005, article_27_veto_power__sovereignty_reading, base_extractiveness, 2005, 0.08).
narrative_ontology:measurement(arti_be_t2025, article_27_veto_power__sovereignty_reading, base_extractiveness, 2025, 0.08).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(article_27_veto_power__sovereignty_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(article_27_veto_power__sovereignty_reading, article_27_veto_power__coordination_reading).
narrative_ontology:affects_constraint(article_27_veto_power__sovereignty_reading, article_27_veto_power__oligopoly_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the article_27_veto_power kernel. sovereignty_reading (this file) authors the veto as a Mountain: a formal recognition of pre-existing enforcement-capacity asymmetry, ε near-zero, no victim class, high accessibility collapse, low resistance. coordination_reading authors the same textual provision as a negotiated Rope/Tangled-Rope solving a genuine great-power-war-prevention coordination problem, with a narrower and more contingent justification than physical necessity. oligopoly_reading authors it as a Tangled-Rope-or-Snare: entrenched extraction of authority rents via Charter immutability, with an explicit victim class of excluded states and elected-but-non-veto members, and substantially higher ε and suppression. The three stories share the same textual object (Article 27) but diverge sharply in claimed_type, ε, beneficiary/victim structure, and persistence mechanism — exactly the situation the ε-invariance principle requires decomposing into separate files rather than reconciling into one averaged claim.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
