% ============================================================================
% CONSTRAINT STORY: article_27_veto_power__oligopoly_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_27_veto_power__oligopoly_reading, []).

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
 *   constraint_id: article_27_veto_power__oligopoly_reading
 *   human_readable: UN Security Council P5 Veto as Entrenched Geopolitical Oligopoly
 *   domain: international_relations/institutional_design/constitutional_law
 *
 * SUMMARY:
 *   This story instantiates the oligopoly reading of the Article 27 veto
 *   kernel: the veto and its associated Charter-amendment lock (Article
 *   108/109) are read as a structurally entrenched extraction mechanism
 *   rather than a coordination device. Under this reading, the 1945
 *   great-power settlement froze relative authority shares at a moment of
 *   specific post-war military dominance and then made that freeze
 *   self-protecting by requiring P5 consent to amend it — a closed loop no
 *   external majority can break. The coordination story (preventing forced
 *   great-power confrontation) is not denied outright, but this reading holds
 *   it functions increasingly as cover: veto usage data show a shift from
 *   confrontation-avoidance toward shielding client states and blocking
 *   accountability measures, while the reform process has produced zero
 *   ratified Charter amendments to Council composition or voting since 1965
 *   despite 30+ years of continuous formal negotiation. Two sibling readings
 *   of the same kernel — coordination_reading (veto as war-prevention
 *   necessity) and sovereignty_reading (veto as Westphalian consent principle
 *   applied to great powers) — are NOT part of this story; they are separate
 *   constraint files linked via network.affects_constraints. This story's ε
 *   is authored strictly for the arrangement as this reading sees it
 *   operating, not for any reform alternative.
 *
 * KEY AGENTS:
 *   - p5_permanent_members: primary beneficiary and agenda_setter (institutional/arbitrage) — holds both the operational veto and the amendment veto
 *   - non_p5_un_member_states: primary target (powerless/trapped) — bears costs of blocked Council action with no reform path
 *   - elected_security_council_members: secondary actor (moderate/constrained) — participates without durable power
 *   - aspirant_permanent_seat_states: excluded voice (powerful/trapped) — structurally barred from the reform conversation despite standing to be seated
 *   - un_secretariat_and_reform_commissions: analytical observer — documents the frozen reform record
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_27_veto_power__oligopoly_reading, 0.81).
domain_priors:suppression_score(article_27_veto_power__oligopoly_reading, 0.88).
domain_priors:theater_ratio(article_27_veto_power__oligopoly_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_27_veto_power__oligopoly_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(article_27_veto_power__oligopoly_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(article_27_veto_power__oligopoly_reading, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_27_veto_power__oligopoly_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(article_27_veto_power__oligopoly_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_27_veto_power__oligopoly_reading, snare).
narrative_ontology:human_readable(article_27_veto_power__oligopoly_reading, "UN Security Council P5 Veto as Entrenched Geopolitical Oligopoly").
narrative_ontology:topic_domain(article_27_veto_power__oligopoly_reading, "international_relations/institutional_design/constitutional_law").

domain_priors:requires_active_enforcement(article_27_veto_power__oligopoly_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_27_veto_power__oligopoly_reading, '73adf70f-3269-4862-a925-187a3f5b4e5e').
narrative_ontology:cs_kernel_codification('73adf70f-3269-4862-a925-187a3f5b4e5e', fixed_text).
narrative_ontology:cs_authority_grounding('73adf70f-3269-4862-a925-187a3f5b4e5e', extraction).
narrative_ontology:cs_interpretation_layer_present('73adf70f-3269-4862-a925-187a3f5b4e5e').
narrative_ontology:cs_reading_relation('73adf70f-3269-4862-a925-187a3f5b4e5e', article_27_veto_power__coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('73adf70f-3269-4862-a925-187a3f5b4e5e', article_27_veto_power__sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('73adf70f-3269-4862-a925-187a3f5b4e5e', foundational, fixed_power_distribution_illegitimately_perpetuated).
narrative_ontology:cs_axiom_status(fixed_power_distribution_illegitimately_perpetuated, holdable).
narrative_ontology:cs_axiom_grounding('73adf70f-3269-4862-a925-187a3f5b4e5e', fixed_power_distribution_illegitimately_perpetuated, empirically_contingent).
narrative_ontology:cs_axiom('73adf70f-3269-4862-a925-187a3f5b4e5e', foundational, self_ratifying_amendment_lock_is_extraction_not_prudence).
narrative_ontology:cs_axiom_status(self_ratifying_amendment_lock_is_extraction_not_prudence, holdable).
narrative_ontology:cs_axiom_grounding('73adf70f-3269-4862-a925-187a3f5b4e5e', self_ratifying_amendment_lock_is_extraction_not_prudence, conventional).
narrative_ontology:cs_reference_frame('73adf70f-3269-4862-a925-187a3f5b4e5e', id_1945_victors_settlement).
narrative_ontology:cs_drift_state('73adf70f-3269-4862-a925-187a3f5b4e5e', contemporary_multipolar_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('73adf70f-3269-4862-a925-187a3f5b4e5e', '').
narrative_ontology:cs_kernel_id(article_27_veto_power__oligopoly_reading, article_27_veto_power).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_27_veto_power__oligopoly_reading, p5_permanent_members).
narrative_ontology:constraint_victim(article_27_veto_power__oligopoly_reading, non_p5_un_member_states).
narrative_ontology:constraint_victim(article_27_veto_power__oligopoly_reading, elected_security_council_members).
narrative_ontology:constraint_victim(article_27_veto_power__oligopoly_reading, aspirant_permanent_seat_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(article_27_veto_power__oligopoly_reading, elected_security_council_members).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold a permanent, unreviewable veto over any Security Council resolution, plus a veto over the Charter amendment process that would be needed to remove or dilute their own veto (Article 108/109 requires P5 ratification of amendments). They administer the enforcement architecture — peacekeeping mandates, sanctions regimes, use-of-force authorizations — and can block any of it unilaterally. Their global reach and nuclear-weapons status give them exit from consequences that bind other states. They set the terms of any reform conversation and have never permitted one to reach a vote that could bind them.
narrative_ontology:constraint_stakeholder(article_27_veto_power__oligopoly_reading, p5_permanent_members, agenda_setter,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(article_27_veto_power__oligopoly_reading, p5_permanent_members, beneficiary).

% The 188 non-permanent UN members have no mechanism to compel Security Council action opposed by any single P5 state, regardless of General Assembly majorities, humanitarian consensus, or documented atrocity. Their only recourse — Charter amendment — is itself vetoable by the P5 under Article 108. They bear the costs of unresolved crises (Syria, Myanmar, Israel-Palestine, Ukraine) that a P5 veto has blocked from binding Council action. Exit from the UN system entirely forfeits collective security, development finance access, and diplomatic standing — not a real option for most states.
narrative_ontology:constraint_stakeholder(article_27_veto_power__oligopoly_reading, non_p5_un_member_states, payer,
    powerless, generational, trapped, global).

% The ten rotating elected members (E10) participate in deliberations, hold two-year terms, and can shape agenda items, but any resolution they support can be nullified by a single P5 veto. They gain prestige and diplomatic access from the seat but no durable power — their influence resets every two years while the P5's is permanent. Some benefit from association with the institution's legitimacy while bearing none of the P5's authority.
narrative_ontology:constraint_stakeholder(article_27_veto_power__oligopoly_reading, elected_security_council_members, payer,
    moderate, immediate, constrained, global).
narrative_ontology:stakeholder_secondary_role(article_27_veto_power__oligopoly_reading, elected_security_council_members, beneficiary).

% States such as India, Brazil, Germany, Japan, and the African Union bloc possess population, economic weight, or regional-security roles comparable to or exceeding some P5 members, and have formally sought permanent or veto-bearing seats for decades (G4, L69, Ezulwini Consensus). They are structurally barred from the reform conversation that would seat them, because P5 consent is required for any Charter change and no P5 state has an interest in diluting its own relative position. Their exclusion is not oversight — it is the specific outcome the amendment veto exists to guarantee.
narrative_ontology:constraint_stakeholder(article_27_veto_power__oligopoly_reading, aspirant_permanent_seat_states, excluded,
    powerful, generational, trapped, global).

% Documents the reform debate (Intergovernmental Negotiations on Security Council reform have run continuously since 1993 without a single Charter amendment reaching ratification), publishes analyses of veto usage patterns, and administers the formal amendment machinery without power to invoke it independently.
narrative_ontology:constraint_stakeholder(article_27_veto_power__oligopoly_reading, un_secretariat_and_reform_commissions, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article_27_veto_power__oligopoly_reading, p5_permanent_members).
narrative_ontology:fixing_cost_class(article_27_veto_power__oligopoly_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The veto nominally coordinates great-power consent so that no Council action can compel a P5 state into confrontation it rejects — this is the coordination story the arrangement is dressed in.
% TRANSFER_FUNCTION: The arrangement transfers durable, unreviewable authority over the international peace-and-security order from the UN membership as a whole to five states, and transfers the political and human cost of blocked crisis response onto states and populations with no P5 patron — while the amendment veto transfers the reform question itself away from any body that could act on it.
% ABSENT_VOICES: Aspirant permanent-seat states (India, Brazil, Germany, Japan, African Union) and the broader Global South bloc have argued for reform since the 1990s in the Intergovernmental Negotiations process but have no procedural path to a binding vote; the P5 are simultaneously the defendants and the sole jury on their own case.
% DISAPPEARANCE_RATIONALE: If the veto and its Charter-amendment lock vanished, Council decision-making would shift toward majority or weighted-majority voting, previously blocked resolutions (on Syria, Israel-Palestine, and others) would become newly viable, and the decades-frozen reform process would immediately produce Charter changes redistributing formal authority — the current arrangement is precisely what prevents that rearrangement from occurring.
% FOUNDING_PROBLEM: In 1945, the founding powers required a mechanism to keep the wartime great-power coalition inside the new organization rather than outside it, on the theory that a security architecture the great powers could walk away from would collapse as the League of Nations had.
% FOUNDING_PROBLEM_CORROBORATION: The P5 themselves and allied scholarship maintain the 1945 coalition-retention logic remains live. Independent corroboration from outside the beneficiary group — UN reform commissions, the G4 and L69 negotiating blocs, and academic analyses of eight decades of veto usage data — documents that veto use has shifted overwhelmingly toward protecting P5 client states and blocking accountability measures rather than preventing great-power military confrontation, and that the amendment-lock function (rather than the confrontation-prevention function) is what has actually persisted.
narrative_ontology:disappearance_verdict(article_27_veto_power__oligopoly_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_27_veto_power__oligopoly_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_27_veto_power__oligopoly_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(article_27_veto_power__oligopoly_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_27_veto_power__oligopoly_reading, 0.81, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_27_veto_power__oligopoly_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(article_27_veto_power__oligopoly_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(article_27_veto_power__oligopoly_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored high (0.81 by 2025) because the value P5 states derive — durable, unreviewable authority over the international security order, plus the ability to shield allies from binding action — is structurally decoupled from any service rendered to the broader membership in proportion. Suppression is authored higher still (0.88) because the amendment-veto mechanism does not merely make reform difficult, it makes reform structurally impossible without the consent of exactly the parties who would lose from it — a closed suppression loop rather than an open political contest. Theater ratio is authored as substantial and rising (0.62 by 2025) because the Intergovernmental Negotiations process has run continuously since 1993 while producing zero ratified outcomes, which this reading interprets as a legitimating performance of reformability that outputs no reform. The claimed_type (snare) is stated independently of these metrics per the framework's claim/metric independence rule, though in this case they point the same direction — that alignment is itself part of what this reading asserts.
 *
 * PERSPECTIVAL GAP:
 *   From the P5 seat, the arrangement reads as prudent great-power management that has coincidentally never needed revision. From the non-P5 majority seat, the same structure reads as an extraction mechanism whose defining feature is that its beneficiaries also control the only lever that could end it. The engine computes these as structurally different seat classifications from the same authored data; this reading asserts the payer-seat reading is the structurally accurate one for this constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   P5 states are declared beneficiaries: they collect durable authority rents (agenda control, allied-state shielding, veto over their own accountability) without a compensating cost proportional to that authority, and their exit options (arbitrage — they can act unilaterally or through alternative fora when the Council is blocked) place them near the full-beneficiary end of directionality. Non-P5 states and aspirant powers are declared victims: they are structurally trapped (no viable exit from the UN system, no procedural path to amendment) and bear the costs of unresolved crises the veto blocks from binding action, placing them near the full-target end. Elected Council members sit closer to symmetric — real but transient access, no durable power transfer.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (retaining the wartime coalition inside rather than outside the postwar order) is authored as contested rather than flatly dead, because this reading acknowledges a residual coordination function still operates. But the corroboration record — independent of the P5 themselves — shows the mechanism has drifted from confrontation-prevention toward authority preservation, while the disappearance verdict (world_rearranges) shows real arrangements depend on the constraint continuing. This combination (contested founding-problem status + world_rearranges) is exactly the signature the framework uses to flag a live-but-drifted arrangement rather than either a pure zombie or a pure going-concern — it prevents this reading from either over-claiming pure extraction (ignoring the real coordination residue) or under-claiming genuine entrenchment (treating 80 years of zero ratified reform as coincidence).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_selection_veto_as_snare,
    'This story is one reading (oligopoly_reading) of the contested kernel article_27_veto_power. Two sibling readings — coordination_reading (veto as necessary war-prevention mechanism) and sovereignty_reading (veto as Westphalian consent principle for great powers) — are authored as separate constraint files with their own ε values and classifications. Which reading best captures the veto''s dominant structural function today?',
    'Comparative analysis of veto usage records: count and characterize vetoes cast to prevent forced great-power military confrontation (supporting coordination_reading) versus vetoes cast to shield allied/client states from accountability measures unrelated to P5-state confrontation risk (supporting oligopoly_reading), plus tracking of the Charter amendment process''s structural blockage pattern versus voluntary non-use.',
    'If usage data show confrontation-prevention dominates, the coordination_reading better describes the constraint''s operative function and this story''s high ε would be reading-specific rather than descriptively dominant. If usage data show client-shielding and reform-blocking dominate, this reading''s classification (snare) is the structurally accurate account of current operation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_selection_veto_as_snare, conceptual, 'Committer-frame: this constraint is one reading of the article_27_veto_power kernel; the choice among readings is itself an open question routed here rather than resolved inside the constraint.').

omega_variable(
    amendment_lock_necessity,
    'Is the Article 108/109 requirement of P5 ratification for Charter amendments a structurally necessary feature of any workable great-power-inclusive security order, or is it a specifically self-protecting extraction device distinguishable from ordinary constitutional entrenchment?',
    'Comparative institutional analysis: examine amendment mechanisms in other multilateral security and treaty regimes with great-power participation (NPT, NATO, EU treaty revision) to assess whether P5-style self-ratification of one''s own veto removal is common practice or an outlier requiring specific justification.',
    'If comparable regimes use similar self-ratification locks without producing analogous 80-year reform freezes, the amendment lock alone does not establish extraction — other factors would need to explain the outcome. If the UN case is a structural outlier in both mechanism and outcome, it strengthens the case for the amendment veto itself, not merely the operational veto, as the extraction mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(amendment_lock_necessity, conceptual, 'Whether Charter self-amendment locks are inherently extractive or a common, defensible constitutional design choice.').

omega_variable(
    veto_usage_trend_corroboration,
    'Does the empirical record of veto casts since 1945 support the claim that usage has shifted from confrontation-avoidance toward client-shielding and accountability-blocking, as asserted in the founding_problem_corroboration?',
    'Systematic coding of all Security Council vetoes (1946-2025) by stated and inferred purpose, cross-referenced with independent conflict and accountability databases, conducted by parties outside the P5 diplomatic apparatus.',
    'Confirmed drift would support the oligopoly reading''s central empirical claim; a flat or reversed trend would substantially weaken this reading''s descriptive accuracy relative to the coordination reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(veto_usage_trend_corroboration, empirical, 'Empirical basis for the claimed drift in veto usage function over 80 years.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_27_veto_power__oligopoly_reading, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t1945, article_27_veto_power__oligopoly_reading, theater_ratio, 1945, 0.2).
narrative_ontology:measurement(arti_tr_t1960, article_27_veto_power__oligopoly_reading, theater_ratio, 1960, 0.28).
narrative_ontology:measurement(arti_tr_t1991, article_27_veto_power__oligopoly_reading, theater_ratio, 1991, 0.35).
narrative_ontology:measurement(arti_tr_t2005, article_27_veto_power__oligopoly_reading, theater_ratio, 2005, 0.48).
narrative_ontology:measurement(arti_tr_t2015, article_27_veto_power__oligopoly_reading, theater_ratio, 2015, 0.55).
narrative_ontology:measurement(arti_tr_t2025, article_27_veto_power__oligopoly_reading, theater_ratio, 2025, 0.62).

% Extraction over time
narrative_ontology:measurement(arti_be_t1945, article_27_veto_power__oligopoly_reading, base_extractiveness, 1945, 0.45).
narrative_ontology:measurement(arti_be_t1960, article_27_veto_power__oligopoly_reading, base_extractiveness, 1960, 0.52).
narrative_ontology:measurement(arti_be_t1991, article_27_veto_power__oligopoly_reading, base_extractiveness, 1991, 0.58).
narrative_ontology:measurement(arti_be_t2005, article_27_veto_power__oligopoly_reading, base_extractiveness, 2005, 0.68).
narrative_ontology:measurement(arti_be_t2015, article_27_veto_power__oligopoly_reading, base_extractiveness, 2015, 0.75).
narrative_ontology:measurement(arti_be_t2025, article_27_veto_power__oligopoly_reading, base_extractiveness, 2025, 0.81).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t1945, article_27_veto_power__oligopoly_reading, suppression_requirement, 1945, 0.55).
narrative_ontology:measurement(arti_su_t1960, article_27_veto_power__oligopoly_reading, suppression_requirement, 1960, 0.62).
narrative_ontology:measurement(arti_su_t1991, article_27_veto_power__oligopoly_reading, suppression_requirement, 1991, 0.68).
narrative_ontology:measurement(arti_su_t2005, article_27_veto_power__oligopoly_reading, suppression_requirement, 2005, 0.76).
narrative_ontology:measurement(arti_su_t2015, article_27_veto_power__oligopoly_reading, suppression_requirement, 2015, 0.82).
narrative_ontology:measurement(arti_su_t2025, article_27_veto_power__oligopoly_reading, suppression_requirement, 2025, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_27_veto_power__oligopoly_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(article_27_veto_power__oligopoly_reading, article_27_veto_power__coordination_reading).
narrative_ontology:affects_constraint(article_27_veto_power__oligopoly_reading, article_27_veto_power__sovereignty_reading).
narrative_ontology:affects_constraint(article_27_veto_power__oligopoly_reading, un_security_council_reform_deadlock).
narrative_ontology:affects_constraint(article_27_veto_power__oligopoly_reading, un_general_assembly_authority_limits).

% DUAL FORMULATION NOTE:
% This story is one of three siblings decomposing the natural-language label 'the P5 veto' per the ε-invariance principle: oligopoly_reading (this file, ε=0.81, snare), coordination_reading (separate file, expected low ε, mountain/rope-leaning), and sovereignty_reading (separate file, expected moderate ε, tangled_rope-leaning). All three share the same textual kernel (UN Charter Article 27(3) plus Articles 108/109) but read its function differently. Each carries its own ε assessed by that reading's own lights, per the fixed-referent rule for kernel-reading stories — none of the three authors ε for a reformed alternative arrangement.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
