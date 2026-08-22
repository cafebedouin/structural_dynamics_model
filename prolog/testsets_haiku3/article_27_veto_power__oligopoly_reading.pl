% ============================================================================
% CONSTRAINT STORY: article_27_veto_power__oligopoly_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: article_27_veto_power__oligopoly_reading
 *   human_readable: Article 27 P5 Veto Power—Oligopoly Reading
 *   domain: geopolitical/institutional
 *
 * SUMMARY:
 *   The UN Security Council's Article 27 veto grants each of the five
 *   permanent members absolute power to block any resolution. Under the
 *   oligopoly reading, this is not a mechanism for preventing great-power war
 *   (the coordination reading) or a principle of Westphalian sovereignty (the
 *   sovereignty reading), but rather a structural device that entrench the
 *   1945 distribution of geopolitical authority while blocking the
 *   institutional evolution necessary to reflect current reality. The reading
 *   frames the veto as a snare: the P5 use Charter immutability (Article 108
 *   requires P5 consent for amendments) to lock themselves in and block
 *   non-P5 majorities from restructuring the institution, despite 80 years of
 *   geopolitical change that has made that distribution obsolete. The veto
 *   blocks its own removal, making it a bootstrapped lock on power. Non-P5
 *   states are trapped: they cannot exit (UN participation is nominal-valued
 *   but loss of voice is costly), cannot reform (any amendment requires P5
 *   consent), and cannot achieve majority-rule governance (the veto blocks
 *   that too). The P5 extract authority rents—unilateral veto over
 *   international action, immunity from enforcement, and the power to shape
 *   global security policy—while using the Charter's immutability to prevent
 *   redistribution.
 *
 * KEY AGENTS:
 *   - Permanent Five Council States (P5: US, Russia, UK, France, China): Agenda-setters and primary beneficiaries. Use the veto to block action against their interests (military interventions, regional allies, human rights violations) and to prevent institutional reform that would dilute their privilege.
 *   - Non-Aligned Majority (~150 UN members): Organized payers. Cannot reform Article 27 without P5 consent; cannot exit the UN without losing what limited voice they retain. Face indefinite institutional stagnation.
 *   - Emerging Powers (India, Brazil, Indonesia, Nigeria): Powerful but excluded payers. Large, consequential states with no Security Council seat, constrained exit (competing blocs exist but lack hard enforcement power). Bear the cost of a system that treats nuclear status as the sole criterion for voice.
 *   - Reform Constituencies (General Assembly coalitions, civil society, reformist governments): Excluded payers. Advocate for expansion or veto weakening but cannot act without P5 consent. Trapped by the amendment rule (Article 108).
 *   - Article 108 (Charter amendment requirement): The structural mechanism that locks the veto in place. Not an agent but a constraint-on-the-constraint: immutability requires P5 consent, making the veto immune to majority-rule reform.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_27_veto_power__oligopoly_reading, 0.81).
domain_priors:suppression_score(article_27_veto_power__oligopoly_reading, 0.78).
domain_priors:theater_ratio(article_27_veto_power__oligopoly_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_27_veto_power__oligopoly_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(article_27_veto_power__oligopoly_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(article_27_veto_power__oligopoly_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_27_veto_power__oligopoly_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(article_27_veto_power__oligopoly_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_27_veto_power__oligopoly_reading, snare).
narrative_ontology:human_readable(article_27_veto_power__oligopoly_reading, "Article 27 P5 Veto Power—Oligopoly Reading").
narrative_ontology:topic_domain(article_27_veto_power__oligopoly_reading, "geopolitical/institutional").

domain_priors:requires_active_enforcement(article_27_veto_power__oligopoly_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_27_veto_power__oligopoly_reading, '61166020-e53b-46ae-98d3-9cba83bb27e4').
narrative_ontology:cs_kernel_codification('61166020-e53b-46ae-98d3-9cba83bb27e4', formalized).
narrative_ontology:cs_authority_grounding('61166020-e53b-46ae-98d3-9cba83bb27e4', extraction).
narrative_ontology:cs_interpretation_layer_present('61166020-e53b-46ae-98d3-9cba83bb27e4').
narrative_ontology:cs_reading_relation('61166020-e53b-46ae-98d3-9cba83bb27e4', article_27_veto_power__coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('61166020-e53b-46ae-98d3-9cba83bb27e4', article_27_veto_power__sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('61166020-e53b-46ae-98d3-9cba83bb27e4', foundational, veto_as_oligopoly_entrenchment).
narrative_ontology:cs_axiom_status(veto_as_oligopoly_entrenchment, holdable).
narrative_ontology:cs_axiom_grounding('61166020-e53b-46ae-98d3-9cba83bb27e4', veto_as_oligopoly_entrenchment, empirically_contingent).
narrative_ontology:cs_axiom('61166020-e53b-46ae-98d3-9cba83bb27e4', foundational, charter_immutability_locks_p5_privilege).
narrative_ontology:cs_axiom_status(charter_immutability_locks_p5_privilege, holdable).
narrative_ontology:cs_axiom_grounding('61166020-e53b-46ae-98d3-9cba83bb27e4', charter_immutability_locks_p5_privilege, deontological).
narrative_ontology:cs_reference_frame('61166020-e53b-46ae-98d3-9cba83bb27e4', post_1945_great_power_consensus).
narrative_ontology:cs_drift_state('61166020-e53b-46ae-98d3-9cba83bb27e4', contemporary_geopolitical_realignment, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('61166020-e53b-46ae-98d3-9cba83bb27e4', '').
narrative_ontology:cs_kernel_id(article_27_veto_power__oligopoly_reading, article_27_veto_power).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_27_veto_power__oligopoly_reading, permanent_five_council_states).
narrative_ontology:constraint_victim(article_27_veto_power__oligopoly_reading, non_aligned_majority).
narrative_ontology:constraint_victim(article_27_veto_power__oligopoly_reading, emerging_powers).
narrative_ontology:constraint_victim(article_27_veto_power__oligopoly_reading, institutional_reform_constituencies).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Possess absolute veto power over Security Council action. Can block any resolution condemning their own conduct, any resolution that constrains their strategic interests, and any amendment to the Charter that would dilute their privilege. Extract authority rents: the veto enables unilateral action (military intervention, selective enforcement of international law) that would be impossible under majority-rule governance. Maintain their position by opposing every proposal for Charter reform, despite 80+ years of geopolitical change that has made the 1945 distribution obsolete.
narrative_ontology:constraint_stakeholder(article_27_veto_power__oligopoly_reading, permanent_five_council_states, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Represent ~150 UN member states with no Security Council seat and no meaningful recourse when a P5 member blocks action in their interest. Their exit option—leaving the UN—is nominal: the organization's legitimacy depends on universal membership, and exit removes them from even the limited voice they retain. They pay through foreclosed reform: any amendment to Article 27 requires P5 consent, giving each permanent member unilateral veto over their own privilege.
narrative_ontology:constraint_stakeholder(article_27_veto_power__oligopoly_reading, non_aligned_majority, payer,
    organized, generational, trapped, global).

% Include states like India, Brazil, Nigeria, and Indonesia—large, consequential, with regional or continental authority—that have no Security Council seat despite representing vast populations. Their exit options are constrained: they can form competing blocs (African Union, BRICS) but the Security Council's hard enforcement power (Chapter VII) and permanent seat's diplomatic weight remain unreachable. They bear the cost of a system that treats 1945 nuclear-power status as the sole criterion for voice.
narrative_ontology:constraint_stakeholder(article_27_veto_power__oligopoly_reading, emerging_powers, payer,
    powerful, generational, constrained, global).

% Include General Assembly coalitions, civil society networks, and reformist governments that advocate for expanding the Security Council, rotating seats, or weakening the veto. They are structurally excluded from the decision—any reform requires P5 consensus, meaning their voice can be heard but never acted on. They pay through indefinite institutional stagnation: the veto blocks evolution that would reflect current geopolitical reality.
narrative_ontology:constraint_stakeholder(article_27_veto_power__oligopoly_reading, institutional_reform_constituencies, payer,
    organized, generational, trapped, global).
narrative_ontology:stakeholder_secondary_role(article_27_veto_power__oligopoly_reading, institutional_reform_constituencies, excluded).

% UN Charter Article 108 requires P5 consent for any amendment. This is not an agent but the structural mechanism that locks the veto in place: it is the constraint-on-the-constraint that makes the veto immutable without the veto-holders' agreement.
narrative_ontology:constraint_stakeholder(article_27_veto_power__oligopoly_reading, charter_amendment_requirement, observer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(article_27_veto_power__oligopoly_reading, charter_amendment_requirement).

% The legal doctrine that treats the P5 veto as a 'special responsibility' and the Charter as a founding achievement—doctrines the P5 cite when defending the veto. This is a vindicated proposition, not an agent; the veto's persistence is supported by this intellectual frame.
narrative_ontology:constraint_stakeholder(article_27_veto_power__oligopoly_reading, international_law_consensus, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(article_27_veto_power__oligopoly_reading, international_law_consensus).

% The authority of the Security Council to enforce its resolutions depends on perceived legitimacy across member states. The veto's use to block action on human rights, conflict prevention, or enforcement of international law erodes this legitimacy, yet the veto cannot be removed without P5 consent. Legitimacy decays as a side effect of the extraction.
narrative_ontology:constraint_stakeholder(article_27_veto_power__oligopoly_reading, security_council_legitimacy, payer,
    analytical, generational, analytical, global).
narrative_ontology:stakeholder_non_agent(article_27_veto_power__oligopoly_reading, security_council_legitimacy).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article_27_veto_power__oligopoly_reading, permanent_five_council_states).
narrative_ontology:fixing_cost_class(article_27_veto_power__oligopoly_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The veto prevents the UN from mandating military action (Chapter VII) that would compel a nuclear-armed great power into confrontation against its will. This addresses a genuine coordination problem: without the veto, Security Council majorities could theoretically force P5 states into wars they have the capacity to prevent through unilateral action.
% TRANSFER_FUNCTION: Transfers ongoing geopolitical authority and immunity-from-enforcement from the General Assembly consensus and international law majority to the P5 oligarchy. Moves the power to block resolutions, to exempt themselves from international judgment, and to pursue unilateral military/political action—resources the non-P5 majority would otherwise possess under majority-rule governance.
% ABSENT_VOICES: Reform constituencies, emerging powers, and states victimized by P5 veto abuse (Ukraine, Syria, Palestine, Yemen) are present in the General Assembly but structurally excluded from the decision-making apparatus that locks in the veto. They cannot consent to their own subordination because consent is not required—the veto blocks any amendment they would support.
% DISAPPEARANCE_RATIONALE: If the veto disappeared overnight, Security Council governance would shift to majority rule (or a qualified majority); the P5 would lose unilateral blocking power and immunity; enforcement of international law would become more universally applicable; institutional evolution would resume (seat expansion, procedural reform, enforcement mandates). The geopolitical architecture would reorganize around a different principle of legitimacy—no longer 1945 nuclear-power status, but proportional representation or rotating responsibility.
% FOUNDING_PROBLEM: Securing agreement from four nuclear-armed great powers (and one occupant of a destroyed power's seat) to participate in a global governance institution when each could unilaterally reject enforcement action against its interests. The veto was the price of participation in 1945.
% FOUNDING_PROBLEM_CORROBORATION: The P5 frame the founding problem as permanently live: nuclear weapons, global reach, and the deterrence of great-power war remain unchanged. Non-P5 coalitions, academic international relations scholarship, and the General Assembly's 80-year record of failed reform efforts attest that the founding problem (securing P5 participation) is satisfied—the P5 participate—but the SOLUTION (veto immunity) has become detached from the problem and now functions as pure entrenchment. No corroborating source outside the P5 argues the veto is necessary for continued P5 participation in the UN.
narrative_ontology:disappearance_verdict(article_27_veto_power__oligopoly_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_27_veto_power__oligopoly_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_27_veto_power__oligopoly_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(article_27_veto_power__oligopoly_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_27_veto_power__oligopoly_reading, 0.81, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness rises from 0.45 (1945) to 0.81 (2026) because the founding problem (securing P5 participation in a global governance institution) has been solved and stable for 80 years, yet the P5 have used the veto to block any institutional evolution that would redistribute authority—the solution has become detached from the problem. Early on, the veto was plausibly tied to the coordination function (preventing compelled great-power war). Over time, the P5 have used it increasingly to shield themselves from international law (Syria, Yemen, Palestine vetoes; protection of allies), to pursue unilateral military action without SC mandate (US invasions), and to block expansion or voting reforms. The extraction is no longer tethered to the original coordination need. Suppression rises from 0.52 to 0.78 because the mechanism that suppresses alternatives—Article 108's requirement for P5 consent to amend—has become the primary focus of P5 strategy. The veto persists not by participant preference but by structural lock: any reform requires the veto-holders to vote against themselves. Theater_ratio rises from 0.22 to 0.48 because the veto's secondary function (justifying the arrangement as a coordination mechanism for war prevention) has become increasingly performative. The P5 invoke great-power deterrence rhetoric when defending the veto in public forums, but their actual use (shielding human rights abuses, protecting client states, blocking humanitarian mandates) reveals the theater: the functional core is authority rent extraction, not war prevention. The gap between stated purpose and actual use is the theater. All three metrics share one time grid (measurements at t=0,12,24,36,48,60,80), enabling temporal analysis of constraint degradation.
 *
 * PERSPECTIVAL GAP:
 *   From the P5 institutional seat, the veto is a hard-won achievement and a necessary safeguard: these states possess the global-reach enforcement capacity to make Security Council decisions binding on them whether they consent or not. Their reasoning: no state should be compelled by international law without consent (a principle traced to Westphalia); the veto ensures P5 consent to any binding action. From this seat, the veto is coordination, not extraction. From the non-P5 majority seat, the veto is pure blocking power: P5 states do consent to many resolutions (they do not veto everything), and the veto is invoked selectively to protect P5 interests and allied regimes while preventing institutional change that would dilute P5 power. From this seat, the veto is not a coordination device but a tool for suppressing alternatives. The engine's per-seat classification will diverge sharply: the P5 institutional seat should compute as rope (or coordination-driven scaffold), while the non-P5 organized-majority seat should compute as snare. The divergence is exactly what this reading's structural data encodes.
 *
 * DIRECTIONALITY LOGIC:
 *   The P5 are the structural beneficiaries: they alone can veto, they extract immunity from enforcement, they can block their own reform, and they use the veto asymmetrically to advance their interests. Their directionality d approaches 1.0 (full target in reverse: full beneficiary) because the constraint subsidizes them. Their exit option is arbitrage (they can threaten to leave and negotiate side agreements; they have leverage). The non-P5 majority are the structural victims: trapped (exit means loss of voice), identity-locked (the UN is the institutional site where non-P5 states have nominal equal voice—exiting means accepting exclusion), and facing indefinite suppression (any reform requires veto-holder consent). Their d approaches 0.0 (full target). The emerging powers sit between: powerful (institutional actors with regional authority) but excluded (no Security Council seat, constrained exit because competing blocs lack hard enforcement power). The institutional reform constituencies are excluded by definition—they want the change but cannot author it because the mechanism that locks it in place (Article 108) requires the lock-holders' consent. Beneficiary/victim declarations map straightforwardly to directionality: beneficiary → low d (subsidy), victim → high d (extraction).
 *
 * MANDATROPHY ANALYSIS:
 *   The oligopoly reading resolves a mandatrophy by explicitly treating the veto as divorced from its founding problem. The founding problem was 'how to bind great powers to international governance without compelled war participation.' The solution was the veto. But after 80 years, the problem is solved—the P5 participate willingly in the UN, they use the veto, they enforce Security Council mandates selectively, and no P5 state is at risk of compelled military action it can prevent. The founding problem is dead (P5 are stable participants; they have never threatened to leave over the veto). Yet the solution persists and has metastasized into an extraction mechanism. The veto is now used primarily to block institutional reform and to shield P5 conduct from international law. The constraint persists not because the founding problem is live but because the beneficiaries (P5) use the lock (Article 108) to prevent themselves from being voted out. A snare reading correctly identifies this: the coordination story (war prevention) is cover; the persistence depends on suppression (blocking alternatives via the amendment lock) and on the asymmetric extraction (P5 immunity). Classifying this as rope (pure coordination) would miss the fact that the veto serves no one equally—it benefits P5 unilaterally and harms non-P5 states uniformly.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_problem_live_vs_solved,
    'Is the founding problem of the veto (preventing compelled great-power war via UN mandate) still live, or has it been solved and the constraint now persists as pure entrenchment?',
    'Historical analysis: have P5 states ever threatened to leave the UN or refuse participation because of Security Council decisions? Have they ever faced compelled military action they could not prevent? Do they withdraw from the UN when the veto is ineffective (e.g., General Assembly resolutions they oppose)? If no to all three, the founding problem is solved. If the P5 still claim the problem is live, ask: what evidence would demonstrate it is solved?',
    'If the founding problem is solved, the veto''s persistence is mandatrophy and the constraint reclassifies from rope/coordination to snare/extraction. If the problem is live, the veto retains its coordination function and the classification shifts back toward rope, with a hybrid (tangled_rope) as a compromise.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(founding_problem_live_vs_solved, empirical, 'Whether the veto''s original justification (preventing compelled war) remains necessary or if the constraint now functions purely as entrenchment.').

omega_variable(
    charter_immutability_intentional,
    'Was Article 108''s requirement for P5 consent to amend the Charter a deliberate lock designed to protect P5 privilege, or an incidental structural feature that both P5 and non-P5 states accepted without understanding its entrenchment consequence?',
    'Historical documents from the 1945 San Francisco Conference: did the P5 explicitly negotiate Article 108 to protect the veto, or did the amendment rule emerge from other concerns (legitimacy, consensus-building)? Did non-P5 states object to it at the time?',
    'If intentional lock-building by P5, the entrenchment is deliberately extractive and the snare classification is strengthened. If incidental, the constraint is still a snare (because of its effect, not its origin), but the P5 may be acting on inertia rather than conscious exploitation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(charter_immutability_intentional, empirical, 'Whether Charter immutability is an intentional P5 protection or an unintended structural consequence.').

omega_variable(
    veto_abuse_vs_legitimate_use,
    'Where is the line between legitimate use of the veto (blocking action against a P5 state''s genuine security interest) and veto abuse (blocking action for capricious or extractive reasons, e.g., protecting client-state human rights abuses)?',
    'Comparative analysis of veto use: do P5 states apply consistent criteria (protecting their own conduct, preventing great-power confrontation) or inconsistent criteria (protecting allies while condemning equivalently-behaved non-allies)? Do they veto equally when they are the target of a resolution vs. when another P5 state is the target?',
    'Consistent criteria suggest the veto serves a coordination function (even if asymmetric). Inconsistent, selective use supports the oligopoly reading: the veto is used to extract privilege, not to solve a genuine coordination problem. Selective use also raises the extraction value ε.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(veto_abuse_vs_legitimate_use, empirical, 'Whether veto use patterns are consistent with coordination or with selective rent extraction.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the non-P5 majority''s suppression of reform proposals structural (the Article 108 lock objectively makes reform impossible) or internalized (non-P5 states have internalized the belief that P5 consent is necessary and do not seriously attempt reforms)?',
    'Post-Article 108 scenario: if the amendment rule were removed and P5 consent no longer required, would non-P5 states immediately initiate comprehensive Council reforms, or would they proceed cautiously? Do reform proposals fail because they cannot muster P5 approval or because reformers expect them to fail and do not try?',
    'If structural, suppression is 0.78 (the lock prevents reform mechanically). If internalized, suppression is higher (targets carry the constraint with them, restraint persists even when the external lock is removed), and the constraint''s hold on non-P5 states is deeper.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether suppression of reform is external (the lock) or self-imposed (internalized acceptance).').

omega_variable(
    kernel_reading_foreclosure_test,
    'Does this reading''s core claim (the veto functions as oligopoly entrenchment, not coordination or sovereignty protection) logically foreclose the coordination reading, or can both coexist as live positions?',
    'Logical analysis: the coordination reading says ''the veto prevents compelled war and solves a real collective-action problem.'' The oligopoly reading says ''the veto persists because P5 use the Charter lock to extract rents and block reform, not because the coordination problem is live.'' These claims differ in what the veto IS FOR and what justifies its persistence. If both can be true (the veto solves a coordination problem AND the P5 use it to extract rents), they coexist. If the oligopoly reading''s evidence (veto abuse, selective use, foundational problem solved) contradicts the coordination reading''s premise (the veto is necessary for great-power peace), they foreclose each other.',
    'Coexistence (weaker relation) means both readings remain live in contemporary discourse. Foreclosure means the readings are incompatible and only one can be defended as structurally true given the evidence. Foreclosure would support the reading as a dominant interpretation; coexistence suggests it is one pole of an unresolved debate.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure_test, conceptual, 'Logical relationship between the oligopoly and coordination readings: can they coexist or does one foreclose the other?').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_27_veto_power__oligopoly_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t0, article_27_veto_power__oligopoly_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(arti_tr_t0, observed).
narrative_ontology:measurement(arti_tr_t12, article_27_veto_power__oligopoly_reading, theater_ratio, 12, 0.28).
narrative_ontology:measurement_basis(arti_tr_t12, observed).
narrative_ontology:measurement(arti_tr_t24, article_27_veto_power__oligopoly_reading, theater_ratio, 24, 0.34).
narrative_ontology:measurement_basis(arti_tr_t24, observed).
narrative_ontology:measurement(arti_tr_t36, article_27_veto_power__oligopoly_reading, theater_ratio, 36, 0.4).
narrative_ontology:measurement_basis(arti_tr_t36, observed).
narrative_ontology:measurement(arti_tr_t48, article_27_veto_power__oligopoly_reading, theater_ratio, 48, 0.45).
narrative_ontology:measurement_basis(arti_tr_t48, observed).
narrative_ontology:measurement(arti_tr_t60, article_27_veto_power__oligopoly_reading, theater_ratio, 60, 0.47).
narrative_ontology:measurement_basis(arti_tr_t60, observed).
narrative_ontology:measurement(arti_tr_t80, article_27_veto_power__oligopoly_reading, theater_ratio, 80, 0.48).
narrative_ontology:measurement_basis(arti_tr_t80, observed).

% Extraction over time
narrative_ontology:measurement(arti_be_t0, article_27_veto_power__oligopoly_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(arti_be_t0, observed).
narrative_ontology:measurement(arti_be_t12, article_27_veto_power__oligopoly_reading, base_extractiveness, 12, 0.54).
narrative_ontology:measurement_basis(arti_be_t12, observed).
narrative_ontology:measurement(arti_be_t24, article_27_veto_power__oligopoly_reading, base_extractiveness, 24, 0.61).
narrative_ontology:measurement_basis(arti_be_t24, observed).
narrative_ontology:measurement(arti_be_t36, article_27_veto_power__oligopoly_reading, base_extractiveness, 36, 0.68).
narrative_ontology:measurement_basis(arti_be_t36, observed).
narrative_ontology:measurement(arti_be_t48, article_27_veto_power__oligopoly_reading, base_extractiveness, 48, 0.74).
narrative_ontology:measurement_basis(arti_be_t48, observed).
narrative_ontology:measurement(arti_be_t60, article_27_veto_power__oligopoly_reading, base_extractiveness, 60, 0.78).
narrative_ontology:measurement_basis(arti_be_t60, observed).
narrative_ontology:measurement(arti_be_t80, article_27_veto_power__oligopoly_reading, base_extractiveness, 80, 0.81).
narrative_ontology:measurement_basis(arti_be_t80, observed).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t0, article_27_veto_power__oligopoly_reading, suppression_requirement, 0, 0.52).
narrative_ontology:measurement_basis(arti_su_t0, observed).
narrative_ontology:measurement(arti_su_t12, article_27_veto_power__oligopoly_reading, suppression_requirement, 12, 0.59).
narrative_ontology:measurement_basis(arti_su_t12, observed).
narrative_ontology:measurement(arti_su_t24, article_27_veto_power__oligopoly_reading, suppression_requirement, 24, 0.65).
narrative_ontology:measurement_basis(arti_su_t24, observed).
narrative_ontology:measurement(arti_su_t36, article_27_veto_power__oligopoly_reading, suppression_requirement, 36, 0.71).
narrative_ontology:measurement_basis(arti_su_t36, observed).
narrative_ontology:measurement(arti_su_t48, article_27_veto_power__oligopoly_reading, suppression_requirement, 48, 0.75).
narrative_ontology:measurement_basis(arti_su_t48, observed).
narrative_ontology:measurement(arti_su_t60, article_27_veto_power__oligopoly_reading, suppression_requirement, 60, 0.77).
narrative_ontology:measurement_basis(arti_su_t60, observed).
narrative_ontology:measurement(arti_su_t80, article_27_veto_power__oligopoly_reading, suppression_requirement, 80, 0.78).
narrative_ontology:measurement_basis(arti_su_t80, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_27_veto_power__oligopoly_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(article_27_veto_power__oligopoly_reading, 0.12).
narrative_ontology:affects_constraint(article_27_veto_power__oligopoly_reading, article_27_veto_power__coordination_reading).
narrative_ontology:affects_constraint(article_27_veto_power__oligopoly_reading, article_27_veto_power__sovereignty_reading).
narrative_ontology:affects_constraint(article_27_veto_power__oligopoly_reading, un_reform_stagnation).
narrative_ontology:affects_constraint(article_27_veto_power__oligopoly_reading, great_power_impunity_from_enforcement).

% DUAL FORMULATION NOTE:
% Article 27 is a contested kernel with three structural readings: coordination (veto prevents compelled war), sovereignty (veto instantiates Westphalian consent), oligopoly (veto entrenchment via Charter lock). This constraint is the oligopoly reading—ε derived from blocked exit and suppressed alternatives; beneficiary is P5; victim is non-P5 majority. The coordinate_reading and sovereignty_reading are separate constraints with different ε, beneficiary/victim structures, and types. All three are linked via network.affects_constraints to show they are readings of the same kernel. The oligopoly reading's high extraction (ε=0.81) contrasts with the coordination reading's low extraction (coordination readings typically ε~0.20-0.35) because the readings fix different referents: the oligopoly reading assesses the veto AS AN ENTRENCHMENT MECHANISM, while the coordination reading assesses it AS A WAR-PREVENTION MECHANISM. Same text, different constraint (OQ-26, ε-invariance principle).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(article_27_veto_power__oligopoly_reading, organized, 0.88).
constraint_indexing:directionality_override(article_27_veto_power__oligopoly_reading, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
