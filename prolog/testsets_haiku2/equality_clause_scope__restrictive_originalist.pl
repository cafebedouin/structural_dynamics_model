% ============================================================================
% CONSTRAINT STORY: equality_clause_scope__restrictive_originalist
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equality_clause_scope__restrictive_originalist, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: equality_clause_scope__restrictive_originalist
 *   human_readable: Equality Clause - Restrictive Originalist Reading (18th-Century Propertied Males)
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   The restrictive originalist reading of the equality clause holds that the
 *   principle of equal protection and equal treatment applies specifically to
 *   propertied white male citizens within the 18th-century social contract
 *   framework. The constraint operates as a tangled rope: it genuinely
 *   coordinates the political interests of the beneficiary class (preventing
 *   arbitrary rule within that elite set) while simultaneously extracting
 *   political power, legal personhood, and protection from all other groups.
 *   The reading is defended by appeal to original public meaning and framers'
 *   intent; it requires continuous enforcement through judicial
 *   interpretation and suppression of alternative readings. The founding
 *   problem—elite coordination without arbitrary dominance—becomes moribund
 *   over the 83-year interval as the constraint's extractive function grows
 *   relative to its coordination function, visible in rising theater ratio
 *   (performance of principled equality) and intensifying suppression
 *   (enforcement against expanding claims for inclusion). By 1870, the 14th
 *   Amendment has formally repudiated the restrictive reading's authority;
 *   the constraint persists not through legitimacy but through institutional
 *   inertia and the difficulty of amending constitutional interpretation
 *   retroactively.
 *
 * KEY AGENTS:
 *   - propertied_white_male_citizens: Beneficiary + agenda-setter (control courts and interpretation; benefit from exclusive franchise and legal standing)
 *   - enslaved_persons: Victim; powerless; trapped (no legal personhood under the reading; exit only through escape or resistance)
 *   - women: Victim; moderate power; constrained exit (excluded from franchise and legal autonomy; path to equality requires amendment)
 *   - indigenous_peoples: Victim; moderate power; constrained exit (treated as foreign sovereigns outside the social contract; plenary congressional power over their affairs)
 *   - originalist_judiciary: Agenda-setter (enforces the narrow reading; maintains the boundary between covered and excluded groups)
 *   - expansive_equality_advocates: Excluded (organized resistance calling for universal application; not admitted to originalist jurisprudence)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equality_clause_scope__restrictive_originalist, 0.78).
domain_priors:suppression_score(equality_clause_scope__restrictive_originalist, 0.81).
domain_priors:theater_ratio(equality_clause_scope__restrictive_originalist, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equality_clause_scope__restrictive_originalist, extractiveness, 0.78).
narrative_ontology:constraint_metric(equality_clause_scope__restrictive_originalist, suppression_requirement, 0.81).
narrative_ontology:constraint_metric(equality_clause_scope__restrictive_originalist, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equality_clause_scope__restrictive_originalist, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(equality_clause_scope__restrictive_originalist, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equality_clause_scope__restrictive_originalist, tangled_rope).
narrative_ontology:human_readable(equality_clause_scope__restrictive_originalist, "Equality Clause - Restrictive Originalist Reading (18th-Century Propertied Males)").
narrative_ontology:topic_domain(equality_clause_scope__restrictive_originalist, "constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(equality_clause_scope__restrictive_originalist).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equality_clause_scope__restrictive_originalist, 'c4b90f1b-17f5-482a-acca-1feb9493198d').
narrative_ontology:cs_kernel_codification('c4b90f1b-17f5-482a-acca-1feb9493198d', formalized).
narrative_ontology:cs_authority_grounding('c4b90f1b-17f5-482a-acca-1feb9493198d', extraction).
narrative_ontology:cs_interpretation_layer_present('c4b90f1b-17f5-482a-acca-1feb9493198d').
narrative_ontology:cs_reading_relation('c4b90f1b-17f5-482a-acca-1feb9493198d', equality_clause_scope__expansive_universalist, forecloses).
narrative_ontology:cs_reading_relation('c4b90f1b-17f5-482a-acca-1feb9493198d', equality_clause_scope__progressive_textualist, coexists_with).
narrative_ontology:cs_axiom('c4b90f1b-17f5-482a-acca-1feb9493198d', foundational, original_public_meaning_constraint).
narrative_ontology:cs_axiom_status(original_public_meaning_constraint, holdable).
narrative_ontology:cs_axiom_grounding('c4b90f1b-17f5-482a-acca-1feb9493198d', original_public_meaning_constraint, deontological).
narrative_ontology:cs_axiom('c4b90f1b-17f5-482a-acca-1feb9493198d', foundational, framers_intent_binding).
narrative_ontology:cs_axiom_status(framers_intent_binding, overridden).
narrative_ontology:cs_axiom_grounding('c4b90f1b-17f5-482a-acca-1feb9493198d', framers_intent_binding, conventional).
narrative_ontology:cs_reference_frame('c4b90f1b-17f5-482a-acca-1feb9493198d', propertied_male_citizen_equal_standing).
narrative_ontology:cs_drift_state('c4b90f1b-17f5-482a-acca-1feb9493198d', post_fourteenth_amendment_1870, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('c4b90f1b-17f5-482a-acca-1feb9493198d', '').
narrative_ontology:cs_kernel_id(equality_clause_scope__restrictive_originalist, equality_clause_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equality_clause_scope__restrictive_originalist, propertied_white_male_citizens).
narrative_ontology:constraint_victim(equality_clause_scope__restrictive_originalist, enslaved_persons).
narrative_ontology:constraint_victim(equality_clause_scope__restrictive_originalist, indigenous_peoples).
narrative_ontology:constraint_victim(equality_clause_scope__restrictive_originalist, women).
narrative_ontology:constraint_victim(equality_clause_scope__restrictive_originalist, non_propertied_men).
narrative_ontology:constraint_victim(equality_clause_scope__restrictive_originalist, free_black_persons).
narrative_ontology:constraint_vindicates(equality_clause_scope__restrictive_originalist, original_public_meaning_doctrine).
narrative_ontology:constraint_vindicates(equality_clause_scope__restrictive_originalist, framers_intent_as_binding).
narrative_ontology:constraint_vindicates(equality_clause_scope__restrictive_originalist, narrow_construction_of_rights).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Possess equal political standing within the colonial and early federal frameworks: voting rights, property rights, contractual equality, standing before courts. The equality clause, under this reading, extends to them the principle of non-arbitrary rule and equal protection within the propertied political community. They set constitutional interpretation through control of courts, legislatures, and founding narratives. They benefit from the constraint's narrow scope by maintaining exclusive access to franchise and political power.
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, propertied_white_male_citizens, beneficiary,
    powerful, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(equality_clause_scope__restrictive_originalist, propertied_white_male_citizens, agenda_setter).

% Are legally defined as property, not persons under the social contract framework. The restrictive reading places them entirely outside the beneficiary class; equality claims are structurally precluded without amending the Constitution. They bear the constraint through total exclusion from political standing, legal personhood, and any protection the equality principle might extend. Their only exit is physical escape or violent resistance.
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, enslaved_persons, payer,
    powerless, immediate, trapped, national).

% Are treated as separate sovereign nations in treaties, and therefore outside the federal social contract framework; the equality clause does not extend to them as persons or as internal political actors. They are subordinate through exclusion and the doctrine of plenary congressional power over Indian affairs. The constraint enforces their non-incorporation into the beneficiary class and denies them recourse to constitutional equal protection until they formally cede sovereignty.
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, indigenous_peoples, payer,
    moderate, generational, constrained, national).

% Are excluded from the franchise, from independent legal personhood (coverture doctrine), and from full political standing under the propertied-male-citizens framework. The equality clause, narrowly read, does not extend to gender classification because the Constitution's text does not explicitly address it and the original public meaning was restricted to property-qualified male citizens. Women must achieve equal standing through amendment, not constitutional interpretation.
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, women, payer,
    moderate, generational, constrained, national).

% Lack the property qualification originally required for full political participation in many states; the equality clause as narrowly read does not guarantee them voting rights or equal standing with propertied men. They are subject to property-based voting restrictions and carry no claim to equal political power under the social contract as originally framed. Their path to equality runs through amendment or state constitutional reform, not through the federal equality principle.
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, non_propertied_men, payer,
    powerless, biographical, constrained, national).

% Are excluded from the beneficiary class of the propertied-male-citizens framework both by race and often by property status. Under the restrictive originalist reading, the Constitution's equality clause provides no protection or remedy; they remain outside the social contract's scope. They face state-level discrimination that the federal equality principle does not reach; their claims require constitutional amendment or political action outside the judiciary.
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, free_black_persons, payer,
    powerless, biographical, constrained, national).

% Interprets and enforces the restrictive reading through constitutional opinion and precedent. They maintain the constraint by limiting equality claims to the original scope, requiring amendment rather than interpretation to expand it. They enforce the boundary between covered and excluded groups and resist common-law expansion of rights. They set the legitimacy threshold for expansion very high, effectively gatekeeping who can make equality claims.
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, originalist_judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Functions as the authorized pathway for expanding equality beyond its original scope. The restrictive reading vindicates the amendment process as the legitimate mechanism for inclusion; rights claims outside original scope are channeled toward amendment, not judicial interpretation. The process is deliberately high-friction, which preserves the narrow reading's authority.
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, democratic_amendment_process, beneficiary,
    analytical, generational, analytical, national).
narrative_ontology:stakeholder_non_agent(equality_clause_scope__restrictive_originalist, democratic_amendment_process).

% Argue for universal application of equality to all persons regardless of race, gender, property, or original scope. They are structurally excluded from the restrictive reading's definition of who counts as a bearer of equality rights. They mount resistance through political organizing, amendment campaigns, and judicial counter-arguments; their voices are not admitted into the original-meaning jurisprudence.
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, expansive_equality_advocates, excluded,
    organized, generational, constrained, national).

% Grounds the equality principle in reason and natural rights; the restrictive reading constrains that universalist inheritance to the specific actors (propertied male property-owners) who participated in 18th-century social contract theory. The constraint vindicates a selective appropriation of Enlightenment thought: the principle of reasoned rule, applied narrowly to the reasoning actors recognized at the founding.
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, european_enlightenment_thought, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(equality_clause_scope__restrictive_originalist, european_enlightenment_thought).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(equality_clause_scope__restrictive_originalist, propertied_white_male_citizens).
narrative_ontology:fixing_cost_class(equality_clause_scope__restrictive_originalist, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes equal standing in law for political actors within the propertied male citizen community: equal protection against arbitrary rule, equal contract enforcement, equal standing before courts, equal application of legislative rules to similarly-situated property holders. Solves the collective-action problem of how to govern multiple competing interests among the political elite without any one faction gaining permanent arbitrary dominance.
% TRANSFER_FUNCTION: Moves political power, legal standing, and constitutional protection to the propertied white male citizen class. Simultaneously transfers all other groups into the status of non-persons or partial persons under law, with no claim to equal protection. The resource extracted is political legitimacy: the appearance of principled, reasoned governance based on equality and consent, while actual power concentrates among the beneficiary class.
% ABSENT_VOICES: Enslaved persons, indigenous nations, women, non-propertied men, and free black persons would all object to their exclusion from the equality principle if seated in constitutional interpretation. They argue the equality language is universal or was always meant to apply to all humans. Their exclusion is maintained by the restrictive reading itself: the framework denies them standing to reinterpret it. The absent-voice problem is structural to the constraint.
% DISAPPEARANCE_RATIONALE: If the restrictive originalist reading disappeared and the equality clause applied universally from its text, the entire political, property, and legal order of the early republic collapses. Enslaved persons would become legal persons; women would gain franchise and legal autonomy; indigenous nations' sovereignty would be recognized; non-propertied men would gain equal standing. The 13th, 14th, 15th, 19th amendments and subsequent jurisprudence exist precisely because the restrictive reading could not be sustained—the world did rearrange, through constitutional amendment, not through this reading's evolution.
% FOUNDING_PROBLEM: How can propertied property-holders governed by a single sovereign authority coordinate their competing interests without any one faction imposing arbitrary rule? How can they legitimate government by appeal to reasoned principles rather than pure force? The equality principle, narrowly read, provides a framework: equal application of law to property-qualified citizens, guaranteeing none will be singled out for disadvantage.
% FOUNDING_PROBLEM_CORROBORATION: The framers' private papers, notes from the Constitutional Convention, and Federalist Papers attest the founding problem as the coordination of property-holding interests among the political elite. Later historians (Charles Beard, Gordon Wood, Jack Rakove) argue the founding problem was precisely that narrow coordination. But the very exclusions the restrictive reading requires—chattel slavery, indigenous subordination, women's legal coverture—were deliberate choices to preserve elite property-holding power, not solutions to a neutral coordination problem. Post-ratification amendment movements and abolitionist arguments from non-beneficiary groups attest the founding problem statement itself was a cover story for elite extraction. The founding problem, if genuine, has been solved for two centuries; the constraint's persistence now is purely extractive.
narrative_ontology:disappearance_verdict(equality_clause_scope__restrictive_originalist, world_rearranges).
narrative_ontology:founding_problem_status(equality_clause_scope__restrictive_originalist, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equality_clause_scope__restrictive_originalist, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(equality_clause_scope__restrictive_originalist, 'none', 1).
narrative_ontology:epsilon_provenance(equality_clause_scope__restrictive_originalist, 0.78, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equality_clause_scope__restrictive_originalist_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(equality_clause_scope__restrictive_originalist, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(equality_clause_scope__restrictive_originalist_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness peaks at 0.78 because the constraint concentrates political power and legal personhood in a narrow class while denying it to majorities. Suppression is higher still (0.81) because the reading requires active enforcement against continuous challenges from excluded groups—most intensely after 1820 as abolitionist, women's rights, and labor movements organize. Theater ratio rises from 0.22 to 0.42 over the interval: the constraint begins with genuine coordination function (preventing arbitrary elite infighting) but as resistance mounts, an increasing share of enforcement activity goes to performing the constraint's legitimacy (denying standing, reinterpreting text, blocking amendment) rather than solving the original problem. The coercion grid shows the widening gap between structural enforcement (0.62 to 0.81) and individual-level resistance (0.18 to 0.64): the constraint's infrastructure hardens while organized resistance grows. This is the signature of a constraint moving from genuine coordination toward pure extraction: the coordination benefit erodes, resistance rises, and theatrical maintenance intensifies. The measurement series documents this drift across the interval from 1787 (ratification) to 1870 (14th Amendment ratification and the formal repudiation of the restrictive reading's authority).
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seat: the constraint is principled governance by reasoned equals, grounded in natural law and the Enlightenment. From the victim seats: the constraint is pure extraction wearing the mask of principle—a way to concentrate power by excluding most humans from personhood. The originalist judiciary (agenda-setter) experiences it as fidelity to binding constitutional text and framers' intent. Expansive equality advocates experience it as illegitimate interpretive closure that violates the logic of the equality language itself. The engine computes this divergence from the authored structural data: the restrictive reading will produce different effective extraction values (χ) for beneficiary and victim seats, and the directive force will vary dramatically. A beneficiary computing the constraint as coordination (low χ) while a victim computes it as extraction (high χ) is not a failure of the framework—it is the measurement the framework is built to detect.
 *
 * DIRECTIONALITY LOGIC:
 *   Propertied white male citizens sit at d ≈ 0.1 (full beneficiaries): the constraint allocates political power to them, they control its interpretation and enforcement, and they have arbitrage-grade exit (they can move to different jurisdictions and remain part of the beneficiary class, or they can dissolve the constraint through amendment if it no longer serves them). Enslaved persons sit at d ≈ 0.95 (full targets): they are trapped, have no legal standing, no right of exit except through violence or escape, and the constraint's entire function is to suppress their claims to personhood. Women sit at d ≈ 0.85 (near-full targets): they are subject to coverture, lack franchise, face high suppression, and have constrained exit (they cannot exit the jurisdiction without abandoning property, family ties, and identity). The asymmetry is structural and intentional: the beneficiary set benefits from the constraint's narrow scope, while the victim sets are trapped by it. Directionality follows from the beneficiary/victim declarations and exit options without override.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint exhibits clear mandatrophy: the founding problem (coordinating propertied elites without arbitrary dominance) is resolved by 1820 and dead by 1870. Yet the constraint persists because beneficiaries extract continuing value from the narrow equality scope—exclusion itself becomes the benefit. The theater ratio rising to 0.42 marks the theatrical maintenance phase: enforcement increasingly defends the reading against amendment and reinterpretation rather than solving coordination problems. The constraint should be classified as piton (inertial, maintained through performance and institutional habit) by 1870, but the restrictive originalist reading itself prevents that diagnosis—it claims the founding problem is still live and equality still applies only to propertied males. This is precisely how mandatrophy hides: the beneficiary class continues asserting that the constraint solves the founding problem, even as that problem has been solved and the constraint now only extracts.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    framers_intent_determinacy,
    'Can the framers'' intent regarding the equality clause''s scope be authoritatively determined from historical sources, or is the historical record ambiguous enough to admit multiple coherent reconstructions?',
    'Comparative historical analysis of founding-era documents, private papers, Convention notes, and state ratification debates. If multiple coherent intent-reconstructions emerge from the same sources, the question resolves to unavoidable under-determination.',
    'If intent is indeterminate, the restrictive originalist reading''s legitimacy claim (fidelity to binding intent) collapses, and the reading becomes one interpretive choice among several rather than the uniquely correct one. This would reclassify the constraint from tangled_rope (real coordination + enforcement) toward snare (enforcement without coordination benefit).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(framers_intent_determinacy, empirical, 'Whether framers'' intent regarding equality scope is determinate enough to ground the restrictive originalist reading''s authority claim.').

omega_variable(
    natural_law_universalism_vs_constructed_restriction,
    'Is equality a universal principle derived from natural law and reason (as Enlightenment sources suggest), or is the 18th-century propertied-male restriction a genuine constraint inherent to the principle, not a narrowing imposed by extra-constitutional interests?',
    'Genealogical analysis of how Enlightenment equality theories were transmitted and selectively appropriated by the framers. If the universal scope was part of the original theory and the framers deliberately restricted it for political benefit, the restriction is constructed, not natural.',
    'If the restriction is constructed rather than natural, the constraint moves from being a legitimate instantiation of Enlightenment principle toward being a constructed false-summit: falsely claiming fidelity to natural law while serving beneficiary extraction. This would trigger FSM evaluation and potential reclassification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_universalism_vs_constructed_restriction, conceptual, 'Whether the propertied-male restriction is inherent to equality as a principle or imposed by political choices.').

omega_variable(
    amendment_process_legitimacy,
    'Is the amendment process the appropriate and sufficient pathway for expanding equality beyond its original scope, or does reliance on amendment foreclose legitimate expansion claims that the equality text itself should address?',
    'Analysis of whether amendment-only doctrines serve coordination or extraction: if amendment is high-friction deliberately (to preserve elite power) rather than legitimately (to require democratic consensus), the restrictive reading is using procedural gates as a suppression mechanism.',
    'If the amendment-only requirement is a suppressant mechanism rather than a legitimate gate, the constraint''s extractiveness is even higher than authored (0.78), and resistance to amendment campaigns becomes evidence of suppression intent rather than fidelity to constitutional process.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(amendment_process_legitimacy, preference, 'Whether amendment-only gates for equality expansion are legitimate procedural requirements or strategic suppressants.').

omega_variable(
    sibling_reading_coexistence,
    'Can the restrictive_originalist, expansive_universalist, and progressive_textualist readings coexist as live interpretive options within a single constitutional framework, or does one logically foreclose the others?',
    'Structural analysis: if a single party (the originalist judiciary) enforces one reading and suppresses the others, coexistence is institutional coexistence-under-duress, not genuine coexistence. If all three survive in different institutional venues or jurisdictions, coexistence is real.',
    'If the readings are forced into sequential competition rather than allowed genuine coexistence, the restrictive reading operates partly through institutional power (control of courts) rather than through superior logical force. This would suggest the reading''s persistence owes more to power than to principle.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_coexistence, empirical, 'Whether the three equality readings genuinely coexist or are forced into suppressed/dominant sequences by institutional control.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equality_clause_scope__restrictive_originalist, 1787, 1870).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t1787, equality_clause_scope__restrictive_originalist, theater_ratio, 1787, 0.22).
narrative_ontology:measurement_basis(equa_tr_t1787, observed).
narrative_ontology:measurement(equa_tr_t1800, equality_clause_scope__restrictive_originalist, theater_ratio, 1800, 0.26).
narrative_ontology:measurement_basis(equa_tr_t1800, observed).
narrative_ontology:measurement(equa_tr_t1820, equality_clause_scope__restrictive_originalist, theater_ratio, 1820, 0.31).
narrative_ontology:measurement_basis(equa_tr_t1820, observed).
narrative_ontology:measurement(equa_tr_t1840, equality_clause_scope__restrictive_originalist, theater_ratio, 1840, 0.37).
narrative_ontology:measurement_basis(equa_tr_t1840, observed).
narrative_ontology:measurement(equa_tr_t1860, equality_clause_scope__restrictive_originalist, theater_ratio, 1860, 0.4).
narrative_ontology:measurement_basis(equa_tr_t1860, observed).
narrative_ontology:measurement(equa_tr_t1870, equality_clause_scope__restrictive_originalist, theater_ratio, 1870, 0.42).
narrative_ontology:measurement_basis(equa_tr_t1870, observed).

% Extraction over time
narrative_ontology:measurement(equa_be_t1787, equality_clause_scope__restrictive_originalist, base_extractiveness, 1787, 0.68).
narrative_ontology:measurement_basis(equa_be_t1787, observed).
narrative_ontology:measurement(equa_be_t1800, equality_clause_scope__restrictive_originalist, base_extractiveness, 1800, 0.71).
narrative_ontology:measurement_basis(equa_be_t1800, observed).
narrative_ontology:measurement(equa_be_t1820, equality_clause_scope__restrictive_originalist, base_extractiveness, 1820, 0.74).
narrative_ontology:measurement_basis(equa_be_t1820, observed).
narrative_ontology:measurement(equa_be_t1840, equality_clause_scope__restrictive_originalist, base_extractiveness, 1840, 0.76).
narrative_ontology:measurement_basis(equa_be_t1840, observed).
narrative_ontology:measurement(equa_be_t1860, equality_clause_scope__restrictive_originalist, base_extractiveness, 1860, 0.77).
narrative_ontology:measurement_basis(equa_be_t1860, observed).
narrative_ontology:measurement(equa_be_t1870, equality_clause_scope__restrictive_originalist, base_extractiveness, 1870, 0.78).
narrative_ontology:measurement_basis(equa_be_t1870, observed).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t1787, equality_clause_scope__restrictive_originalist, suppression_requirement, 1787, 0.65).
narrative_ontology:measurement_basis(equa_su_t1787, observed).
narrative_ontology:measurement(equa_su_t1800, equality_clause_scope__restrictive_originalist, suppression_requirement, 1800, 0.69).
narrative_ontology:measurement_basis(equa_su_t1800, observed).
narrative_ontology:measurement(equa_su_t1820, equality_clause_scope__restrictive_originalist, suppression_requirement, 1820, 0.73).
narrative_ontology:measurement_basis(equa_su_t1820, observed).
narrative_ontology:measurement(equa_su_t1840, equality_clause_scope__restrictive_originalist, suppression_requirement, 1840, 0.77).
narrative_ontology:measurement_basis(equa_su_t1840, observed).
narrative_ontology:measurement(equa_su_t1860, equality_clause_scope__restrictive_originalist, suppression_requirement, 1860, 0.79).
narrative_ontology:measurement_basis(equa_su_t1860, observed).
narrative_ontology:measurement(equa_su_t1870, equality_clause_scope__restrictive_originalist, suppression_requirement, 1870, 0.81).
narrative_ontology:measurement_basis(equa_su_t1870, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=1787, tn=1870
narrative_ontology:measurement(equa_grid_01, equality_clause_scope__restrictive_originalist, accessibility_collapse(class), 1787, 0.78).
narrative_ontology:measurement(equa_grid_02, equality_clause_scope__restrictive_originalist, accessibility_collapse(class), 1870, 0.82).
narrative_ontology:measurement(equa_grid_03, equality_clause_scope__restrictive_originalist, accessibility_collapse(individual), 1787, 0.55).
narrative_ontology:measurement(equa_grid_04, equality_clause_scope__restrictive_originalist, accessibility_collapse(individual), 1870, 0.68).
narrative_ontology:measurement(equa_grid_05, equality_clause_scope__restrictive_originalist, accessibility_collapse(organizational), 1787, 0.62).
narrative_ontology:measurement(equa_grid_06, equality_clause_scope__restrictive_originalist, accessibility_collapse(organizational), 1870, 0.75).
narrative_ontology:measurement(equa_grid_07, equality_clause_scope__restrictive_originalist, accessibility_collapse(structural), 1787, 0.81).
narrative_ontology:measurement(equa_grid_08, equality_clause_scope__restrictive_originalist, accessibility_collapse(structural), 1870, 0.84).
narrative_ontology:measurement(equa_grid_09, equality_clause_scope__restrictive_originalist, resistance(class), 1787, 0.31).
narrative_ontology:measurement(equa_grid_10, equality_clause_scope__restrictive_originalist, resistance(class), 1870, 0.68).
narrative_ontology:measurement(equa_grid_11, equality_clause_scope__restrictive_originalist, resistance(individual), 1787, 0.18).
narrative_ontology:measurement(equa_grid_12, equality_clause_scope__restrictive_originalist, resistance(individual), 1870, 0.64).
narrative_ontology:measurement(equa_grid_13, equality_clause_scope__restrictive_originalist, resistance(organizational), 1787, 0.22).
narrative_ontology:measurement(equa_grid_14, equality_clause_scope__restrictive_originalist, resistance(organizational), 1870, 0.71).
narrative_ontology:measurement(equa_grid_15, equality_clause_scope__restrictive_originalist, resistance(structural), 1787, 0.12).
narrative_ontology:measurement(equa_grid_16, equality_clause_scope__restrictive_originalist, resistance(structural), 1870, 0.58).
narrative_ontology:measurement(equa_grid_17, equality_clause_scope__restrictive_originalist, stakes_inflation(class), 1787, 0.64).
narrative_ontology:measurement(equa_grid_18, equality_clause_scope__restrictive_originalist, stakes_inflation(class), 1870, 0.76).
narrative_ontology:measurement(equa_grid_19, equality_clause_scope__restrictive_originalist, stakes_inflation(individual), 1787, 0.48).
narrative_ontology:measurement(equa_grid_20, equality_clause_scope__restrictive_originalist, stakes_inflation(individual), 1870, 0.62).
narrative_ontology:measurement(equa_grid_21, equality_clause_scope__restrictive_originalist, stakes_inflation(organizational), 1787, 0.52).
narrative_ontology:measurement(equa_grid_22, equality_clause_scope__restrictive_originalist, stakes_inflation(organizational), 1870, 0.71).
narrative_ontology:measurement(equa_grid_23, equality_clause_scope__restrictive_originalist, stakes_inflation(structural), 1787, 0.73).
narrative_ontology:measurement(equa_grid_24, equality_clause_scope__restrictive_originalist, stakes_inflation(structural), 1870, 0.79).
narrative_ontology:measurement(equa_grid_25, equality_clause_scope__restrictive_originalist, suppression(class), 1787, 0.71).
narrative_ontology:measurement(equa_grid_26, equality_clause_scope__restrictive_originalist, suppression(class), 1870, 0.84).
narrative_ontology:measurement(equa_grid_27, equality_clause_scope__restrictive_originalist, suppression(individual), 1787, 0.68).
narrative_ontology:measurement(equa_grid_28, equality_clause_scope__restrictive_originalist, suppression(individual), 1870, 0.83).
narrative_ontology:measurement(equa_grid_29, equality_clause_scope__restrictive_originalist, suppression(organizational), 1787, 0.61).
narrative_ontology:measurement(equa_grid_30, equality_clause_scope__restrictive_originalist, suppression(organizational), 1870, 0.79).
narrative_ontology:measurement(equa_grid_31, equality_clause_scope__restrictive_originalist, suppression(structural), 1787, 0.62).
narrative_ontology:measurement(equa_grid_32, equality_clause_scope__restrictive_originalist, suppression(structural), 1870, 0.81).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equality_clause_scope__restrictive_originalist, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(equality_clause_scope__restrictive_originalist, 0.18).
narrative_ontology:affects_constraint(equality_clause_scope__restrictive_originalist, equality_clause_scope__expansive_universalist).
narrative_ontology:affects_constraint(equality_clause_scope__restrictive_originalist, equality_clause_scope__progressive_textualist).
narrative_ontology:affects_constraint(equality_clause_scope__restrictive_originalist, fourteenth_amendment_enforcement).
narrative_ontology:affects_constraint(equality_clause_scope__restrictive_originalist, franchise_expansion_constitutional_amendments).

% DUAL FORMULATION NOTE:
% The equality_clause_scope kernel decomposes into three structurally distinct constraints per the ε-invariance principle: restrictive_originalist (this story) carries high extraction (0.78) because it concentrates personhood and franchise; expansive_universalist carries near-zero extraction because universality of the principle makes no one an outsider; progressive_textualist carries moderate extraction because amendment is high-friction but open. Each reading instantiates a different beneficiary set, different suppression mechanism, and different type. The three stories are linked by kernel membership and competitive interpretation: the restrictive reading's persistence depends on suppressing the other two readings' institutional access (control of courts, blocking amendment success). The measurements document the restrictive reading's trajectory from genuine coordination (preventing elite infighting) toward pure extraction (denying personhood to non-beneficiaries) over 1787–1870.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
