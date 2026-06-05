% ============================================================================
% CONSTRAINT STORY: basic_rights_catalog__proportionality_doctrine
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_basic_rights_catalog__proportionality_doctrine, []).

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
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: basic_rights_catalog__proportionality_doctrine
 *   human_readable: Proportionality Doctrine in Basic Rights Catalog
 *   domain: constitutional_law/fundamental_rights
 *
 * SUMMARY:
 *   The proportionality doctrine represents a specific judicial reading of
 *   the basic rights catalog: every limitation of every right must pass
 *   through the same four-step test — is the limitation suitable to achieve a
 *   legitimate aim? Is it necessary (no less restrictive alternative)? Is it
 *   proportionate in the narrow sense (does the benefit to the legitimate aim
 *   outweigh the harm to the right)? This reading of the catalog creates a
 *   tangled coordination-extraction hybrid. It coordinates by creating a
 *   transparent, principled method for adjudicating rights claims and
 *   regulatory limitations. But it extracts by suppressing categorical
 *   certainty (prior rules that prohibited or protected conduct categorically
 *   are now subject to case-by-case rebalancing), by shifting enforcement
 *   costs to litigants and legislative drafters, and by creating
 *   opportunities for strategic use of proportionality language to justify
 *   outcomes that post-hoc rationalize political preferences. The doctrine's
 *   theater has grown over its 40-year application: early proportionality
 *   (1980s) was genuinely methodological; contemporary proportionality
 *   (2020s) is heavily performative, invoked as legitimate reasoning
 *   regardless of whether the balance actually controls the outcome. The
 *   catalog kernel is contested among four reading traditions:
 *   proportionality_doctrine (this story), essence_guarantee (a floor beneath
 *   proportionality), informational_self_determination (new rights read from
 *   old text), and objective_values_order (rights radiating into private
 *   law). This story instantiates only the proportionality reading.
 *
 * KEY AGENTS:
 *   - Individual Rights Claimants: Primary beneficiary from case-by-case balancing (institutional/arbitrage or moderate/constrained depending on litigation access) — gain avenue to challenge categorical prohibitions
 *   - Categorical Rule Frameworks: Primary victim (powerless/trapped) — categorical certainty is suppressed; prior rules are re-litigable under proportionality standard; affected actors face compliance uncertainty
 *   - Rights Advocacy Organizations: Secondary beneficiary (organized/mobile) — control gatekeeping for which claims get litigated; coordinate collective action; extract value from litigation strategy and expertise
 *   - Legislative Bodies: Mixed victim-beneficiary (institutional/constrained) — benefit from legitimate justification framework; harmed by increased burden of justifying every limitation
 *   - Constitutional Courts: Primary administrator (institutional/arbitrage) — empower themselves through requirement to conduct proportionality balancing; maintain interpretive authority
 *   - Bright-Line-Rule Certainty Seekers: Secondary victim (institutional/constrained or powerless/trapped) — prior certainty (conduct was clearly permitted or prohibited) is dissolved into case-by-case litigation risk
 *   - Analytical Observer: Civilizational analytical context — risks naturalizing proportionality as inherent to all legitimate governance, obscuring that it is a specific doctrinal choice with real distributional effects
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(basic_rights_catalog__proportionality_doctrine, 0.58).
domain_priors:suppression_score(basic_rights_catalog__proportionality_doctrine, 0.62).
domain_priors:theater_ratio(basic_rights_catalog__proportionality_doctrine, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(basic_rights_catalog__proportionality_doctrine, extractiveness, 0.58).
narrative_ontology:constraint_metric(basic_rights_catalog__proportionality_doctrine, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(basic_rights_catalog__proportionality_doctrine, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(basic_rights_catalog__proportionality_doctrine, tangled_rope).
narrative_ontology:human_readable(basic_rights_catalog__proportionality_doctrine, "Proportionality Doctrine in Basic Rights Catalog").
narrative_ontology:topic_domain(basic_rights_catalog__proportionality_doctrine, "constitutional_law/fundamental_rights").

domain_priors:requires_active_enforcement(basic_rights_catalog__proportionality_doctrine).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(basic_rights_catalog__proportionality_doctrine, 'b319f0ee-96b3-448a-bd53-9d56be44f1a1').
narrative_ontology:cs_kernel_codification('b319f0ee-96b3-448a-bd53-9d56be44f1a1', formalized).
narrative_ontology:cs_authority_grounding('b319f0ee-96b3-448a-bd53-9d56be44f1a1', lineage).
narrative_ontology:cs_interpretation_layer_present('b319f0ee-96b3-448a-bd53-9d56be44f1a1').
narrative_ontology:cs_reading_relation('b319f0ee-96b3-448a-bd53-9d56be44f1a1', basic_rights_catalog__essence_guarantee, coexists_with).
narrative_ontology:cs_reading_relation('b319f0ee-96b3-448a-bd53-9d56be44f1a1', basic_rights_catalog__informational_self_determination, influences).
narrative_ontology:cs_reading_relation('b319f0ee-96b3-448a-bd53-9d56be44f1a1', basic_rights_catalog__objective_values_order, influences).
narrative_ontology:cs_axiom('b319f0ee-96b3-448a-bd53-9d56be44f1a1', foundational, uniform_four_step_methodology).
narrative_ontology:cs_axiom_status(uniform_four_step_methodology, holdable).
narrative_ontology:cs_axiom_grounding('b319f0ee-96b3-448a-bd53-9d56be44f1a1', uniform_four_step_methodology, instrumental).
narrative_ontology:cs_axiom('b319f0ee-96b3-448a-bd53-9d56be44f1a1', foundational, weighability_of_all_limited_interests).
narrative_ontology:cs_axiom_status(weighability_of_all_limited_interests, overridden).
narrative_ontology:cs_axiom_grounding('b319f0ee-96b3-448a-bd53-9d56be44f1a1', weighability_of_all_limited_interests, deontological).
narrative_ontology:cs_reference_frame('b319f0ee-96b3-448a-bd53-9d56be44f1a1', proportionality_as_universal_method).
narrative_ontology:cs_drift_state('b319f0ee-96b3-448a-bd53-9d56be44f1a1', contemporary_performativity_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('b319f0ee-96b3-448a-bd53-9d56be44f1a1', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(basic_rights_catalog__proportionality_doctrine, basic_rights_catalog).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(basic_rights_catalog__proportionality_doctrine, individual_claimants_with_weighable_interests).
narrative_ontology:constraint_victim(basic_rights_catalog__proportionality_doctrine, categorical_rule_frameworks).
narrative_ontology:constraint_victim(basic_rights_catalog__proportionality_doctrine, bright_line_certainty_seekers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CATEGORICAL RULE HOLDER (SNARE) — A person or institution whose conduct was previously governed by clear categorical rules (e.g., 'religious headwear prohibited in public service') now faces case-by-case proportionality balancing. They are trapped: cannot exit the jurisdiction; cannot predict what conduct is permissible until litigated; bear the cost of compliance uncertainty and potential retroactive reclassification. Proportionality doctrine suppresses the categorical certainty they relied on.
constraint_indexing:constraint_classification(basic_rights_catalog__proportionality_doctrine, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: RIGHTS CLAIMANT / MODERATE EXIT (ROPE) — Individual claimant seeking to vindicate a right against categorical suppression (e.g., seeking exemption from headwear rule on religious grounds). Experiences proportionality as coordination mechanism: the doctrine creates a forum for claiming that the categorical rule is not proportionate to their legitimate interest. Benefits from access to balancing; constrained by litigation cost and delay. This perspective sees genuine coordination.
constraint_indexing:constraint_classification(basic_rights_catalog__proportionality_doctrine, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: RIGHTS ADVOCACY ORGANIZATION (TANGLED ROPE) — Organized agents (NGOs, unions, professional associations) use proportionality doctrine to challenge categorical rules. They coordinate access to courts, develop litigation strategy, and create jurisprudence that benefits their constituencies. But the coordination is mixed with extraction: the machinery for proportionality balancing becomes a site of resource capture. Litigation funding gaps mean some claimants get organized advocacy (beneficiary effect) while others do not (victim effect). The organizations extract value from their gatekeeping role in determining which claims get brought.
constraint_indexing:constraint_classification(basic_rights_catalog__proportionality_doctrine, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: LEGISLATIVE BODY (TANGLED ROPE) — Parliament uses proportionality doctrine to enact limitations on rights. The doctrine coordinates legitimate regulation: legislators must show that a right limitation is suitable, necessary, and narrowly proportionate. But it also extracts: the doctrine creates an obligation to justify every rule, raising legislative costs and creating opportunities for technical legal challenge. Legislatures are constrained: they cannot simply prohibit conduct categorically; they must weigh and explain. Some of this is genuine coordination (transparent justification); some is extraction (burden shifting to legislative drafters, empowering courts to second-guess political choices).
constraint_indexing:constraint_classification(basic_rights_catalog__proportionality_doctrine, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: CONSTITUTIONAL COURT (SCAFFOLD) — The judicial institution that administers proportionality balancing. Courts see proportionality as a temporary scaffold: it enables case-by-case calibration of rights and regulations during the maturation of new social configurations (digital rights, religious pluralism, gender identity). The court's temporal role is to mediate until legislative categories catch up. Once a new category is settled (e.g., informational self-determination becomes standard data protection law), the proportionality work diminishes and the scaffold becomes less necessary. But courts benefit from the doctrine's requirement to litigate every limitation — the doctrine empowers judicial review and generates jurisprudential authority. Low theater ratio for the court's structural role, but courts have arbitrage: they can move between strict categorical review and deferential proportionality balancing depending on political conditions.
constraint_indexing:constraint_classification(basic_rights_catalog__proportionality_doctrine, scaffold,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ACADEMIC DOCTRINAL SYSTEM (PITON) — Legal scholarship treats proportionality as the foundational method for rights adjudication. Academic commentary is heavily performative: proportionality is invoked as the rational way to handle rights limitations, but the doctrine's actual application varies wildly across contexts, and most rights limitations would likely pass scrutiny under any four-step test if administered sincerely. The academic system maintains the proportionality framework through institutional inertia and because no alternative doctrine offers cleaner legitimacy claims. Theater ratio is high because the scholarly consensus that proportionality is 'the method' persists despite evidence that proportionality is often post-hoc rationalization. But the system extracts value: academics maintain interpretive authority over what counts as 'suitable' and 'proportionate,' and this authority translates into consulting roles, expert witness positions, and influence over legal training.
constraint_indexing:constraint_classification(basic_rights_catalog__proportionality_doctrine, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational, universal perspective, proportionality appears as an immutable feature of legitimate governance: any rights system must balance protection of the right against legitimate public interests; there is no escape from proportionality reasoning; it follows from the logic of having both rights and regulatory authority. However, this perspective risks naturalizing a contingent doctrinal choice. The proportionality doctrine is a specific reading of the basic rights catalog — other readings (essence guarantee, objective values order, informational self-determination) offer different calibrations. The engine will detect this as a false summit: naturalizing proportionality as universal law obscures that it is one reading among contested alternatives.
constraint_indexing:constraint_classification(basic_rights_catalog__proportionality_doctrine, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(basic_rights_catalog__proportionality_doctrine_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(basic_rights_catalog__proportionality_doctrine, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(basic_rights_catalog__proportionality_doctrine, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(basic_rights_catalog__proportionality_doctrine, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(basic_rights_catalog__proportionality_doctrine, TR),
    TR >= 0.70.

:- end_tests(basic_rights_catalog__proportionality_doctrine_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The proportionality doctrine coordinates legitimate rights adjudication but extracts through suppression of categorical certainty and shifting of enforcement costs. The extraction is not maximal (0.72+) because genuine coordination benefits exist — claimants gain access to principled balancing forums, and regulators gain legitimate justification framework. But the extraction is real: the doctrine redistributes power from those who relied on categorical rules to those with resources for litigation and expert advocacy. The value reflects that extractiveness has grown over time (measurement trajectory 0.35→0.48→0.58) as proportionality has become more performative and less genuinely methodological. Suppression (0.62): Moderate-high. The doctrine suppresses categorical alternatives, litigation finality (prior outcomes become subject to rebalancing), and political closure (every rule can be re-litigated). Suppression is not total because courts can defer under proportionality (treating the formula as permitting wide regulatory latitude), and legislation can respond by re-enacting rules in proportionality-compatible language. Theater ratio (0.68): Moderate-high. Proportionality balancing is substantially performative: courts invoke the four-step framework but often reach predetermined conclusions, and proportionality language is used to legitimate outcomes that turn on unweighted political preferences. The ratio has grown (0.45→0.62→0.68) as the doctrine has accumulated doctrinal elaboration, complexity, and case law exception-building. Claimed type (tangled_rope): The doctrine exhibits genuine coordination (transparent method for rights adjudication) alongside asymmetric extraction (suppression of categorical alternatives, resource capture by litigation gatekeepers). Requires active enforcement (true): Courts must conduct proportionality analysis and maintain the four-step methodology across cases. Beneficiaries (individual claimants with weighable interests): Those whose interests can be articulated as rights and weighted in a balancing formula. Victims (categorical rule frameworks, bright-line certainty seekers): Those who relied on categorical rules and lose the benefit of settled expectations.
 *
 * PERSPECTIVAL GAP:
 *   The proportionality doctrine produces a maximal perspectival gap across all seven perspectives. The categorical rule holder sees a snare (trapped in compliance uncertainty and subject to retroactive rebalancing). The individual rights claimant with moderate exit sees a rope (access to balancing forum). The organized advocacy group sees tangled rope (coordination + gatekeeping extraction). The legislative body sees tangled rope (coordination + burden). The constitutional court sees a scaffold (temporal medium for case-by-case mediation, with sunset as categories mature). Academic doctrine sees a piton (performative framework maintained through institutional inertia). The civilizational analytical observer risks seeing a mountain (proportionality as inherent to legitimate rights governance) — but this is a false summit detected by the structural data showing identifiable beneficiaries and victims. The gap reveals that proportionality is not a universal principle but a specific reading of the contested catalog kernel, one that benefits certain actors (rights claimants with litigation resources) while harming others (rule clarity seekers).
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality is determined by the agent's structural position relative to the proportionality doctrine's extraction flow. Categorical rule holders are full targets (d ≈ 0.95): they lose prior certainty and bear compliance costs, face no exit option. Individual claimants are mixed (d ≈ 0.50 for moderate, d ≈ 0.35 for organized advocates): they gain access to balancing but also bear litigation costs. Courts are beneficiaries with arbitrage (d ≈ 0.15): the doctrine empowers their review authority. The analytical observer's directionality is neutral at civilization scale (d ≈ 0.73), but this neutrality masks that from biographical or generational time, the observer can identify winners and losers. The derived d values feed the chi formula: χ = ε × f(d) × σ(S). For the trapped categorical rule holder, f(d) is maximum (~1.42), amplifying experienced extractiveness. For the beneficiary institutional actor with arbitrage, f(d) is minimum (~-0.12), producing negative experienced extraction (the doctrine subsidizes their authority).
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION: The mandatrophy is resolved by recognizing that proportionality doctrine is not a universal method but a specific reading of the contested catalog kernel. At the institutional/analytical contexts (perspectives 5, 6, 7), the doctrine appears rational and foundational. At the biographical powerless context (perspective 1), it appears as a snare. At the generational organized contexts (perspectives 3, 4), it appears as mixed coordination and extraction. The 'mandatrophy' — the apparent trap where all method becomes method-less — dissolves when the reading is recognized as one sibling interpretation among contested alternatives. The essence guarantee reading forecloses proportionality at the essence floor. Informational self-determination reading treats some interests (privacy, personality) as unweighable. Objective values order reading treats rights as radiating into private law, not just limiting regulation. By acknowledging that this story instantiates proportionality_doctrine and not the siblings, the framework admits that the catalog kernel is genuinely contested and the doctrine's universality claim is itself a substantive choice, not a neutral method.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    proportionality_vs_essence_precedence,
    'When proportionality balancing encounters a right''s essence (the irreducible core that cannot be weighed), which principle governs: the proportionality formula or the essence guarantee?',
    'Case law analysis: does the court apply proportionality all the way to the essence, or does it treat the essence as a floor beneath which no balancing applies? Historical review of cases where dignity, personhood, or core identity claims meet proportionality doctrine.',
    'If proportionality governs even the essence: the doctrine absorbs all limitation claims into the four-step mill (full tangled_rope). If the essence is a true floor: proportionality applies only to regulations above the floor, and the essence guarantee becomes a competing reading that forecloses proportionality at the boundary (tangled_rope with a structural ceiling).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(proportionality_vs_essence_precedence, empirical, 'Whether proportionality balancing applies to the essence of rights or encounters a floor').

omega_variable(
    categorical_suppression_mechanism,
    'Does proportionality doctrine actually suppress categorical rules that lack case-by-case weighing, or do legislative categorical rules survive because courts defer under the proportionality standard?',
    'Empirical study of legislation before and after proportionality doctrine adoption: do categorical rules persist (legislators continue to pass them, courts uphold them under proportionality deference), or are they replaced by case-by-case standards? Measurement of litigation burden and success rates for challenges to categorical rules.',
    'If categorical rules survive under proportionality deference: suppression is lower than modeled (the doctrine has limited practical force, behaves more like a piton). If categorical rules are systematically challenged and reformed: suppression is accurate (doctrine functions as modeled, tangled_rope holds).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(categorical_suppression_mechanism, empirical, 'Empirical effect of proportionality on categorical rule persistence').

omega_variable(
    weighting_bias_and_asymmetry,
    'Does the proportionality four-step mill systematically bias toward proportionate limitations of rights over proportionate assertion of rights? Does the doctrine weight regulatory interests more heavily than claimant interests?',
    'Corpus analysis of proportionality jurisprudence: distribution of outcomes favoring state interests vs individual rights; examination of how ''suitable'' and ''necessary'' are applied to regulatory justifications vs rights claims; measurement of the evidentiary burden imposed on each side.',
    'If systematic bias toward regulation: extractiveness rises (the doctrine operates as a tool for legitimating limitations while suppressing rights claims). If neutral weighting: extractiveness drops (the doctrine functions as genuine coordination). If bias toward rights: doctrine becomes a snare for state action (beneficiary is rights claimants, victim is regulatory capacity).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(weighting_bias_and_asymmetry, empirical, 'Whether proportionality four-step exhibits systematic weighting bias').

omega_variable(
    case_law_instability_and_retroactivity,
    'How stable are proportionality outcomes across similar cases? When courts revise proportionality assessments (e.g., deciding that religious headwear, previously regulated categorically, is proportionately protected in some contexts), do prior prohibitions remain justified or does retroactive rebalancing create unfair surprise?',
    'Longitudinal case law analysis: tracking of specific categorical rules and how proportionality assessment has shifted; measurement of outcome variability when panels with different compositions assess the same limitation; analysis of retroactivity doctrines (do claimants who obeyed the old categorical rule get relief when proportionality reverses it?)',
    'If outcomes are highly variable and retroactivity is asymmetric (new proportionality insights favor claimants going forward but don''t remedy past violations): extractiveness and suppression both rise (doctrine operates as trap for prior actors, uncertain framework for future actors). If outcomes are stable: extractiveness and theater ratio fall.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(case_law_instability_and_retroactivity, empirical, 'Stability of proportionality assessment across cases and time').

omega_variable(
    reading_kernel_ambiguity,
    'Is the basic rights catalog''s kernel of legitimacy fixed by proportionality doctrine, or does the proportionality reading coexist with competing readings (essence guarantee, informational self-determination, objective values order) that could structure the catalog differently?',
    'Textual and jurisprudential analysis: Does constitutional text mandate proportionality, or is proportionality one interpretive tradition among others? Can the same catalog text support the essence guarantee reading, informational self-determination reading, and objective values order reading, or does adopting one reading foreclose the others?',
    'If proportionality is the unique legitimate reading: the catalog itself is constituted by the four-step mill (mountain from some perspectives, false summit from others). If proportionality coexists with genuine alternatives: the readings are sibling constraints, each with its own ε, each legitimate from its own framework (the kernel itself is contested, and this story is one reading among live alternatives).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_kernel_ambiguity, conceptual, 'Whether proportionality is the unique legitimate reading of the basic rights catalog kernel or coexists with competing readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(basic_rights_catalog__proportionality_doctrine, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prop_theater_early, basic_rights_catalog__proportionality_doctrine, theater_ratio, 0, 0.45).
narrative_ontology:measurement(prop_theater_mid, basic_rights_catalog__proportionality_doctrine, theater_ratio, 20, 0.62).
narrative_ontology:measurement(prop_theater_late, basic_rights_catalog__proportionality_doctrine, theater_ratio, 40, 0.68).

% Extraction over time
narrative_ontology:measurement(prop_extract_early, basic_rights_catalog__proportionality_doctrine, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(prop_extract_mid, basic_rights_catalog__proportionality_doctrine, base_extractiveness, 20, 0.48).
narrative_ontology:measurement(prop_extract_late, basic_rights_catalog__proportionality_doctrine, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(prop_suppression_early, basic_rights_catalog__proportionality_doctrine, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(prop_suppression_mid, basic_rights_catalog__proportionality_doctrine, suppression_requirement, 20, 0.58).
narrative_ontology:measurement(prop_suppression_late, basic_rights_catalog__proportionality_doctrine, suppression_requirement, 40, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(basic_rights_catalog__proportionality_doctrine, enforcement_mechanism).
narrative_ontology:affects_constraint(basic_rights_catalog__proportionality_doctrine, basic_rights_catalog__essence_guarantee).
narrative_ontology:affects_constraint(basic_rights_catalog__proportionality_doctrine, basic_rights_catalog__informational_self_determination).
narrative_ontology:affects_constraint(basic_rights_catalog__proportionality_doctrine, basic_rights_catalog__objective_values_order).

% DUAL FORMULATION NOTE:
% This story is one reading of the basic_rights_catalog kernel. The catalog itself is contested among four reading traditions (proportionality_doctrine, essence_guarantee, informational_self_determination, objective_values_order). Each reading has its own constraint story with its own epsilon value, beneficiary/victim structure, and perspectives. Proportionality_doctrine story establishes that every limitation runs through the four-step mill (ε ≈ 0.58, tangled_rope). Essence_guarantee story establishes a floor beneath proportionality (ε varies, likely snare or tangled_rope at different contexts). Network edges document that proportionality_doctrine influences (creates downstream pressure on) the siblings by establishing the default framework for rights adjudication — sibling readings must position themselves relative to proportionality, either by adding constraints (essence floor) or by reinterpreting what counts as a weighable interest (informational self-determination) or by extending rights into new domains (objective values order).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(basic_rights_catalog__proportionality_doctrine, institutional, 0.15).
constraint_indexing:directionality_override(basic_rights_catalog__proportionality_doctrine, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
