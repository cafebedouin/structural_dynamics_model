% ============================================================================
% CONSTRAINT STORY: geneva_conventions_1949__humanitarian_ceiling_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_geneva_conventions_1949__humanitarian_ceiling_reading, []).

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
 *   constraint_id: geneva_conventions_1949__humanitarian_ceiling_reading
 *   human_readable: Geneva Conventions 1949: Humanitarian Ceiling Reading
 *   domain: international_humanitarian_law/law_of_armed_conflict
 *
 * SUMMARY:
 *   The Geneva Conventions of 1949 and their Additional Protocols establish
 *   absolute minimum standards for the treatment of protected persons
 *   (civilians, prisoners of war, detainees) in armed conflict. The
 *   humanitarian ceiling reading asserts that these protections are
 *   non-negotiable: states must respect them regardless of whether
 *   adversaries comply, whether reciprocity is honored, or whether security
 *   pressures demand exceptions. This reading instantiates one of three
 *   structurally distinct readings of the 1949 kernel. The humanitarian
 *   ceiling reading competes with a conditional reciprocity reading
 *   (protections are contingent on mutual compliance) and a security
 *   maximization reading (humanitarian norms yield when state security is
 *   threatened). From the perspective of protected persons, military field
 *   commanders, and adversary combatants, the humanitarian ceiling reading
 *   functions as extraction: it constrains state military operational
 *   flexibility, suppresses security-based justifications for violence, and
 *   imposes asymmetric burdens on signatory states. From the perspective of
 *   the IHL epistemic community and state governments that endorse the
 *   humanitarian framing, the Conventions function as coordination: they
 *   establish rules that all states benefit from, enabling bounded conflict
 *   and mutual POW protection. The constraint exhibits high suppression
 *   (0.72) because state military institutions face structural barriers to
 *   override the humanitarian minimum—political authority, international law,
 *   reputational costs, and institutional commitment—even when security
 *   pressures are acute. The theater_ratio (0.58) reflects that much IHL
 *   compliance is performative: formal investigations of alleged violations,
 *   ritualized procedures, legal documentation that provides cover for
 *   marginal violations or selective enforcement.
 *
 * KEY AGENTS:
 *   - Protected Persons (Civilians, POWs, Detainees): Primary victims (powerless/trapped) — cannot exit the regime; dependent on state compliance for safety
 *   - Subordinate Military Command: Secondary victim (moderate/constrained) — face operational restrictions and training requirements; subject to institutional hierarchy enforcement
 *   - State Military Institution: Institutional actor (organized/constrained) — experiences mixed coordination benefit (reciprocal POW protection) and extraction (operational flexibility suppression); asymmetric burden relative to security-maximization alternatives
 *   - Signatory State Government (Humanitarian Reading): Institutional beneficiary (institutional/arbitrage) — chooses commitment to humanitarian ceiling; derives legitimacy and coordination benefits from the regime
 *   - Military Intelligence Specialist (Humanitarian Committed): Identity-locked agent (moderate/identity_locked) — professional identity fused with humanitarian framework; structurally mobile but psychologically trapped within the humanitarian reading
 *   - Non-Signatory Adversary Combatant: Primary victim of regime expansion (moderate/constrained) — subject to humanitarian protections imposed by signatory states without formal consent; experiences extraction through universalization of the regime
 *   - International Humanitarian Law Community (ICRC, NGOs, Lawyers): Institutional beneficiary (institutional/arbitrage) — benefits from mandate, expertise, and moral authority; sees the regime as pure coordination (Rope perspective)
 *   - Analytical Observer: Civilizational context (analytical/analytical) — risks naturalizing a constructed constraint; the humanitarian ceiling may appear as an immutable law of armed conflict
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(geneva_conventions_1949__humanitarian_ceiling_reading, 0.68).
domain_priors:suppression_score(geneva_conventions_1949__humanitarian_ceiling_reading, 0.72).
domain_priors:theater_ratio(geneva_conventions_1949__humanitarian_ceiling_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(geneva_conventions_1949__humanitarian_ceiling_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(geneva_conventions_1949__humanitarian_ceiling_reading, snare).
narrative_ontology:human_readable(geneva_conventions_1949__humanitarian_ceiling_reading, "Geneva Conventions 1949: Humanitarian Ceiling Reading").
narrative_ontology:topic_domain(geneva_conventions_1949__humanitarian_ceiling_reading, "international_humanitarian_law/law_of_armed_conflict").

domain_priors:requires_active_enforcement(geneva_conventions_1949__humanitarian_ceiling_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(geneva_conventions_1949__humanitarian_ceiling_reading, 'b47e9c2f-8d1a-4e2c-9f3a-7e2c1d4b9a6f').
narrative_ontology:cs_kernel_codification('b47e9c2f-8d1a-4e2c-9f3a-7e2c1d4b9a6f', fixed_text).
narrative_ontology:cs_authority_grounding('b47e9c2f-8d1a-4e2c-9f3a-7e2c1d4b9a6f', lineage).
narrative_ontology:cs_interpretation_layer_present('b47e9c2f-8d1a-4e2c-9f3a-7e2c1d4b9a6f').
narrative_ontology:cs_reading_relation('b47e9c2f-8d1a-4e2c-9f3a-7e2c1d4b9a6f', geneva_conventions_1949__conditional_reciprocity_reading, coexists_with).
narrative_ontology:cs_reading_relation('b47e9c2f-8d1a-4e2c-9f3a-7e2c1d4b9a6f', geneva_conventions_1949__security_maximization_reading, influences).
narrative_ontology:cs_axiom('b47e9c2f-8d1a-4e2c-9f3a-7e2c1d4b9a6f', foundational, humanitarian_protection_non_negotiable).
narrative_ontology:cs_axiom_status(humanitarian_protection_non_negotiable, holdable).
narrative_ontology:cs_axiom_grounding('b47e9c2f-8d1a-4e2c-9f3a-7e2c1d4b9a6f', humanitarian_protection_non_negotiable, deontological).
narrative_ontology:cs_axiom('b47e9c2f-8d1a-4e2c-9f3a-7e2c1d4b9a6f', foundational, absolute_restraint_regardless_reciprocity).
narrative_ontology:cs_axiom_status(absolute_restraint_regardless_reciprocity, holdable).
narrative_ontology:cs_axiom_grounding('b47e9c2f-8d1a-4e2c-9f3a-7e2c1d4b9a6f', absolute_restraint_regardless_reciprocity, deontological).
narrative_ontology:cs_reference_frame('b47e9c2f-8d1a-4e2c-9f3a-7e2c1d4b9a6f', universal_humanitarian_protection_absolute).
narrative_ontology:cs_drift_state('b47e9c2f-8d1a-4e2c-9f3a-7e2c1d4b9a6f', contemporary_post_2000, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('b47e9c2f-8d1a-4e2c-9f3a-7e2c1d4b9a6f', '2026-02-27T14:22:33Z').
narrative_ontology:cs_kernel_id(geneva_conventions_1949__humanitarian_ceiling_reading, geneva_conventions_1949).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__humanitarian_ceiling_reading, protected_persons_civilians).
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__humanitarian_ceiling_reading, prisoners_of_war).
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__humanitarian_ceiling_reading, detainees_and_irregular_combatants).
narrative_ontology:constraint_victim(geneva_conventions_1949__humanitarian_ceiling_reading, military_operational_flexibility).
narrative_ontology:constraint_victim(geneva_conventions_1949__humanitarian_ceiling_reading, state_security_discretion).
narrative_ontology:constraint_victim(geneva_conventions_1949__humanitarian_ceiling_reading, belligerent_war_fighting_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PROTECTED PERSONS (SNARE) — Civilians, prisoners of war, and detainees are trapped within the humanitarian regime. They cannot exit the constraint; their only leverage is the regime's enforcement. Suppression is structural: state military forces control the physical environment of detention, interrogation, and treatment. The constraint binds because external enforcement (ICRC, international courts) is weak relative to state capacity. High experienced extraction: the constraint depends entirely on state compliance despite internal military pressure to violate it.
constraint_indexing:constraint_classification(geneva_conventions_1949__humanitarian_ceiling_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: SUBORDINATE MILITARY COMMAND (SNARE) — Military field commanders face high suppression: they are subordinate to political authority that has ratified the Conventions but face direct operational pressure from security threats, intel requirements, and combat stress. Exit options are constrained — disobedience risks court-martial; compliance risks unit vulnerability. The constraint extracts significant operational flexibility (cannot use torture for intel, cannot execute prisoners, cannot indiscriminately target civilians). The Conventions function as extraction devices limiting tactical options, enforced by institutional hierarchy rather than external enforcement.
constraint_indexing:constraint_classification(geneva_conventions_1949__humanitarian_ceiling_reading, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: STATE MILITARY INSTITUTION (TANGLED ROPE) — At institutional scale, the Conventions coordinate interstate conflict by establishing predictable rules. States benefit from mutual commitment: if all states follow the Conventions, all states' soldiers receive POW protection if captured. This is genuine coordination (Rope component). However, the reading declares an asymmetric burden: the Conventions suppress security rationales, force protection of adversary combatants and detainees, and restrict operational methods. The military institution experiences this as extraction of restraint capacity. The constraint is hybrid: coordination benefit (mutual POW protection) + extraction (operational restrictions). The state is constrained by reciprocity expectations; if adversaries violate, the state cannot retaliate without delegitimization cost (reputational damage, legal liability).
constraint_indexing:constraint_classification(geneva_conventions_1949__humanitarian_ceiling_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: SIGNATORY STATE GOVERNMENT / HUMANITARIAN READING (ROPE) — From the perspective of state leadership that endorses the humanitarian ceiling reading, the Conventions are a coordination mechanism. States benefit from a rules-based war system that reduces violence escalation, provides legal clarity, and builds legitimacy through commitment to universal humanitarian standards. The state has arbitrage options: it can withdraw from the Conventions (legally possible, though costly), but chooses to stay. Net beneficiary — the regime serves state interests in predictable, bounded conflict and moral authority.
constraint_indexing:constraint_classification(geneva_conventions_1949__humanitarian_ceiling_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: INTERNATIONAL HUMANITARIAN LAW COMMUNITY (ROPE) — ICRC, treaty bodies, humanitarian NGOs, and the epistemic community of international lawyers see the Conventions as coordination (pure Rope). The community benefits from the constraint through institutional mandate, funding, professional expertise, and moral authority. Exit options are arbitrage — the community could reframe around different principles (security maximization, conditional reciprocity) but maintains commitment to the humanitarian ceiling. No contradiction with military institution perspective; the IHL community and military institution differ in their power position relative to the constraint.
constraint_indexing:constraint_classification(geneva_conventions_1949__humanitarian_ceiling_reading, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: MILITARY INTELLIGENCE SPECIALIST / IDENTITY-LOCKED (TANGLED ROPE) — An intelligence officer committed to the humanitarian ceiling reading experiences structural mobility (could legally pursue security-maximization approaches, faces not-insurmountable barriers to exit the organization) but is identity-locked: their professional identity, ethical self-concept, and career narrative are constituted through the humanitarian framework. Exit would require abandoning not just a job but a fused identity (the righteous warrior, the ethical professional). The constraint extracts interrogation methods and detainee treatment restrictions while coordinatinga rule-based professional community. At biographical time, identity-locked agents see Tangled Rope (they perceive the constraint as normatively changeable in principle, but cannot change their relationship to it without identity dissolution). This perspective instantiates the diagnostic gap: the agent is structurally mobile but psychologically trapped.
constraint_indexing:constraint_classification(geneva_conventions_1949__humanitarian_ceiling_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 7: NON-SIGNATORY ADVERSARY COMBATANT (SNARE) — Fighters from states or armed groups not formally bound by the Conventions (de jure non-signatories or de facto non-compliers) still face the regime's extraction: signatory states treat them as subjects of humanitarian law anyway (claiming customary international law universality). They bear costs (POW treatment obligations, restrictions on targeting) with minimal reciprocal benefit. High suppression: they have no seat at the treaty negotiation, no ratification choice, no exit. The Conventions function as extraction: a regime imposed on them by signatory powers.
constraint_indexing:constraint_classification(geneva_conventions_1949__humanitarian_ceiling_reading, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, this reading risks naturalizing the humanitarian ceiling as an unchangeable principle of law. The observer sees the Conventions as reflecting immutable human dignity, universal ethical minimums, and logical prerequisites for any legal order (the mountain: 'you cannot legally wage war without humanitarian protections'). However, the structural data reveals beneficiaries, suppressed security rationales, and enforcement mechanisms — all signatures of a constructed constraint. The engine will flag this as a false summit.
constraint_indexing:constraint_classification(geneva_conventions_1949__humanitarian_ceiling_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(geneva_conventions_1949__humanitarian_ceiling_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(geneva_conventions_1949__humanitarian_ceiling_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(geneva_conventions_1949__humanitarian_ceiling_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(geneva_conventions_1949__humanitarian_ceiling_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(geneva_conventions_1949__humanitarian_ceiling_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High-moderate. The humanitarian ceiling reading suppresses security rationales and restricts operational flexibility, producing substantial extraction from military institutions and field commands. However, the extraction is not maximal because genuine coordination benefits exist (reciprocal POW protection, mutual restraint reducing violence escalation, legitimacy gains from humanitarian commitment). The increasing trajectory (0.55 → 0.62 → 0.68 over 40 years) reflects that extractive burden has grown as international law has expanded protections beyond the 1949 text: Protocol I (1977) extended combatant protections, Protocol II (1977) addressed non-international conflicts, and customary law doctrines have further universalized obligations. Suppression (0.72): High. Military institutions cannot easily bypass the regime. State political leadership, international law, reputational costs, and institutional hierarchy create multiple layers of suppression. Subordinate commanders cannot override because of chain of command; states cannot withdraw without diplomatic costs; military populations are trained and institutionalized into compliance. However, suppression is not absolute (0.85+) because states retain de facto enforcement discretion—violations occur routinely, enforcement is selective, and powerful states negotiate reservations (US exemptions on Protocol I). Theater_ratio (0.58): Moderate-high. The regime produces significant performative activity: formal investigation procedures for alleged violations, legal documentation of compliance, military training on IHL, ICRC monitoring and reporting. However, theater is not dominant (0.70+) because genuine protective mechanisms exist (ICRC access, international courts, customary law enforcement through litigation). The increasing trajectory reflects growing gap between formal procedures and substantive protection: more documentation and investigation procedures have been added, but enforcement against powerful states remains weak.
 *
 * PERSPECTIVAL GAP:
 *   The humanitarian ceiling reading produces maximum perspectival divergence. Protected persons and subordinate commanders see Snare (trapped, high extraction, suppression enforced through hierarchy and asymmetric vulnerability). The military institution at generational scale sees Tangled Rope (genuine coordination benefit from reciprocal POW protection, but asymmetric suppression of operational flexibility). State governments that endorse the humanitarian reading see Rope (mutual coordination, arbitrage exit option, net benefit from legitimacy and bounded conflict). The IHL community sees pure Rope (coordination function, professional benefit). The military intelligence officer locked in humanitarian identity sees Tangled Rope at biographical scale but Mountain at immediate scale (perceives the regime as an immutable ethical law). Non-signatory combatants see Snare (imposed protections without consent, asymmetric burden). The analytical observer risks seeing Mountain (naturalizing the humanitarian ceiling as immutable law of armed conflict) when the structural data reveals beneficiaries and suppression mechanisms.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective derives from beneficiary/victim status and exit options. Protected persons (trapped, victims) → d ≈ 0.95 → f(d) ≈ 1.42 (powerless experience). Subordinate commanders (constrained, victims of operational restriction) → d ≈ 0.72 → f(d) ≈ 1.10 (moderate-high experience). Military institution (both beneficiary of reciprocal POW protection AND victim of operational restriction) → d ≈ 0.50 → f(d) ≈ 0.65 (symmetric). State government endorsing humanitarian reading (beneficiary of coordination and legitimacy, arbitrage exit) → d ≈ 0.15 → f(d) ≈ -0.01 (net beneficiary, low chi). IHL community (beneficiary, arbitrage) → d ≈ 0.10 → f(d) ≈ -0.10 (institutional beneficiary). Intelligence specialist (identity-locked victim, moderate power) → d ≈ 0.60 → f(d) ≈ 0.85 (moderate-high experienced extraction despite structural mobility). Non-signatory combatant (trapped, victim, no exit) → d ≈ 0.95 → f(d) ≈ 1.42 (powerless experience). The humanitarian ceiling reading amplifies d for all agent categories: it declares a wide beneficiary class (protected persons as beneficiaries of protections, even though they experience high extraction) and a correspondingly wide victim class (security-focused state institutions, military commands, non-signatories). The schema reconciles this: beneficiaries under the humanitarian ceiling reading are those whom the reading aims to protect (the logic of humanitarian protection), not those who capture rents. The reading's structural consequence is to extract from military flexibility (d ≈ 0.60-0.75 for military actors) and grant protection to vulnerable populations (d ≈ 0.95 for trapped protected persons).
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by the reading structure itself. The humanitarian ceiling reading declares that humanitarian protections are non-negotiable minimums. This declaration produces high suppression and high extractiveness because it forecloses the security-maximization escape route: states cannot say 'we will abandon humanitarian protections when security pressures are acute.' However, the reading also produces genuine coordination benefits (reciprocal POW protection, bounded conflict) that distinguish Snare from pure Snare. The classification is Snare from the protected persons' and military commands' perspectives (high suppression, asymmetric extraction) but Tangled Rope and Rope from institutional perspectives (genuine coordination mixed with extraction). The mandatrophy resolves through perspectival pluralism: there is no single 'true' type because the reading declares different agent positions relative to the constraint. The reading's success (or failure) depends on whether states voluntarily sustain the humanitarian commitment despite security pressures. If enforcement capacity is weak and states routinely violate with minimal consequence, the Snare classification is accurate. If enforcement is strong (reputational costs, institutional pressure, legal liability) and violations trigger recalibration, the constraint approaches Rope (states actually benefit from mutual compliance).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    humanitarian_ceiling_vs_security_maximization,
    'Is the humanitarian ceiling reading a genuine commitment to universal moral minimums, or a strategic constraint that benefits certain states by restricting operational flexibility?',
    'Historical analysis of state ratification patterns, compliance trends, and selective enforcement. Compare compliance rates for wealthy vs resource-constrained states. Analyze non-ratification by powerful states (e.g., US reservations on Protocol I). If compliance is selective and aligned with state power, the reading masks security extraction.',
    'If genuine commitment: the constraint is closer to Rope (mutual coordination). If strategic constraint: the classification remains Snare, revealing the humanitarian framing as cover for military extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(humanitarian_ceiling_vs_security_maximization, empirical, 'Whether humanitarian ceiling reflects universal ethics or strategic state interest').

omega_variable(
    enforcement_mechanism_gap,
    'What is the actual enforcement capacity of international humanitarian law mechanisms (ICRC, international courts, treaty bodies) relative to state military power?',
    'Quantitative analysis of enforcement outcomes: conviction rates for war crimes, compliance-to-violation ratios, ICRC access success rates, reputational costs to violating states. Compare enforcement against powerful states vs weak states. Measure lag between violation and accountability.',
    'If enforcement capacity is high: the constraint functions as Rope (mutual coordination with teeth). If enforcement capacity is low: the constraint is Snare sustained by states'' voluntary compliance and fear of reputational spillover.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforcement_mechanism_gap, empirical, 'Actual enforcement capacity of international humanitarian law regime').

omega_variable(
    alternative_reading_foreclosure,
    'Does the humanitarian ceiling reading logically foreclose the security-maximization reading, or do they coexist as different parties'' live positions?',
    'Examine whether a state could coherently hold both readings simultaneously: can a state commit to humanitarian minimums AND reserve the right to security override? Historical examples: US treatment of detainees post-9/11, Israeli security operations, Russian military doctrine. If states do hold both (with stated exceptions and caveats), the readings coexist rather than foreclose each other.',
    'If readings foreclose: the constraint''s type is determined by which reading prevails (humanitarian vs security). If readings coexist: the constraint is inherently ambiguous, and both Snare and Tangled Rope classifications are valid for different institutional actors.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_reading_foreclosure, conceptual, 'Logical foreclosure vs coexistence of humanitarian and security readings').

omega_variable(
    irregular_combatant_protection_scope,
    'What is the actual scope of humanitarian protections for irregular combatants and non-state armed groups under the humanitarian ceiling reading?',
    'Textual analysis of Protocol I (1977) and Protocol II (1977) regarding combatant recognition and POW status. Examine case law on definition of ''lawful combatant'' vs terrorism classification. Compare state practice: which states extend humanitarian protections to non-uniformed fighters, and under what conditions? Measure variance across treaty bodies and international courts.',
    'If protections are broad: the humanitarian ceiling constrains state discretion significantly (supports high suppression). If protections are narrow and state-determined: the ceiling is aspirational, and states retain effective security override (lowers suppression).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(irregular_combatant_protection_scope, empirical, 'Actual scope of protections for irregular combatants').

omega_variable(
    kernel_codification_stability,
    'Is the 1949 Geneva Convention text the actual kernel that grounds humanitarian authority, or is the kernel the evolving interpretive tradition and customary law that extends protections beyond the written text?',
    'Trace historical interpretation of the 1949 text: how much of current humanitarian practice relies on reading-between-the-lines, protocols added in 1977 and later, customary law doctrines, ICRC commentaries, and case law? If current practice substantially departs from 1949 text, the kernel has migrated from fixed-text to interpretive tradition.',
    'If kernel is fixed text (formalized): the humanitarian ceiling reading is constrained by 1949 language. If kernel is interpretive tradition (distributed): the ceiling reading has more flexibility and can expand protections without formal amendment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_codification_stability, conceptual, 'Locus of kernel authority in 1949 text vs interpretive tradition').

omega_variable(
    reciprocity_collapse_mechanism,
    'What happens to the humanitarian ceiling reading when adversaries violate the Conventions reciprocally? Does the reading permit retaliation, or does it insist on unilateral compliance regardless?',
    'Examine state doctrine on reprisals and retaliation. Review IHL literature on whether humanitarian obligations are conditional on reciprocity. Analyze historical cases where one party violated (e.g., irregular combatants not wearing uniforms, execution of prisoners) and measure whether signatory states maintained humanitarian compliance or escalated. If states maintain compliance despite violations, the reading is robust; if they escalate, reciprocity exceptions are live.',
    'If reading insists on unilateral compliance: suppression remains high (states absorb the cost). If reciprocity exceptions are permitted: suppression is lower (states have retaliatory options). The classification could shift from Snare to Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reciprocity_collapse_mechanism, empirical, 'Whether humanitarian ceiling permits reciprocal exception or requires unilateral compliance').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(geneva_conventions_1949__humanitarian_ceiling_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(genc_ceiling_tr_t0, geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 0, 0.45).
narrative_ontology:measurement(genc_ceiling_tr_t20, geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 20, 0.52).
narrative_ontology:measurement(genc_ceiling_tr_t40, geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 40, 0.58).

% Extraction over time
narrative_ontology:measurement(genc_ceiling_be_t0, geneva_conventions_1949__humanitarian_ceiling_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(genc_ceiling_be_t20, geneva_conventions_1949__humanitarian_ceiling_reading, base_extractiveness, 20, 0.62).
narrative_ontology:measurement(genc_ceiling_be_t40, geneva_conventions_1949__humanitarian_ceiling_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(genc_ceiling_su_t0, geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(genc_ceiling_su_t20, geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 20, 0.69).
narrative_ontology:measurement(genc_ceiling_su_t40, geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 40, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(geneva_conventions_1949__humanitarian_ceiling_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(geneva_conventions_1949__humanitarian_ceiling_reading, geneva_conventions_1949__conditional_reciprocity_reading).
narrative_ontology:affects_constraint(geneva_conventions_1949__humanitarian_ceiling_reading, geneva_conventions_1949__security_maximization_reading).
narrative_ontology:affects_constraint(geneva_conventions_1949__humanitarian_ceiling_reading, international_criminal_court_enforcement).
narrative_ontology:affects_constraint(geneva_conventions_1949__humanitarian_ceiling_reading, torture_prohibition_absolute_norm).

% DUAL FORMULATION NOTE:
% The 1949 Geneva Conventions contain a contested kernel that spawns three structurally distinct constraints: humanitarian_ceiling_reading (absolute minimums, ε ≈ 0.68, Snare from military perspective), conditional_reciprocity_reading (protections conditional on mutual compliance, ε ≈ 0.42, Tangled Rope), and security_maximization_reading (humanitarian norms override by security, ε ≈ 0.55, Tangled Rope). Each reading produces a different ε because they differ on what obligations states actually bear. The humanitarian ceiling reading is the most extractive from military institutions because it permits no security exceptions. The other readings have lower ε because they incorporate exit routes (reciprocity exception, security override). All three readings interpret the same text; the constraint family decomposes the textual kernel into its three coherent readings and measures each separately.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(geneva_conventions_1949__humanitarian_ceiling_reading, organized, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
