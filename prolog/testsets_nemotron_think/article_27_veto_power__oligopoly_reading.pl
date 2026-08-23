% ============================================================================
% CONSTRAINT STORY: article_27_veto_power__oligopoly_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: article_27_veto_power__oligopoly_reading
 *   human_readable: UN Security Council P5 Veto Power (Oligopoly Reading)
 *   domain: international_relations/institutional_design/constitutional_law
 *
 * SUMMARY:
 *   The UN Security Council veto (UN Charter Article 27(3)) grants each of
 *   the five permanent members an absolute block on substantive Council
 *   resolutions. This oligopoly reading argues the veto has evolved from a
 *   1945 great-power management tool into a structural entrenchment of
 *   geopolitical oligopoly. The Charter's amendment formula (Article 108)
 *   requires P5 consent for any change, creating a closed loop: the
 *   beneficiaries of the veto hold a veto over its reform. Extraction derives
 *   from the authority rents the P5 collect — privileged decision-making on
 *   peace, security, admissions, and Secretary-General selection — while the
 *   non-P5 majority bears the costs of blocked action with no exit from the
 *   system and no path to reform. The coordination cover story (preventing
 *   great-power war) is empirically weak: the veto did not prevent Cold War
 *   proxy wars, the Iraq War, Ukraine, or other conflicts involving P5
 *   members. Theater ratio has risen as veto use increasingly performs
 *   procedural rituals (penholder negotiations, Arria-formula meetings) while
 *   substantive outcomes remain blocked.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_27_veto_power__oligopoly_reading, 0.78).
domain_priors:suppression_score(article_27_veto_power__oligopoly_reading, 0.85).
domain_priors:theater_ratio(article_27_veto_power__oligopoly_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_27_veto_power__oligopoly_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(article_27_veto_power__oligopoly_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(article_27_veto_power__oligopoly_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_27_veto_power__oligopoly_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(article_27_veto_power__oligopoly_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_27_veto_power__oligopoly_reading, snare).
narrative_ontology:human_readable(article_27_veto_power__oligopoly_reading, "UN Security Council P5 Veto Power (Oligopoly Reading)").
narrative_ontology:topic_domain(article_27_veto_power__oligopoly_reading, "international_relations/institutional_design/constitutional_law").

domain_priors:requires_active_enforcement(article_27_veto_power__oligopoly_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_27_veto_power__oligopoly_reading, '410797ea-259a-40d2-81ec-1609a7d30cde').
narrative_ontology:cs_kernel_codification('410797ea-259a-40d2-81ec-1609a7d30cde', formalized).
narrative_ontology:cs_authority_grounding('410797ea-259a-40d2-81ec-1609a7d30cde', lineage).
narrative_ontology:cs_interpretation_layer_present('410797ea-259a-40d2-81ec-1609a7d30cde').
narrative_ontology:cs_reading_relation('410797ea-259a-40d2-81ec-1609a7d30cde', article_27_veto_power__coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('410797ea-259a-40d2-81ec-1609a7d30cde', article_27_veto_power__sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('410797ea-259a-40d2-81ec-1609a7d30cde', foundational, veto_as_oligopoly_rent_extraction).
narrative_ontology:cs_axiom_status(veto_as_oligopoly_rent_extraction, holdable).
narrative_ontology:cs_axiom_grounding('410797ea-259a-40d2-81ec-1609a7d30cde', veto_as_oligopoly_rent_extraction, empirically_contingent).
narrative_ontology:cs_axiom('410797ea-259a-40d2-81ec-1609a7d30cde', foundational, charter_immutability_blocks_reform).
narrative_ontology:cs_axiom_status(charter_immutability_blocks_reform, holdable).
narrative_ontology:cs_axiom_grounding('410797ea-259a-40d2-81ec-1609a7d30cde', charter_immutability_blocks_reform, conventional).
narrative_ontology:cs_reference_frame('410797ea-259a-40d2-81ec-1609a7d30cde', postwar_great_power_concert).
narrative_ontology:cs_drift_state('410797ea-259a-40d2-81ec-1609a7d30cde', contemporary_multipolar_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('410797ea-259a-40d2-81ec-1609a7d30cde', '').
narrative_ontology:cs_kernel_id(article_27_veto_power__oligopoly_reading, article_27_veto_power).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_27_veto_power__oligopoly_reading, p5_permanent_members).
narrative_ontology:constraint_victim(article_27_veto_power__oligopoly_reading, non_p5_member_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(article_27_veto_power__oligopoly_reading, elected_security_council_members).
narrative_ontology:constraint_victim(article_27_veto_power__oligopoly_reading, elected_security_council_members).
narrative_ontology:constraint_vindicates(article_27_veto_power__oligopoly_reading, great_power_management_necessity).
narrative_ontology:constraint_vindicates(article_27_veto_power__oligopoly_reading, charter_immutability_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The five permanent members (US, UK, France, Russia, China) each hold an absolute veto over Security Council substantive resolutions. They administer the veto power through procedural control of the Council agenda and the threat of unilateral blocking. They collect authority rents: privileged decision-making on peace and security, insulation from collective pressure, and the ability to shape or block outcomes affecting their interests. Exit is arbitrage-grade: they designed the system and can veto any reform.
narrative_ontology:constraint_stakeholder(article_27_veto_power__oligopoly_reading, p5_permanent_members, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(article_27_veto_power__oligopoly_reading, p5_permanent_members, beneficiary).

% The ~188 non-permanent UN member states participate in the UN system but have no path to veto power or Charter reform (Article 108 requires P5 consent). They bear costs when the veto blocks action on conflicts, humanitarian crises, or norm enforcement that would benefit them. They are trapped: leaving the UN forfeits all multilateral standing, but staying means accepting structural subordination. Regional groups and the G77 have sought reform for decades without structural progress.
narrative_ontology:constraint_stakeholder(article_27_veto_power__oligopoly_reading, non_p5_member_states, payer,
    organized, biographical, trapped, global).

% The ten elected non-permanent members serve two-year terms on the Council. They gain temporary agenda access and prestige (beneficiary) but their votes are structurally subordinated to the veto — any resolution they support can be unilaterally blocked (payer). Their exit is constrained: they rotate off the Council and return to ordinary member state status with no accumulated leverage.
narrative_ontology:constraint_stakeholder(article_27_veto_power__oligopoly_reading, elected_security_council_members, payer,
    moderate, immediate, constrained, global).
narrative_ontology:stakeholder_secondary_role(article_27_veto_power__oligopoly_reading, elected_security_council_members, beneficiary).

% The Secretary-General and Secretariat implement Council mandates but have no vote. They experience the veto as operational paralysis: mandates they must execute are blocked, watered down, or never authorized. They have analytical exit (can document and report the pattern) but no structural power to change it.
narrative_ontology:constraint_stakeholder(article_27_veto_power__oligopoly_reading, un_secretariat, observer,
    institutional, generational, analytical, global).

% The General Assembly represents all member states equally but has no binding authority on peace and security (Uniting for Peace resolution notwithstanding). It would object to veto paralysis if structurally empowered, but its role is confined to recommendations. Its exclusion is constitutional: the Charter assigns primary responsibility to the Security Council.
narrative_ontology:constraint_stakeholder(article_27_veto_power__oligopoly_reading, general_assembly, excluded,
    organized, generational, constrained, global).

% Sees the full structure: a 1945 great-power concert embedded in a universal organization, where the founding privilege has become a permanent extraction mechanism. The coordination story (preventing great-power war) is empirically contested — the veto has not prevented proxy wars, arms races, or conflicts involving P5 members — while the extraction of authority rents is structurally evident in the blocked reform pathway.
narrative_ontology:constraint_stakeholder(article_27_veto_power__oligopoly_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(article_27_veto_power__oligopoly_reading, analytical_observer).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The claimed coordination function is preventing direct great-power war by ensuring no Security Council resolution can compel a nuclear-armed permanent member into military confrontation it rejects. This reading argues the coordination story is cover: the veto has not prevented great-power conflict (Korea, Vietnam, Afghanistan, Ukraine, proxy wars), and its primary structural effect is to freeze the 1945 power distribution.
% TRANSFER_FUNCTION: Moves effective decision-making authority on international peace and security from the collective Security Council (where a 9-vote majority could act) to the P5 oligopoly (where any single permanent member can block). Transfers the cost of inaction — unresolved conflicts, unenforced norms, humanitarian catastrophes — from the veto-wielding P5 to the non-P5 majority and affected populations.
% ABSENT_VOICES: The global south (G77, African Group, LDCs), small island developing states, and populations in conflict zones where vetoes have blocked action. They are structurally excluded from the Security Council's permanent tier and have no path to reform under Article 108. Their objections appear in General Assembly debates and reform proposals (e.g., Ezulwini Consensus, ACT Group) but cannot alter the constraint.
% DISAPPEARANCE_RATIONALE: If the veto disappeared overnight, the Security Council would operate on its Article 27 majority rule (9 of 15 votes). Resolutions on Syria, Ukraine, Palestine, Myanmar, and other blocked situations could pass. The P5 would lose their unilateral blocking power and would need to build coalitions. The UN's legitimacy would likely rise among non-P5 states, but great-power participation might decrease if they feel unconstrained. The institutional architecture would reorganize around majority decision-making on security.
% FOUNDING_PROBLEM: Post-WWII great-power management: creating a security organization that the victorious great powers would join and not destroy, by guaranteeing they could not be outvoted on matters of vital interest — especially direct military confrontation between nuclear-armed states.
% FOUNDING_PROBLEM_CORROBORATION: The P5 and their allies attest the problem is live, citing ongoing great-power competition and nuclear deterrence. Non-P5 states, the ACT Group (Accountability, Coherence, Transparency), the Elders, and independent scholars (e.g., Weiss, Taylor, Luck) attest the founding problem is substantially altered: the Cold War bipolarity that made the veto a crisis-management tool has given way to multipolar contestation where the veto functions as oligopoly protection. The 2024 Summit of the Future reform debates document this split.
narrative_ontology:disappearance_verdict(article_27_veto_power__oligopoly_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_27_veto_power__oligopoly_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_27_veto_power__oligopoly_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(article_27_veto_power__oligopoly_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_27_veto_power__oligopoly_reading, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness is high (0.78) because the P5 collect concentrated authority rents (decision control, agenda control, insulation) while the costs of paralysis are diffuse across 188+ states and affected populations. Suppression is very high (0.85) because the constraint's persistence depends on active exercise of the veto and the structural closure of Article 108 — alternatives are not merely discouraged but institutionally impossible. Theater ratio (0.42) reflects the growing gap between the Council's procedural activity and its substantive output. Accessibility collapse (0.72) is high because the Charter amendment barrier is near-absolute for any reform touching the veto. Resistance (0.58) is moderate: reform movements exist (ACT Group, Ezulwini Consensus, Uniting for Peace) but have achieved zero structural change in 80 years.
 *
 * PERSPECTIVAL GAP:
 *   From the P5 seat, the veto is a coordination mechanism that prevents catastrophic great-power conflict — a genuine Rope. From the non-P5 seat, the same structure is a Snare: extraction of authority rents with suppressed alternatives. The engine computes this seat divergence from the structural data. The oligopoly reading argues the non-P5 seat reveals the constraint's true nature because the P5 seat's perception is constituted by the very privilege the constraint grants.
 *
 * DIRECTIONALITY LOGIC:
 *   The P5 are structural beneficiaries (d ≈ 0.1): they collect authority rents, control the agenda, and have arbitrage-grade exit (they can veto reform). Non-P5 member states are full targets (d ≈ 0.95): they bear diffuse costs of paralysis, are trapped in the system (leaving forfeits all multilateral standing), and have zero structural path to reform. Elected Council members sit near the target end (d ≈ 0.8): temporary agenda access without structural power. The Secretariat and General Assembly are observers/excluded with analytical or constrained exit. The engine derives these directionalities from the beneficiary/victim declarations and exit options.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (1945 great-power management) is contested: the P5 claim it remains live; the non-P5 majority and reform advocates argue it has been overtaken by geopolitical change (decolonization, multipolarity, new security threats). The constraint persists not because the founding problem is solved, but because the beneficiaries control the reform pathway. This is classic mandatrophy: the arrangement's mandate has atrophied relative to current conditions, but the institutional inertia of Charter immutability (Article 108) prevents adaptation. The classification as Snare (not Piton) is warranted because the P5 actively exercise the veto to extract ongoing authority rents — it is not merely inertial performance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the Article 27 veto power a single constraint with multiple interpretations, or are the coordination_reading, oligopoly_reading, and sovereignty_reading structurally distinct constraints sharing a label?',
    'Apply the ε-invariance test: if measuring the constraint via the coordination lens (veto usage preventing P5 conflict) yields low ε, but measuring via the oligopoly lens (veto blocking reform and extracting authority rents) yields high ε, they are distinct constraints. The decomposition is warranted if ε differs by a wide margin across readings.',
    'If distinct, each reading gets its own constraint story with independent ε, stakeholders, and classification. The kernel structure is then a family linked by network.affects_constraints, not a single story with measurement-dependent classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the three declared readings are one constraint or a constraint family').

omega_variable(
    veto_as_war_prevention_empirical,
    'Has the veto empirically prevented direct great-power war, or is the correlation spurious (nuclear deterrence, bipolarity, economic interdependence doing the work)?',
    'Counterfactual analysis: compare conflict patterns in veto-blocked vs. veto-permitted Security Council actions; assess whether veto incidents correlate with avoided P5-P5 military confrontation. Nuclear deterrence literature provides alternative explanatory variables.',
    'If the coordination function is empirically unsupported, the oligopoly reading''s claim that the coordination story is cover gains weight. If supported, the constraint may be a genuine Tangled Rope (coordination + extraction) rather than pure Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(veto_as_war_prevention_empirical, empirical, 'Empirical status of the veto''s claimed coordination function').

omega_variable(
    article_108_reform_closure,
    'Is Article 108''s requirement of P5 consent for Charter amendment an absolute structural barrier, or are there viable reform pathways (Uniting for Peace, Article 109 review conference, customary law evolution) that the non-P5 majority could pursue?',
    'Legal analysis of Charter amendment history (1963, 1965, 1971, 1973 amendments all required P5 consent), Uniting for Peace precedent (GA Resolution 377), and Article 109 review conference mechanism (never convened). Political analysis of whether P5 would ever consent to veto dilution.',
    'If Article 108 is absolute, non-P5 exit is truly trapped (d ≈ 0.95). If viable pathways exist, exit options improve to constrained or mobile, lowering effective extraction for non-P5 states and potentially shifting classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(article_108_reform_closure, conceptual, 'Whether the Charter amendment barrier is absolute or permeable').

omega_variable(
    p5_unity_vs_competition,
    'Do the P5 act as a cohesive oligopoly protecting shared privilege, or does their geopolitical competition (US-China, US-Russia) undermine the oligopoly reading''s unitary beneficiary assumption?',
    'Analyze veto coincidence patterns: do P5 members veto to protect each other''s interests (oligopoly cohesion) or primarily their own (competitive extraction)? Compare Cold War (US vs. USSR vetoes) vs. post-2011 (Russia/China double vetoes on Syria) vs. current multipolar dynamics.',
    'If P5 competition fractures the beneficiary class, the constraint may have multiple sub-seats with different directionalities. The oligopoly reading assumes a unified beneficiary; evidence of fracture would require decomposing p5_permanent_members into distinct stakeholder seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(p5_unity_vs_competition, empirical, 'Whether P5 form a cohesive oligopoly or competitive extractors').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_27_veto_power__oligopoly_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t0, article_27_veto_power__oligopoly_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(arti_tr_t10, article_27_veto_power__oligopoly_reading, theater_ratio, 10, 0.22).
narrative_ontology:measurement(arti_tr_t25, article_27_veto_power__oligopoly_reading, theater_ratio, 25, 0.31).
narrative_ontology:measurement(arti_tr_t45, article_27_veto_power__oligopoly_reading, theater_ratio, 45, 0.38).
narrative_ontology:measurement(arti_tr_t60, article_27_veto_power__oligopoly_reading, theater_ratio, 60, 0.4).
narrative_ontology:measurement(arti_tr_t80, article_27_veto_power__oligopoly_reading, theater_ratio, 80, 0.42).

% Extraction over time
narrative_ontology:measurement(arti_be_t0, article_27_veto_power__oligopoly_reading, base_extractiveness, 0, 0.65).
narrative_ontology:measurement(arti_be_t10, article_27_veto_power__oligopoly_reading, base_extractiveness, 10, 0.72).
narrative_ontology:measurement(arti_be_t25, article_27_veto_power__oligopoly_reading, base_extractiveness, 25, 0.75).
narrative_ontology:measurement(arti_be_t45, article_27_veto_power__oligopoly_reading, base_extractiveness, 45, 0.76).
narrative_ontology:measurement(arti_be_t60, article_27_veto_power__oligopoly_reading, base_extractiveness, 60, 0.77).
narrative_ontology:measurement(arti_be_t80, article_27_veto_power__oligopoly_reading, base_extractiveness, 80, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t0, article_27_veto_power__oligopoly_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(arti_su_t10, article_27_veto_power__oligopoly_reading, suppression_requirement, 10, 0.75).
narrative_ontology:measurement(arti_su_t25, article_27_veto_power__oligopoly_reading, suppression_requirement, 25, 0.8).
narrative_ontology:measurement(arti_su_t45, article_27_veto_power__oligopoly_reading, suppression_requirement, 45, 0.82).
narrative_ontology:measurement(arti_su_t60, article_27_veto_power__oligopoly_reading, suppression_requirement, 60, 0.84).
narrative_ontology:measurement(arti_su_t80, article_27_veto_power__oligopoly_reading, suppression_requirement, 80, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_27_veto_power__oligopoly_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(article_27_veto_power__oligopoly_reading, 0.12).
narrative_ontology:affects_constraint(article_27_veto_power__oligopoly_reading, un_charter_amendment_formula).
narrative_ontology:affects_constraint(article_27_veto_power__oligopoly_reading, security_council_reform_proposals).
narrative_ontology:affects_constraint(article_27_veto_power__oligopoly_reading, uniting_for_peace_resolution).
narrative_ontology:affects_constraint(article_27_veto_power__oligopoly_reading, responsibility_to_protect_norm).

% DUAL FORMULATION NOTE:
% This oligopoly_reading is one of three constraints in the article_27_veto_power kernel family. The coordination_reading frames the veto as a necessary great-power war prevention mechanism (claimed Mountain/Rope). The sovereignty_reading frames it as Westphalian sovereignty applied to global-reach powers (claimed Mountain). This reading frames it as oligopoly rent extraction (claimed Snare). The ε values differ substantially: coordination_reading ε ≈ 0.15 (if coordination function holds), oligopoly_reading ε ≈ 0.78, sovereignty_reading ε ≈ 0.25. They are linked via network.affects_constraints in each story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(article_27_veto_power__oligopoly_reading, institutional, 0.1).
constraint_indexing:directionality_override(article_27_veto_power__oligopoly_reading, organized, 0.95).
constraint_indexing:directionality_override(article_27_veto_power__oligopoly_reading, moderate, 0.8).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
