% ============================================================================
% CONSTRAINT STORY: total_war_possibility_space__nuclear_taboo_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_total_war_possibility_space__nuclear_taboo_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: total_war_possibility_space__nuclear_taboo_reading
 *   human_readable: Total War Normative Prohibition (Nuclear Taboo Reading)
 *   domain: international_relations/strategic_studies
 *
 * SUMMARY:
 *   This constraint describes the normative prohibition of total war as a
 *   constructed and actively maintained taboo, independent of material
 *   capability to wage it. The nuclear-taboo reading asserts that great
 *   powers retain the physical ability to initiate total war but are
 *   behaviorally and institutionally foreclosed from doing so by a powerful
 *   international norm system centered on delegitimizing nuclear first use,
 *   enforced through non-proliferation regimes, intelligence operations,
 *   diplomatic isolation, and the identity fusion of norm-maintenance elites
 *   with the taboo's persistence. Unlike the deterrence-equilibrium reading
 *   (which grounds the prohibition in mutual vulnerability) or the
 *   space-contraction reading (which grounds it in epistemic impossibility),
 *   this reading insists the taboo is CONSTRUCTED: it persists because
 *   institutions, norm entrepreneurs, and enforcement mechanisms actively
 *   maintain it, not because capability is absent or incentives have
 *   structurally vanished. Consequently, the taboo is vulnerable to norm
 *   entrepreneur exit and would collapse if the beneficiary coalition lost
 *   interest in maintaining it. The taboo operates as tangled rope: genuine
 *   coordination function (prevents inadvertent escalation, stabilizes
 *   great-power competition within bounds), but asymmetric extraction
 *   (non-nuclear threshold states and revisionist powers pay through
 *   constrained strategic options while norm maintainers benefit from
 *   preserved hierarchy).
 *
 * KEY AGENTS:
 *   - non_nuclear_great_powers: Institutional beneficiaries and agenda-setters; set and enforce the norm through treaty regimes and intelligence operations.
 *   - international_order_maintainers: Institutional beneficiaries whose professional standing and budgets depend on taboo maintenance.
 *   - revisionist_nuclear_threshold_states: Moderate power, constrained exit; pay through economic sanctions, diplomatic isolation, and forced concealment of weapons programs.
 *   - regional_powers_excluded_from_nuclear_club: Powerful regionally but identity-locked to the non-proliferation regime; pay through strategic limitation and concealment burden.
 *   - norm_entrepreneurs_maintaining_taboo: Agenda-setters whose careers and identity are fused with norm defense.
 *   - counterproliferation_military_apparatus: Institutional actors executing material enforcement; budget and organizational existence depend on proliferation threat.
 *   - academic_security_studies_community: Analytical seat and beneficiary; professional standing depends on taboo's theoretical intelligibility.
 *   - proliferation_advocates: Excluded; would contest the taboo openly if not systematically delegitimized.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_possibility_space__nuclear_taboo_reading, 0.38).
domain_priors:suppression_score(total_war_possibility_space__nuclear_taboo_reading, 0.67).
domain_priors:theater_ratio(total_war_possibility_space__nuclear_taboo_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_possibility_space__nuclear_taboo_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 0.67).
narrative_ontology:constraint_metric(total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_possibility_space__nuclear_taboo_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(total_war_possibility_space__nuclear_taboo_reading, resistance, 0.44).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_possibility_space__nuclear_taboo_reading, tangled_rope).
narrative_ontology:human_readable(total_war_possibility_space__nuclear_taboo_reading, "Total War Normative Prohibition (Nuclear Taboo Reading)").
narrative_ontology:topic_domain(total_war_possibility_space__nuclear_taboo_reading, "international_relations/strategic_studies").

domain_priors:requires_active_enforcement(total_war_possibility_space__nuclear_taboo_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_possibility_space__nuclear_taboo_reading, '2f22779c-2db3-4d90-8f4e-eaedbf5ea8bb').
narrative_ontology:cs_kernel_codification('2f22779c-2db3-4d90-8f4e-eaedbf5ea8bb', formalized).
narrative_ontology:cs_authority_grounding('2f22779c-2db3-4d90-8f4e-eaedbf5ea8bb', extraction).
narrative_ontology:cs_interpretation_layer_present('2f22779c-2db3-4d90-8f4e-eaedbf5ea8bb').
narrative_ontology:cs_reading_relation('2f22779c-2db3-4d90-8f4e-eaedbf5ea8bb', total_war_possibility_space__deterrence_equilibrium_reading, coexists_with).
narrative_ontology:cs_reading_relation('2f22779c-2db3-4d90-8f4e-eaedbf5ea8bb', total_war_possibility_space__space_contraction_reading, influences).
narrative_ontology:cs_axiom('2f22779c-2db3-4d90-8f4e-eaedbf5ea8bb', foundational, taboo_independent_of_capability).
narrative_ontology:cs_axiom_status(taboo_independent_of_capability, holdable).
narrative_ontology:cs_axiom_grounding('2f22779c-2db3-4d90-8f4e-eaedbf5ea8bb', taboo_independent_of_capability, deontological).
narrative_ontology:cs_axiom('2f22779c-2db3-4d90-8f4e-eaedbf5ea8bb', secondary, norm_entrepreneur_agency_necessary).
narrative_ontology:cs_axiom_status(norm_entrepreneur_agency_necessary, holdable).
narrative_ontology:cs_axiom_grounding('2f22779c-2db3-4d90-8f4e-eaedbf5ea8bb', norm_entrepreneur_agency_necessary, empirically_contingent).
narrative_ontology:cs_reference_frame('2f22779c-2db3-4d90-8f4e-eaedbf5ea8bb', taboo_as_autonomous_prohibition).
narrative_ontology:cs_drift_state('2f22779c-2db3-4d90-8f4e-eaedbf5ea8bb', contemporary_2026, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('2f22779c-2db3-4d90-8f4e-eaedbf5ea8bb', '').
narrative_ontology:cs_kernel_id(total_war_possibility_space__nuclear_taboo_reading, total_war_possibility_space).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_possibility_space__nuclear_taboo_reading, non_nuclear_great_powers).
narrative_ontology:constraint_beneficiary(total_war_possibility_space__nuclear_taboo_reading, international_order_maintainers).
narrative_ontology:constraint_victim(total_war_possibility_space__nuclear_taboo_reading, revisionist_nuclear_threshold_states).
narrative_ontology:constraint_victim(total_war_possibility_space__nuclear_taboo_reading, regional_powers_excluded_from_nuclear_club).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(total_war_possibility_space__nuclear_taboo_reading, academic_security_studies_community).
narrative_ontology:constraint_vindicates(total_war_possibility_space__nuclear_taboo_reading, norm_based_international_constraint).
narrative_ontology:constraint_vindicates(total_war_possibility_space__nuclear_taboo_reading, taboo_as_structural_force).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Have institutionalized the norm against total war through non-proliferation regimes, diplomatic engagement, and diplomatic isolation of violators. They benefit from a world where nuclear war is taboo because it preserves the possibility of limited conflict, deterrence through conventional forces, and great-power competition within bounded rules. They set the norm-enforcement agenda through the UN Security Council, treaty regimes, and intelligence operations targeting proliferators.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, non_nuclear_great_powers, beneficiary,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(total_war_possibility_space__nuclear_taboo_reading, non_nuclear_great_powers, agenda_setter).

% International institutions (IAEA, NPT secretariat, UN disarmament bodies), epistemic communities of non-proliferation scholars and security experts, and diplomatic networks that reproduce and defend the taboo. They benefit from the taboo's existence because their legitimacy and professional standing depend on its maintenance. They actively teach, enforce, and repair the norm through publications, training, monitoring, and public advocacy.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, international_order_maintainers, beneficiary,
    institutional, generational, analytical, global).

% States pursuing nuclear capacity to alter regional power balances or deter conventional superiority. They pay through economic sanctions, diplomatic isolation, military containment efforts, and forced technological disguise of weapons programs. The taboo makes weaponization costly and operationally delicate because open deployment violates the norm and triggers coordinated response. Their exit options are severely constrained: declaring openly nuclear status invokes unified international pressure; covert development is resource-intensive and risks catastrophic discovery.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, revisionist_nuclear_threshold_states, payer,
    moderate, biographical, constrained, regional).

% Major regional powers (India, Pakistan, Israel historically; Iran, Saudi Arabia currently) that face pressure to forgo or conceal nuclear capacity to avoid the full weight of the non-proliferation taboo. They pay through constrained strategic options: cannot openly employ nuclear deterrence as openly as the original five powers, cannot signal nuclear readiness without triggering isolation, face identity fusion with 'outlaw nuclear state' if they violate the norm. Their power locally is substantial but their legitimacy globally is tied to compliance or at least concealment.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, regional_powers_excluded_from_nuclear_club, payer,
    powerful, generational, identity_locked, regional).

% Specific governments (US, UK, Russia, China at different moments), non-governmental organizations (ICAN, Pugwash, CND), and individual diplomats and scholars whose careers are constituted by norm defense. They invest professional identity and institutional standing in keeping the taboo alive through speeches, policy, funding non-proliferation research, and diplomatic pressure. Their exit from norm maintenance would require career reorientation and loss of institutional standing.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, norm_entrepreneurs_maintaining_taboo, agenda_setter,
    institutional, biographical, identity_locked, global).

% Military commands and intelligence agencies dedicated to interdicting proliferation, from Israeli air strikes on reactor sites to US-led naval interdiction of WMD shipments to covert sabotage of uranium enrichment. They execute the material enforcement of the normative prohibition. Their organizational existence and budgets depend on proliferation remaining a threat requiring active suppression.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, counterproliferation_military_apparatus, agenda_setter,
    institutional, generational, trapped, global).

% Scholars in international relations, strategic studies, arms control, and security whose professional standing, publication opportunities, and funding depend on articulating, defending, and refining the taboo's theoretical justification. They benefit from the taboo because it is the dominant framing through which state behavior is intelligible; a world where the taboo collapsed would require wholesale retheorization.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, academic_security_studies_community, beneficiary,
    analytical, biographical, analytical, global).
narrative_ontology:stakeholder_secondary_role(total_war_possibility_space__nuclear_taboo_reading, academic_security_studies_community, observer).

% Political leaders, military strategists, and technology entrepreneurs in threshold states who see nuclear weapons as legitimate deterrent or power-projection tools and who would argue openly for nuclearization if the taboo did not constrain them. They are systematically excluded from international norm-setting forums and face rhetorical delegitimation when they advocate for nuclear capacity.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, proliferation_advocates, excluded,
    powerful, biographical, trapped, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(total_war_possibility_space__nuclear_taboo_reading, non_nuclear_great_powers).
narrative_ontology:fixing_cost_class(total_war_possibility_space__nuclear_taboo_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents inadvertent escalation into total nuclear war through a stabilizing norm system that makes nuclear first use rhetorically indefensible and strategically delegitimized, even when material capability exists. Solves the coordination problem of mutual vulnerability: two powers with mutual assured destruction need some shared agreement that total escalation is off the table, or deterrence becomes unstable (each side fears the other will eventually launch first). The taboo provides that shared agreement by making total war normatively unspeakable.
% TRANSFER_FUNCTION: Transfers strategic limitation from nuclear-armed great powers to non-nuclear threshold states and revisionist regional powers. Great powers retain first-use options (documented exceptions in strategic policy documents); threshold states are foreclosed from declaring nuclear weapons and face unified international response if they pursue them. The constraint also transfers legitimacy and institutional standing from proliferators to non-proliferation maintainers: governments and organizations that defend the taboo gain prestige and budgetary support; those that violate it lose international recognition and face sanctions.
% ABSENT_VOICES: Proliferation advocates within threshold states and revisionist powers who would argue that nuclear weapons are legitimate deterrent tools against regional superiors and that the non-proliferation regime enforces a unjust hierarchy. Military strategists in non-nuclear great powers who believe that great-power nuclear forces are destabilizing and should themselves be abolished rather than used to enforce prohibition on others. Technological entrepreneurs who see nuclear energy and weapons technology as legitimate commercial and strategic goods. These voices are systematically excluded from international norm-setting forums (NPT review conferences) and are reframed as 'rogue' or 'illegitimate' when they advocate openly.
% DISAPPEARANCE_RATIONALE: If the taboo suddenly disappeared and non-proliferation enforcement ceased overnight, the strategic landscape would rearrange dramatically: threshold states would accelerate nuclear programs, regional powers would open declare nuclear forces, the nuclear club would expand from 9 to 25+ states within 10 years. The institutional infrastructure (IAEA, NPT secretariat, non-proliferation diplomacy corps) would dissolve or be repurposed. International relations would shift from a system where nuclear weapons are strategically present but normatively banned to one where they are both materially and rhetorically normalized. Great powers' strategic doctrines would change because the constraint on others' nuclearization would no longer be available; strategic competition would operate in a fundamentally different possibility space. The disappearance would also reorganize professional careers: non-proliferation experts would lose institutional standing, counterproliferation military commands would be repurposed, academic security studies would require wholesale retheorization.
% FOUNDING_PROBLEM: In the 1960s-1970s, after the Cuban Missile Crisis and during the period when multiple powers were acquiring nuclear capability, the international community faced the risk of inadvertent escalation spirals in which nuclear-armed states' competing deterrence claims would become increasingly incredible and therefore increasingly prone to provocation and counter-provocation, potentially triggering total war. The taboo was constructed through the Non-Proliferation Treaty (1968) and subsequent regime-building to prevent this by making proliferation itself illegitimate, thereby limiting the number of nuclear powers and reducing the complexity of deterrence calculations.
% FOUNDING_PROBLEM_CORROBORATION: Non-nuclear great powers and non-proliferation institutions attest the founding problem remains live: proliferation continues to pose escalation risks, and the taboo is necessary to contain it. Revisionist threshold states and some strategic theorists attest the founding problem is largely solved: deterrence stability among existing nuclear powers is robust, and the prohibition of others' nuclear weapons serves hierarchical interests rather than safety interests. Academic analysis from outside the non-proliferation beneficiary coalition (notably work by scholars arguing for nuclear abolition or for proliferation to make deterrence more robust) supports the contested reading: some believe the taboo is necessary, others believe it is a false solution that obscures the true source of restraint (deterrence incentives or technical impossibility).
narrative_ontology:disappearance_verdict(total_war_possibility_space__nuclear_taboo_reading, world_rearranges).
narrative_ontology:founding_problem_status(total_war_possibility_space__nuclear_taboo_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_possibility_space__nuclear_taboo_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'f1436bd4937f864097dabaad92b27bd9b6eec212', '2026-08-03',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(total_war_possibility_space__nuclear_taboo_reading, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_possibility_space__nuclear_taboo_reading, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_possibility_space__nuclear_taboo_reading_tests).
:- end_tests(total_war_possibility_space__nuclear_taboo_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.38 at t=2026) because the taboo imposes real constraints on revisionist states and threshold powers but the constraint is relatively indivisible: once constructed, it applies at low marginal enforcement cost to well-integrated powers and near-zero cost to those already invested in the order. Theater ratio is high (0.52) and rising through the 1980s-2010s, indicating that a growing share of non-proliferation activity is performative maintenance of the norm rather than actual capability prevention — intelligence operations to disrupt proliferation programs are real, but public denunciations of 'nuclear terrorism,' media campaigns about 'the scourge of nuclear weapons,' and institutional ceremonies (NPT review conferences) increasingly serve to reinforce the taboo symbolically rather than to prevent material capability. Suppression is substantial (0.67) and steadily rising, indicating that norm maintenance requires increasing coercive effort over the interval: more sanctions, more intelligence targeting, more diplomatic isolation of defectors, more military interdiction. The rising suppression over time is the diagnostic signature of a constraint whose acceptance is not internalized but is being re-imposed continuously. Accessibility collapse is high (0.71) because the taboo has become deeply institutionalized and alternative visions of justified nuclearization are systematically excluded from legitimate discourse, particularly at the organizational and structural levels. Resistance is moderate (0.44) because threshold states continue to pursue nuclear capacity despite the taboo, showing that normative prohibition is not sufficient to eliminate the goal, only to make pursuit costly and dangerous. The coercion grid shows that suppression and stakes inflation rise across all four levels from 1970 to 2026, but the structural and organizational levels bear much higher intensity than individual and class levels — the taboo operates primarily as an institutional-order prohibition, not as a popular consensus or individual conviction.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (non-nuclear great powers, norm entrepreneurs), the taboo is a genuinely coordinating norm that solved a real problem (preventing inadvertent escalation spiral) and continues to serve a coordination function (bounded great-power competition). From the payer seat (revisionist threshold states), the constraint is experienced as extraction masked as coordination — non-nuclear powers are allowed limited regional wars while nuclear powers retain strategic options; the taboo enforces an asymmetric status hierarchy. From the identity-locked seat (regional powers like Pakistan, Israel, Iran), the constraint generates identity fusion: countries pursuing nuclear capacity for legitimate regional deterrence are rhetorically repositioned as 'rogue states' and 'proliferators,' forcing them to either abandon the goal or hide it, converting a policy choice into an identity violation. The engine computes these divergent types from the structural data: the agenda-setter and international-order-maintainer seats experience tangled rope (benefiting from the coordination while collecting from the enforcement). The revisionist and excluded seats experience snare (pure extraction with the coordination cover story). The counterproliferation apparatus experiences rope (coordinating the inhibition of capability) but with the risk of capturing the norm for its own budget perpetuation, slipping into piton if the actual proliferation threat diminishes but the institutional apparatus persists.
 *
 * DIRECTIONALITY LOGIC:
 *   Non-nuclear great powers are beneficiaries (d ≈ 0.1–0.2): they set and benefit from the norm without bearing its costs; they have complete exit options (could abandon norm maintenance, adopt deterrence-equilibrium framing, or negotiate regulated proliferation). Revisionist threshold states are targets (d ≈ 0.8–0.9): they bear extraction costs (sanctions, isolation, military interdiction) directly proportional to pursuit of nuclear weapons; their exit options are constrained (open nuclearization invokes unified international response; covert development is resource-intensive and catastrophic if discovered). Regional powers excluded from the nuclear club sit in between (d ≈ 0.6–0.7): they experience the constraint as both coordination (stability benefit) and extraction (foreclosure of legitimate deterrent options), and their exit options are partially identity-locked (abandoning regional power ambitions and nuclear status-seeking requires wholesale reorientation of national strategy). The norm-entrepreneur seats (academic community, diplomatic cadres) have d ≈ 0.15–0.3 (beneficiaries, though with some capture risk if the norm becomes performative). Overrides are not necessary: the derived directionalities follow naturally from the beneficiary/victim declarations and exit-options structure.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing inadvertent escalation into total nuclear war) was live in 1970 and remains contested in 2026. The measured theater_ratio rising from 0.35 to 0.52 indicates that non-proliferation activity has increasingly become performative maintenance of the taboo rather than capability prevention. This is the diagnostic signal of potential mandatrophy: the original mandate (prevent escalation spirals through mutual vulnerability acknowledgment) has been partially displaced by a secondary, institutional mandate (enforce a norm system that preserves great-power hierarchy and prevents others from acquiring strategic independence). The classification as tangled rope (not piton) is justified because genuine coordination remains: the taboo does stabilize great-power competition and does reduce certain categories of escalation risk. But the rising theater ratio and rising suppression requirement indicate the constraint is drifting toward piton-like characteristics — an increasing share of enforcement effort goes to maintaining the norm's institutional form rather than preventing the underlying hazard. A full piton reclassification would require the founding problem to be dead while the constraint persists; here the founding problem is contested (some believe escalation risk remains; others believe structural deterrence has made it moot), so tangled rope with mandatrophy risk captures the state accurately.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    taboo_vs_deterrence_structural_difference,
    'Is the observed prohibition on total war fundamentally a constructed norm system (taboo), or is it a rational equilibrium outcome from deterrence incentives that just happens to be expressed in normative language?',
    'Historical counterfactual analysis of near-escalation events (Cuban Missile Crisis, Kargil War, recent Taiwan crises) combined with decision-maker testimony about whether their restraint was motivated by fear of mutual destruction (deterrence) or by fear of norm violation (taboo). A scenario where two powers with mutual assured destruction remained in competition but abandoned the taboo would falsify the taboo reading''s distinctiveness.',
    'If deterrence is the true driver, the constraint should be reclassified to deterrence_equilibrium_reading with different type (possibly rope or mountain rather than tangled rope). The taboo reading''s claim to independent explanatory power depends on norm-based restraint being distinct from incentive-based restraint.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(taboo_vs_deterrence_structural_difference, conceptual, 'Whether the taboo operates independently from deterrence incentives or is merely the rhetorical expression of an incentive-based equilibrium.').

omega_variable(
    norm_entrepreneur_dependency,
    'What is the causal relationship between active norm maintenance (by non-nuclear great powers, international institutions, academic communities) and the observed restraint from total war? Would the taboo persist if all norm entrepreneurs simultaneously exited?',
    'Analytic comparison with other international norms that have collapsed when their supporting coalition dissolved (e.g., the Concert of Europe, the League of Nations institutional legitimacy). If comparable norms show that maintaining them requires active, continuous effort, then the taboo reading''s dependency on norm entrepreneurs is confirmed. A ''natural law'' reading would predict the taboo persists even if entrepreneurs exit; the reading''s test is whether exit leads to decay.',
    'If norm entrepreneur exit is sufficient to collapse the taboo, the constraint is vulnerable and should be classified as depending on continuous enforcement (supporting tangled rope classification). If the taboo is robust to entrepreneur exit, it may be approaching mountain-like status (natural law). This determination affects long-term stability analysis and policy recommendations for non-proliferation regimes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(norm_entrepreneur_dependency, empirical, 'Degree of structural dependency on active norm maintenance.').

omega_variable(
    identity_lock_mechanism_internalization,
    'For regional powers with identity-locked exit options (Pakistan, Israel, Iran pursuing nuclear capacity), is the suppression mechanism primarily structural (external coercion: sanctions, military interdiction) or internalized (the identity fusion itself makes nuclearization feel illegitimate)?',
    'Post-constraint analysis if a regional power acquires open nuclear status: does suppression persist in the form of diplomatic isolation and sanctions (structural), or does the country''s strategic behavior and self-presentation shift to accept new identity as nuclear power (internalized suppression dissolves)? If structural suppression persists despite open nuclearization, the identity-lock mechanism is genuine and partly internalized.',
    'If suppression is largely structural and dissolves with regime change or external pressure lift, the constraint''s hold on identity-locked actors is more fragile than measured. If suppression is internalized and persists even after open nuclearization, the taboo has achieved deep cognitive capture. This affects whether the constraint can be undone through political change or requires deeper institutional/cultural shift.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_internalization, empirical, 'Structural vs. internalized suppression mechanism for identity-locked actors.').

omega_variable(
    theater_rise_function,
    'Does the rising theater_ratio (0.35 to 0.52) indicate genuine mandate drift (the primary non-proliferation function is being displaced by norm-maintenance theater), or does it reflect more sophisticated non-proliferation strategy (overt norm-enforcement diplomacy combined with covert capability interdiction, where the ratio only measures the visible portion)?',
    'Classified budget and intelligence analysis comparing overt non-proliferation spending (treaties, inspections, public advocacy) to covert spending (sabotage of enrichment facilities, interdiction of technology transfers, assassinations of weapons scientists). If covert capability-disruption has remained constant or grown faster than overt norm-maintenance, the rising theater ratio reflects measurement bias, not actual mandatrophy. If overt performance has grown much faster than covert disruption, the theatrical rise is real.',
    'If the theater rise is real, the constraint is drifting toward piton (performative maintenance, institutional inertia). If the rise reflects hidden capability-disruption effort, the constraint remains substantively tangled rope (coordinating inhibition while extracting from outliers). The mandatrophy-resolved status depends on this determination.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(theater_rise_function, empirical, 'Measurement bias vs. genuine mandate drift in theater ratio rise.').

omega_variable(
    nuclear_threshold_vs_non_nuclear_asymmetry,
    'The constraint applies qualitatively differently to nuclear-armed great powers vs. non-nuclear states: great powers retain the option to use nuclear weapons in escalation scenarios (no first-use pledges are not treaties and have exceptions), while threshold states are foreclosed from even declaring nuclear status. Is this asymmetry justified by a coherent principle (deterrence stability, rule-of-law, technical safety), or is it pure extraction masked as coordination?',
    'Comparative analysis of the stated rationales for nuclear policies of great powers vs. threshold states. If great powers justify their nuclear forces in terms of deterrence stability and existential security while condemning threshold states'' nuclear programs as destabilizing and illegitimate, the asymmetry is framed as principled but may be rationalization of power preservation. If a single principle (e.g., ''minimizing existential risks'') is applied consistently across great and non-great powers, the asymmetry is less extractive.',
    'If the asymmetry is purely extractive (great powers preserving strategic options while denying them to others), the constraint should be reclassified toward snare for threshold-state seats. If the asymmetry is structurally justified (great-power nuclear forces are more stable or less likely to be used than threshold-state forces, for empirical reasons), tangled rope with justified asymmetric distribution is more accurate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(nuclear_threshold_vs_non_nuclear_asymmetry, empirical, 'Principled vs. extractive justification for asymmetric nuclear constraints across power levels.').

omega_variable(
    kernel_reading_distinctiveness,
    'Does this reading (taboo-based prohibition) instantiate a genuinely distinct constraint from its siblings (deterrence_equilibrium_reading, space_contraction_reading), or do all three readings converge on the same structural outcome because they differ only in narrative/causal framing rather than in predicted behavior?',
    'Articulate and test differential predictions across the three readings for specific scenarios (e.g., if deterrence credibility were somehow undermined, would total war become thinkable under the space_contraction reading but not the taboo reading? If the international taboo collapsed, would deterrence-based prohibition remain under the deterrence_equilibrium reading but not the space_contraction reading?). If different readings make different predictions about what would trigger total war, they are genuinely distinct constraints. If all three readings predict the same absence of total war regardless of which causal path holds, they may be observationally equivalent.',
    'If readings are genuinely distinct, the corpus should include all three as separate constraint stories, linked via network.affects_constraints. If they are observationally equivalent, the distinction is rhetorical rather than structural, and the prompt''s kernel framing may have created false analytical categories.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_distinctiveness, conceptual, 'Structural distinctiveness vs. rhetorical framing difference across kernel readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_possibility_space__nuclear_taboo_reading, 1970, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(total_war_taboo_tr_t1970, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 1970, 0.35).
narrative_ontology:measurement_basis(total_war_taboo_tr_t1970, observed).
narrative_ontology:measurement(total_war_taboo_tr_t1985, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 1985, 0.4).
narrative_ontology:measurement_basis(total_war_taboo_tr_t1985, observed).
narrative_ontology:measurement(total_war_taboo_tr_t2000, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 2000, 0.48).
narrative_ontology:measurement_basis(total_war_taboo_tr_t2000, observed).
narrative_ontology:measurement(total_war_taboo_tr_t2010, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 2010, 0.52).
narrative_ontology:measurement_basis(total_war_taboo_tr_t2010, observed).
narrative_ontology:measurement(total_war_taboo_tr_t2020, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 2020, 0.54).
narrative_ontology:measurement_basis(total_war_taboo_tr_t2020, observed).
narrative_ontology:measurement(total_war_taboo_tr_t2026, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 2026, 0.52).
narrative_ontology:measurement_basis(total_war_taboo_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(total_war_taboo_be_t1970, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 1970, 0.28).
narrative_ontology:measurement_basis(total_war_taboo_be_t1970, observed).
narrative_ontology:measurement(total_war_taboo_be_t1985, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 1985, 0.32).
narrative_ontology:measurement_basis(total_war_taboo_be_t1985, observed).
narrative_ontology:measurement(total_war_taboo_be_t2000, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 2000, 0.38).
narrative_ontology:measurement_basis(total_war_taboo_be_t2000, observed).
narrative_ontology:measurement(total_war_taboo_be_t2010, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 2010, 0.4).
narrative_ontology:measurement_basis(total_war_taboo_be_t2010, observed).
narrative_ontology:measurement(total_war_taboo_be_t2020, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 2020, 0.37).
narrative_ontology:measurement_basis(total_war_taboo_be_t2020, observed).
narrative_ontology:measurement(total_war_taboo_be_t2026, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 2026, 0.38).
narrative_ontology:measurement_basis(total_war_taboo_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(total_war_taboo_su_t1970, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 1970, 0.55).
narrative_ontology:measurement_basis(total_war_taboo_su_t1970, observed).
narrative_ontology:measurement(total_war_taboo_su_t1985, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 1985, 0.62).
narrative_ontology:measurement_basis(total_war_taboo_su_t1985, observed).
narrative_ontology:measurement(total_war_taboo_su_t2000, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 2000, 0.68).
narrative_ontology:measurement_basis(total_war_taboo_su_t2000, observed).
narrative_ontology:measurement(total_war_taboo_su_t2010, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 2010, 0.7).
narrative_ontology:measurement_basis(total_war_taboo_su_t2010, observed).
narrative_ontology:measurement(total_war_taboo_su_t2020, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 2020, 0.68).
narrative_ontology:measurement_basis(total_war_taboo_su_t2020, observed).
narrative_ontology:measurement(total_war_taboo_su_t2026, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 2026, 0.67).
narrative_ontology:measurement_basis(total_war_taboo_su_t2026, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=1970, tn=2026
narrative_ontology:measurement(total_war_taboo_grid_01, total_war_possibility_space__nuclear_taboo_reading, accessibility_collapse(class), 1970, 0.45).
narrative_ontology:measurement(total_war_taboo_grid_02, total_war_possibility_space__nuclear_taboo_reading, accessibility_collapse(class), 2026, 0.68).
narrative_ontology:measurement(total_war_taboo_grid_03, total_war_possibility_space__nuclear_taboo_reading, accessibility_collapse(individual), 1970, 0.35).
narrative_ontology:measurement(total_war_taboo_grid_04, total_war_possibility_space__nuclear_taboo_reading, accessibility_collapse(individual), 2026, 0.55).
narrative_ontology:measurement(total_war_taboo_grid_05, total_war_possibility_space__nuclear_taboo_reading, accessibility_collapse(organizational), 1970, 0.58).
narrative_ontology:measurement(total_war_taboo_grid_06, total_war_possibility_space__nuclear_taboo_reading, accessibility_collapse(organizational), 2026, 0.7).
narrative_ontology:measurement(total_war_taboo_grid_07, total_war_possibility_space__nuclear_taboo_reading, accessibility_collapse(structural), 1970, 0.62).
narrative_ontology:measurement(total_war_taboo_grid_08, total_war_possibility_space__nuclear_taboo_reading, accessibility_collapse(structural), 2026, 0.71).
narrative_ontology:measurement(total_war_taboo_grid_09, total_war_possibility_space__nuclear_taboo_reading, resistance(class), 1970, 0.55).
narrative_ontology:measurement(total_war_taboo_grid_10, total_war_possibility_space__nuclear_taboo_reading, resistance(class), 2026, 0.48).
narrative_ontology:measurement(total_war_taboo_grid_11, total_war_possibility_space__nuclear_taboo_reading, resistance(individual), 1970, 0.6).
narrative_ontology:measurement(total_war_taboo_grid_12, total_war_possibility_space__nuclear_taboo_reading, resistance(individual), 2026, 0.42).
narrative_ontology:measurement(total_war_taboo_grid_13, total_war_possibility_space__nuclear_taboo_reading, resistance(organizational), 1970, 0.48).
narrative_ontology:measurement(total_war_taboo_grid_14, total_war_possibility_space__nuclear_taboo_reading, resistance(organizational), 2026, 0.52).
narrative_ontology:measurement(total_war_taboo_grid_15, total_war_possibility_space__nuclear_taboo_reading, resistance(structural), 1970, 0.35).
narrative_ontology:measurement(total_war_taboo_grid_16, total_war_possibility_space__nuclear_taboo_reading, resistance(structural), 2026, 0.38).
narrative_ontology:measurement(total_war_taboo_grid_17, total_war_possibility_space__nuclear_taboo_reading, stakes_inflation(class), 1970, 0.48).
narrative_ontology:measurement(total_war_taboo_grid_18, total_war_possibility_space__nuclear_taboo_reading, stakes_inflation(class), 2026, 0.6).
narrative_ontology:measurement(total_war_taboo_grid_19, total_war_possibility_space__nuclear_taboo_reading, stakes_inflation(individual), 1970, 0.4).
narrative_ontology:measurement(total_war_taboo_grid_20, total_war_possibility_space__nuclear_taboo_reading, stakes_inflation(individual), 2026, 0.52).
narrative_ontology:measurement(total_war_taboo_grid_21, total_war_possibility_space__nuclear_taboo_reading, stakes_inflation(organizational), 1970, 0.52).
narrative_ontology:measurement(total_war_taboo_grid_22, total_war_possibility_space__nuclear_taboo_reading, stakes_inflation(organizational), 2026, 0.68).
narrative_ontology:measurement(total_war_taboo_grid_23, total_war_possibility_space__nuclear_taboo_reading, stakes_inflation(structural), 1970, 0.58).
narrative_ontology:measurement(total_war_taboo_grid_24, total_war_possibility_space__nuclear_taboo_reading, stakes_inflation(structural), 2026, 0.72).
narrative_ontology:measurement(total_war_taboo_grid_25, total_war_possibility_space__nuclear_taboo_reading, suppression(class), 1970, 0.5).
narrative_ontology:measurement(total_war_taboo_grid_26, total_war_possibility_space__nuclear_taboo_reading, suppression(class), 2026, 0.62).
narrative_ontology:measurement(total_war_taboo_grid_27, total_war_possibility_space__nuclear_taboo_reading, suppression(individual), 1970, 0.42).
narrative_ontology:measurement(total_war_taboo_grid_28, total_war_possibility_space__nuclear_taboo_reading, suppression(individual), 2026, 0.58).
narrative_ontology:measurement(total_war_taboo_grid_29, total_war_possibility_space__nuclear_taboo_reading, suppression(organizational), 1970, 0.58).
narrative_ontology:measurement(total_war_taboo_grid_30, total_war_possibility_space__nuclear_taboo_reading, suppression(organizational), 2026, 0.68).
narrative_ontology:measurement(total_war_taboo_grid_31, total_war_possibility_space__nuclear_taboo_reading, suppression(structural), 1970, 0.6).
narrative_ontology:measurement(total_war_taboo_grid_32, total_war_possibility_space__nuclear_taboo_reading, suppression(structural), 2026, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_possibility_space__nuclear_taboo_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(total_war_possibility_space__nuclear_taboo_reading, 0.12).
narrative_ontology:affects_constraint(total_war_possibility_space__nuclear_taboo_reading, deterrence_equilibrium_reading).
narrative_ontology:affects_constraint(total_war_possibility_space__nuclear_taboo_reading, space_contraction_reading).
narrative_ontology:affects_constraint(total_war_possibility_space__nuclear_taboo_reading, non_proliferation_treaty_enforcement).
narrative_ontology:affects_constraint(total_war_possibility_space__nuclear_taboo_reading, great_power_concert_stability).

% DUAL FORMULATION NOTE:
% Total war possibility space is instantiated by three structural readings of a contested kernel: the nuclear-taboo reading (this story) asserts normative prohibition independent of material capability; deterrence_equilibrium_reading grounds prohibition in mutual vulnerability incentives; space_contraction_reading grounds it in epistemic impossibility of calculation. Each reading has different ε value and different type. They are linked as a constraint family through network.affects_constraints because each reading's causal claim influences (but does not foreclose) the others' empirical status. If norm entrepreneurs exit and the taboo collapses (taboo reading), deterrence incentives remain (deterrence reading) or do not, depending on capability distribution. If space contraction is real (space reading), norm taboo and deterrence both become secondary stabilizers.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(total_war_possibility_space__nuclear_taboo_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
