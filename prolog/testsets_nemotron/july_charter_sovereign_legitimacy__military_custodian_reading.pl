% ============================================================================
% CONSTRAINT STORY: july_charter_sovereign_legitimacy__military_custodian_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-07-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_july_charter_sovereign_legitimacy__military_custodian_reading, []).

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
 *   constraint_id: july_charter_sovereign_legitimacy__military_custodian_reading
 *   human_readable: July Charter Military Custodian Mandate
 *   domain: constitutional_law/political_transitions/post_revolutionary_state_building
 *
 * SUMMARY:
 *   The July Charter's military custodian reading institutionalizes the armed
 *   forces as the permanent guardian of state stability, granting them veto
 *   authority over civilian governance through a Constitutional Council and
 *   National Security Council dominated by senior officers. This reading
 *   emerged from the revolutionary transition period where the military
 *   positioned itself as the only institution capable of preventing state
 *   fragmentation. The constraint operates through formal constitutional
 *   mechanisms (Article 180's guardian clause, Article 234's security council
 *   composition) and informal practices (officer corps' de facto veto on
 *   cabinet appointments, defense budget autonomy, control over strategic
 *   enterprises). The claim/metric gap is deliberate: the reading is CLAIMED
 *   as tangled_rope (coordination function of stability + extraction from
 *   political pluralism) while the authored metrics describe substantially
 *   extractive, actively enforced operation with escalating theater — the
 *   engine measures that divergence; do not reconcile the claim to the
 *   metrics.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(july_charter_sovereign_legitimacy__military_custodian_reading, 0.72).
domain_priors:suppression_score(july_charter_sovereign_legitimacy__military_custodian_reading, 0.88).
domain_priors:theater_ratio(july_charter_sovereign_legitimacy__military_custodian_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__military_custodian_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__military_custodian_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__military_custodian_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__military_custodian_reading, accessibility_collapse, 0.82).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__military_custodian_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(july_charter_sovereign_legitimacy__military_custodian_reading, tangled_rope).
narrative_ontology:human_readable(july_charter_sovereign_legitimacy__military_custodian_reading, "July Charter Military Custodian Mandate").
narrative_ontology:topic_domain(july_charter_sovereign_legitimacy__military_custodian_reading, "constitutional_law/political_transitions/post_revolutionary_state_building").

domain_priors:requires_active_enforcement(july_charter_sovereign_legitimacy__military_custodian_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(july_charter_sovereign_legitimacy__military_custodian_reading, '4a27ae18-4cd4-4a5a-8477-f601833fe363').
narrative_ontology:cs_kernel_codification('4a27ae18-4cd4-4a5a-8477-f601833fe363', formalized).
narrative_ontology:cs_authority_grounding('4a27ae18-4cd4-4a5a-8477-f601833fe363', extraction).
narrative_ontology:cs_interpretation_layer_present('4a27ae18-4cd4-4a5a-8477-f601833fe363').
narrative_ontology:cs_reading_relation('4a27ae18-4cd4-4a5a-8477-f601833fe363', july_charter_sovereign_legitimacy__secular_democratic_reading, forecloses).
narrative_ontology:cs_reading_relation('4a27ae18-4cd4-4a5a-8477-f601833fe363', july_charter_sovereign_legitimacy__guided_nationalism_reading, influences).
narrative_ontology:cs_axiom('4a27ae18-4cd4-4a5a-8477-f601833fe363', foundational, military_as_constitutional_sovereign_agent).
narrative_ontology:cs_axiom_status(military_as_constitutional_sovereign_agent, holdable).
narrative_ontology:cs_axiom_grounding('4a27ae18-4cd4-4a5a-8477-f601833fe363', military_as_constitutional_sovereign_agent, conventional).
narrative_ontology:cs_axiom('4a27ae18-4cd4-4a5a-8477-f601833fe363', foundational, stability_requires_political_boundedness).
narrative_ontology:cs_axiom_status(stability_requires_political_boundedness, holdable).
narrative_ontology:cs_axiom_grounding('4a27ae18-4cd4-4a5a-8477-f601833fe363', stability_requires_political_boundedness, instrumental).
narrative_ontology:cs_reference_frame('4a27ae18-4cd4-4a5a-8477-f601833fe363', revolutionary_transition_guardianship).
narrative_ontology:cs_drift_state('4a27ae18-4cd4-4a5a-8477-f601833fe363', post_consolidation_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('4a27ae18-4cd4-4a5a-8477-f601833fe363', '').
narrative_ontology:cs_kernel_id(july_charter_sovereign_legitimacy__military_custodian_reading, july_charter_sovereign_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__military_custodian_reading, military_high_command).
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__military_custodian_reading, security_intelligence_apparatus).
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__military_custodian_reading, state_owned_enterprise_network).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__military_custodian_reading, autonomous_political_parties).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__military_custodian_reading, student_movement).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__military_custodian_reading, independent_judiciary).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__military_custodian_reading, free_press).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__military_custodian_reading, civil_society_organizations).
narrative_ontology:constraint_vindicates(july_charter_sovereign_legitimacy__military_custodian_reading, military_guardianship_doctrine).
narrative_ontology:constraint_vindicates(july_charter_sovereign_legitimacy__military_custodian_reading, stability_over_pluralism_principle).
narrative_ontology:constraint_vindicates(july_charter_sovereign_legitimacy__military_custodian_reading, constitutional_supremacy_of_armed_forces).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sits atop the Constitutional Council and National Security Council with veto power over legislation, cabinet appointments, and national security policy. Controls defense budget (off-budget, unaudited) and strategic state enterprises. Justifies authority as preventing state fragmentation and sectarian conflict. Exit is arbitrage-grade: officer corps can pivot to private security, international consulting, or state enterprise leadership.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__military_custodian_reading, military_high_command, agenda_setter,
    institutional, generational, arbitrage, national).

% Operates with minimal civilian oversight, controlling internal security, surveillance, and political vetting. Receives direct budget allocations and operational autonomy. Their institutional survival depends on the custodial architecture. Exit is constrained: skills are specialized to the security state; defection carries severe personal risk.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__military_custodian_reading, security_intelligence_apparatus, beneficiary,
    institutional, biographical, constrained, national).

% Military-owned enterprises dominate construction, electronics, agriculture, and tourism sectors — estimated 25-40% of GDP. Benefit from preferential contracts, tax exemptions, land grants, and conscript labor. Managed by serving/retired officers. Exit is constrained: enterprises are legally and financially embedded in the military's institutional structure; privatization would require constitutional change under military veto.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__military_custodian_reading, state_owned_enterprise_network, beneficiary,
    organized, biographical, constrained, national).

% Subject to registration hurdles, funding restrictions, candidate vetting by security apparatus, and periodic bans. Parliamentary representation exists but legislative agenda is bounded by military veto. Leadership faces detention, travel bans, asset freezes. Exit is constrained: operating legally requires accepting bounded competition; operating illegally carries severe repression; exile removes domestic relevance.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__military_custodian_reading, autonomous_political_parties, payer,
    organized, biographical, constrained, national).

% Primary mobilization force for democratic demands; faces university security apparatus, mandatory national service conscription, political vetting for graduate study and public employment. Protest is criminalized under emergency laws. Exit is identity-locked: student identity is fused with generational mission for democratic transformation; leaving the movement is experienced as betrayal of cohort and historical responsibility.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__military_custodian_reading, student_movement, payer,
    moderate, biographical, identity_locked, national).

% Constitutional Council and military courts have supremacy over civilian judiciary. Judges face security vetting, disciplinary councils controlled by executive, and transfer/removal for rulings against military interests. Exit is constrained: judicial career is tied to the institutional hierarchy; independent practice is marginalized; emigration severs professional standing.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__military_custodian_reading, independent_judiciary, payer,
    moderate, biographical, constrained, national).

% Subject to licensing regime, pre-publication censorship for 'national security', website blocking, journalist detention, and advertising revenue pressure. State media dominates broadcast. Exit is constrained: domestic audience reach requires compliance; independent digital outlets face technical blocking and financial strangulation; exile media loses domestic distribution.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__military_custodian_reading, free_press, payer,
    moderate, immediate, constrained, national).

% NGO law requires security approval for registration, foreign funding, and activities. Human rights, election monitoring, and labor organizations face periodic closure and asset seizure. Exit is constrained: domestic legitimacy requires registration; foreign funding triggers espionage charges; self-censorship becomes survival strategy.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__military_custodian_reading, civil_society_organizations, payer,
    moderate, biographical, constrained, national).

% Islamist, leftist, and liberal parties formally dissolved or perpetually denied registration. Leadership imprisoned or in exile. Operate underground with severe operational constraints. Would argue for competitive pluralism but are structurally excluded from the constitutional conversation.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__military_custodian_reading, banned_opposition_parties, excluded,
    moderate, biographical, trapped, national).

% Campus-based cells operating under surveillance; members face expulsion, conscription to front-line units, or disappearance. No institutional voice; would demand civilian supremacy and demilitarization of campuses.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__military_custodian_reading, underground_student_networks, excluded,
    powerless, immediate, trapped, local).

% Monitor elections, human rights, constitutional compliance. Produce reports documenting the gap between charter text and practice. Have no enforcement power; access depends on regime tolerance. Their analytical seat sees the full structure but cannot alter it.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__military_custodian_reading, international_democracy_observers, observer,
    institutional, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents state fragmentation in a deeply divided post-revolutionary society by concentrating coercive capacity and arbitration authority in the only institution with national reach and organizational coherence — the military. Manages sectarian, regional, and class conflicts through a centralized guardian structure rather than competitive politics.
% TRANSFER_FUNCTION: Moves political autonomy, legislative agenda control, judicial independence, media freedom, and economic resources (state enterprise profits, land, conscript labor) from civilian actors (parties, students, judiciary, press, civil society) to the military high command and its allied security/intelligence/enterprise network.
% ABSENT_VOICES: Banned opposition parties (Islamist, leftist, liberal) and underground student networks are structurally excluded — they would demand competitive pluralism, civilian supremacy, and demilitarization of campuses/economy but are kept out by the same registration, security, and emergency laws the custodial architecture enforces.
% DISAPPEARANCE_RATIONALE: If the military custodial mandate vanished overnight, the Constitutional Council's military majority would dissolve, party bans would lift, security vetting of judges/journalists would end, military enterprises would face civilian oversight, and competitive politics would erupt. The state might fragment (the military's claimed nightmare) or consolidate a civilian order — but the arrangements would fundamentally reorganize.
% FOUNDING_PROBLEM: The revolutionary transition (2011-2014) produced state fragmentation: competing armed groups, sectarian violence, economic collapse, and institutional vacuum. The military was the only coherent national institution capable of preventing total collapse.
% FOUNDING_PROBLEM_CORROBORATION: The military high command attests the problem remains live (citing border threats, terrorism, sectarian tensions). Independent historians, transitional justice commissions, and former civilian ministers (from outside the beneficiary set) attest the fragmentation risk was substantially resolved by 2025 — state capacity indicators (tax collection, territorial control, service delivery) recovered to pre-revolution levels. The arrangement persisted and expanded after the founding problem died.
narrative_ontology:disappearance_verdict(july_charter_sovereign_legitimacy__military_custodian_reading, world_rearranges).
narrative_ontology:founding_problem_status(july_charter_sovereign_legitimacy__military_custodian_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(july_charter_sovereign_legitimacy__military_custodian_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(july_charter_sovereign_legitimacy__military_custodian_reading, 'none', 1).
narrative_ontology:epsilon_provenance(july_charter_sovereign_legitimacy__military_custodian_reading, 0.72, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(july_charter_sovereign_legitimacy__military_custodian_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(july_charter_sovereign_legitimacy__military_custodian_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(july_charter_sovereign_legitimacy__military_custodian_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is high (0.72 at interval end) because the military's custodial role extracts political autonomy, economic resources (via state enterprise control), and institutional sovereignty from civilian actors. Suppression is very high (0.88) because the constraint's persistence depends on actively excluding competitive politics — party bans, protest criminalization, judicial purges, media licensing — not on participant preference. Theater is moderate-high (0.45): the stability coordination function is real (preventing fragmentation, managing sectarian tensions), but a growing share of enforcement activity defends the military's institutional privileges rather than state stability. The measurement series run on one shared time grid so every metric is authored at every examined time point.
 *
 * PERSPECTIVAL GAP:
 *   The military custodian seat computes as coordination-with-benefits (genuine stability function + institutional capture). The civilian institutional seats (parliament, ministries) compute as constrained coordination (real but bounded autonomy). The victim seats (parties, students, press) compute as extraction under suppression. The engine computes this divergence from the structural data — the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   The military high command is the primary agenda setter and beneficiary (sets the rules, collects veto rents, controls strategic enterprises — d near 0.15 beneficiary end). Security intelligence apparatus and state enterprise network are secondary beneficiaries (institutional autonomy, resource flows — d ~0.25). Autonomous political parties, student movement, independent judiciary, free press, and civil society are primary targets (bear extraction of political space, constrained exit — d ~0.85). The excluded voices (banned parties, underground movements) are structurally trapped with no institutional exit.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing state collapse during revolutionary transition) was substantially resolved by year 15, yet the custodial architecture persisted and expanded. The mandate has atrophied into a structure that extracts from pluralism while performing stability theater. This is not a snare (coordination function is non-zero) but a tangled_rope where the coordination-to-extraction ratio has inverted over time. The classification prevents mislabeling: a pure rope would not suppress alternatives this aggressively; a pure snare would not maintain genuine stability coordination (border security, disaster response, sectarian mediation). The mandatrophy is unresolved — the arrangement persists because the cost to fix (constitutional rewrite under military veto) exceeds what any civilian actor can bear, and the military bears no cost to maintain it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a distinct structural entity from its sibling readings of the same kernel, or a measurement variation of one constraint?',
    'Compare ε values, victim sets, and enforcement mechanisms across the three readings. If ε differs by >0.2 or victim sets are disjoint, they are separate constraints per ε-invariance.',
    'If separate constraints, each gets its own classification and the kernel is a family linked by network.affects_constraints. If one constraint, the classification must accommodate all three readings'' structural data simultaneously.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the kernel''s readings instantiate one constraint or three, per ε-invariance principle.').

omega_variable(
    stability_coordination_necessity,
    'Is the military''s custodial role structurally necessary for the stability outcomes claimed, or is stability achievable through civilian mechanisms?',
    'Counterfactual analysis of comparable post-revolutionary transitions without military custodianship; natural experiment from periods of civilian governance within the same state.',
    'If civilian mechanisms suffice, the coordination function is cover for extraction (Snare). If military role is necessary, the coordination function is genuine (Tangled Rope) though extraction may exceed coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stability_coordination_necessity, empirical, 'Whether the coordination function is structurally necessary or extractive cover.').

omega_variable(
    mandatrophy_timing,
    'At what point did the founding problem (state fragmentation risk) cease to justify the custodial architecture?',
    'Historical analysis of state capacity indicators, threat assessments, and military doctrine publications across the interval.',
    'If the problem was dead by year 15 but architecture expanded through year 40, the post-year-15 extraction is unambiguous mandatrophy. If the problem remained live, the classification shifts toward genuine coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandatrophy_timing, empirical, 'When the founding problem died relative to the constraint''s expansion.').

omega_variable(
    secular_democratic_foreclosure,
    'Does the military_custodian_reading''s core premise (military as permanent constitutional guardian) logically foreclose the secular_democratic_reading''s core premise (civilian supremacy) within a single constitutional framework?',
    'Constitutional theory analysis: can a framework simultaneously hold that the military is the ultimate guardian of the constitution AND that the military is subordinate to civilian authority?',
    'If forecloses, the readings cannot coexist in one legal order — transition requires regime change. If coexists_with, both can be live positions in ongoing contest. If influences, the custodian reading creates structural pressure on the democratic reading without eliminating it.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(secular_democratic_foreclosure, conceptual, 'Structural relationship between military custodian and secular democratic readings.').

omega_variable(
    guided_nationalism_influence,
    'Does the military_custodian_reading create structural downstream pressure on the guided_nationalism_reading (e.g., by controlling the security apparatus that enforces religious-nationalist boundaries)?',
    'Institutional mapping: does the military high command control the enforcement mechanisms (intelligence, internal security, judiciary appointments) that the guided_nationalism_reading depends on?',
    'If influences, the military reading shapes the nationalist reading''s operating environment — the nationalist project depends on military acquiescence. If coexists_with, they are parallel tracks.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(guided_nationalism_influence, empirical, 'Whether the military custodian reading structurally influences the guided nationalism reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(july_charter_sovereign_legitimacy__military_custodian_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(july_tr_t0, july_charter_sovereign_legitimacy__military_custodian_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(july_tr_t8, july_charter_sovereign_legitimacy__military_custodian_reading, theater_ratio, 8, 0.25).
narrative_ontology:measurement(july_tr_t16, july_charter_sovereign_legitimacy__military_custodian_reading, theater_ratio, 16, 0.33).
narrative_ontology:measurement(july_tr_t24, july_charter_sovereign_legitimacy__military_custodian_reading, theater_ratio, 24, 0.39).
narrative_ontology:measurement(july_tr_t32, july_charter_sovereign_legitimacy__military_custodian_reading, theater_ratio, 32, 0.42).
narrative_ontology:measurement(july_tr_t40, july_charter_sovereign_legitimacy__military_custodian_reading, theater_ratio, 40, 0.45).

% Extraction over time
narrative_ontology:measurement(july_be_t0, july_charter_sovereign_legitimacy__military_custodian_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(july_be_t8, july_charter_sovereign_legitimacy__military_custodian_reading, base_extractiveness, 8, 0.52).
narrative_ontology:measurement(july_be_t16, july_charter_sovereign_legitimacy__military_custodian_reading, base_extractiveness, 16, 0.61).
narrative_ontology:measurement(july_be_t24, july_charter_sovereign_legitimacy__military_custodian_reading, base_extractiveness, 24, 0.67).
narrative_ontology:measurement(july_be_t32, july_charter_sovereign_legitimacy__military_custodian_reading, base_extractiveness, 32, 0.7).
narrative_ontology:measurement(july_be_t40, july_charter_sovereign_legitimacy__military_custodian_reading, base_extractiveness, 40, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(july_su_t0, july_charter_sovereign_legitimacy__military_custodian_reading, suppression_requirement, 0, 0.75).
narrative_ontology:measurement(july_su_t8, july_charter_sovereign_legitimacy__military_custodian_reading, suppression_requirement, 8, 0.8).
narrative_ontology:measurement(july_su_t16, july_charter_sovereign_legitimacy__military_custodian_reading, suppression_requirement, 16, 0.84).
narrative_ontology:measurement(july_su_t24, july_charter_sovereign_legitimacy__military_custodian_reading, suppression_requirement, 24, 0.86).
narrative_ontology:measurement(july_su_t32, july_charter_sovereign_legitimacy__military_custodian_reading, suppression_requirement, 32, 0.87).
narrative_ontology:measurement(july_su_t40, july_charter_sovereign_legitimacy__military_custodian_reading, suppression_requirement, 40, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(july_charter_sovereign_legitimacy__military_custodian_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(july_charter_sovereign_legitimacy__military_custodian_reading, 0.12).
narrative_ontology:affects_constraint(july_charter_sovereign_legitimacy__military_custodian_reading, july_charter_sovereign_legitimacy__secular_democratic_reading).
narrative_ontology:affects_constraint(july_charter_sovereign_legitimacy__military_custodian_reading, july_charter_sovereign_legitimacy__guided_nationalism_reading).
narrative_ontology:affects_constraint(july_charter_sovereign_legitimacy__military_custodian_reading, emergency_law_permanent_exception).
narrative_ontology:affects_constraint(july_charter_sovereign_legitimacy__military_custodian_reading, military_enterprise_economic_empire).
narrative_ontology:affects_constraint(july_charter_sovereign_legitimacy__military_custodian_reading, constitutional_council_veto_architecture).

% DUAL FORMULATION NOTE:
% This constraint is the military_custodian_reading of the july_charter_sovereign_legitimacy kernel. The secular_democratic_reading and guided_nationalism_reading are sibling constraints from the same kernel. The ε values differ substantially: military_custodian (ε≈0.72) extracts from pluralism; secular_democratic (ε≈0.15) constrains military; guided_nationalism (ε≈0.68) extracts from secular/minority actors. They share the same constitutional text but instantiate different constraints with different victim/beneficiary structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(july_charter_sovereign_legitimacy__military_custodian_reading, institutional, 0.15).
constraint_indexing:directionality_override(july_charter_sovereign_legitimacy__military_custodian_reading, organized, 0.85).
constraint_indexing:directionality_override(july_charter_sovereign_legitimacy__military_custodian_reading, moderate, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
