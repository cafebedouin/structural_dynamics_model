% ============================================================================
% CONSTRAINT STORY: article_51_self_defense__expansive_preventive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_51_self_defense__expansive_preventive_reading, []).

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
 *   constraint_id: article_51_self_defense__expansive_preventive_reading
 *   human_readable: Expansive Preventive Self-Defense Reading of Article 51
 *   domain: international_law/security_studies/constitutional_interpretation
 *
 * SUMMARY:
 *   This constraint story models the expansive preventive reading of Article
 *   51 self-defense — the claim that states may use force preemptively or
 *   preventively against non-state actors or emerging threats based on a
 *   self-judged necessity determination, without requiring an actual or
 *   imminent armed attack attributable to a state. The reading emerged
 *   progressively from the UN Charter's restrictive text through the Caroline
 *   criteria, the 1967 Six-Day War anticipatory invocation, post-Cold War
 *   humanitarian intervention claims, the post-9/11 'war on terror' expansion
 *   against non-state actors, and the contemporary drone/strike campaign
 *   practice. The constraint presents as a coordination mechanism (channeling
 *   force through a necessity standard) but operates with substantial
 *   extraction: the acting state judges its own necessity, target populations
 *   bear the costs of error or overreach, and multilateral authorization
 *   mechanisms are bypassed. The reading's beneficiaries are militarily
 *   capable states and their defense sectors; its victims are populations in
 *   target regions and the multilateral veto authority the UN Charter
 *   designed.
 *
 * KEY AGENTS:
 *   - militarily_capable_states: Primary beneficiaries (institutional/arbitrage) — wield the reading to authorize unilateral force
 *   - defense_sectors: Beneficiaries (organized/arbitrage) — receive budgetary and operational mandate expansion
 *   - executive_branches: Agenda setters (institutional/arbitrage) — control the necessity determination process
 *   - target_region_populations: Primary victims (powerless/trapped) — bear kinetic and structural costs with no exit
 *   - multilateral_veto_authority: Victims (institutional/constrained) — bypassed by self-judged necessity claims
 *   - weaker_states: Victims (moderate/constrained) — subject to preventive strikes without recourse
 *   - international_legal_scholars: Observers (analytical/analytical) — contest the reading's legitimacy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_51_self_defense__expansive_preventive_reading, 0.68).
domain_priors:suppression_score(article_51_self_defense__expansive_preventive_reading, 0.72).
domain_priors:theater_ratio(article_51_self_defense__expansive_preventive_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_51_self_defense__expansive_preventive_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(article_51_self_defense__expansive_preventive_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(article_51_self_defense__expansive_preventive_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_51_self_defense__expansive_preventive_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(article_51_self_defense__expansive_preventive_reading, resistance, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_51_self_defense__expansive_preventive_reading, tangled_rope).
narrative_ontology:human_readable(article_51_self_defense__expansive_preventive_reading, "Expansive Preventive Self-Defense Reading of Article 51").
narrative_ontology:topic_domain(article_51_self_defense__expansive_preventive_reading, "international_law/security_studies/constitutional_interpretation").

domain_priors:requires_active_enforcement(article_51_self_defense__expansive_preventive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_51_self_defense__expansive_preventive_reading, '68cfdaa6-7bc1-4665-a4bf-3b145595cb4e').
narrative_ontology:cs_kernel_codification('68cfdaa6-7bc1-4665-a4bf-3b145595cb4e', fixed_text).
narrative_ontology:cs_authority_grounding('68cfdaa6-7bc1-4665-a4bf-3b145595cb4e', lineage).
narrative_ontology:cs_interpretation_layer_present('68cfdaa6-7bc1-4665-a4bf-3b145595cb4e').
narrative_ontology:cs_reading_relation('68cfdaa6-7bc1-4665-a4bf-3b145595cb4e', article_51_self_defense__narrow_armed_attack_reading, coexists_with).
narrative_ontology:cs_reading_relation('68cfdaa6-7bc1-4665-a4bf-3b145595cb4e', article_51_self_defense__unable_unwilling_doctrine_reading, influences).
narrative_ontology:cs_axiom('68cfdaa6-7bc1-4665-a4bf-3b145595cb4e', foundational, necessity_self_judged_by_acting_state).
narrative_ontology:cs_axiom_status(necessity_self_judged_by_acting_state, holdable).
narrative_ontology:cs_axiom_grounding('68cfdaa6-7bc1-4665-a4bf-3b145595cb4e', necessity_self_judged_by_acting_state, conventional).
narrative_ontology:cs_axiom('68cfdaa6-7bc1-4665-a4bf-3b145595cb4e', foundational, non_state_actor_attack_equivalence_to_state_armed_attack).
narrative_ontology:cs_axiom_status(non_state_actor_attack_equivalence_to_state_armed_attack, holdable).
narrative_ontology:cs_axiom_grounding('68cfdaa6-7bc1-4665-a4bf-3b145595cb4e', non_state_actor_attack_equivalence_to_state_armed_attack, conventional).
narrative_ontology:cs_axiom('68cfdaa6-7bc1-4665-a4bf-3b145595cb4e', foundational, preventive_force_against_emerging_threats_permissible).
narrative_ontology:cs_axiom_status(preventive_force_against_emerging_threats_permissible, holdable).
narrative_ontology:cs_axiom_grounding('68cfdaa6-7bc1-4665-a4bf-3b145595cb4e', preventive_force_against_emerging_threats_permissible, instrumental).
narrative_ontology:cs_reference_frame('68cfdaa6-7bc1-4665-a4bf-3b145595cb4e', post_911_self_defense_expansion).
narrative_ontology:cs_drift_state('68cfdaa6-7bc1-4665-a4bf-3b145595cb4e', contemporary_drone_campaign_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('68cfdaa6-7bc1-4665-a4bf-3b145595cb4e', '').
narrative_ontology:cs_kernel_id(article_51_self_defense__expansive_preventive_reading, article_51_self_defense).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_51_self_defense__expansive_preventive_reading, militarily_capable_states).
narrative_ontology:constraint_beneficiary(article_51_self_defense__expansive_preventive_reading, defense_sectors).
narrative_ontology:constraint_beneficiary(article_51_self_defense__expansive_preventive_reading, executive_branches).
narrative_ontology:constraint_victim(article_51_self_defense__expansive_preventive_reading, target_region_populations).
narrative_ontology:constraint_victim(article_51_self_defense__expansive_preventive_reading, multilateral_veto_authority).
narrative_ontology:constraint_victim(article_51_self_defense__expansive_preventive_reading, weaker_states).
narrative_ontology:constraint_vindicates(article_51_self_defense__expansive_preventive_reading, necessity_self_judged_doctrine).
narrative_ontology:constraint_vindicates(article_51_self_defense__expansive_preventive_reading, preemptive_force_legitimacy).
narrative_ontology:constraint_vindicates(article_51_self_defense__expansive_preventive_reading, non_state_actor_armed_attack_equivalence).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% States with power projection capability (US, Russia, China, Israel, UK, France, etc.) invoke this reading to authorize unilateral strikes without Security Council approval. They control the necessity determination process, collect operational autonomy and strategic initiative, and can revert to narrower legal positions when politically convenient. Their military-industrial complexes receive sustained demand signals.
narrative_ontology:constraint_stakeholder(article_51_self_defense__expansive_preventive_reading, militarily_capable_states, beneficiary,
    institutional, generational, arbitrage, global).

% Defense departments, intelligence agencies, and defense contractors benefit from expanded operational mandates, budgetary allocations for preventive capabilities (drones, surveillance, strike platforms), and institutional prestige. They shape the threat assessments that feed necessity determinations. They can pivot to other threat narratives if this reading loses political support.
narrative_ontology:constraint_stakeholder(article_51_self_defense__expansive_preventive_reading, defense_sectors, beneficiary,
    organized, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(article_51_self_defense__expansive_preventive_reading, defense_sectors, agenda_setter).

% National executives (presidents, prime ministers, cabinets) control the necessity determination — they decide when a threat is 'emerging' and force is 'necessary.' They extract institutional authority from being the sole judge of their own constraint compliance. They can change the reading by adopting stricter internal guidelines but face political incentives to maintain flexibility.
narrative_ontology:constraint_stakeholder(article_51_self_defense__expansive_preventive_reading, executive_branches, agenda_setter,
    institutional, biographical, arbitrage, national).

% Civilians in regions where preventive strikes occur (Afghanistan, Pakistan, Yemen, Somalia, Syria, Iraq, Gaza, etc.) bear kinetic harm (death, injury, displacement), infrastructure destruction, psychological trauma, and economic devastation. They have no exit from the targeting logic — they cannot leave the 'threat zone' because the zone is defined by the striker. No meaningful recourse exists: domestic courts of the acting state lack jurisdiction, international courts lack enforcement, and the acting state's necessity determination is final.
narrative_ontology:constraint_stakeholder(article_51_self_defense__expansive_preventive_reading, target_region_populations, payer,
    powerless, immediate, trapped, local).

% The UN Security Council's Chapter VII authorization mechanism — particularly the P5 veto structure — is the institutional gatekeeper this reading bypasses. When a capable state acts unilaterally under self-judged necessity, the Council's authority is structurally eroded. The Council members (including non-acting P5) remain in the institution but their designed function is suppressed. They cannot 'exit' the Charter system but their authority is hollowed out by repeated bypass.
narrative_ontology:constraint_stakeholder(article_51_self_defense__expansive_preventive_reading, multilateral_veto_authority, payer,
    institutional, generational, constrained, global).

% States lacking power projection capability are subject to preventive strikes on their territory (e.g., Pakistan, Yemen, Syria) without consent and without Security Council mandate. They bear sovereignty costs and domestic political instability. They have constrained exit: they can protest diplomatically, seek regional alliance support, or appeal to international bodies, but none reliably stops a determined capable state. Some acquire deterrent capabilities (nuclear, asymmetric) as exit — but this is generational and not available to most.
narrative_ontology:constraint_stakeholder(article_51_self_defense__expansive_preventive_reading, weaker_states, payer,
    moderate, biographical, constrained, regional).

% Scholars and jurists analyze, critique, and categorize the reading's legitimacy. They do not collect rents or bear kinetic costs. Their 'exit' is intellectual — they can adopt any reading or reject the framework entirely. Their work feeds the legitimacy contest but does not structurally constrain the acting states.
narrative_ontology:constraint_stakeholder(article_51_self_defense__expansive_preventive_reading, international_legal_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article_51_self_defense__expansive_preventive_reading, executive_branches).
narrative_ontology:fixing_cost_class(article_51_self_defense__expansive_preventive_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Channels unilateral force decisions through a necessity standard, providing a legal framework for responding to non-state actor threats and emerging dangers when the Security Council is paralyzed by veto politics. Solves the collective action problem of 'who decides when force is necessary' by vesting it in the threatened state.
% TRANSFER_FUNCTION: Moves the authority to initiate force and the costs of that force from the multilateral system (Security Council authorization, collective burden-sharing) to the acting state (operational control) and target populations (kinetic, displacement, infrastructure costs). The acting state gains strategic initiative; target populations lose security and sovereignty; the multilateral system loses gatekeeping authority.
% ABSENT_VOICES: Populations in target regions who would object to being designated 'emerging threats' — they are not represented in the acting state's necessity determination, the Security Council is bypassed, and their own states often lack capacity or will to protect them. Future generations who inherit the precedent of low-threshold preventive war. States that would be victims but have not yet been targeted — their silence is structural, not voluntary.
% DISAPPEARANCE_RATIONALE: If the expansive preventive reading vanished overnight, capable states would lose their primary legal basis for unilateral strikes against non-state actors outside declared war zones. They would either revert to the narrow armed attack standard (requiring Security Council authorization for most actions), invoke the unable/unwilling doctrine (still requiring host state failure), or operate openly outside Charter frameworks. Target populations would gain the protection of the Charter's collective security mechanism (however imperfect). The multilateral system would regain its designed gatekeeping function. The global strike architecture (drone bases, intelligence sharing, legal advisers embedded in targeting) would face legal challenge or dismantlement.
% FOUNDING_PROBLEM: The Security Council's veto paralysis prevents collective response to genuine imminent threats, particularly from non-state actors operating from failed or hostile states. States need a legal basis for defensive action when the multilateral system cannot act in time.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (veto paralysis + non-state threats) is attested by the 2004 High-Level Panel on Threats, Challenges and Change, the 2005 World Summit Outcome Document, and numerous Security Council deadlocks (Syria, Ukraine, etc.) — sources outside the beneficiary set. However, the reading's expansion to purely preventive strikes against capacity development (not imminent attacks) is attested by the beneficiaries themselves (US 2002 NSS, Israeli 'beginning doctrine', Russian 'preventive force' claims) with no independent corroboration that this expansion solves the founding problem rather than exploiting it.
narrative_ontology:disappearance_verdict(article_51_self_defense__expansive_preventive_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_51_self_defense__expansive_preventive_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_51_self_defense__expansive_preventive_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(article_51_self_defense__expansive_preventive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_51_self_defense__expansive_preventive_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_51_self_defense__expansive_preventive_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(article_51_self_defense__expansive_preventive_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(article_51_self_defense__expansive_preventive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) reflects the reading's structural transfer: acting states gain low-constraint force authorization while externalizing costs to target populations and multilateral institutions. The necessity standard is self-judged, creating a classic principal-agent problem where the agent (executive) judges its own compliance. Suppression (0.72) is high because the reading's persistence depends on actively marginalizing the Security Council's authorization role and treating legal challenges as politically motivated — the constraint suppresses the multilateral alternative. Theater ratio (0.42) is substantial: the necessity demonstration, proportionality assessments, and reporting rituals are real but increasingly performative as the threshold for 'emerging threat' lowers and post-strike justification replaces prior authorization. The measurement series shows steady extraction accumulation from 1945 (Charter baseline) through 2024, with inflection points at 1967 (anticipatory self-defense), 2001 (non-state actor expansion), and 2011 (drone campaign normalization).
 *
 * PERSPECTIVAL GAP:
 *   From the acting state's seat, the reading is genuine coordination: it channels force through a necessity standard, provides legal certainty for defensive planning, and solves the collective action problem of responding to non-state threats that the Security Council cannot address due to veto politics. From the target population's seat, the same structure is extraction: a self-judged standard with no independent review, applied by militarily superior actors against whom they have no recourse. From the multilateral institution's seat, it is institutional erosion: the Charter's collective security architecture is bypassed by a reading that claims Charter authority while hollowing out its central mechanism. The engine computes this divergence from the declared power/exit/beneficiary structure — the claimed_type (tangled_rope) acknowledges both coordination and extraction are structurally present.
 *
 * DIRECTIONALITY LOGIC:
 *   Militarily capable states and defense sectors are structural beneficiaries: they control the necessity determination (d near 0.1), collect operational autonomy and budgetary resources, and possess arbitrage-grade exit (can revert to narrow reading if politically convenient). Executive branches are agenda setters with institutional power and arbitrage exit — they administer the constraint and could change it but extract institutional authority from maintaining it. Target region populations are full targets (d near 0.95): powerless, trapped, identity-locked by geography, bearing kinetic and displacement costs with no meaningful exit. Multilateral veto authority (Security Council permanent members not acting) are institutional victims: their designed gatekeeping function is structurally suppressed (constrained exit — they remain in the institution but their authority is bypassed). Weaker states are moderate/constrained victims: they lack military capacity to reciprocate but retain some diplomatic exit options. International legal scholars are analytical observers.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (responding to imminent threats when the Security Council is paralyzed) remains partially live — veto paralysis is real. But the reading has expanded far beyond that problem: from imminent state attacks to emerging non-state threats to preventive strikes against capacity development. The mandate has atrophied into a general low-constraint force authorization. The theater_ratio rise (0.08 to 0.42) tracks this: necessity demonstrations increasingly ritualize decisions already made on strategic grounds. The constraint persists because beneficiaries (capable states) extract sufficient value to defend it, while victims (target populations) lack power to overturn it, and the multilateral alternative remains veto-blocked. This is not a scaffold (no sunset, no transition plan) but a tangled_rope: genuine coordination function (necessity standard channels force) fused with asymmetric extraction (self-judging, cost externalization).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_boundary,
    'Does this reading''s self-judged necessity standard foreclose the narrow armed attack reading''s requirement of an attributable state armed attack within a single legal framework?',
    'Analyze whether a state adopting the expansive preventive reading as its operative doctrine is logically committed to rejecting the narrow reading''s core premise, or whether both can coexist as positions held by different states in the international system.',
    'If forecloses, the two readings cannot both be holdable within one state''s legal framework; if coexists_with, they are live rival positions across the international community. Determines reading_relations classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Whether expansive preventive reading logically forecloses narrow armed attack reading').

omega_variable(
    necessity_standard_objectivity,
    'Is the ''necessity demonstrated'' standard in this reading a genuine constraint on executive power, or a performative threshold that the acting state can always satisfy ex post?',
    'Comparative analysis of invoked necessity claims vs. subsequent judicial or political review outcomes; track record of necessity findings being rejected by domestic courts or international bodies.',
    'If performative, the reading''s coordination function (constraining unilateral force) collapses and extraction approaches 1.0 for target populations; if constraining, genuine coordination persists alongside extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(necessity_standard_objectivity, empirical, 'Whether the necessity standard operates as genuine constraint or executive blank check').

omega_variable(
    unable_unwilled_doctrine_boundary,
    'Where does the unable/unwilling doctrine reading''s hybrid trigger end and this reading''s pure preventive trigger begin?',
    'Case law analysis: identify the threshold where a host state''s inability/unwillingness becomes irrelevant and the acting state''s self-judged necessity alone suffices.',
    'If the boundaries blur completely, the three readings collapse into a single extractive continuum; if distinct, each reading occupies a different structural position in the constraint family.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unable_unwilled_doctrine_boundary, conceptual, 'Structural boundary between preventive and unable/unwilled self-defense triggers').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_51_self_defense__expansive_preventive_reading, 1945, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t1945, article_51_self_defense__expansive_preventive_reading, theater_ratio, 1945, 0.08).
narrative_ontology:measurement(arti_tr_t1967, article_51_self_defense__expansive_preventive_reading, theater_ratio, 1967, 0.15).
narrative_ontology:measurement(arti_tr_t1990, article_51_self_defense__expansive_preventive_reading, theater_ratio, 1990, 0.25).
narrative_ontology:measurement(arti_tr_t2001, article_51_self_defense__expansive_preventive_reading, theater_ratio, 2001, 0.38).
narrative_ontology:measurement(arti_tr_t2011, article_51_self_defense__expansive_preventive_reading, theater_ratio, 2011, 0.41).
narrative_ontology:measurement(arti_tr_t2024, article_51_self_defense__expansive_preventive_reading, theater_ratio, 2024, 0.42).

% Extraction over time
narrative_ontology:measurement(arti_be_t1945, article_51_self_defense__expansive_preventive_reading, base_extractiveness, 1945, 0.15).
narrative_ontology:measurement(arti_be_t1967, article_51_self_defense__expansive_preventive_reading, base_extractiveness, 1967, 0.22).
narrative_ontology:measurement(arti_be_t1990, article_51_self_defense__expansive_preventive_reading, base_extractiveness, 1990, 0.35).
narrative_ontology:measurement(arti_be_t2001, article_51_self_defense__expansive_preventive_reading, base_extractiveness, 2001, 0.52).
narrative_ontology:measurement(arti_be_t2011, article_51_self_defense__expansive_preventive_reading, base_extractiveness, 2011, 0.61).
narrative_ontology:measurement(arti_be_t2024, article_51_self_defense__expansive_preventive_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t1945, article_51_self_defense__expansive_preventive_reading, suppression_requirement, 1945, 0.12).
narrative_ontology:measurement(arti_su_t1967, article_51_self_defense__expansive_preventive_reading, suppression_requirement, 1967, 0.25).
narrative_ontology:measurement(arti_su_t1990, article_51_self_defense__expansive_preventive_reading, suppression_requirement, 1990, 0.45).
narrative_ontology:measurement(arti_su_t2001, article_51_self_defense__expansive_preventive_reading, suppression_requirement, 2001, 0.65).
narrative_ontology:measurement(arti_su_t2011, article_51_self_defense__expansive_preventive_reading, suppression_requirement, 2011, 0.71).
narrative_ontology:measurement(arti_su_t2024, article_51_self_defense__expansive_preventive_reading, suppression_requirement, 2024, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_51_self_defense__expansive_preventive_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(article_51_self_defense__expansive_preventive_reading, 0.12).
narrative_ontology:affects_constraint(article_51_self_defense__expansive_preventive_reading, article_51_self_defense__narrow_armed_attack_reading).
narrative_ontology:affects_constraint(article_51_self_defense__expansive_preventive_reading, article_51_self_defense__unable_unwilling_doctrine_reading).
narrative_ontology:affects_constraint(article_51_self_defense__expansive_preventive_reading, un_charter_chapter_vii_authority).
narrative_ontology:affects_constraint(article_51_self_defense__expansive_preventive_reading, international_humanitarian_law_civilian_protection).

% DUAL FORMULATION NOTE:
% This reading and its siblings form the article_51_self_defense constraint family. The expansive preventive reading (this story) has the highest extractiveness (0.68) and lowest constraint on unilateral action. The narrow armed attack reading has near-zero extractiveness but high accessibility_collapse (genuine legal constraint). The unable/unwilling doctrine reading sits between them structurally. The ε values differ because each reading instantiates a different constraint: different beneficiary/victim structures, different suppression mechanisms, different coordination functions. They are linked by network.affects_constraints because the expansive reading's operation degrades the narrow reading's authority (contamination propagation) and the unable/unwilling reading is often invoked as a stepping stone to the expansive reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(article_51_self_defense__expansive_preventive_reading, institutional, 0.15).
constraint_indexing:directionality_override(article_51_self_defense__expansive_preventive_reading, powerless, 0.95).
constraint_indexing:directionality_override(article_51_self_defense__expansive_preventive_reading, moderate, 0.75).
constraint_indexing:directionality_override(article_51_self_defense__expansive_preventive_reading, analytical, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
