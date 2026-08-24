% ============================================================================
% CONSTRAINT STORY: war_powers_allocation__inherent_executive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_war_powers_allocation__inherent_executive_reading, []).

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
 *   constraint_id: war_powers_allocation__inherent_executive_reading
 *   human_readable: Inherent Executive Authority to Deploy Force Without Prior Congressional Authorization
 *   domain: constitutional_law/separation_of_powers/war_powers
 *
 * SUMMARY:
 *   This constraint story captures the 'inherent executive authority' reading
 *   of the war powers allocation kernel — the constitutional theory that the
 *   Commander-in-Chief Clause grants the president independent power to
 *   initiate military force in defense of national interests without prior
 *   congressional authorization. This reading has been advanced by every
 *   administration since Truman (Korea, 1950) through OLC opinions, signing
 *   statements, and practice. It treats congressional authorization as a
 *   political courtesy, not a constitutional requirement, and relies on
 *   appropriations bills as retrospective ratification. The constraint is a
 *   tangled rope: it solves a genuine coordination problem (decisive command
 *   in crisis) but extracts asymmetrically from Congress's constitutional war
 *   power and from public accountability. The claimed type (tangled_rope) and
 *   authored metrics are independent — the executive branch claims this is a
 *   'rope' (pure coordination), but the metrics describe substantial
 *   extraction and active enforcement.
 *
 * KEY AGENTS:
 *   - president: Primary agenda setter (institutional/arbitrage) — claims inherent authority, controls deployment decisions
 *   - executive_branch: Primary beneficiary (institutional/constrained) — gains institutional power via OLC opinions and NSC control
 *   - national_security_establishment: Beneficiary/payer (organized/constrained) — gains operational autonomy, bears operational risk
 *   - congress: Primary victim (institutional/constrained) — formal powers atrophy under repeated unilateral executive action
 *   - legislative_war_powers: Victim (organized/trapped) — the constitutional mechanism itself erodes structurally
 *   - public_accountability: Victim (powerless/trapped) — bears costs without representation or effective exit
 *   - courts: Observer (institutional/analytical) — avoids adjudication via political question doctrine, enabling executive practice
 *   - foreign_governments: Excluded (powerful/trapped) — subject to force with no voice in the authorizing structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(war_powers_allocation__inherent_executive_reading, 0.75).
domain_priors:suppression_score(war_powers_allocation__inherent_executive_reading, 0.45).
domain_priors:theater_ratio(war_powers_allocation__inherent_executive_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(war_powers_allocation__inherent_executive_reading, extractiveness, 0.75).
narrative_ontology:constraint_metric(war_powers_allocation__inherent_executive_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(war_powers_allocation__inherent_executive_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(war_powers_allocation__inherent_executive_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(war_powers_allocation__inherent_executive_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(war_powers_allocation__inherent_executive_reading, tangled_rope).
narrative_ontology:human_readable(war_powers_allocation__inherent_executive_reading, "Inherent Executive Authority to Deploy Force Without Prior Congressional Authorization").
narrative_ontology:topic_domain(war_powers_allocation__inherent_executive_reading, "constitutional_law/separation_of_powers/war_powers").

domain_priors:requires_active_enforcement(war_powers_allocation__inherent_executive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(war_powers_allocation__inherent_executive_reading, 'bbde8c85-368a-43cf-b9fd-1ef541e32bd6').
narrative_ontology:cs_kernel_codification('bbde8c85-368a-43cf-b9fd-1ef541e32bd6', fixed_text).
narrative_ontology:cs_authority_grounding('bbde8c85-368a-43cf-b9fd-1ef541e32bd6', extraction).
narrative_ontology:cs_interpretation_layer_present('bbde8c85-368a-43cf-b9fd-1ef541e32bd6').
narrative_ontology:cs_reading_relation('bbde8c85-368a-43cf-b9fd-1ef541e32bd6', war_powers_allocation__congressional_primacy_reading, forecloses).
narrative_ontology:cs_reading_relation('bbde8c85-368a-43cf-b9fd-1ef541e32bd6', war_powers_allocation__functional_accommodation_reading, influences).
narrative_ontology:cs_axiom('bbde8c85-368a-43cf-b9fd-1ef541e32bd6', foundational, commander_in_chief_inherent_authority).
narrative_ontology:cs_axiom_status(commander_in_chief_inherent_authority, holdable).
narrative_ontology:cs_axiom_grounding('bbde8c85-368a-43cf-b9fd-1ef541e32bd6', commander_in_chief_inherent_authority, deontological).
narrative_ontology:cs_axiom('bbde8c85-368a-43cf-b9fd-1ef541e32bd6', secondary, appropriations_as_implied_authorization).
narrative_ontology:cs_axiom_status(appropriations_as_implied_authorization, holdable).
narrative_ontology:cs_axiom_grounding('bbde8c85-368a-43cf-b9fd-1ef541e32bd6', appropriations_as_implied_authorization, conventional).
narrative_ontology:cs_reference_frame('bbde8c85-368a-43cf-b9fd-1ef541e32bd6', founding_era_constitutional_design).
narrative_ontology:cs_drift_state('bbde8c85-368a-43cf-b9fd-1ef541e32bd6', post_9_11_perpetual_authorization_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('bbde8c85-368a-43cf-b9fd-1ef541e32bd6', '').
narrative_ontology:cs_kernel_id(war_powers_allocation__inherent_executive_reading, war_powers_allocation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(war_powers_allocation__inherent_executive_reading, president).
narrative_ontology:constraint_beneficiary(war_powers_allocation__inherent_executive_reading, executive_branch).
narrative_ontology:constraint_beneficiary(war_powers_allocation__inherent_executive_reading, national_security_establishment).
narrative_ontology:constraint_victim(war_powers_allocation__inherent_executive_reading, congress).
narrative_ontology:constraint_victim(war_powers_allocation__inherent_executive_reading, legislative_war_powers).
narrative_ontology:constraint_victim(war_powers_allocation__inherent_executive_reading, public_accountability).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(war_powers_allocation__inherent_executive_reading, national_security_establishment).
narrative_ontology:constraint_vindicates(war_powers_allocation__inherent_executive_reading, unitary_executive_theory).
narrative_ontology:constraint_vindicates(war_powers_allocation__inherent_executive_reading, commander_in_chief_plenary_power).
narrative_ontology:constraint_vindicates(war_powers_allocation__inherent_executive_reading, appropriations_as_implied_authorization).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Claims inherent constitutional authority as Commander-in-Chief to initiate military force without prior congressional approval. Directs deployments, sets operational parameters, and controls intelligence that shapes threat assessments. Gains institutional power and operational flexibility; bears political risk of failed operations.
narrative_ontology:constraint_stakeholder(war_powers_allocation__inherent_executive_reading, president, agenda_setter,
    institutional, biographical, arbitrage, global).

% Gains expanded institutional authority through OLC opinions, NSC decision-making, and control of military planning. The Office of Legal Counsel produces authoritative interpretations that legitimate unilateral action. Career officials rotate through positions that institutionalize the reading across administrations.
narrative_ontology:constraint_stakeholder(war_powers_allocation__inherent_executive_reading, executive_branch, beneficiary,
    institutional, generational, constrained, global).

% Military and intelligence agencies receive clear command authority and operational autonomy without legislative micromanagement. They also bear the operational risks, casualties, and strategic blowback of wars launched without broad political buy-in. Their budget requests and posture reviews shape the threat narratives that justify deployments.
narrative_ontology:constraint_stakeholder(war_powers_allocation__inherent_executive_reading, national_security_establishment, beneficiary,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(war_powers_allocation__inherent_executive_reading, national_security_establishment, payer).

% Formally retains the declare-war power and the power of the purse, but in practice faces a repeated choice: fund troops already in harm's way or be accused of abandoning them. War Powers Resolution (1973) creates procedural hurdles but lacks enforcement teeth. Individual members avoid accountability by not voting; leadership avoids bringing authorizations to the floor.
narrative_ontology:constraint_stakeholder(war_powers_allocation__inherent_executive_reading, congress, payer,
    institutional, biographical, constrained, national).

% The constitutional mechanism itself — the collective legislative check on offensive war — is the entity that atrophies. Each unilateral deployment that faces only retrospective appropriations ratification establishes precedent that narrows the space for meaningful prior authorization. No single actor 'owns' this loss; it is a structural erosion of a constitutional function.
narrative_ontology:constraint_stakeholder(war_powers_allocation__inherent_executive_reading, legislative_war_powers, payer,
    organized, generational, trapped, national).

% The public bears the costs — blood, treasure, strategic consequences — of wars launched without their representatives' deliberative consent. Electoral accountability is diluted because responsibility is diffused: the president claims constitutional authority, Congress claims it was never asked, and the courts claim non-justiciability. No effective exit from the consequences of executive war-making.
narrative_ontology:constraint_stakeholder(war_powers_allocation__inherent_executive_reading, public_accountability, payer,
    powerless, biographical, trapped, national).

% Federal courts, particularly the Supreme Court, are repeatedly asked to adjudicate war powers disputes. They consistently invoke political question doctrine, standing barriers, and prudential avoidance to decline merits rulings. Their non-intervention functions as tacit ratification of executive practice. Lower courts occasionally issue narrow rulings that are reversed or limited.
narrative_ontology:constraint_stakeholder(war_powers_allocation__inherent_executive_reading, courts, observer,
    institutional, generational, analytical, national).

% States targeted by or drawn into U.S. military actions have no voice in the constitutional allocation that authorizes those actions. They experience the effects — invasion, bombing, regime change, refugee flows — but cannot petition Congress, sue in U.S. courts, or influence the OLC opinions that legitimate the force. Their only leverage is diplomatic, military, or asymmetric response.
narrative_ontology:constraint_stakeholder(war_powers_allocation__inherent_executive_reading, foreign_governments, excluded,
    powerful, biographical, trapped, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(war_powers_allocation__inherent_executive_reading, executive_branch).
narrative_ontology:fixing_cost_class(war_powers_allocation__inherent_executive_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides unified, decisive command authority for military operations in time-sensitive crises where legislative deliberation would be impractical or dangerous. Solves the genuine coordination problem of who decides when minutes matter: the Constitution designates a single Commander-in-Chief, not 535 legislators.
% TRANSFER_FUNCTION: Moves the effective power to initiate offensive military force from Congress (as the Constitution's text assigns) to the President. Transfers political accountability from the legislative branch (which must vote on record) to the executive branch (which acts unilaterally and dares Congress to defund). Transfers the costs of war — human, financial, strategic — to the public and to foreign populations without their consent.
% ABSENT_VOICES: The foreign populations subject to U.S. military action are structurally excluded — they have no constitutional standing, no congressional representation, no access to U.S. courts. Future generations who inherit the strategic consequences (blowback, destabilization, debt) are excluded by temporal distance. The War Powers Resolution's consultation requirements are routinely satisfied by perfunctory notifications that exclude meaningful legislative debate.
% DISAPPEARANCE_RATIONALE: If the inherent-executive reading vanished overnight — i.e., if the constitutional consensus shifted to require explicit prior authorization for all non-defensive force — the last 75 years of U.S. military practice (Korea, Vietnam, Grenada, Panama, Kosovo, Libya, Syria, etc.) would have been constitutionally unauthorized. The executive branch would lose its primary legal basis for unilateral action; Congress would be forced to debate and vote on each deployment; the OLC opinion apparatus would be repurposed or dismantled; the global posture of continuous military engagement would become legally unsustainable without constant legislative renewal.
% FOUNDING_PROBLEM: The founding problem was not 'how to let the president wage war alone' but 'how to enable rapid defensive response while preserving legislative control over offensive war.' The Framers rejected giving the president the power to 'make war' (changed to 'declare war' precisely to lodge offensive initiation in Congress). The inherent-executive reading reframes the founding problem as 'how to empower the Commander-in-Chief to defend national interests without legislative paralysis' — a reframing that treats the legislative check as the problem rather than the solution.
% FOUNDING_PROBLEM_CORROBORATION: The congressional-primacy reading is corroborated by the Convention records (Madison's notes, the 'make' to 'declare' change), early practice (Washington seeking authorization for offensive operations against Native tribes, Adams for the Quasi-War), and the Federalist Papers (Federalist 69 contrasting the president's power with the British king's). The functional-accommodation reading is corroborated by the post-1945 practice of both branches — Congress authorizing major wars (Gulf War, Iraq 2002) while acquiescing in limited actions — and by scholarly consensus (e.g., the War Powers Resolution's bipartisan passage). The inherent-executive reading's corroboration comes primarily from executive branch actors themselves (OLC opinions, presidential signing statements) and a subset of unitary-executive scholars; no independent branch or external observer corroborates it as the original understanding.
narrative_ontology:disappearance_verdict(war_powers_allocation__inherent_executive_reading, world_rearranges).
narrative_ontology:founding_problem_status(war_powers_allocation__inherent_executive_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(war_powers_allocation__inherent_executive_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(war_powers_allocation__inherent_executive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(war_powers_allocation__inherent_executive_reading, 0.75, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(war_powers_allocation__inherent_executive_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(war_powers_allocation__inherent_executive_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(war_powers_allocation__inherent_executive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.75) because the reading transfers the core constitutional power — initiating offensive war — from the legislature to the executive, and this transfer has expanded over 80 years from 'emergency defense' to 'national interests' broadly construed. Suppression is moderate (0.45) because Congress retains formal powers (appropriations, War Powers Resolution) and occasionally asserts them, but these are structurally weakened by the appropriations-ratification dynamic and political question doctrine. Theater ratio rises from 0.15 to 0.40: early unilateral actions (Korea) had minimal performative consultation; later practice (post-9/11) adds extensive congressional briefings, notifications, and AUMF debates that change nothing operationally. Accessibility collapse (0.55) reflects that alternatives (formal declarations, specific AUMFs, War Powers Resolution compliance) exist legally but are treated as politically optional. Resistance (0.50) captures periodic congressional pushback (Church Committee, War Powers Resolution, Iran-Contra investigations, Yemen War Powers votes) that achieves temporary constraint but not structural reversal.
 *
 * PERSPECTIVAL GAP:
 *   From the president's seat (agenda_setter, d≈0.1), the constraint is genuine coordination: unified command enables effective defense. From Congress's seat (payer, d≈0.8), the same structure is extraction: their constitutional check is hollowed out while they bear blame for outcomes they didn't authorize. From the public_accountability seat (payer, d≈0.9), it is extraction without representation. The national_security_establishment (beneficiary/payer, d≈0.4) experiences both: operational clarity and institutional prestige, but also the burden of wars launched without political ownership. The engine computes these divergences from the structural data; the claimed type does not resolve them.
 *
 * DIRECTIONALITY LOGIC:
 *   The president and executive branch are structural beneficiaries: they collect the power to initiate force, control the legal interpretations (OLC), and set the terms of engagement. Congress and legislative_war_powers are structural victims: their textually assigned power is displaced, their institutional role reduced to retrospective funding or performative objection. Public_accountability is a victim with trapped exit: they pay the costs but have no mechanism to withhold consent. The national_security_establishment is dual-positioned: they benefit from clear command authority but pay in operational risk and strategic blowback from wars lacking democratic legitimacy. Courts are observers with analytical exit: they could intervene but structurally choose not to. Foreign governments are excluded with trapped exit: they suffer the effects with zero procedural standing.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — rapid defensive response — was real but narrow. The inherent-executive reading solves it by eliminating the legislative check entirely for a constantly expanding category of 'national interests.' This is mandatrophy: the arrangement (unilateral executive war-making) has outlived its founding justification (imminent threat response) and now serves as a permanent institutional power grab. The coordination function (decisive command in genuine emergencies) is real but has been stretched to cover routine offensive warfare. The extraction function (aggrandizing executive power, insulating war from democratic accountability) has become the dominant operative logic. The classification as tangled_rope captures this dual nature — preventing the mislabeling of pure coordination (rope) or pure extraction (snare) — because both functions are structurally present and actively maintained.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    original_understanding_vs_practice,
    'Does the historical record of the Framing and early practice support the inherent-executive reading, or does it support congressional primacy with only narrow defensive exceptions?',
    'Constitutional history scholarship: Convention records, ratification debates, early presidential practice (Washington, Adams, Jefferson), and the ''make war'' to ''declare war'' textual change. A definitive historical consensus would resolve the founding_problem_status from ''contested'' to ''live'' or ''dead''.',
    'If original understanding supports congressional primacy, the inherent-executive reading is a constructed constraint benefiting the executive (false summit candidate for any ''mountain'' claim of constitutional necessity). If original understanding is genuinely ambiguous, the functional_accommodation reading gains structural ground.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(original_understanding_vs_practice, empirical, 'Whether the Framers'' design supports inherent executive authority or congressional primacy.').

omega_variable(
    appropriations_ratification_mechanism,
    'Does congressional appropriation of funds for troops already deployed constitute meaningful ratification of the deployment, or is it a coerced choice that renders the power of the purse a ratification trap?',
    'Political science analysis of congressional voting behavior on war funding: do members treat appropriations as authorization votes? Do leadership strategies avoid standalone authorization votes? Game-theoretic modeling of the ''support the troops'' framing.',
    'If appropriations function as genuine ratification, the coordination function includes a legislative check (albeit retrospective). If they function as a ratification trap, the extraction is more severe — Congress''s only remaining lever is structurally captured.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(appropriations_ratification_mechanism, conceptual, 'Whether the appropriations power operates as a check or a trap in the inherent-executive framework.').

omega_variable(
    judicial_avoidance_as_ratification,
    'Does the judiciary''s consistent refusal to adjudicate war powers disputes (political question doctrine) constitute a structural feature of the separation of powers, or an abdication that enables executive aggrandizement?',
    'Analysis of Supreme Court war powers jurisprudence: frequency of merits rulings vs. dismissals, the evolution of political question doctrine in this domain, and whether any justice has articulated a limiting principle for executive unilateralism.',
    'If judicial avoidance is a constitutional design feature (courts lack competence), the constraint''s coordination function includes judicial deference. If it is abdication, the extraction is amplified by the removal of the third branch''s check.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(judicial_avoidance_as_ratification, conceptual, 'Whether court non-intervention is a feature or a bug of the war powers system.').

omega_variable(
    national_interests_scope_creep,
    'Is ''defense of national interests'' a stable limiting principle, or does it inevitably expand to cover any deployment the executive chooses to characterize as such?',
    'Empirical survey of OLC opinions and presidential statements from 1950-present: how has the category of deployments justified as ''defense of national interests'' expanded? Correlation with strategic doctrine documents (NSC-68, post-9/11 NSS, etc.).',
    'If the category is inherently open-ended, the constraint has no internal limiting principle — extraction is unbounded. If it has stable boundaries (e.g., imminent threat, treaty obligation, U.S. territory), the coordination function remains cabined.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(national_interests_scope_creep, empirical, 'Whether the triggering condition for inherent authority has stable boundaries or inevitable scope creep.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(war_powers_allocation__inherent_executive_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(war_powers_inherent_exec_tr_t0, war_powers_allocation__inherent_executive_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(war_powers_inherent_exec_tr_t5, war_powers_allocation__inherent_executive_reading, theater_ratio, 5, 0.2).
narrative_ontology:measurement(war_powers_inherent_exec_tr_t10, war_powers_allocation__inherent_executive_reading, theater_ratio, 10, 0.25).
narrative_ontology:measurement(war_powers_inherent_exec_tr_t15, war_powers_allocation__inherent_executive_reading, theater_ratio, 15, 0.28).
narrative_ontology:measurement(war_powers_inherent_exec_tr_t20, war_powers_allocation__inherent_executive_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement(war_powers_inherent_exec_tr_t30, war_powers_allocation__inherent_executive_reading, theater_ratio, 30, 0.33).
narrative_ontology:measurement(war_powers_inherent_exec_tr_t40, war_powers_allocation__inherent_executive_reading, theater_ratio, 40, 0.35).
narrative_ontology:measurement(war_powers_inherent_exec_tr_t50, war_powers_allocation__inherent_executive_reading, theater_ratio, 50, 0.38).
narrative_ontology:measurement(war_powers_inherent_exec_tr_t65, war_powers_allocation__inherent_executive_reading, theater_ratio, 65, 0.39).
narrative_ontology:measurement(war_powers_inherent_exec_tr_t80, war_powers_allocation__inherent_executive_reading, theater_ratio, 80, 0.4).

% Extraction over time
narrative_ontology:measurement(war_powers_inherent_exec_be_t0, war_powers_allocation__inherent_executive_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(war_powers_inherent_exec_be_t5, war_powers_allocation__inherent_executive_reading, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(war_powers_inherent_exec_be_t10, war_powers_allocation__inherent_executive_reading, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(war_powers_inherent_exec_be_t15, war_powers_allocation__inherent_executive_reading, base_extractiveness, 15, 0.55).
narrative_ontology:measurement(war_powers_inherent_exec_be_t20, war_powers_allocation__inherent_executive_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(war_powers_inherent_exec_be_t30, war_powers_allocation__inherent_executive_reading, base_extractiveness, 30, 0.65).
narrative_ontology:measurement(war_powers_inherent_exec_be_t40, war_powers_allocation__inherent_executive_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement(war_powers_inherent_exec_be_t50, war_powers_allocation__inherent_executive_reading, base_extractiveness, 50, 0.72).
narrative_ontology:measurement(war_powers_inherent_exec_be_t65, war_powers_allocation__inherent_executive_reading, base_extractiveness, 65, 0.74).
narrative_ontology:measurement(war_powers_inherent_exec_be_t80, war_powers_allocation__inherent_executive_reading, base_extractiveness, 80, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(war_powers_inherent_exec_su_t0, war_powers_allocation__inherent_executive_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(war_powers_inherent_exec_su_t5, war_powers_allocation__inherent_executive_reading, suppression_requirement, 5, 0.25).
narrative_ontology:measurement(war_powers_inherent_exec_su_t10, war_powers_allocation__inherent_executive_reading, suppression_requirement, 10, 0.3).
narrative_ontology:measurement(war_powers_inherent_exec_su_t15, war_powers_allocation__inherent_executive_reading, suppression_requirement, 15, 0.35).
narrative_ontology:measurement(war_powers_inherent_exec_su_t20, war_powers_allocation__inherent_executive_reading, suppression_requirement, 20, 0.4).
narrative_ontology:measurement(war_powers_inherent_exec_su_t30, war_powers_allocation__inherent_executive_reading, suppression_requirement, 30, 0.42).
narrative_ontology:measurement(war_powers_inherent_exec_su_t40, war_powers_allocation__inherent_executive_reading, suppression_requirement, 40, 0.43).
narrative_ontology:measurement(war_powers_inherent_exec_su_t50, war_powers_allocation__inherent_executive_reading, suppression_requirement, 50, 0.44).
narrative_ontology:measurement(war_powers_inherent_exec_su_t65, war_powers_allocation__inherent_executive_reading, suppression_requirement, 65, 0.45).
narrative_ontology:measurement(war_powers_inherent_exec_su_t80, war_powers_allocation__inherent_executive_reading, suppression_requirement, 80, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(war_powers_allocation__inherent_executive_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(war_powers_allocation__inherent_executive_reading, 0.12).
narrative_ontology:affects_constraint(war_powers_allocation__inherent_executive_reading, war_powers_allocation__congressional_primacy_reading).
narrative_ontology:affects_constraint(war_powers_allocation__inherent_executive_reading, war_powers_allocation__functional_accommodation_reading).
narrative_ontology:affects_constraint(war_powers_allocation__inherent_executive_reading, aumf_2001_perpetual_authorization).
narrative_ontology:affects_constraint(war_powers_allocation__inherent_executive_reading, war_powers_resolution_effectiveness).
narrative_ontology:affects_constraint(war_powers_allocation__inherent_executive_reading, olc_opinion_authority).

% DUAL FORMULATION NOTE:
% This story is one of three in the war_powers_allocation constraint family. The congressional_primacy_reading treats the constitutional text as assigning offensive war initiation exclusively to Congress (extractiveness near zero from congressional seat, high from executive seat). The functional_accommodation_reading treats the allocation as context-dependent (moderate extractiveness, varying by operational tempo). This inherent_executive_reading treats the Commander-in-Chief Clause as granting plenary initiation authority (high extractiveness from congressional seat, low from executive seat). The three readings share the same constitutional kernel but instantiate different constraints with different ε values, beneficiary/victim structures, and effective extractions per seat.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(war_powers_allocation__inherent_executive_reading, institutional, 0.15).
constraint_indexing:directionality_override(war_powers_allocation__inherent_executive_reading, organized, 0.35).
constraint_indexing:directionality_override(war_powers_allocation__inherent_executive_reading, powerless, 0.9).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
