% ============================================================================
% CONSTRAINT STORY: war_powers_allocation__congressional_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_war_powers_allocation__congressional_primacy_reading, []).

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
 *   constraint_id: war_powers_allocation__congressional_primacy_reading
 *   human_readable: Congressional War Powers Authorization Requirement
 *   domain: constitutional/political
 *
 * SUMMARY:
 *   This constraint story instantiates the congressional-primacy reading of
 *   the war-powers allocation kernel: the interpretation that the
 *   Constitution mandates explicit legislative authorization before the
 *   executive can deploy military force beyond immediate defense. This is one
 *   reading of a contested constitutional claim about who holds war-making
 *   power. The sibling readings—functional accommodation (context-dependent
 *   thresholds) and inherent executive authority (unilateral presidential
 *   power)—are separate constraints, each with their own ε values and
 *   stakeholder structures. This story focuses on the congressional-primacy
 *   claim: that legislative approval is a constitutional necessity, not
 *   merely prudent practice. The referent constraint is the standing
 *   arrangement where the president acts unilaterally while claiming inherent
 *   authority; this reading assesses that arrangement as extractive (the
 *   executive extracts war-decision power from congress), suppressed
 *   (inherent-authority claims suppress congressional assertion), and
 *   requiring active enforcement (courts must validate congressional
 *   authorization requirements against executive assertions of inherent
 *   power).
 *
 * KEY AGENTS:
 *   - legislative_branch: holds war-declaration power; benefits from the constraint by maintaining gatekeeping authority
 *   - executive_branch: subject to the authorization requirement; bears the cost of seeking legislative consent
 *   - courts: enforce the constraint by adjudicating war-power challenges and interpreting scope of immediate defense
 *   - congress_members: individual beneficiaries of institutional war-power authority
 *   - military_commanders: constrained to operate under authorized parameters, not unilateral executive will
 *   - general_public: benefits from legislative deliberation, bears war costs collectively
 *   - inherent_executive_advocates: excluded from authorization process; would argue for a reading dissolving the constraint
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(war_powers_allocation__congressional_primacy_reading, 0.68).
domain_priors:suppression_score(war_powers_allocation__congressional_primacy_reading, 0.72).
domain_priors:theater_ratio(war_powers_allocation__congressional_primacy_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(war_powers_allocation__congressional_primacy_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(war_powers_allocation__congressional_primacy_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(war_powers_allocation__congressional_primacy_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(war_powers_allocation__congressional_primacy_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(war_powers_allocation__congressional_primacy_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(war_powers_allocation__congressional_primacy_reading, tangled_rope).
narrative_ontology:human_readable(war_powers_allocation__congressional_primacy_reading, "Congressional War Powers Authorization Requirement").
narrative_ontology:topic_domain(war_powers_allocation__congressional_primacy_reading, "constitutional/political").

domain_priors:requires_active_enforcement(war_powers_allocation__congressional_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(war_powers_allocation__congressional_primacy_reading, 'b37b5367-0de4-4b8b-9c53-a9280260158a').
narrative_ontology:cs_kernel_codification('b37b5367-0de4-4b8b-9c53-a9280260158a', formalized).
narrative_ontology:cs_authority_grounding('b37b5367-0de4-4b8b-9c53-a9280260158a', lineage).
narrative_ontology:cs_interpretation_layer_present('b37b5367-0de4-4b8b-9c53-a9280260158a').
narrative_ontology:cs_reading_relation('b37b5367-0de4-4b8b-9c53-a9280260158a', war_powers_allocation__inherent_executive_reading, coexists_with).
narrative_ontology:cs_reading_relation('b37b5367-0de4-4b8b-9c53-a9280260158a', war_powers_allocation__functional_accommodation_reading, influences).
narrative_ontology:cs_axiom('b37b5367-0de4-4b8b-9c53-a9280260158a', foundational, legislative_war_declaration_primacy).
narrative_ontology:cs_axiom_status(legislative_war_declaration_primacy, holdable).
narrative_ontology:cs_axiom_grounding('b37b5367-0de4-4b8b-9c53-a9280260158a', legislative_war_declaration_primacy, deontological).
narrative_ontology:cs_axiom('b37b5367-0de4-4b8b-9c53-a9280260158a', secondary, emergency_immediate_defense_exception).
narrative_ontology:cs_axiom_status(emergency_immediate_defense_exception, holdable).
narrative_ontology:cs_axiom_grounding('b37b5367-0de4-4b8b-9c53-a9280260158a', emergency_immediate_defense_exception, instrumental).
narrative_ontology:cs_reference_frame('b37b5367-0de4-4b8b-9c53-a9280260158a', constitutional_legislative_supremacy_in_war).
narrative_ontology:cs_drift_state('b37b5367-0de4-4b8b-9c53-a9280260158a', contemporary_executive_assertion_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b37b5367-0de4-4b8b-9c53-a9280260158a', '').
narrative_ontology:cs_kernel_id(war_powers_allocation__congressional_primacy_reading, war_powers_allocation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(war_powers_allocation__congressional_primacy_reading, legislative_branch).
narrative_ontology:constraint_beneficiary(war_powers_allocation__congressional_primacy_reading, constitutional_check_and_balance_doctrine).
narrative_ontology:constraint_victim(war_powers_allocation__congressional_primacy_reading, executive_branch).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(war_powers_allocation__congressional_primacy_reading, congress_members).
narrative_ontology:constraint_beneficiary(war_powers_allocation__congressional_primacy_reading, general_public).
narrative_ontology:constraint_victim(war_powers_allocation__congressional_primacy_reading, military_commanders).
narrative_ontology:constraint_victim(war_powers_allocation__congressional_primacy_reading, general_public).
narrative_ontology:constraint_victim(war_powers_allocation__congressional_primacy_reading, national_security_establishment).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds the formal constitutional authority to declare war and appropriate funds for military operations. Under this reading, the legislature benefits from the constraint by maintaining its gatekeeping role over decisions to deploy force. When the executive acts unilaterally, the legislature is bypassed, and the constraint's enforcement weakens.
narrative_ontology:constraint_stakeholder(war_powers_allocation__congressional_primacy_reading, legislative_branch, beneficiary,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(war_powers_allocation__congressional_primacy_reading, legislative_branch, agenda_setter).

% Constrained by the requirement to seek congressional authorization before deploying force beyond immediate defense. The executive bears the political and operational cost of requesting authorization, justifying military action to a deliberative body, and accepting congressional conditions or refusal. Unilateral action violates the constraint and triggers legal/political challenge.
narrative_ontology:constraint_stakeholder(war_powers_allocation__congressional_primacy_reading, executive_branch, payer,
    institutional, biographical, constrained, national).

% Operate under orders that must be justified through the authorization constraint. They cannot act on executive will alone; the requirement for congressional authorization constrains operational timing and forces transparency about military objectives. Compliance requires public justification and accepts legislative conditions.
narrative_ontology:constraint_stakeholder(war_powers_allocation__congressional_primacy_reading, military_commanders, payer,
    organized, biographical, constrained, national).

% Individual legislators benefit from the institutional power the constraint confers on the body as a whole. They participate in war-authorization decisions, shape the terms of military engagement, and share political accountability for decisions to go to war. The constraint makes their consent necessary.
narrative_ontology:constraint_stakeholder(war_powers_allocation__congressional_primacy_reading, congress_members, beneficiary,
    organized, biographical, analytical, national).

% Enforce the constraint by adjudicating constitutional challenges to unilateral military action and interpreting the scope of 'immediate defense' versus operations requiring authorization. Judicial review of war-power claims is the enforcement mechanism; courts validate or invalidate executive action against this reading.
narrative_ontology:constraint_stakeholder(war_powers_allocation__congressional_primacy_reading, courts, agenda_setter,
    institutional, generational, analytical, national).

% Benefits from legislative deliberation over war decisions (participation via elected representatives, debate, conditions on engagement). Simultaneously bears the costs of war: conscription, taxation, casualties, social disruption. The constraint makes legislative consent a requirement before these costs are imposed, but provides no direct voice in the decision.
narrative_ontology:constraint_stakeholder(war_powers_allocation__congressional_primacy_reading, general_public, beneficiary,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(war_powers_allocation__congressional_primacy_reading, general_public, payer).

% Intelligence agencies, defense department, joint chiefs—operate under the constraint that their operational plans must be justified to congress before deployment beyond immediate defense. They experience the requirement as a delay, a political filter, and a source of leaked strategic information. Their preferences often align with the executive.
narrative_ontology:constraint_stakeholder(war_powers_allocation__congressional_primacy_reading, national_security_establishment, payer,
    organized, biographical, constrained, national).

% Constitutional scholars, officials, and advisors who argue the president has inherent commander-in-chief authority to act without prior authorization. They are structurally excluded from the authorization process itself—their arguments inform the executive's legal position but do not participate in the legislative deliberation the constraint mandates. They would argue for a reading that dissolves the constraint.
narrative_ontology:constraint_stakeholder(war_powers_allocation__congressional_primacy_reading, inherent_executive_authority_advocates, excluded,
    powerful, biographical, trapped, national).

% External actors who would benefit from rapid, decisive unilateral U.S. military response and who are harmed by delay for congressional authorization. They are excluded from the authorization process, though their interests affect how the constraint plays out. Geopolitical pressure to act quickly runs counter to the constraint's deliberative demand.
narrative_ontology:constraint_stakeholder(war_powers_allocation__congressional_primacy_reading, international_allies_and_adversaries, excluded,
    powerful, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(war_powers_allocation__congressional_primacy_reading, legislative_branch).
narrative_ontology:fixing_cost_class(war_powers_allocation__congressional_primacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates military decision-making between executive (operational commander) and legislative (war-power holder) branches by requiring shared consent before deploying force beyond immediate defense. Solves the problem of how a democracy distinguishes between delegated authority (commander-in-chief power in emergencies) and reserved authority (power to declare war), and how to constrain unilateral action that could circumvent popular sovereignty.
% TRANSFER_FUNCTION: Transfers war-decision authority from unilateral executive prerogative to require congressional concurrence. The executive loses the ability to initiate force unilaterally; congress gains the gatekeeping power. Subordinate: resources (appropriations must come from congress) and legitimacy (public justification to elected representatives becomes a prerequisite, not optional).
% ABSENT_VOICES: Inherent-executive-authority advocates (academics and officials arguing for presidential war power) are excluded from the authorization process itself—their scholarly arguments inform the executive's legal position but do not participate in the congressional deliberation the constraint mandates. International actors and future generations whose interests are affected by war decisions have no seat at the authorization table. Military commanders must justify operations to civilians, but their operational expertise does not override the authorization requirement.
% DISAPPEARANCE_RATIONALE: If the constraint vanished—if the requirement for congressional authorization were removed and the president could deploy force unilaterally at will—the constitutional separation of powers would collapse in the war domain. Congress would lose its reserved power; the executive would acquire a unilateral capacity to commit the nation to war. The allocation of military decision authority would reorganize entirely around presidential will, not shared constitutional authority.
% FOUNDING_PROBLEM: The Framers created a government structure in which the executive holds power to command armed forces but not to decide to wage war; the legislature holds power to declare war but not to command forces. This division was intended to prevent both executive tyranny (unilateral power to make war) and legislative paralysis (inability to respond to immediate threats). The founding problem was: how to allocate authority to decide on war such that both the operational commander and the people's representatives have a voice, and such that defensive response does not require legislative delay while deliberation occurs.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional scholars across the political spectrum (Youngstown Steel framework, Fisher, Ackerman, Sunstein) attest that the structural tension between executive urgency and legislative deliberation remains live and unresolved. Historical evidence from the Constitutional Convention (Madison's notes, Federalist Papers) and legislative history of the War Powers Resolution (1973) corroborates that the founders and 20th-century congress agreed the problem persists. Presidents of both parties have asserted inherent authority, and Congress has repeatedly reasserted its authorization role, demonstrating the problem is actively contested, not settled.
narrative_ontology:disappearance_verdict(war_powers_allocation__congressional_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(war_powers_allocation__congressional_primacy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(war_powers_allocation__congressional_primacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(war_powers_allocation__congressional_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(war_powers_allocation__congressional_primacy_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(war_powers_allocation__congressional_primacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(war_powers_allocation__congressional_primacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(war_powers_allocation__congressional_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness measures how much the constraint extracts from its target (executive) under the congressional-primacy reading: 0.68 reflects that the executive loses unilateral decision authority and must justify military action to a deliberative body with power to deny or condition authorization. The executive cannot act on pure will; it must build political consensus. Suppression (0.72) is higher because enforcing the constraint requires actively suppressing inherent-authority claims—the executive does not willingly accept the limitation; courts must push back against each invocation of executive prerogative. The constraint persists through judicial enforcement and legislative reassertion, not through voluntary compliance. Theater ratio (0.41) is moderate: some theatrical performance exists (Congress performs oversight; the executive performs compliance while exploring gray areas like humanitarian intervention and imminent-threat justifications), but the core coordination function (legislative deliberation before war) is real and sometimes binds. The measurement series shows extractiveness plateauing by midpoint, suppression stabilizing, and theater ratio reaching a steady state—suggesting the constraint has found an equilibrium where the executive knows it will be challenged and Congress knows it must assert authority, but neither can fully eliminate the other's ambition to act unilaterally (or broadly interpret immediate defense).
 *
 * PERSPECTIVAL GAP:
 *   The executive seat and the legislative seat should compute very differently on this constraint. From the legislative perspective, the constraint is protective coordination—it preserves the people's representatives' voice in war decisions and prevents executive tyranny. From the executive perspective, the constraint is a limitation on operational authority and a vulnerability to legislative obstruction during time-critical threats. The engine should compute legislative seats as beneficiaries (low d, low/negative χ) and executive seats as targets (high d, high χ) because the constraint's structure imposes asymmetric costs on executive action while preserving legislative gatekeeping. Courts occupy an enforcement seat: they benefit from the clear constitutional assignment (war power to congress, commander-in-chief to executive) but must bear the political cost of adjudicating disputes. The general public benefits from democratic deliberation but also bears diffuse costs—inability to respond instantly to threats, public debate exposing strategy, legislative obstruction of popular military objectives. This reading produces multiple simultaneously-held positions, which is the structural marker of a tangled-rope constraint: genuine coordination (executive + legislature must align, not just one choosing) and asymmetric extraction (executive loses unilateral authority, legislature gains gatekeeping).
 *
 * DIRECTIONALITY LOGIC:
 *   Legislative branch: d approaches 0.0 (full beneficiary). It gains gatekeeping authority; the constraint protects its constitutional prerogative from executive encroachment. Exit options are analytical (the branch can always reassert its authority through legislation or appropriations). Executive branch: d approaches 1.0 (full target). It loses unilateral war power and must seek legislative consent, which imposes delay, political friction, and the risk of denial. Exit options are constrained (the president cannot simply ignore Congress; the courts have repeatedly held that unilateral action without authorization, absent imminent threat, violates Article I). Military commanders: d moderate-to-high (they are targets of the constraint—they must operate under the authorization framework and cannot act on executive will alone). Courts: d near 0.5 (symmetric or slightly beneficiary—they benefit from clear constitutional role, but bear political cost and must actively enforce). General public: d moderate-high (they bear diffuse war costs and have only indirect voice through legislative representatives; they benefit from deliberation but are trapped in the outcome). Inherent-executive advocates: excluded from the authorization process itself, so directionality is not computed (they are not stakeholders in this reading's enforcement structure—they are intellectual critics, not parties to the constraint).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (How to allocate war authority such that both commander and people's representatives have voice, and defensive response does not require legislative delay) remains LIVE and CONTESTED, not resolved. This prevents mandatrophy classification. The constraint is not a zombie executing a dead mandate. Congressional authorization is still asserted by Congress, still challenged by the executive, still adjudicated by courts. The functional accommodation reading (sibling) proposes that immediate threat permits unilateral action, while prolonged campaigns require authorization—a context-dependent partition of the founding problem. The inherent executive reading claims the founding problem is solved by vesting commander-in-chief authority in the president, with Congress limited to formal declaration for full-scale war. This congressional-primacy reading holds the founding problem is solved by requiring explicit authorization before force beyond immediate defense, with immediate defense narrowly construed. The constraint persists because none of these readings has achieved canonical status; each administration tests the boundary, and courts oscillate between deference and enforcement. The constraint is classifiable as tangled_rope precisely because it must be actively enforced (courts keep pushing back) and because the coordination function (legislative deliberation) and extraction (executive loss of unilateral authority) coexist in the same structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    immediate_defense_scope_ambiguity,
    'What constitutes ''immediate defense'' sufficiently clear that the executive can act unilaterally? Is imminent attack the only trigger, or does it extend to preemptive strikes on confirmed threats, humanitarian crises requiring rapid response, or counter-terrorism operations?',
    'Judicial interpretation of war-power cases (Youngstown Steel framework applied to specific threat scenarios) or legislative definition via statute narrowing executive claims to immediate defense.',
    'If immediate defense is narrowly construed, the executive loses a major escape valve and the constraint is much more binding. If broadly construed (preemption, counter-terrorism, humanitarian response), the constraint is substantially eroded—the executive retains unilateral authority for most operations it deems urgent. The scope of immediate defense determines whether this reading is enforceable in practice.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(immediate_defense_scope_ambiguity, conceptual, 'The boundary between emergency unilateral action and deliberated authorized force').

omega_variable(
    congressional_capacity_enforcement,
    'Can Congress actually enforce the authorization requirement, or do structural incentives (political pressure from the executive, classified information advantage, rally-around-the-flag effects) make legislative assertion too costly?',
    'Historical analysis of congressional responses to unilateral executive action (War Powers Resolution enforcement record, appropriations-withholding episodes, authorization-denial cases) and political economy of legislative war-power assertion.',
    'If Congress cannot enforce (political incentives favor deference), the constraint becomes a paper rule—the executive acts, Congress accommodates after the fact. The suppression metric reflects this: the constraint requires active enforcement because legislative assertion is politically difficult. If Congress could overcome the incentive problem, enforcement cost would drop and the constraint would approach a rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(congressional_capacity_enforcement, empirical, 'Whether legislative actors have sufficient political capacity to enforce the authorization requirement against executive assertion').

omega_variable(
    reading_contest_kernel_identity,
    'Is the war-powers-allocation kernel itself contested (different parties genuinely hold incompatible readings), or is one reading the ''correct'' constitutional interpretation with others merely expressing political preference?',
    'Constitutional theory (originalist, living constitutionalist, structural approaches evaluate the text and history) and institutional practice (which reading do courts enforce? which do successive administrations claim?). No purely empirical resolution; this is a constitutional law question that legal scholarship addresses without settling.',
    'If one reading is constitutionally correct, this reading is either validated or invalidated as law, not merely one position among coequal others. If the kernel is genuinely contested and indeterminate, all three readings (congressional-primacy, functional-accommodation, inherent-executive) remain live and coexisting, and the constraint persists through repeated assertion and challenge. The current story assumes the reading is one live interpretation of an open kernel; if constitutional law were to canonize a different reading, this constraint''s classification would shift.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_contest_kernel_identity, conceptual, 'Whether the kernel admits multiple defensible readings or whether constitutional law has a single right answer').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the suppression measured in this constraint structural (legal/institutional barriers to executive unilateral action) or internalized (executives believe they should seek authorization, have internalized the constitutional duty)?',
    'Behavioral analysis: when presidents act unilaterally, do they do so reluctantly and defensively (claiming narrow emergency exception, framing action as temporary), or confidently (asserting inherent authority as core executive power)? If reluctant and defensive, suppression is partly internalized—the president believes the constraint is legitimate. If assertive, suppression is mostly structural—enforced by courts and Congress, not by executive self-limitation.',
    'If suppression is internalized, the constraint would persist even if court enforcement weakened—presidents would self-limit out of constitutional belief. If mostly structural, a change in judicial composition or congressional composition could rapidly erode enforcement. The measured suppression (0.72) likely reflects mixed structural and internalized components, but the balance is unclear.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether the constraint''s suppressive force is structural enforcement or internalized executive norms').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(war_powers_allocation__congressional_primacy_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(war__tr_t0, war_powers_allocation__congressional_primacy_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(war__tr_t7, war_powers_allocation__congressional_primacy_reading, theater_ratio, 7, 0.31).
narrative_ontology:measurement(war__tr_t14, war_powers_allocation__congressional_primacy_reading, theater_ratio, 14, 0.34).
narrative_ontology:measurement(war__tr_t21, war_powers_allocation__congressional_primacy_reading, theater_ratio, 21, 0.37).
narrative_ontology:measurement(war__tr_t28, war_powers_allocation__congressional_primacy_reading, theater_ratio, 28, 0.39).
narrative_ontology:measurement(war__tr_t35, war_powers_allocation__congressional_primacy_reading, theater_ratio, 35, 0.4).
narrative_ontology:measurement(war__tr_t42, war_powers_allocation__congressional_primacy_reading, theater_ratio, 42, 0.41).
narrative_ontology:measurement(war__tr_t50, war_powers_allocation__congressional_primacy_reading, theater_ratio, 50, 0.41).

% Extraction over time
narrative_ontology:measurement(war__be_t0, war_powers_allocation__congressional_primacy_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(war__be_t7, war_powers_allocation__congressional_primacy_reading, base_extractiveness, 7, 0.59).
narrative_ontology:measurement(war__be_t14, war_powers_allocation__congressional_primacy_reading, base_extractiveness, 14, 0.63).
narrative_ontology:measurement(war__be_t21, war_powers_allocation__congressional_primacy_reading, base_extractiveness, 21, 0.65).
narrative_ontology:measurement(war__be_t28, war_powers_allocation__congressional_primacy_reading, base_extractiveness, 28, 0.67).
narrative_ontology:measurement(war__be_t35, war_powers_allocation__congressional_primacy_reading, base_extractiveness, 35, 0.68).
narrative_ontology:measurement(war__be_t42, war_powers_allocation__congressional_primacy_reading, base_extractiveness, 42, 0.68).
narrative_ontology:measurement(war__be_t50, war_powers_allocation__congressional_primacy_reading, base_extractiveness, 50, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(war__su_t0, war_powers_allocation__congressional_primacy_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(war__su_t7, war_powers_allocation__congressional_primacy_reading, suppression_requirement, 7, 0.62).
narrative_ontology:measurement(war__su_t14, war_powers_allocation__congressional_primacy_reading, suppression_requirement, 14, 0.66).
narrative_ontology:measurement(war__su_t21, war_powers_allocation__congressional_primacy_reading, suppression_requirement, 21, 0.69).
narrative_ontology:measurement(war__su_t28, war_powers_allocation__congressional_primacy_reading, suppression_requirement, 28, 0.71).
narrative_ontology:measurement(war__su_t35, war_powers_allocation__congressional_primacy_reading, suppression_requirement, 35, 0.72).
narrative_ontology:measurement(war__su_t42, war_powers_allocation__congressional_primacy_reading, suppression_requirement, 42, 0.72).
narrative_ontology:measurement(war__su_t50, war_powers_allocation__congressional_primacy_reading, suppression_requirement, 50, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(war_powers_allocation__congressional_primacy_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(war_powers_allocation__congressional_primacy_reading, 0.12).
narrative_ontology:affects_constraint(war_powers_allocation__congressional_primacy_reading, war_powers_allocation__inherent_executive_reading).
narrative_ontology:affects_constraint(war_powers_allocation__congressional_primacy_reading, war_powers_allocation__functional_accommodation_reading).

% DUAL FORMULATION NOTE:
% The war_powers_allocation kernel is contested across three structurally distinct readings. This story (congressional_primacy_reading) asserts legislative authorization is a constitutional necessity; it coexists with the inherent_executive_reading (president has unilateral commander-in-chief power) and the functional_accommodation_reading (context-dependent thresholds). The three readings share a referent (the existing arrangement of executive war power), but each reading authors a different ε value from the standpoint of its own epistemic framework. Congressional-primacy reads unilateral action as extraction from legislative authority; inherent-executive reads authorization requirements as constraint on legitimate executive power; functional-accommodation treats both authorization and emergency action as legitimate in context. All three are live positions in constitutional law, held by different institutional actors. Each sister reading is a separate constraint with its own stakeholder structure and classification. Network links enable contamination analysis: if courts validate one reading, pressure propagates through the linked constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(war_powers_allocation__congressional_primacy_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
