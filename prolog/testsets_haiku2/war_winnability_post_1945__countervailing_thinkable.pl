% ============================================================================
% CONSTRAINT STORY: war_winnability_post_1945__countervailing_thinkable
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_war_winnability_post_1945__countervailing_thinkable, []).

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
 *   constraint_id: war_winnability_post_1945__countervailing_thinkable
 *   human_readable: Countervailing Strategy and Limited Victory Thinkability Post-1945
 *   domain: strategic_studies/nuclear_deterrence/international_relations
 *
 * SUMMARY:
 *   In the immediate post-1945 nuclear era, American strategic doctrine
 *   treated atomic weapons as extreme deterrents whose use would be
 *   apocalyptic and therefore never rational. By the 1960s, after Soviet
 *   nuclear parity arrived, strategists began reconstructing doctrine around
 *   the premise that nuclear war, while catastrophic, could remain winnable
 *   through careful targeting of military (not civilian) assets. This
 *   'countervailing strategy' reading holds that limited nuclear war remains
 *   in the reachable strategic space, achievable through counterforce strikes
 *   on weapons systems rather than cities. The constraint itself is the
 *   institutional and doctrinal persistence of this winnability frame: it
 *   enables war planning to continue, justifies force modernization, and
 *   undermines arms control by redefining nuclear weapons as tools of
 *   statecraft rather than absolute deterrents. The shell of public discourse
 *   shifted toward 'stability' and 'mutual assured destruction' language, but
 *   the interior of military planning never abandoned war-fighting as a live
 *   category. This is the countervailing-thinkable reading: winnability
 *   persists as operational and doctrinal fact, constrained but not
 *   eliminated by the scale of nuclear arsenals.
 *
 * KEY AGENTS:
 *   - Nuclear weapons planners (STRATCOM, military strategy institutions): maintain targeting doctrines modeling limited nuclear war as strategically calculable
 *   - Military-industrial complex (defense contractors, research institutions): benefit from doctrine that sustains procurement and force modernization
 *   - Arms control regimes (NPT, START negotiators, disarmament advocates): undercut by winnability framing, which treats nuclear weapons as persistent tools rather than abolished categories
 *   - Non-nuclear states: constrained by the doctrine they do not author
 *   - Global public: bears existential risk under doctrines treating their destruction as calculable cost
 *   - Abolition movements: excluded from mainline strategic planning
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(war_winnability_post_1945__countervailing_thinkable, 0.68).
domain_priors:suppression_score(war_winnability_post_1945__countervailing_thinkable, 0.72).
domain_priors:theater_ratio(war_winnability_post_1945__countervailing_thinkable, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(war_winnability_post_1945__countervailing_thinkable, extractiveness, 0.68).
narrative_ontology:constraint_metric(war_winnability_post_1945__countervailing_thinkable, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(war_winnability_post_1945__countervailing_thinkable, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(war_winnability_post_1945__countervailing_thinkable, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(war_winnability_post_1945__countervailing_thinkable, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(war_winnability_post_1945__countervailing_thinkable, tangled_rope).
narrative_ontology:human_readable(war_winnability_post_1945__countervailing_thinkable, "Countervailing Strategy and Limited Victory Thinkability Post-1945").
narrative_ontology:topic_domain(war_winnability_post_1945__countervailing_thinkable, "strategic_studies/nuclear_deterrence/international_relations").

domain_priors:requires_active_enforcement(war_winnability_post_1945__countervailing_thinkable).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(war_winnability_post_1945__countervailing_thinkable, '0f44f002-ac76-488f-9eaa-b2858cc2b9cc').
narrative_ontology:cs_kernel_codification('0f44f002-ac76-488f-9eaa-b2858cc2b9cc', formalized).
narrative_ontology:cs_authority_grounding('0f44f002-ac76-488f-9eaa-b2858cc2b9cc', extraction).
narrative_ontology:cs_interpretation_layer_present('0f44f002-ac76-488f-9eaa-b2858cc2b9cc').
narrative_ontology:cs_reading_relation('0f44f002-ac76-488f-9eaa-b2858cc2b9cc', war_winnability_post_1945__deterrence_unthinkable, coexists_with).
narrative_ontology:cs_reading_relation('0f44f002-ac76-488f-9eaa-b2858cc2b9cc', war_winnability_post_1945__rhetorical_contraction, influences).
narrative_ontology:cs_axiom('0f44f002-ac76-488f-9eaa-b2858cc2b9cc', foundational, limited_nuclear_war_strategically_winnable).
narrative_ontology:cs_axiom_status(limited_nuclear_war_strategically_winnable, holdable).
narrative_ontology:cs_axiom_grounding('0f44f002-ac76-488f-9eaa-b2858cc2b9cc', limited_nuclear_war_strategically_winnable, instrumental).
narrative_ontology:cs_axiom('0f44f002-ac76-488f-9eaa-b2858cc2b9cc', secondary, counterforce_targeting_stabilizing).
narrative_ontology:cs_axiom_status(counterforce_targeting_stabilizing, holdable).
narrative_ontology:cs_axiom_grounding('0f44f002-ac76-488f-9eaa-b2858cc2b9cc', counterforce_targeting_stabilizing, empirically_contingent).
narrative_ontology:cs_reference_frame('0f44f002-ac76-488f-9eaa-b2858cc2b9cc', post_parity_credibility_maintenance).
narrative_ontology:cs_drift_state('0f44f002-ac76-488f-9eaa-b2858cc2b9cc', contemporary_arms_control_collapse, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('0f44f002-ac76-488f-9eaa-b2858cc2b9cc', '').
narrative_ontology:cs_kernel_id(war_winnability_post_1945__countervailing_thinkable, war_winnability_post_1945).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(war_winnability_post_1945__countervailing_thinkable, military_industrial_complex).
narrative_ontology:constraint_beneficiary(war_winnability_post_1945__countervailing_thinkable, nuclear_weapons_planners).
narrative_ontology:constraint_victim(war_winnability_post_1945__countervailing_thinkable, arms_control_regimes).
narrative_ontology:constraint_victim(war_winnability_post_1945__countervailing_thinkable, non_nuclear_states).
narrative_ontology:constraint_victim(war_winnability_post_1945__countervailing_thinkable, global_public).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(war_winnability_post_1945__countervailing_thinkable, deterrence_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Military strategists and war planners in nuclear-armed states maintain and refine targeting plans and force postures on the assumption that limited nuclear war is winnable through counterforce strikes. They justify this as essential deterrence credibility: if nuclear war is unthinkable, the threat loses force. They author military doctrine, oversee weapons development, and conduct strategic exercises modeling victory scenarios. Their professional identity and organizational mission depend on treating war-winning as a live operational category.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__countervailing_thinkable, nuclear_weapons_planners, agenda_setter,
    institutional, generational, identity_locked, global).

% Defense contractors, research institutions, and their political sponsors benefit from sustained demand for advanced nuclear systems justified by counterforce targeting doctrine. Maintaining winnability as a strategic concept sustains procurement cycles, weapons modernization programs, and the institutional legitimacy of nuclear forces. They actively shape doctrine and policy through lobbying, research funding, and the revolving door between government and industry.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__countervailing_thinkable, military_industrial_complex, beneficiary,
    powerful, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(war_winnability_post_1945__countervailing_thinkable, military_industrial_complex, agenda_setter).

% Non-Proliferation Treaty signatories, START negotiators, and disarmament advocates work to reduce nuclear arsenals and restrict targeting doctrines. The countervailing-thinkable reading directly undermines their legitimacy: if limited nuclear war is plannable and winnable, then arms reduction is merely managing a 'normal' wartime instrument rather than eliminating an exceptional threat. Verification becomes harder; negotiating partners cite war-fighting requirements as reasons to retain warheads and modernize systems. Their institutional mission — moving toward a nuclear-free world — is constrained by the persistence of winnability framing.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__countervailing_thinkable, arms_control_regimes, payer,
    institutional, biographical, constrained, global).

% States without nuclear weapons remain under potential nuclear threat, while the countervailing strategy normalizes nuclear war-fighting as rational strategic calculation. They pay through reduced security assurances, extended deterrence uncertainty, and the political cost of living under the shadow of great-power nuclear conflict scenarios. Their ability to influence the constraint is minimal; they must accept the strategic premise set by nuclear powers.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__countervailing_thinkable, non_nuclear_states, payer,
    moderate, generational, constrained, global).

% Civilian populations in nuclear-armed and allied states bear the existential risk that 'limited' nuclear war planning proves operationally insufficient and escalates to civilization-ending exchanges. The winnability premise provides rhetorical cover for doctrines that model their destruction as a manageable cost of strategic victory. They have no seat at the policy table, no exit, and maximal stakes.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__countervailing_thinkable, global_public, payer,
    powerless, immediate, trapped, universal).

% Strategic thinkers and policymakers who defend nuclear deterrence as a lesser evil argue that only credible war-fighting plans maintain the threat-power needed to keep peace. For them, winnability-as-thinkable is not extraction but the structural requirement of deterrence. They benefit from the doctrine's legitimacy, which sustains both their policy influence and the intellectual coherence of their strategic worldview.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__countervailing_thinkable, deterrence_advocates, beneficiary,
    institutional, generational, constrained, global).

% Anti-nuclear and disarmament movements argue for zero-weapons systems and categorical rejection of nuclear war-fighting. They are structurally excluded from mainline strategic planning: their voice is treated as moral sentiment rather than strategic analysis. The countervailing-thinkable reading persists partly because the planning institutions that author it do not give abolition advocates standing in the doctrine-setting process. Their argument — that winnability is incoherent and should be ruled out by normative principle — is not engaged as a strategic premise but dismissed as ideological.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__countervailing_thinkable, abolition_movements, excluded,
    organized, generational, constrained, global).

% Independent strategic analysts, academic researchers, and historians document the constraint's structure and measure its drift over time. They observe how winnability-framing shifted after 1960s escalation fears, how counterforce doctrine sustained itself through arms control negotiations, and how the constraint persists despite repeated efforts to delegitimize it.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__countervailing_thinkable, analytical_observer, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(war_winnability_post_1945__countervailing_thinkable, military_industrial_complex).
narrative_ontology:fixing_cost_class(war_winnability_post_1945__countervailing_thinkable, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains strategic communication of credible nuclear deterrence among great powers: by sustaining the belief that nuclear war outcomes are strategically calculable and limited victory possible, each nuclear power preserves the threat-power of its arsenal against peer competitors. Paradoxically, the coordination problem the constraint solves is 'how to make deterrence credible' — which requires that war, if it occurs, be treated as winnable.
% TRANSFER_FUNCTION: Transfers political legitimacy from arms control (reduction, abolition, categorical renunciation) to nuclear war-fighting (force modernization, targeting doctrine development, procurement justification). Moves institutional resources and policy attention from disarmament toward deterrence-through-capability. Moves strategic debate away from 'should we have nuclear weapons' toward 'how do we win with them.'
% ABSENT_VOICES: Abolition movements and civilian disarmament advocates are structurally excluded: mainline strategic planning does not recognize anti-nuclear premises as valid inputs to doctrine. Non-nuclear states have voice in NPT forums but no veto on the nuclear powers' targeting doctrines. Populations of potential nuclear-conflict zones (Europe during Cold War, contemporary Asia) are not consulted on whether winnability assumptions should anchor their security. Their exclusion is enforced by treating nuclear strategy as a classified, expert domain where military and defense-industry voices dominate.
% DISAPPEARANCE_RATIONALE: If the countervailing-thinkable constraint vanished — if great powers ceased to treat limited nuclear war as strategically winnable — military procurement would shift toward conventional forces and non-strategic deterrence; arms control agreements would become politically viable in forms currently blocked by war-fighting doctrine; strategic competition would reframe around conventional and economic power instead of nuclear victory scenarios. The entire post-1960 structure of nuclear force planning and doctrine would require reconstruction. Institutions built on winnability-as-assumption would lose their operational rationale.
% FOUNDING_PROBLEM: After Soviet nuclear parity (circa 1969), American strategic planners faced a credibility crisis: if both superpowers could destroy each other, would nuclear deterrence still work? How could the U.S. maintain the threat of nuclear retaliation as credible if the cost — mutual annihilation — seemed irrational? The solution was counterforce targeting: plan for disarming strikes against the adversary's weapons, not cities. This makes nuclear war 'winnable' (or at least not immediately catastrophic), which restores the rationality of the threat.
% FOUNDING_PROBLEM_CORROBORATION: Defense intellectuals and STRATCOM planners attest the credibility problem was live and winnability framing was the necessary solution. Critics and arms control scholars attest the problem was a false dilemma: deterrence could have rested on assured destruction without war-fighting credibility. Independent historians (e.g., Kaplan, Freedman) document that winnability framing was institutionally useful whether or not the credibility crisis was real. No consensus exists outside the planning community on whether the founding problem persists.
narrative_ontology:disappearance_verdict(war_winnability_post_1945__countervailing_thinkable, world_rearranges).
narrative_ontology:founding_problem_status(war_winnability_post_1945__countervailing_thinkable, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(war_winnability_post_1945__countervailing_thinkable, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(war_winnability_post_1945__countervailing_thinkable, 'none', 1).
narrative_ontology:epsilon_provenance(war_winnability_post_1945__countervailing_thinkable, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(war_winnability_post_1945__countervailing_thinkable_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(war_winnability_post_1945__countervailing_thinkable, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(war_winnability_post_1945__countervailing_thinkable_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness has accumulated from 0.15 (early Cold War, when atomic weapons seemed too catastrophic to plan for) to 0.68 (contemporary, when counterforce targeting is routine in strategic posture reviews and force planning). The growth marks the shift from 'nuclear war is unthinkable' to 'nuclear war is winnable through careful targeting.' Theater ratio has also risen (0.05 to 0.44), indicating an increasing share of the constraint's operational weight is devoted to maintaining rhetorical cover (arms control negotiations, strategic stability discourse) rather than functional coordination. Suppression has risen to 0.72 because the constraint requires active institutional suppression of alternative framings: abolition arguments are excluded from classified strategic planning; critics within the defense establishment are managed through clearance hierarchies; academic and policy dissenters are marginalized as ideological rather than strategic voices. The measurement series is aligned on one shared time grid (1945, 1962, 1974, 1990, 2008, 2024), marking key strategic inflection points: Soviet parity (1962), détente and MIRV deployment (1974), Cold War end (1990), counterterrorism reorientation (2008), and contemporary great-power renewal (2024).
 *
 * PERSPECTIVAL GAP:
 *   The payer seats (arms control, non-nuclear states, global public) perceive the constraint as Snare or Piton — extraction without legitimate coordination. The beneficiary seats (planners, contractors) perceive it as necessary Rope, pure coordination with no extraction. The divergence is structural: beneficiaries author doctrine, control access to classified planning, and define what counts as 'credible deterrence' — their perception is institutionally enforced. Victims' perception is accurate about the extraction but lacks voice in the planning process. The engine computes per-seat classification from power, exit, and beneficiary/victim positioning; the marked gap between agenda-setter and payer seats should emerge clearly in computed types.
 *
 * DIRECTIONALITY LOGIC:
 *   Military-industrial beneficiaries derive d near 0.1–0.2 (beneficiary power, arbitrage-grade exit, capacity to shape doctrine). Nuclear weapons planners derive d near 0.3 (institutional power, but identity-locked by professional commitment to war-fighting; they benefit from the constraint's persistence but cannot simply walk away). Arms control regimes derive d near 0.7–0.8 (institutional power to negotiate, but constrained by being excluded from classified planning; targeted by the constraint, not its agents). Non-nuclear states derive d near 0.75 (moderate power, constrained exit, bearing costs they did not authorize). Global public derives d near 1.0 (powerless, trapped, maximal existential exposure). No directionality override is needed: the structural derivation from beneficiary/victim + power + exit produces the correct d vector.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (credibility crisis post-parity) has a contested status. If it is alive, the countervailing-thinkable constraint solves a live coordination problem (how to maintain deterrence credibility) and merits Tangled Rope classification: genuine coordination (deterrence) + asymmetric extraction (war-planning benefits military-industrial, harms disarmament). If the founding problem is dead (deterrence is stable without war-fighting doctrine), then winnability framing becomes pure extraction riding on an atrophied function — a Piton. The measurement data show increasing theater_ratio (rhetorical maintenance rising faster than functional coordination), which is a Piton signature. However, defense planners continue to argue the credibility problem is live, and their institutional power ensures the winnability doctrine persists in actual targeting systems. The classification remains Tangled Rope because the coordination function (deterrence through threat credibility) is real enough to sustain the constraint, even if a growing share is performative. If theater_ratio continued rising toward 0.7+, the constraint would drift toward Piton; current state (0.44) still places the functional component at ~56% of activity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    winnability_epistemic_status,
    'Is the countervailing-thinkable premise (limited nuclear war is strategically winnable through counterforce) an accurate description of strategic reality, a self-fulfilling institutional narrative, or an incoherent fantasy maintained by sunk-cost commitment?',
    'Systematic analysis of force-exchange models, war-game outcomes, and escalation-control mechanisms used in classified planning; independent review of whether counterforce targeting actually produces the outcomes planners model, or whether models are artifacts of institutional assumptions. Post-conflict case study if deterrence fails and actual use data emerge.',
    'If winnability is accurate, the constraint reflects real strategic logic and Tangled Rope classification holds. If it is an institutional narrative, the constraint drifts toward Piton (performative maintenance). If incoherent, it is a Snare sustained by institutional power alone.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(winnability_epistemic_status, empirical, 'Whether countervailing strategy produces the strategic outcomes its planners model.').

omega_variable(
    credibility_problem_persistence,
    'Does the founding problem (nuclear deterrence credibility crisis post-parity) remain live, or has it been solved by institutional normalization and mutual understanding of deterrence stable equilibrium?',
    'Analysis of strategic doctrine evolution, statements by strategic command, and behavioral evidence from crisis negotiation. If planners claim credibility requires war-fighting doctrine but crisis behavior shows deterrence holds without it, the problem is declared dead. If deterrence fails and escalates, the problem was live but the solution (winnability framing) was incoherent.',
    'If the problem is dead, the constraint is Piton (atrophied function, sustained by inertia). If live, the Tangled Rope classification holds. If the problem exists but winnability is not the solution, the constraint is Snare (extraction masquerading as coordination).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(credibility_problem_persistence, empirical, 'Whether nuclear deterrence actually requires war-fighting credibility.').

omega_variable(
    counterforce_targeting_feasibility,
    'Can counterforce targeting actually limit nuclear war as strategists assume, or does the targeting doctrine create escalation pathways that make uncontrolled war more likely?',
    'Technical analysis of counterforce feasibility (targeting accuracy, command-control reliability, opponent''s ability to distinguish counterforce from countervalue in real time, escalation pressure under uncertainty). Simulation studies accounting for fog of war, communication breakdown, and irrational actor models.',
    'If feasible, winnability framing retains technical coherence. If infeasible, the constraint is Snare — a doctrine maintained for institutional benefit despite incoherence. If it increases escalation risk, the constraint is extractive (benefits planners, harms global public).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterforce_targeting_feasibility, empirical, 'Whether the technical assumptions underlying counterforce doctrine hold under realistic operational conditions.').

omega_variable(
    kernel_reading_commensurability,
    'Which of the three kernel readings (countervailing_thinkable, deterrence_unthinkable, rhetorical_contraction) correctly captures how nuclear powers actually operate strategically?',
    'Triangulation of classified strategic documents (to the extent accessible through declassification), public policy statements, force posture decisions, and arms control negotiating positions. If declassified material shows planners privately reject winnability (it is rhetorical), the reading shifts toward rhetorical_contraction. If it shows categorical rejection of war-planning, it shifts toward deterrence_unthinkable.',
    'The three readings are not independent constraints — they are competing framings of a single kernel. The corpus measures which reading is structurally correct. If countervailling_thinkable is correct, Tangled Rope or Snare (depending on coordination credibility). If deterrence_unthinkable is correct, the winnability doctrine is pure extraction and should classify as Snare. If rhetorical_contraction is correct, the constraint is Piton with high theater_ratio.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_commensurability, conceptual, 'Which reading of the winnability kernel is operationally true in actual strategic planning.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the suppression of abolition voices and disarmament framings structural (exclusion from classified planning, institutional gatekeeping) or internalized (analysts self-censor, believing winnability framing is technically correct)?',
    'Post-exit suppression analysis: if declassified documents or whistleblower accounts show planners privately acknowledged winnability''s incoherence but maintained it publicly, suppression is structural. If analysts who exit the system continue to endorse winnability doctrine, suppression has become internalized.',
    'If structural, the constraint''s suppression is an institutional property the constraint requires to persist. If internalized, the constraint has captured its own observers — dismantling the institutional suppression might not reverse the doctrine''s hold. If mixed, suppression has multiple reinforcing mechanisms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression of alternative strategic framings is institutional exclusion or internalized belief capture.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(war_winnability_post_1945__countervailing_thinkable, 1945, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(war__tr_t1945, war_winnability_post_1945__countervailing_thinkable, theater_ratio, 1945, 0.05).
narrative_ontology:measurement(war__tr_t1962, war_winnability_post_1945__countervailing_thinkable, theater_ratio, 1962, 0.18).
narrative_ontology:measurement(war__tr_t1974, war_winnability_post_1945__countervailing_thinkable, theater_ratio, 1974, 0.28).
narrative_ontology:measurement(war__tr_t1990, war_winnability_post_1945__countervailing_thinkable, theater_ratio, 1990, 0.36).
narrative_ontology:measurement(war__tr_t2008, war_winnability_post_1945__countervailing_thinkable, theater_ratio, 2008, 0.41).
narrative_ontology:measurement(war__tr_t2024, war_winnability_post_1945__countervailing_thinkable, theater_ratio, 2024, 0.44).

% Extraction over time
narrative_ontology:measurement(war__be_t1945, war_winnability_post_1945__countervailing_thinkable, base_extractiveness, 1945, 0.15).
narrative_ontology:measurement(war__be_t1962, war_winnability_post_1945__countervailing_thinkable, base_extractiveness, 1962, 0.35).
narrative_ontology:measurement(war__be_t1974, war_winnability_post_1945__countervailing_thinkable, base_extractiveness, 1974, 0.52).
narrative_ontology:measurement(war__be_t1990, war_winnability_post_1945__countervailing_thinkable, base_extractiveness, 1990, 0.61).
narrative_ontology:measurement(war__be_t2008, war_winnability_post_1945__countervailing_thinkable, base_extractiveness, 2008, 0.65).
narrative_ontology:measurement(war__be_t2024, war_winnability_post_1945__countervailing_thinkable, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(war__su_t1945, war_winnability_post_1945__countervailing_thinkable, suppression_requirement, 1945, 0.2).
narrative_ontology:measurement(war__su_t1962, war_winnability_post_1945__countervailing_thinkable, suppression_requirement, 1962, 0.45).
narrative_ontology:measurement(war__su_t1974, war_winnability_post_1945__countervailing_thinkable, suppression_requirement, 1974, 0.58).
narrative_ontology:measurement(war__su_t1990, war_winnability_post_1945__countervailing_thinkable, suppression_requirement, 1990, 0.66).
narrative_ontology:measurement(war__su_t2008, war_winnability_post_1945__countervailing_thinkable, suppression_requirement, 2008, 0.7).
narrative_ontology:measurement(war__su_t2024, war_winnability_post_1945__countervailing_thinkable, suppression_requirement, 2024, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(war_winnability_post_1945__countervailing_thinkable, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(war_winnability_post_1945__countervailing_thinkable, 0.12).
narrative_ontology:affects_constraint(war_winnability_post_1945__countervailing_thinkable, war_winnability_post_1945__deterrence_unthinkable).
narrative_ontology:affects_constraint(war_winnability_post_1945__countervailing_thinkable, war_winnability_post_1945__rhetorical_contraction).
narrative_ontology:affects_constraint(war_winnability_post_1945__countervailing_thinkable, arms_control_treaty_enforcement).
narrative_ontology:affects_constraint(war_winnability_post_1945__countervailing_thinkable, nuclear_force_modernization_procurement).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the war_winnability_post_1945 kernel. The kernel persists across all three readings; the readings differ on whether winnability should be treated as (1) strategically coherent and operationally thinkable (countervailing_thinkable, this constraint), (2) categorically unthinkable and incompatible with strategic logic (deterrence_unthinkable), or (3) operationally planned but discursively taboo (rhetorical_contraction). Each reading instantiates a different constraint with a different ε, different beneficiary/victim structure, and different classification. The three constraints are linked by affecting each other: the persistence of countervailing-thinkable doctrine undermines arms control regimes and influences deterrence_unthinkable readings (makes them harder to sustain institutionally); rhetorical_contraction emerges as a compromise between the two poles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(war_winnability_post_1945__countervailing_thinkable, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
