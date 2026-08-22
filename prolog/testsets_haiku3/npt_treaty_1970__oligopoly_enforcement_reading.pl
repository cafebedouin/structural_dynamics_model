% ============================================================================
% CONSTRAINT STORY: npt_treaty_1970__oligopoly_enforcement_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-13
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_npt_treaty_1970__oligopoly_enforcement_reading, []).

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
 *   constraint_id: npt_treaty_1970__oligopoly_enforcement_reading
 *   human_readable: NPT Oligopoly Enforcement Reading: Articles I-II Binding, VI Aspirational
 *   domain: international_law/security
 *
 * SUMMARY:
 *   The Nuclear Nonproliferation Treaty (1970) presents itself as a
 *   reciprocal bargain: non-nuclear weapon states (NNWS) agree not to acquire
 *   weapons (Articles I-II) in exchange for nuclear weapon states (NWS)
 *   committing to disarmament (Article VI) and sharing peaceful nuclear
 *   technology (Article IV). The oligopoly enforcement reading interprets
 *   Articles I-II as primary and binding obligations, while Article VI is
 *   read as aspirational or contingent on security conditions—creating an
 *   asymmetric regime where the majority of states accept permanent
 *   nonproliferation while the five permanent members preserve their arsenals
 *   indefinitely. This reading benefits the P5 by codifying their strategic
 *   monopoly and enabling enforcement mechanisms that exclude threshold
 *   states from deterrents. The reading coexists with alternative readings
 *   (reciprocal disarmament reading, which elevates Article VI; withdrawal
 *   sovereignty reading, which prioritizes Article X) but enforces
 *   organizational dominance through P5 institutional control.
 *
 * KEY AGENTS:
 *   - Permanent Five Nuclear Powers (NWS): agenda-setters of enforcement regime, beneficiaries of strategic monopoly
 *   - Non-Nuclear Weapon States (NNWS): payers bearing inspection burden and technology restrictions
 *   - Threshold States: identity-locked victims denied deterrent protection
 *   - IAEA Technical Staff: enforce asymmetry while lacking verification authority over NWS
 *   - Disarmament Advocates: excluded from governance, contest the reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_treaty_1970__oligopoly_enforcement_reading, 0.68).
domain_priors:suppression_score(npt_treaty_1970__oligopoly_enforcement_reading, 0.71).
domain_priors:theater_ratio(npt_treaty_1970__oligopoly_enforcement_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_treaty_1970__oligopoly_enforcement_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(npt_treaty_1970__oligopoly_enforcement_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(npt_treaty_1970__oligopoly_enforcement_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_treaty_1970__oligopoly_enforcement_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(npt_treaty_1970__oligopoly_enforcement_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_treaty_1970__oligopoly_enforcement_reading, tangled_rope).
narrative_ontology:human_readable(npt_treaty_1970__oligopoly_enforcement_reading, "NPT Oligopoly Enforcement Reading: Articles I-II Binding, VI Aspirational").
narrative_ontology:topic_domain(npt_treaty_1970__oligopoly_enforcement_reading, "international_law/security").

domain_priors:requires_active_enforcement(npt_treaty_1970__oligopoly_enforcement_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_treaty_1970__oligopoly_enforcement_reading, 'eeb4ce52-1eec-43d1-9f34-b7461ec0f578').
narrative_ontology:cs_kernel_codification('eeb4ce52-1eec-43d1-9f34-b7461ec0f578', fixed_text).
narrative_ontology:cs_authority_grounding('eeb4ce52-1eec-43d1-9f34-b7461ec0f578', extraction).
narrative_ontology:cs_interpretation_layer_present('eeb4ce52-1eec-43d1-9f34-b7461ec0f578').
narrative_ontology:cs_reading_relation('eeb4ce52-1eec-43d1-9f34-b7461ec0f578', npt_treaty_1970__reciprocal_disarmament_reading, coexists_with).
narrative_ontology:cs_reading_relation('eeb4ce52-1eec-43d1-9f34-b7461ec0f578', npt_treaty_1970__withdrawal_sovereignty_reading, influences).
narrative_ontology:cs_axiom('eeb4ce52-1eec-43d1-9f34-b7461ec0f578', foundational, article_vi_aspirational_not_binding).
narrative_ontology:cs_axiom_status(article_vi_aspirational_not_binding, holdable).
narrative_ontology:cs_axiom_grounding('eeb4ce52-1eec-43d1-9f34-b7461ec0f578', article_vi_aspirational_not_binding, conventional).
narrative_ontology:cs_axiom('eeb4ce52-1eec-43d1-9f34-b7461ec0f578', foundational, nws_deterrent_preservation_necessary).
narrative_ontology:cs_axiom_status(nws_deterrent_preservation_necessary, holdable).
narrative_ontology:cs_axiom_grounding('eeb4ce52-1eec-43d1-9f34-b7461ec0f578', nws_deterrent_preservation_necessary, instrumental).
narrative_ontology:cs_reference_frame('eeb4ce52-1eec-43d1-9f34-b7461ec0f578', nws_strategic_monopoly_framework).
narrative_ontology:cs_drift_state('eeb4ce52-1eec-43d1-9f34-b7461ec0f578', contemporary_disarmament_pressure_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('eeb4ce52-1eec-43d1-9f34-b7461ec0f578', '').
narrative_ontology:cs_kernel_id(npt_treaty_1970__oligopoly_enforcement_reading, npt_treaty_1970).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_treaty_1970__oligopoly_enforcement_reading, permanent_five_nuclear_powers).
narrative_ontology:constraint_victim(npt_treaty_1970__oligopoly_enforcement_reading, non_nuclear_weapon_states).
narrative_ontology:constraint_victim(npt_treaty_1970__oligopoly_enforcement_reading, threshold_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(npt_treaty_1970__oligopoly_enforcement_reading, strategic_planners_nws).
narrative_ontology:constraint_victim(npt_treaty_1970__oligopoly_enforcement_reading, iaea_technical_staff).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set and enforce the treaty regime through their permanent UNSC seats and technical dominance of nuclear weapons design. Under the oligopoly reading, they interpret Articles I-II as binding and Article VI as aspirational, enabling them to maintain nuclear arsenals indefinitely while enforcing nonproliferation inspections on all others. They design the IAEA inspection framework, withhold sensitive technology, and preserve deterrent exclusivity.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__oligopoly_enforcement_reading, permanent_five_nuclear_powers, agenda_setter,
    institutional, generational, arbitrage, global).

% Accept comprehensive IAEA safeguards on all civil nuclear activity under Article III, accepting inspection burden and technology restrictions, in exchange for a nonproliferation bargain promised in Articles I-II and Article VI. Under the oligopoly reading, they bear the inspection burden while Article VI disarmament commitments remain unscheduled and unmeasured. Their exit options are limited: withdrawal triggers security concerns and international isolation; staying means indefinite subordination to an unchanging hierarchy.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__oligopoly_enforcement_reading, non_nuclear_weapon_states, payer,
    moderate, generational, constrained, global).

% States with advanced nuclear fuel-cycle capabilities and security concerns (India, Pakistan, Israel, others) face a choice: sign the NPT and accept asymmetric inspections without the deterrent they seek, or stay outside and endure sanctions and isolation. The oligopoly reading denies them the deterrent while offering only NNWS status. Their identity as security-seeking states conflicts with permanent subordination; staying outside becomes rational but illegal under treaty interpretation; joining means accepting extraction without protection.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__oligopoly_enforcement_reading, threshold_states, payer,
    powerful, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(npt_treaty_1970__oligopoly_enforcement_reading, threshold_states, excluded).

% Operate the technical inspection regime under P5 oversight, carrying out verification of NNWS compliance while lacking authority to verify NWS arsenals. They administer the asymmetry and enforce compliance mechanisms, but have no mechanism to verify reciprocal P5 obligations. Their institutional identity is wedded to the inspection function; resistance to the asymmetry risks their role.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__oligopoly_enforcement_reading, iaea_technical_staff, agenda_setter,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(npt_treaty_1970__oligopoly_enforcement_reading, iaea_technical_staff, payer).

% Military and strategic elites in NWS use the oligopoly reading to justify continued deterrent development. The constraint serves their operational needs by preserving nuclear exclusivity and preventing proliferation that might erode their strategic advantage.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__oligopoly_enforcement_reading, strategic_planners_nws, beneficiary,
    powerful, generational, analytical, global).

% Civil society, non-aligned states, and humanitarian movements argue for reciprocal disarmament (Article VI as binding); they are systematically excluded from treaty governance and their readings do not shape enforcement.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__oligopoly_enforcement_reading, disarmament_advocates, excluded,
    moderate, generational, constrained, global).

% International Court of Justice and advisory opinion processes can assess whether the oligopoly reading is faithful to the treaty text, but lack enforcement power over P5; their rulings are non-binding on veto-wielding states.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__oligopoly_enforcement_reading, observer_international_court, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(npt_treaty_1970__oligopoly_enforcement_reading, permanent_five_nuclear_powers).
narrative_ontology:fixing_cost_class(npt_treaty_1970__oligopoly_enforcement_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents proliferation of nuclear weapons beyond the five weapons states that possessed them at treaty entry in 1968 — coordinates a global commitment that the majority of states will not acquire weapons, generating stability through predictable nonproliferation.
% TRANSFER_FUNCTION: Moves inspection transparency, technology restrictions, and deterrent denial FROM non-nuclear weapon states and threshold states TO the permanent five, who collect strategic monopoly, technology control, and deterrent exclusivity. NNWS bear inspection costs and deterrent subordination; NWS retain weapons and strategic autonomy.
% ABSENT_VOICES: Threshold states with legitimate security concerns are structurally excluded from holding deterrents; disarmament advocates and humanitarian movements are excluded from treaty governance; future generations denied both weaponization options and reciprocal disarmament are not represented in current agreements.
% DISAPPEARANCE_RATIONALE: If the NPT and its oligopoly enforcement regime disappeared overnight, threshold states would rapidly pursue nuclear weapons development, regional proliferation cascades would follow, and the post-1968 hierarchical stability would shatter. Strategic balances at the regional level would transform. The constraint's persistence prevents this rearrangement; its absence enables it.
% FOUNDING_PROBLEM: In 1968, the nuclear threshold was widening; advanced industrial states were acquiring or approaching weapons capability. The founding problem was: prevent a cascade of new weapons states while stabilizing the Cold War superpower deterrent.
% FOUNDING_PROBLEM_CORROBORATION: The P5 attest the proliferation problem is live and the NPT constrains it successfully. Non-aligned states, disarmament advocates, and threshold-state security analysts attest the problem is compounded by asymmetric enforcement: proliferation is constrained for the weak but enabled for the strong; the remedy creates new problems (regional security dilemmas, deterrent denial, treaty delegitimacy). The oligopoly reading persists despite scholarly and diplomatic challenge because P5 enforcement power sustains it, not because corroboration is universal.
narrative_ontology:disappearance_verdict(npt_treaty_1970__oligopoly_enforcement_reading, world_rearranges).
narrative_ontology:founding_problem_status(npt_treaty_1970__oligopoly_enforcement_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_treaty_1970__oligopoly_enforcement_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(npt_treaty_1970__oligopoly_enforcement_reading, 'none', 1).
narrative_ontology:epsilon_provenance(npt_treaty_1970__oligopoly_enforcement_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(npt_treaty_1970__oligopoly_enforcement_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(npt_treaty_1970__oligopoly_enforcement_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(npt_treaty_1970__oligopoly_enforcement_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) reflects the transfer of strategic autonomy and deterrent capability from NNWS to NWS; the transfer is substantial and visible. Suppression (0.71) is driven by multiple mechanisms: (1) structural—NNWS cannot legally acquire deterrents under Article II; (2) institutional—IAEA inspections constrain civil fuel cycles; (3) informational—Article VI remains vague on timelines, enabling indefinite deferral. Theater (0.44) moderately reflects performative aspects: periodic review conferences stage consensus on the nonproliferation commitment while Article VI discussions produce rhetoric without binding commitments or timelines. Accessibility collapse (0.62) is moderate because NNWS theoretically retain the exit option of treaty withdrawal (Article X), but withdrawal carries severe costs (sanctions, isolation, security deterrent loss) that make the exit constrained rather than genuinely accessible. Resistance (0.58) is substantial because threshold states actively contest the asymmetry through diplomatic channels, disarmament advocates push for Article VI revision at review conferences, and non-aligned states periodically demand reciprocal obligations—resistance is real but structurally unable to compel P5 compliance.
 *
 * PERSPECTIVAL GAP:
 *   The oligopoly reading enters the constraint story as the articulated interpretation used by P5 states to justify enforcement asymmetry. The reciprocal disarmament reading (sibling constraint) would reweight Article VI into primary obligation status and reframe the bargain as reciprocal—causing the measured extractiveness to collapse (Article VI compliance would compress P5 arsenals, reducing their strategic monopoly gain) and reclassify the type at the P5 seat from beneficiary to constrained. The withdrawal sovereignty reading (sibling constraint) would emphasize Article X as a legitimate exit option, increasing accessibility collapse downward (exit becomes real and legally sanctioned) and reclassifying NNWS seats from constrained to mobile. This constraint is ONE reading; the alternative readings are other stories. The prompt asks for this reading only—the oligopoly enforcement reading that interprets Articles I-II as primary and Article VI as contingent.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is d ≈ 0.8 for threshold states (full targets: trapped or identity-locked, denied a resource—deterrents—they perceive as necessary for security, while the constraint remains enforced indefinitely). Directionality is d ≈ 0.6 for NNWS that are not threshold states (moderate targets: inspection burden and technology restrictions, but not identity-locked; exit is theoretically possible). Directionality is d ≈ 0.15 for the P5 (full beneficiaries: set the rules, collect strategic monopoly, carry no inspection burden). Directionality is d ≈ 0.5 for IAEA staff (symmetric: genuine coordination benefit from a functioning verification system, but constrained exit from institutional identity and moderate suppression of role-divergence).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint is NOT mandatrophic under the oligopoly reading because the founding problem (proliferation prevention) remains live and the constraint continues to prevent threshold-state proliferation successfully (India, Pakistan, Israel pursued deterrents outside the regime; NNWS that signed remain non-nuclear). However, tension appears in the founding problem's secondary component: the constraint was supposed to create conditions for disarmament (Article VI), but that mandate has atrophied—no P5 disarmament schedule exists, no timeline, no binding commitment. The oligopoly reading resolves this tension by reclassifying Article VI from binding obligation to aspirational rhetoric. Under the reciprocal disarmament reading (sibling), the atrophy becomes mandatrophy: the founding problem's second component is dead while the constraint persists, making that reading vulnerable to mandatrophy classification. This story (oligopoly reading) avoids mandatrophy by declaring Article VI contingent, not binding.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    article_vi_binding_status_ambiguity,
    'Is Article VI a binding legal obligation on NWS with enforceable timelines and metrics, or is it aspirational/conditional on security environments?',
    'International Court of Justice advisory opinion or interpretation by a consensus treaty amendment; alternately, emergence of P5 disarmament schedules with specific arsenal reduction targets and verification mechanisms that treat Article VI as binding.',
    'If Article VI is binding with timelines, effective extraction drops substantially (NWS obligations compress strategic monopoly gains), and the constraint reclassifies as weakly extractive or reciprocal at the NWS seat. If Article VI remains aspirational, the oligopoly reading is sustained, extractiveness remains high, and mandatrophy tension in the founding problem persists.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(article_vi_binding_status_ambiguity, conceptual, 'The legal status of Article VI disarmament commitments—binding or aspirational—is the hinge between oligopoly and reciprocal disarmament readings.').

omega_variable(
    threshold_state_identity_locking_mechanism,
    'Is threshold-state pursuit of deterrents primarily driven by structural security competition (would dissipate with regional agreements, confidence-building measures), or is it identity-constituted (security status as inextricable from sovereignty, would persist regardless of structural changes)?',
    'Regional security agreements that offer alternative deterrent substitutes (security guarantees, extended deterrence, defense technology access); measure whether threshold states cease weapons pursuit under those conditions. Alternately, diplomatic history and security studies analysis of whether deterrent pursuit is a policy choice or identity-fusion.',
    'If identity-constituted, threshold states are genuinely identity-locked and exit options remain constrained even under structural improvements—extraction persists. If driven by security competition, alternative arrangements could open exit (reclassify from identity-locked to constrained), and effective extraction on threshold-state seats would drop.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threshold_state_identity_locking_mechanism, empirical, 'Whether threshold-state nuclear pursuit is driven by structural security dilemmas or identity-fusion with deterrent sovereignty.').

omega_variable(
    oligopoly_vs_reciprocal_reading_foreclosure,
    'Do the oligopoly and reciprocal disarmament readings represent incompatible legal interpretations that one framework cannot hold both, or do they coexist as competing political readings that different parties maintain simultaneously?',
    'Textual analysis of Article VI language and intent records; treaty negotiation history; assessment of whether the text permits both interpretations or forecloses one. Political observation: whether P5 states ever acknowledge reciprocal disarmament as a live binding obligation, or whether they maintain the oligopoly reading as the only permissible interpretation.',
    'If readings foreclose each other, the NPT kernel instantiates a genuine either/or—only one reading can be operationalized at a time. If readings coexist, they compete in organizational power terms; the oligopoly reading dominates currently because P5 institutional control enforces it, but could be displaced if alternative power coalitions emerge (at review conferences, via Charter authority shifts). Classification of the foreclosure relation (forecloses vs. coexists_with) depends on this assessment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(oligopoly_vs_reciprocal_reading_foreclosure, conceptual, 'Whether oligopoly and reciprocal readings are logically incompatible or politically competing interpretations of ambiguous treaty language.').

omega_variable(
    inspection_asymmetry_suppression_mechanism,
    'Is the asymmetry between comprehensive IAEA inspections of NNWS civil fuel cycles and the absence of verification of NWS arsenals structural (required by the physics of verification and nuclear secrecy), or is it a choice to maintain P5 opacity?',
    'Technical feasibility study: can NWS arsenals be verified with acceptable confidence? Examine whether other weapons-control regimes (biological weapons, chemical weapons) achieve verification symmetry. Historical analysis: was comprehensive P5 verification rejected on technical grounds or policy grounds?',
    'If structural/technical, the inspection asymmetry is not extractive overhead but coordination cost—reclassifies part of measured suppression as necessary rather than coercive. If chosen, the asymmetry is a pure extraction mechanism—supports high suppression and extractiveness scores and confirms the tangled_rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inspection_asymmetry_suppression_mechanism, empirical, 'Whether verification asymmetry is technically necessary or a policy choice to preserve P5 opacity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_treaty_1970__oligopoly_enforcement_reading, 0, 55).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt__tr_t0, npt_treaty_1970__oligopoly_enforcement_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(npt__tr_t0, observed).
narrative_ontology:measurement(npt__tr_t7, npt_treaty_1970__oligopoly_enforcement_reading, theater_ratio, 7, 0.32).
narrative_ontology:measurement_basis(npt__tr_t7, observed).
narrative_ontology:measurement(npt__tr_t14, npt_treaty_1970__oligopoly_enforcement_reading, theater_ratio, 14, 0.38).
narrative_ontology:measurement_basis(npt__tr_t14, observed).
narrative_ontology:measurement(npt__tr_t21, npt_treaty_1970__oligopoly_enforcement_reading, theater_ratio, 21, 0.42).
narrative_ontology:measurement_basis(npt__tr_t21, observed).
narrative_ontology:measurement(npt__tr_t28, npt_treaty_1970__oligopoly_enforcement_reading, theater_ratio, 28, 0.44).
narrative_ontology:measurement_basis(npt__tr_t28, observed).
narrative_ontology:measurement(npt__tr_t35, npt_treaty_1970__oligopoly_enforcement_reading, theater_ratio, 35, 0.44).
narrative_ontology:measurement_basis(npt__tr_t35, observed).
narrative_ontology:measurement(npt__tr_t42, npt_treaty_1970__oligopoly_enforcement_reading, theater_ratio, 42, 0.44).
narrative_ontology:measurement_basis(npt__tr_t42, observed).
narrative_ontology:measurement(npt__tr_t55, npt_treaty_1970__oligopoly_enforcement_reading, theater_ratio, 55, 0.44).
narrative_ontology:measurement_basis(npt__tr_t55, observed).

% Extraction over time
narrative_ontology:measurement(npt__be_t0, npt_treaty_1970__oligopoly_enforcement_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(npt__be_t0, observed).
narrative_ontology:measurement(npt__be_t7, npt_treaty_1970__oligopoly_enforcement_reading, base_extractiveness, 7, 0.52).
narrative_ontology:measurement_basis(npt__be_t7, observed).
narrative_ontology:measurement(npt__be_t14, npt_treaty_1970__oligopoly_enforcement_reading, base_extractiveness, 14, 0.58).
narrative_ontology:measurement_basis(npt__be_t14, observed).
narrative_ontology:measurement(npt__be_t21, npt_treaty_1970__oligopoly_enforcement_reading, base_extractiveness, 21, 0.63).
narrative_ontology:measurement_basis(npt__be_t21, observed).
narrative_ontology:measurement(npt__be_t28, npt_treaty_1970__oligopoly_enforcement_reading, base_extractiveness, 28, 0.65).
narrative_ontology:measurement_basis(npt__be_t28, observed).
narrative_ontology:measurement(npt__be_t35, npt_treaty_1970__oligopoly_enforcement_reading, base_extractiveness, 35, 0.67).
narrative_ontology:measurement_basis(npt__be_t35, observed).
narrative_ontology:measurement(npt__be_t42, npt_treaty_1970__oligopoly_enforcement_reading, base_extractiveness, 42, 0.68).
narrative_ontology:measurement_basis(npt__be_t42, observed).
narrative_ontology:measurement(npt__be_t55, npt_treaty_1970__oligopoly_enforcement_reading, base_extractiveness, 55, 0.68).
narrative_ontology:measurement_basis(npt__be_t55, observed).

% Suppression requirement over time
narrative_ontology:measurement(npt__su_t0, npt_treaty_1970__oligopoly_enforcement_reading, suppression_requirement, 0, 0.52).
narrative_ontology:measurement_basis(npt__su_t0, observed).
narrative_ontology:measurement(npt__su_t7, npt_treaty_1970__oligopoly_enforcement_reading, suppression_requirement, 7, 0.58).
narrative_ontology:measurement_basis(npt__su_t7, observed).
narrative_ontology:measurement(npt__su_t14, npt_treaty_1970__oligopoly_enforcement_reading, suppression_requirement, 14, 0.64).
narrative_ontology:measurement_basis(npt__su_t14, observed).
narrative_ontology:measurement(npt__su_t21, npt_treaty_1970__oligopoly_enforcement_reading, suppression_requirement, 21, 0.68).
narrative_ontology:measurement_basis(npt__su_t21, observed).
narrative_ontology:measurement(npt__su_t28, npt_treaty_1970__oligopoly_enforcement_reading, suppression_requirement, 28, 0.7).
narrative_ontology:measurement_basis(npt__su_t28, observed).
narrative_ontology:measurement(npt__su_t35, npt_treaty_1970__oligopoly_enforcement_reading, suppression_requirement, 35, 0.71).
narrative_ontology:measurement_basis(npt__su_t35, observed).
narrative_ontology:measurement(npt__su_t42, npt_treaty_1970__oligopoly_enforcement_reading, suppression_requirement, 42, 0.71).
narrative_ontology:measurement_basis(npt__su_t42, observed).
narrative_ontology:measurement(npt__su_t55, npt_treaty_1970__oligopoly_enforcement_reading, suppression_requirement, 55, 0.71).
narrative_ontology:measurement_basis(npt__su_t55, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_treaty_1970__oligopoly_enforcement_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(npt_treaty_1970__oligopoly_enforcement_reading, 0.12).
narrative_ontology:affects_constraint(npt_treaty_1970__oligopoly_enforcement_reading, npt_treaty_1970__reciprocal_disarmament_reading).
narrative_ontology:affects_constraint(npt_treaty_1970__oligopoly_enforcement_reading, npt_treaty_1970__withdrawal_sovereignty_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three structurally distinct readings of the NPT kernel (npt_treaty_1970). The three constraints share the treaty text but assign different binding force to different articles, producing different beneficiary/victim sets and enforcement asymmetries. The oligopoly_enforcement_reading interprets Articles I-II as primary and Article VI as aspirational, enabling P5 strategic monopoly. The reciprocal_disarmament_reading elevates Article VI to binding, compressing NWS arsenals and reclassifying extraction. The withdrawal_sovereignty_reading prioritizes Article X exit rights, increasing accessibility and reclassifying NNWS from constrained to mobile seats. All three coexist in live diplomatic discourse; the oligopoly reading currently dominates through P5 institutional control.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(npt_treaty_1970__oligopoly_enforcement_reading, powerful, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
