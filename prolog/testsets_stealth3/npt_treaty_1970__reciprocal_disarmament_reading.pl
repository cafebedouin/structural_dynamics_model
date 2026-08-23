% ============================================================================
% CONSTRAINT STORY: npt_treaty_1970__reciprocal_disarmament_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_npt_treaty_1970__reciprocal_disarmament_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: npt_treaty_1970__reciprocal_disarmament_reading
 *   human_readable: NPT Grand Bargain - Reciprocal Disarmament Reading (Article VI as Binding Obligation with Temporal Urgency)
 *   domain: international_law/nuclear_nonproliferation/regime_theory
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the kernel npt_treaty_1970: the
 *   reciprocal disarmament reading, under which Article VI is a binding legal
 *   obligation carrying temporal urgency and horizontal nonproliferation
 *   (NNWS renunciation, verified) and vertical nonproliferation (NWS
 *   disarmament, promised) form a single reciprocal bargain. Assessed by this
 *   reading's own lights, the standing arrangement extracts substantially
 *   from the non-nuclear majority: their side of the trade is permanent
 *   (sealed by the 1995 indefinite extension), intrusive, and externally
 *   verified, while the arsenal states' side is unbounded in time,
 *   unverified, and fifty-five years unfulfilled. The reading's declared
 *   structural deltas are honored in the data: NWS strategic autonomy enters
 *   the victim set as a nominal legal constraint (their modernization freedom
 *   is what Article VI, taken seriously, forecloses), the NNWS coalition
 *   holds normative leverage (organized, with a parallel prohibition treaty
 *   as its instrument), and the absence of any Article VI verification
 *   mechanism is treated as structural injustice rather than an
 *   implementation detail. The claim/metric gap is deliberate where it
 *   appears: the arrangement is CLAIMED as tangled_rope because a genuine
 *   cascade-prevention coordination function coexists with asymmetric
 *   extraction, while the metrics describe strongly extractive, actively
 *   enforced, increasingly theatrical operation. The engine measures any
 *   divergence; the claim is not tuned to predicted output. KEY AGENTS (by
 *   structural relationship): see key_agents.
 *
 * KEY AGENTS:
 *   - - nuclear_weapon_states: Agenda-setter and dual-positioned collector (institutional/arbitrage) - retains exclusive arsenals, self-certifies the disarmament side, and nominally bears the Article VI obligation this reading treats as binding
 *   - - nonnuclear_weapon_states_coalition: Primary bearer of the bargain's verified side (organized/constrained) - renounced the weapons option permanently under intrusive verification against promissory consideration
 *   - - iaea_verification_apparatus: Administrator of the horizontal half (institutional/constrained) - verifies civilian programs, barred from arsenal-state military programs
 *   - - international_court_of_justice: Analytical observer (institutional/analytical) - fixed the good-faith-conclusion standard in 1996 without procedural reach
 *   - - nonparty_nuclear_states: Excluded outsiders (powerful/arbitrage) - armed outside the bargain, governed by its export-control perimeter
 *   - - future_generations: Absent bearers of tail risk (powerless/trapped) - no seat in the maintenance process
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_treaty_1970__reciprocal_disarmament_reading, 0.72).
domain_priors:suppression_score(npt_treaty_1970__reciprocal_disarmament_reading, 0.62).
domain_priors:theater_ratio(npt_treaty_1970__reciprocal_disarmament_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_treaty_1970__reciprocal_disarmament_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(npt_treaty_1970__reciprocal_disarmament_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(npt_treaty_1970__reciprocal_disarmament_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_treaty_1970__reciprocal_disarmament_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(npt_treaty_1970__reciprocal_disarmament_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_treaty_1970__reciprocal_disarmament_reading, tangled_rope).
narrative_ontology:human_readable(npt_treaty_1970__reciprocal_disarmament_reading, "NPT Grand Bargain - Reciprocal Disarmament Reading (Article VI as Binding Obligation with Temporal Urgency)").
narrative_ontology:topic_domain(npt_treaty_1970__reciprocal_disarmament_reading, "international_law/nuclear_nonproliferation/regime_theory").

domain_priors:requires_active_enforcement(npt_treaty_1970__reciprocal_disarmament_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_treaty_1970__reciprocal_disarmament_reading, '3f236890-bcdd-448e-afd6-4b93bc3d69f6').
narrative_ontology:cs_kernel_codification('3f236890-bcdd-448e-afd6-4b93bc3d69f6', fixed_text).
narrative_ontology:cs_authority_grounding('3f236890-bcdd-448e-afd6-4b93bc3d69f6', distributed).
narrative_ontology:cs_reading_relation('3f236890-bcdd-448e-afd6-4b93bc3d69f6', npt_treaty_1970__oligopoly_enforcement_reading, forecloses).
narrative_ontology:cs_reading_relation('3f236890-bcdd-448e-afd6-4b93bc3d69f6', npt_treaty_1970__withdrawal_sovereignty_reading, influences).
narrative_ontology:cs_axiom('3f236890-bcdd-448e-afd6-4b93bc3d69f6', foundational, article_vi_binding_temporal_urgency).
narrative_ontology:cs_axiom_status(article_vi_binding_temporal_urgency, holdable).
narrative_ontology:cs_axiom_grounding('3f236890-bcdd-448e-afd6-4b93bc3d69f6', article_vi_binding_temporal_urgency, conventional).
narrative_ontology:cs_axiom('3f236890-bcdd-448e-afd6-4b93bc3d69f6', foundational, vertical_horizontal_nonproliferation_reciprocity).
narrative_ontology:cs_axiom_status(vertical_horizontal_nonproliferation_reciprocity, holdable).
narrative_ontology:cs_axiom_grounding('3f236890-bcdd-448e-afd6-4b93bc3d69f6', vertical_horizontal_nonproliferation_reciprocity, conventional).
narrative_ontology:cs_reference_frame('3f236890-bcdd-448e-afd6-4b93bc3d69f6', reciprocal_bargain_equilibrium).
narrative_ontology:cs_drift_state('3f236890-bcdd-448e-afd6-4b93bc3d69f6', post_tpnw_contemporary, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('3f236890-bcdd-448e-afd6-4b93bc3d69f6', '2026-06-12T09:00:00Z').
narrative_ontology:cs_kernel_id(npt_treaty_1970__reciprocal_disarmament_reading, npt_treaty_1970).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_treaty_1970__reciprocal_disarmament_reading, nuclear_weapon_states).
narrative_ontology:constraint_beneficiary(npt_treaty_1970__reciprocal_disarmament_reading, nonnuclear_weapon_states_coalition).
narrative_ontology:constraint_victim(npt_treaty_1970__reciprocal_disarmament_reading, nonnuclear_weapon_states_coalition).
narrative_ontology:constraint_victim(npt_treaty_1970__reciprocal_disarmament_reading, nuclear_weapon_states).
narrative_ontology:constraint_vindicates(npt_treaty_1970__reciprocal_disarmament_reading, reciprocal_bargain_doctrine).
narrative_ontology:constraint_vindicates(npt_treaty_1970__reciprocal_disarmament_reading, icj_1996_good_faith_conclusion_standard).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Five states that possessed nuclear weapons when the treaty opened for signature. They committed to pursue negotiations on ending the arms race and achieving disarmament, and in exchange retained exclusive possession of arsenals while every other signatory forswore them. They report on their own disarmament performance with no external verification, control the security architecture that substitutes for disarmament for many allies, and hold veto power over Security Council responses to treaty crises. Formally exiting their own obligation would cost them little; honoring it fully would require dismantling deterrents they regard as existential.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__reciprocal_disarmament_reading, nuclear_weapon_states, agenda_setter,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(npt_treaty_1970__reciprocal_disarmament_reading, nuclear_weapon_states, beneficiary).

% Roughly 185 states that renounced nuclear weapons under the treaty, accepting intrusive verification of their civilian programs and permanent renunciation of the weapons option, sealed by the 1995 indefinite extension. In return they were promised good-faith disarmament negotiations, peaceful-technology access, and security assurances. A subset shelters under extended-deterrence umbrellas provided by the arsenal states; another subset, increasingly organized through the humanitarian initiative, concluded the promised disarmament will not arrive and built a parallel prohibition treaty. Formal exit exists under Article X but carries sanctions, isolation, and security deterioration, as the North Korean departure demonstrated.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__reciprocal_disarmament_reading, nonnuclear_weapon_states_coalition, payer,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(npt_treaty_1970__reciprocal_disarmament_reading, nonnuclear_weapon_states_coalition, beneficiary).

% The secretariat and inspectorate that verifies civilian nuclear programs against diversion, operating safeguard agreements and the Additional Protocol on budgets and mandates its member states control. Its findings can carry a country to the Security Council, but its remit stops at the military programs of the arsenal states, which it never inspects. Its funding is perennially strained and its board reflects the same coalitions that contest the treaty's meaning.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__reciprocal_disarmament_reading, iaea_verification_apparatus, agenda_setter,
    institutional, generational, constrained, global).

% Rendered the 1996 advisory opinion holding that Article VI imposes an obligation to pursue in good faith and bring to a conclusion negotiations leading to nuclear disarmament, while finding no completed rule outlawing possessory deterrence. Every coalition cites the opinion; it binds no party procedurally, and the court has no compliance docket over the treaty.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__reciprocal_disarmament_reading, international_court_of_justice, observer,
    institutional, generational, analytical, global).

% India, Pakistan, and Israel developed arsenals outside the treaty and never accepted its bargain, objecting to a framework that freezes a 1967-vintage hierarchy of legitimate possession. India negotiated a partial accommodations track (an export-control waiver and safeguards on declared civilian facilities) without joining; Israel maintains ambiguity; Pakistan cites regional parity. They participate in export-control and security politics while remaining outside both the obligations and the review process.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__reciprocal_disarmament_reading, nonparty_nuclear_states, excluded,
    powerful, generational, arbitrage, regional).

% People not yet born who will inherit either the arsenals, the waste streams, or the precedent this settlement sets. They cannot appear at review conferences, cannot ratify or withdraw, and bear the tail risk of the arrangement's failure without any seat in its maintenance.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__reciprocal_disarmament_reading, future_generations, excluded,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(npt_treaty_1970__reciprocal_disarmament_reading, future_generations).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(npt_treaty_1970__reciprocal_disarmament_reading, nuclear_weapon_states).
narrative_ontology:fixing_cost_class(npt_treaty_1970__reciprocal_disarmament_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Caps the number of nuclear-armed states at the five that tested before 1967 by trading verified renunciation (safeguards, inspections, export controls) for promised disarmament and shared peaceful nuclear technology, solving the cascade problem in which each new weapon state triggers its neighbors' programs.
% TRANSFER_FUNCTION: Moves verified security restraint from the non-nuclear majority into the collective pool and into the arsenal states' strategic position; moves unverified disarmament promises from the arsenal states to the non-nuclear majority; moves prestige and positional rent to the five; moves inspection burdens and foregone-option costs onto the many.
% ABSENT_VOICES: The nonparty nuclear states were never admitted to the bargain yet are governed by its export-control perimeter; future generations hold no seat anywhere in the review process; within the arsenal states, domestic disarmament constituencies lack standing in delegation politics; umbrella-client publics are not consulted on the extended-deterrence arrangements struck in their name.
% DISAPPEARANCE_RATIONALE: Overnight disappearance removes the legal anchor for export controls, the inspection mandate, and the security-assurance architecture. Hedging programs in industrialized non-nuclear states would surface within years, regional arms competitions in the Middle East, Northeast Asia, and South Asia would lose their principal damper, and the five would confront a proliferating environment their postures currently assume away.
% FOUNDING_PROBLEM: In the 1960s the projected cascade of new nuclear states threatened general instability; the treaty was built to cap possession at the existing five while preserving peaceful nuclear commerce, with disarmament pledged as the arsenal states' side of the trade.
% FOUNDING_PROBLEM_CORROBORATION: The horizontal half is attested live by IAEA safeguards reporting and by the same non-nuclear coalitions that protest the vertical half. The vertical half's persistence is attested from outside the benefiting parties by the ICJ's 1996 advisory opinion, by the prohibition-treaty ratification campaign led by non-nuclear governments and civil society, and by independent arms-control scholarship tracking modernization across all five arsenals. The arsenal states themselves attest only the horizontal half; no arsenal-state government corroborates the claim that the disarmament side remains a live obligation carrying temporal urgency.
narrative_ontology:disappearance_verdict(npt_treaty_1970__reciprocal_disarmament_reading, world_rearranges).
narrative_ontology:founding_problem_status(npt_treaty_1970__reciprocal_disarmament_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_treaty_1970__reciprocal_disarmament_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(npt_treaty_1970__reciprocal_disarmament_reading, 'none', 1).
narrative_ontology:epsilon_provenance(npt_treaty_1970__reciprocal_disarmament_reading, 0.72, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(npt_treaty_1970__reciprocal_disarmament_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(npt_treaty_1970__reciprocal_disarmament_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(npt_treaty_1970__reciprocal_disarmament_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.72 at interval end) because the consideration flows are asymmetric in kind, not merely degree: the NNWS side is permanent, specific, and externally verified; the NWS side is open-ended, unspecific, and self-certified. Suppression (0.62) is a raw structural property, unscaled: it reflects the horizontal enforcement machinery (safeguards, Additional Protocol, export controls, sanctions precedents) that actively closes the weapons option for the many while nothing comparable touches the few. Theater (0.48) concentrates on the Article VI side - review-conference action plans (13 Steps in 2000, 64 actions in 2010), consensus language about creating conditions for disarmament, and periodic reaffirmations that commit to nothing measurable - while the safeguards function remains substantively real, keeping theater below the piton-signaling range. Accessibility collapse is moderate (0.40): understood alternatives persist (latency hedging, Article X withdrawal, the prohibition-treaty track, regional weapon-free zones), so the constraint does not present as natural law. Resistance is high (0.65): the humanitarian initiative and the 2017 prohibition treaty are open institutional resistance by the paying coalition, alongside arsenal-state refusal of verification and outlier safeguard defiance. The temporal series run on ONE shared grid (points 0, 10, 20, 25, 35, 45, 55) with every tracked metric authored at every point; the 1990 dip and 1995 jump encode the START-era reductions followed by indefinite extension without disarmament milestones. The suppression_requirement series is authored because the story specifically tracks enforcement-capacity change: the horizontal machinery hardened materially (Iraq discoveries, the 93+2 program, the Additional Protocol, Resolution 1540, sanctions ratchets) while vertical enforcement capacity remained at zero throughout - the series records the build-up of the asymmetric apparatus itself. The five-year review cycle produces a sawtooth within the monotonic trend; the series samples through it rather than resolving it.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the arsenal-state seat, the arrangement is a regime they built and administer: they experience the Article VI obligation as a rhetorical cost only, since they certify their own performance, so their computed extraction is damped toward subsidy and the structure reads as manageable coordination. From the non-nuclear payer seat, the identical text operates as enforced extraction: a verified permanent burden against an uncollectable promise, amplified by the regime's global scope (verification is hardest exactly where the obligation is largest). The verification apparatus experiences a functioning professional machine whose jurisdiction ends precisely at the arsenals; the excluded nonparty states experience governance by norms they never accepted; the analytical seat sees the whole asymmetry at once. The engine computes these divergences from the structural data; this story authors the data, not the verdicts.
 *
 * DIRECTIONALITY LOGIC:
 *   Both principal groups are dual-positioned, and the exit atoms separate them where the dual declarations alone would blur them. The arsenal states appear in both beneficiary and victim sets (they collect the oligopoly and, under this reading, nominally bear the binding Article VI constraint on their strategic autonomy), but their exit from their own obligation is arbitrage-grade - self-certification, reinterpretation, indefinite deferral - which places them near the beneficiary end of directionality despite the nominal victim listing. The non-nuclear coalition likewise appears in both sets (they receive technology access, assurances, and crisis stability), but their exit is merely constrained - withdrawal is legal yet sanction-laden and security-corrosive, as the Korean departure showed - which places them near the target end. No directionality_overrides are authored: the derivation chain already produces the correct ordering through exit modulation, and an override keyed to the institutional power atom would collide with the verification apparatus, which shares that atom but holds a near-symmetric administrative position. The residual asymmetry - same formal reciprocity, different verification - is the reading's content, and it survives the derivation intact.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification discipline matters here because both mislabelings are live temptations. Reading the arrangement as pure extraction (snare) erases the real coordination achievement: cascade prevention is a genuine collective-action solution that most parties would repurchase, and the safeguards function is not cover. Reading it as pure coordination (rope) erases the verified/unverified asymmetry that defines the paying coalition's grievance and drove them to build a parallel treaty. Tangled rope holds both truths: coordination function plus asymmetric extraction plus active enforcement. On genealogy: the founding problem's horizontal half is alive and attested; its vertical half is the contested portion - the mismatch consumer reads founding_problem_status (contested) against disappearance_verdict (world_rearranges), which does not trip the dead-plus-rearranges zombie flag, correctly, because the arrangement still performs its original horizontal function even as its reciprocal justification decays. The theater trajectory (0.15 to 0.48) tracks the progressive substitution of performative reaffirmation for delivered consideration on the vertical side; if that trajectory crosses the functional threshold while the horizontal function also erodes, the structure drifts toward inertial maintenance, which the measurement series exists to catch.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This constraint is one reading of kernel npt_treaty_1970 (reading: reciprocal_disarmament_reading); how would instantiating the sibling readings change the structural data and computed classifications?',
    'Author and compile the sibling stories (npt_treaty_1970__oligopoly_enforcement_reading, npt_treaty_1970__withdrawal_sovereignty_reading) and compare computed per-seat classifications over the shared kernel text.',
    'Under the oligopoly sibling, the arsenal states exit the victim set (Article VI aspirational) and their effective extraction drops toward subsidy, lowering regime-wide chi. Under the withdrawal sibling, non-nuclear obligations become conditional on the security environment and the victim set migrates toward states facing acute threats. Cross-reading comparison isolates which classification features are reading-indexed versus kernel-stable; the disagreement is located in the legal status of Article VI and the conditionality of obligations, not in the treaty text all readings share.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Reading-indexed nature of epsilon and victim structure over the shared NPT kernel.').

omega_variable(
    article_vi_verifiability_gap,
    'Can Article VI performance ever be specified and verified well enough to be enforceable, or is the obligation categorically unverifiable?',
    'Negotiated verification standards in a successor framework: declared warhead ceilings, a fissile-material cutoff with inspections, verified dismantlement protocols. Existence of an agreed metric set resolves the omega affirmatively.',
    'If verifiable standards emerge, the extraction asymmetry becomes remediable inside the regime and measured extractiveness falls; if not, the unenforceable obligation hardens into permanent structural injustice and corrective pressure migrates wholesale to the prohibition-treaty track, converting the tangled-rope structure toward pure extraction with organized exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(article_vi_verifiability_gap, empirical, 'Whether the disarmament side of the bargain is verifiable in principle or only in rhetoric.').

omega_variable(
    nnws_net_position_heterogeneity,
    'Are the non-nuclear states net payers or net beneficiaries in aggregate - does the extended-deterrence client subset offset the non-client majority?',
    'Seat-level separation of umbrella clients (NATO members, Japan, South Korea) from non-clients, scoring security received against option-value forgone and inspection burden borne.',
    'If clients are net beneficiaries, aggregate extraction narrows and the regime trends toward ordinary coordination with a dissident minority; if non-clients dominate, the asymmetry is starker than any aggregate metric shows and the paying coalition''s resistance is structurally rational rather than free-riding.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nnws_net_position_heterogeneity, empirical, 'Heterogeneity of net position within the non-nuclear coalition.').

omega_variable(
    enforcement_asymmetry_stability,
    'Is the asymmetric enforcement equilibrium - intrusive on the horizontal side, absent on the vertical side - stable, or decaying toward regime fracture via the prohibition-treaty defection spiral?',
    'Track prohibition-treaty ratification growth, umbrella-client retention, and arsenal-state participation in review conferences across successive cycles.',
    'Decay converts the structure toward extraction sustained against organized exit, with the paying coalition''s alternative venue maturing into a rival regime; stabilization keeps the bargain legible and internally reformable. The direction determines whether the interval-end classification is a plateau or a waypoint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_asymmetry_stability, empirical, 'Stability of the two-speed enforcement equilibrium over coming review cycles.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_treaty_1970__reciprocal_disarmament_reading, 0, 55).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt_recip_tr_t0, npt_treaty_1970__reciprocal_disarmament_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(npt_recip_tr_t0, observed).
narrative_ontology:measurement(npt_recip_tr_t10, npt_treaty_1970__reciprocal_disarmament_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement_basis(npt_recip_tr_t10, observed).
narrative_ontology:measurement(npt_recip_tr_t20, npt_treaty_1970__reciprocal_disarmament_reading, theater_ratio, 20, 0.22).
narrative_ontology:measurement_basis(npt_recip_tr_t20, observed).
narrative_ontology:measurement(npt_recip_tr_t25, npt_treaty_1970__reciprocal_disarmament_reading, theater_ratio, 25, 0.28).
narrative_ontology:measurement_basis(npt_recip_tr_t25, observed).
narrative_ontology:measurement(npt_recip_tr_t35, npt_treaty_1970__reciprocal_disarmament_reading, theater_ratio, 35, 0.38).
narrative_ontology:measurement_basis(npt_recip_tr_t35, observed).
narrative_ontology:measurement(npt_recip_tr_t45, npt_treaty_1970__reciprocal_disarmament_reading, theater_ratio, 45, 0.44).
narrative_ontology:measurement_basis(npt_recip_tr_t45, observed).
narrative_ontology:measurement(npt_recip_tr_t55, npt_treaty_1970__reciprocal_disarmament_reading, theater_ratio, 55, 0.48).
narrative_ontology:measurement_basis(npt_recip_tr_t55, observed).

% Extraction over time
narrative_ontology:measurement(npt_recip_be_t0, npt_treaty_1970__reciprocal_disarmament_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(npt_recip_be_t0, observed).
narrative_ontology:measurement(npt_recip_be_t10, npt_treaty_1970__reciprocal_disarmament_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement_basis(npt_recip_be_t10, observed).
narrative_ontology:measurement(npt_recip_be_t20, npt_treaty_1970__reciprocal_disarmament_reading, base_extractiveness, 20, 0.5).
narrative_ontology:measurement_basis(npt_recip_be_t20, observed).
narrative_ontology:measurement(npt_recip_be_t25, npt_treaty_1970__reciprocal_disarmament_reading, base_extractiveness, 25, 0.62).
narrative_ontology:measurement_basis(npt_recip_be_t25, observed).
narrative_ontology:measurement(npt_recip_be_t35, npt_treaty_1970__reciprocal_disarmament_reading, base_extractiveness, 35, 0.66).
narrative_ontology:measurement_basis(npt_recip_be_t35, observed).
narrative_ontology:measurement(npt_recip_be_t45, npt_treaty_1970__reciprocal_disarmament_reading, base_extractiveness, 45, 0.7).
narrative_ontology:measurement_basis(npt_recip_be_t45, observed).
narrative_ontology:measurement(npt_recip_be_t55, npt_treaty_1970__reciprocal_disarmament_reading, base_extractiveness, 55, 0.72).
narrative_ontology:measurement_basis(npt_recip_be_t55, observed).

% Suppression requirement over time
narrative_ontology:measurement(npt_recip_su_t0, npt_treaty_1970__reciprocal_disarmament_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement_basis(npt_recip_su_t0, observed).
narrative_ontology:measurement(npt_recip_su_t10, npt_treaty_1970__reciprocal_disarmament_reading, suppression_requirement, 10, 0.34).
narrative_ontology:measurement_basis(npt_recip_su_t10, observed).
narrative_ontology:measurement(npt_recip_su_t20, npt_treaty_1970__reciprocal_disarmament_reading, suppression_requirement, 20, 0.38).
narrative_ontology:measurement_basis(npt_recip_su_t20, observed).
narrative_ontology:measurement(npt_recip_su_t25, npt_treaty_1970__reciprocal_disarmament_reading, suppression_requirement, 25, 0.42).
narrative_ontology:measurement_basis(npt_recip_su_t25, observed).
narrative_ontology:measurement(npt_recip_su_t35, npt_treaty_1970__reciprocal_disarmament_reading, suppression_requirement, 35, 0.52).
narrative_ontology:measurement_basis(npt_recip_su_t35, observed).
narrative_ontology:measurement(npt_recip_su_t45, npt_treaty_1970__reciprocal_disarmament_reading, suppression_requirement, 45, 0.58).
narrative_ontology:measurement_basis(npt_recip_su_t45, observed).
narrative_ontology:measurement(npt_recip_su_t55, npt_treaty_1970__reciprocal_disarmament_reading, suppression_requirement, 55, 0.62).
narrative_ontology:measurement_basis(npt_recip_su_t55, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_treaty_1970__reciprocal_disarmament_reading, resource_allocation).
narrative_ontology:affects_constraint(npt_treaty_1970__reciprocal_disarmament_reading, npt_treaty_1970__oligopoly_enforcement_reading).
narrative_ontology:affects_constraint(npt_treaty_1970__reciprocal_disarmament_reading, npt_treaty_1970__withdrawal_sovereignty_reading).
narrative_ontology:affects_constraint(npt_treaty_1970__reciprocal_disarmament_reading, tpnw_prohibition_regime).

% DUAL FORMULATION NOTE:
% Constraint family decomposition of the colloquial label 'the NPT': the natural-language concept covers at least three structurally distinct constraints corresponding to three readings of one kernel. This file instantiates the reciprocal_disarmament_reading (epsilon 0.72; NWS nominally in the victim set via the binding-Article-VI premise; NNWS coalition holds normative leverage; the verification gap is structural injustice). The oligopoly_enforcement_reading sibling carries a different epsilon profile (NWS as near-pure beneficiaries, Article VI aspirational, extraction concentrated on would-be proliferators), and the withdrawal_sovereignty_reading sibling shifts the victim set toward states facing acute security threats with obligations rendered conditional. Each story has its own stable epsilon, beneficiaries, and victims; they are linked here and in their own files via affects_constraints. The upstream edge to tpnw_prohibition_regime records that this reading's logic (unfulfilled reciprocity justifies organized NNWS action) is the causal parent of the parallel prohibition treaty.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
