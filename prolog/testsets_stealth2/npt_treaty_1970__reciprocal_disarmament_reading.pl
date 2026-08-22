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
 *   constraint_id: npt_treaty_1970__reciprocal_disarmament_reading
 *   human_readable: NPT Reciprocal Disarmament Reading: Article VI as Binding Bargain
 *   domain: international_law/security/regime_theory
 *
 * SUMMARY:
 *   This story instantiates one reading of the NPT kernel cleanly: the
 *   reciprocal-disarmament reading holds Article VI as a binding legal
 *   obligation carrying temporal urgency, and reads the treaty as one
 *   reciprocal bargain — non-nuclear parties forgo weapons and accept
 *   safeguards; the five recognized nuclear-armed states pursue good-faith
 *   disarmament negotiations, an obligation the ICJ unanimously held in 1996
 *   extends to bringing negotiations to a conclusion. Assessed by this
 *   reading's own lights, the standing arrangement — the regime as it has
 *   actually operated from entry into force to the present — is a
 *   coordination structure with real collective value (the 1960s
 *   proliferation cascade was frozen; more states abandoned weapons programs
 *   than acquired weapons) fused with asymmetric extraction: the horizontal
 *   side is verified with escalating rigor while the vertical side carries no
 *   verification machinery at all, and the disarmament side of the bargain
 *   remains unperformed after five decades of arsenal retention and
 *   modernization. Epsilon's referent is the standing regime, not the
 *   compliant regime this reading advocates. Under this reading,
 *   nuclear-weapon-state strategic autonomy — modernization programs, force
 *   posture — is what Article VI legitimately constrains; the enforcement gap
 *   is what keeps the treaty's intended targets on the beneficiary side of
 *   the ledger. That structural delta is carried in the omega variables. The
 *   claimed type (tangled_rope) and the metrics are authored independently:
 *   the claim states the reading's structural judgment; the metrics state the
 *   regime's descriptive operation.
 *
 * KEY AGENTS:
 *   - nuclear_weapon_states: agenda-setting beneficiary (institutional/arbitrage) — retain and modernize arsenals, control enforcement through the Security Council veto, collect the proliferation freeze; the disarmament side of their bargain remains unperformed and unverified
 *   - compliant_nnws: primary payer (organized/constrained) — forgo weapons, accept safeguards, bear restraint costs; the reciprocity they were promised is undelivered
 *   - extended_deterrence_dependents: beneficiary with payer costs (powerful/identity_locked) — collect the umbrella that substitutes for disarmament; their non-nuclear identity is fused with alliance commitments
 *   - nnws_disarmament_coalition: payer with normative leverage (organized/constrained) — presses Article VI as a live legal claim through Review Conferences, the ICJ, and the prohibition treaty
 *   - would_be_proliferators: excluded (powerful/arbitrage) — armed outside the treaty, object to the codified hierarchy, hold no seat in its governance
 *   - iaea_safeguards_system: agenda-setter for the horizontal machinery (institutional/constrained) — verifies non-nuclear parties with escalating rigor; holds no Article VI mandate
 *   - civil_society_disarmament_movements: analytical observer (moderate/analytical) — documents humanitarian consequences and the compliance record
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_treaty_1970__reciprocal_disarmament_reading, 0.7).
domain_priors:suppression_score(npt_treaty_1970__reciprocal_disarmament_reading, 0.6).
domain_priors:theater_ratio(npt_treaty_1970__reciprocal_disarmament_reading, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_treaty_1970__reciprocal_disarmament_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(npt_treaty_1970__reciprocal_disarmament_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(npt_treaty_1970__reciprocal_disarmament_reading, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_treaty_1970__reciprocal_disarmament_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(npt_treaty_1970__reciprocal_disarmament_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_treaty_1970__reciprocal_disarmament_reading, tangled_rope).
narrative_ontology:human_readable(npt_treaty_1970__reciprocal_disarmament_reading, "NPT Reciprocal Disarmament Reading: Article VI as Binding Bargain").
narrative_ontology:topic_domain(npt_treaty_1970__reciprocal_disarmament_reading, "international_law/security/regime_theory").

domain_priors:requires_active_enforcement(npt_treaty_1970__reciprocal_disarmament_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_treaty_1970__reciprocal_disarmament_reading, '3ce3347a-31be-448d-85fd-dab32c1b76cc').
narrative_ontology:cs_kernel_codification('3ce3347a-31be-448d-85fd-dab32c1b76cc', fixed_text).
narrative_ontology:cs_authority_grounding('3ce3347a-31be-448d-85fd-dab32c1b76cc', extraction).
narrative_ontology:cs_interpretation_layer_present('3ce3347a-31be-448d-85fd-dab32c1b76cc').
narrative_ontology:cs_reading_relation('3ce3347a-31be-448d-85fd-dab32c1b76cc', npt_treaty_1970__oligopoly_enforcement_reading, coexists_with).
narrative_ontology:cs_reading_relation('3ce3347a-31be-448d-85fd-dab32c1b76cc', npt_treaty_1970__withdrawal_sovereignty_reading, influences).
narrative_ontology:cs_axiom('3ce3347a-31be-448d-85fd-dab32c1b76cc', foundational, article_vi_binding_legal_obligation).
narrative_ontology:cs_axiom_status(article_vi_binding_legal_obligation, holdable).
narrative_ontology:cs_axiom_grounding('3ce3347a-31be-448d-85fd-dab32c1b76cc', article_vi_binding_legal_obligation, conventional).
narrative_ontology:cs_axiom('3ce3347a-31be-448d-85fd-dab32c1b76cc', foundational, horizontal_compliance_conditional_on_vertical_performance).
narrative_ontology:cs_axiom_status(horizontal_compliance_conditional_on_vertical_performance, holdable).
narrative_ontology:cs_axiom_grounding('3ce3347a-31be-448d-85fd-dab32c1b76cc', horizontal_compliance_conditional_on_vertical_performance, deontological).
narrative_ontology:cs_reference_frame('3ce3347a-31be-448d-85fd-dab32c1b76cc', binding_reciprocal_bargain).
narrative_ontology:cs_drift_state('3ce3347a-31be-448d-85fd-dab32c1b76cc', post_tpnw_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('3ce3347a-31be-448d-85fd-dab32c1b76cc', '').
narrative_ontology:cs_kernel_id(npt_treaty_1970__reciprocal_disarmament_reading, npt_treaty_1970).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_treaty_1970__reciprocal_disarmament_reading, nuclear_weapon_states).
narrative_ontology:constraint_beneficiary(npt_treaty_1970__reciprocal_disarmament_reading, extended_deterrence_dependents).
narrative_ontology:constraint_victim(npt_treaty_1970__reciprocal_disarmament_reading, compliant_nnws).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(npt_treaty_1970__reciprocal_disarmament_reading, extended_deterrence_dependents).
narrative_ontology:constraint_victim(npt_treaty_1970__reciprocal_disarmament_reading, nnws_disarmament_coalition).
narrative_ontology:constraint_vindicates(npt_treaty_1970__reciprocal_disarmament_reading, nuclear_taboo_norm).
narrative_ontology:constraint_vindicates(npt_treaty_1970__reciprocal_disarmament_reading, horizontal_nonproliferation_norm).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The five states the treaty recognizes as nuclear-weapon states. They retain arsenals and run modernization programs across the whole interval; they anchor the regime's enforcement through the Security Council veto and set interpretive practice through their statements and Review Conference positions. They receive the proliferation freeze that stabilizes their strategic position and the legitimacy of their arsenals; the disarmament negotiations they committed to pursue remain unopened in the form the bargain described, and no verification machinery examines their programs. Their relationship to the regime's obligations is theirs to define: they can reinterpret, delay, or veto.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__reciprocal_disarmament_reading, nuclear_weapon_states, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(npt_treaty_1970__reciprocal_disarmament_reading, nuclear_weapon_states, beneficiary).

% The large majority of treaty parties. They forgo nuclear weapons, accept full-scope safeguards on their nuclear activities, and bear the security cost of unilateral restraint, including exposure to armed neighbors inside and outside the treaty. In return they receive peaceful-cooperation benefits under Article IV, negative security assurances of varying legal weight, and the disarmament commitment whose non-performance defines this reading's account of the record. Leaving is priced: withdrawal invites sanctions and isolation, as the one exiting party's experience shows.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__reciprocal_disarmament_reading, compliant_nnws, payer,
    organized, generational, constrained, global).

% Non-nuclear allies that forgo their own weapons while receiving security guarantees from nuclear patrons. They collect extended deterrence, the security substitute the patrons provide in place of disarmament, and their own non-nuclear commitments are bound up with alliance identity: nuclearization would mean breaking the alliance that constitutes their security identity, not merely paying a price. They defend the patrons' positions in regime forums while hosting the arrangements (nuclear sharing, forward deployments) that the disarmament side of the bargain would eventually call into question.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__reciprocal_disarmament_reading, extended_deterrence_dependents, beneficiary,
    powerful, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(npt_treaty_1970__reciprocal_disarmament_reading, extended_deterrence_dependents, payer).

% The bloc of non-nuclear parties — Non-Aligned Movement members, the New Agenda Coalition, humanitarian-initiative states — that treats the disarmament commitment as a live legal claim rather than a satisfied promise. They press it through Review Conferences, the ICJ's advisory jurisdiction, and the 2017 negotiations that produced a prohibition treaty the nuclear-armed states boycott. They bear the same restraint costs as other compliant parties; their leverage is normative and coalition-based, exercised through majorities in forums the armed states cannot fully control.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__reciprocal_disarmament_reading, nnws_disarmament_coalition, payer,
    organized, generational, constrained, global).

% States that acquired nuclear weapons outside the treaty — never joining, or joining and withdrawing before testing. They object that the bargain entrenches a permanent hierarchy of the armed and the unarmed, and their existence is cited by every seat: by patrons as proof the treaty needs enforcement, by the coalition as proof the armed states will not disarm, by the armed states as proof restraint is naive. They hold no seat in the treaty's governance; the regime's sanctions and export-control machinery is aimed partly at them.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__reciprocal_disarmament_reading, would_be_proliferators, excluded,
    powerful, generational, arbitrage, regional).

% The verification body that administers safeguards on non-nuclear parties' nuclear activities. Its horizontal mandate has deepened repeatedly across the interval — strengthened safeguards after the Iraq discovery, the Additional Protocol, Security Council reporting duties. It holds no mandate over the disarmament side: no protocol exists by which it could verify arsenal reductions, and it administers that asymmetry without setting its terms.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__reciprocal_disarmament_reading, iaea_safeguards_system, agenda_setter,
    institutional, generational, constrained, global).

% Campaigns, NGOs, and academic networks — the humanitarian initiative and the coalition behind the prohibition treaty — that document the humanitarian consequences of nuclear weapons and compile the compliance record. They supply the legal and empirical analysis the coalition seats use, convene the forums where the disarmament claim is kept live, and observe the regime without holding party rights.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__reciprocal_disarmament_reading, civil_society_disarmament_movements, observer,
    moderate, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(npt_treaty_1970__reciprocal_disarmament_reading, nuclear_weapon_states).
narrative_ontology:fixing_cost_class(npt_treaty_1970__reciprocal_disarmament_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the proliferation-cascade collective-action problem: mutual restraint among latent-capable states is made credible and verifiable through universal safeguards and export controls, so no state must arm because its neighbor armed; and it anchors peaceful nuclear cooperation (Article IV) under assurance against diversion to weapons.
% TRANSFER_FUNCTION: Moves defense options from non-nuclear parties into the collective restraint — foregone weapons capability and intrusive verification of their nuclear activities — and moves legitimacy and strategic stability to the five recognized arsenals; in the other direction it moves promises (disarmament negotiations, peaceful cooperation, negative security assurances) and, for allied states, extended deterrence.
% ABSENT_VOICES: The armed states outside the treaty would object that the bargain codifies a permanent hierarchy, but they hold no seat in its governance. The publics of nuclear-armed states — who bear the fiscal cost and the risk of arsenals they cannot vote on — are unrepresented at Review Conferences. Future generations exposed to the risk of use have no seat anywhere in the process.
% DISAPPEARANCE_RATIONALE: Without the treaty's verification and normative architecture, latent-capable states would face immediate pressure to hedge or weaponize; export controls and safeguards would lose their legal anchor; the prohibition- and test-ban architectures built on the NPT's legitimacy would unravel; and the cascade the bargain froze would resume within a decade.
% FOUNDING_PROBLEM: The 1960s proliferation cascade: intelligence projections anticipated dozens of nuclear-armed states within a decade or two. The bargain froze the existing five while capping the cascade, exchanging non-nuclear restraint for disarmament commitments and peaceful cooperation — a package struck in the Eighteen-Nation Disarmament Committee and opened for signature in 1968.
% FOUNDING_PROBLEM_CORROBORATION: The cascade problem and the bargain's reciprocal structure are attested from outside the nuclear-weapon-state beneficiary set: the ICJ's 1996 advisory opinion (unanimous that Article VI imposes an obligation to pursue and bring to a conclusion negotiations on nuclear disarmament), the 1968 negotiating record, and the 122-state conference that negotiated the prohibition treaty. The nuclear-weapon states attest the cascade problem's persistence but deny the reciprocal reading of the bargain — their attestation on the disarmament side is uncorroborated by performance and contradicted by the modernization record.
narrative_ontology:disappearance_verdict(npt_treaty_1970__reciprocal_disarmament_reading, world_rearranges).
narrative_ontology:founding_problem_status(npt_treaty_1970__reciprocal_disarmament_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_treaty_1970__reciprocal_disarmament_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(npt_treaty_1970__reciprocal_disarmament_reading, 'none', 1).
narrative_ontology:epsilon_provenance(npt_treaty_1970__reciprocal_disarmament_reading, 0.7, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is authored at 0.70 (the interval's end state): non-nuclear parties pay the bargain's price — foregone defense options, intrusive verification of their nuclear activities, restraint costs — while the vertical side is unperformed and, by design, unverifiable; the freeze itself entrenches the armed/unarmed hierarchy the bargain was meant to dissolve. Suppression (0.60) is structural, not interpersonal: safeguards, export controls, Security Council sanctions, and withdrawal penalties suppress horizontal movement, and the measurement series shows that machinery ratcheting upward (0.30 to 0.60) across the interval — while the vertical side acquires no enforcement machinery at all. That asymmetry, not the level, is this reading's central structural fact, which is why suppression_requirement is tracked here: the story specifically traces enforcement-capacity change on one side only. Theater (0.50 at end) rises from 0.20: Review Conference consensus documents, categorical-review language, and nuclear-weapon-state disarmament statements increasingly substitute interpretive performance for disarmament performance — Goodhart drift on consensus as the metric of regime health — while safeguards verification remains functionally real. Accessibility_collapse (0.62): alternatives are substantially foreclosed (withdrawal is sanctionable and stigmatized) but not fully — one party has exited and tested, three armed states remain outside, and a rival forum now exists. Resistance (0.58): the humanitarian initiative and the 2017 prohibition-treaty negotiations are organized coalition resistance from inside the payer set; the armed-state side resists the binding reading itself. All three series share one time grid (t = 0, 10, 20, 25, 30, 40, 45, 55 years from entry into force).
 *
 * PERSPECTIVAL GAP:
 *   The seats compute different constraints from the same text. From the nuclear-weapon-state seat, the regime is the security architecture it anchors and administers: coordination it built, with the disarmament article as a long-horizon aspiration — low effective extraction, near-arbitrage exit. From the compliant non-nuclear seat, the same structure operates as a contract performed on one side only: high effective extraction, exit priced at sanctions and isolation. The coalition seat adds a third computation: a breached bargain whose breach is legally cognizable — leverage, not helplessness. Extended-deterrence dependents compute subsidized security: they forgo their own weapons but collect the umbrella, and their non-nuclear identity is fused with the alliance that provides it. Same-level differentiation: compliant_nnws and extended_deterrence_dependents hold the same formal treaty position and comparable power, but differ in exit (constrained vs identity_locked) and in what they collect (a promise vs a guarantee) — constraint-specific factors, not global standing, drive the divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (nuclear_weapon_states, extended_deterrence_dependents) and victim declarations (compliant_nnws) drive the derivation: the nuclear-weapon states sit near the beneficiary end — they collect the freeze, face no verification of the vertical side, and can veto enforcement (arbitrage-grade exit); compliant non-nuclear parties sit near the target end — they pay the bargain price with only constrained exit; the dependents sit between, collecting the umbrella while bearing the forgoing cost. The reading's distinctive structural move is that this beneficiary-side placement of the nuclear-weapon states is itself the injustice: Article VI as binding would make their strategic autonomy the constraint's proper target set, and the enforcement gap is what keeps them off it. That delta is carried as an omega rather than a directionality override because the override surface is keyed to power atoms and cannot separate the nuclear-weapon-state beneficiary seat from other institutional actors at the same power level.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled-rope claim prevents two mislabelings. Reading the regime as pure coordination (the practical effect of the oligopoly reading) erases the payer side — the non-nuclear compliance that subsidizes the arrangement — and launders a breached bargain into a public good. Reading it as pure extraction erases the real function: the cascade was frozen, safeguards work, most non-nuclear parties genuinely prefer restraint, and the arrangement's collapse would rearrange the world catastrophically. The rope half and the extraction half are the same structure, which is why active enforcement is required to hold it and why the payer set is identifiable. On mandatrophy: the founding problem (the cascade) is live, so the regime is not an inertial remnant maintained by performance alone; but the bargain's vertical half has drifted from performance to performance-talk — the theater series (0.20 to 0.50) tracks that substitution — and the 1995 indefinite extension removed the last structural moment at which the bargain's terms would have been renegotiated. Founding-problem status is authored contested rather than dead: the horizontal problem persists, and the parties dispute whether the problem the arrangement was built to solve includes the arsenals themselves.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'This constraint is one reading of the NPT kernel (npt_treaty_1970). How would the sibling readings — oligopoly-enforcement (Articles I-II binding, Article VI aspirational) and withdrawal-sovereignty (obligations contingent, Article X exit legitimate) — restructure the victim set, directionality, and epsilon?',
    'Convergence of authoritative interpretation: Review Conference consensus language, ICJ treatment, withdrawal-dispute jurisprudence, or nuclear-weapon-state practice entrenching one reading as operative.',
    'Under the oligopoly reading, nuclear_weapon_states exit the victim set and measured extraction drops toward coordination cost; under the withdrawal-sovereignty reading, non-nuclear compliance becomes contingent and the bargain dissolves into conditional commitments, raising suppression and lowering coordination value.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Which reading of the NPT kernel governs — determines the victim set and epsilon.').

omega_variable(
    article_vi_verification_gap,
    'Is the absence of any Article VI verification machinery an implementation detail or the load-bearing mechanism of the extraction — would verified vertical nonproliferation be feasible if the nuclear-weapon states sought it?',
    'Feasibility analysis of verified fissile-material and warhead-dismantlement regimes (IPNDV work, FMCT verification proposals) and nuclear-weapon-state responses to verification offers.',
    'If verification is feasible and withheld, the gap is deliberate capture and effective extraction on compliant non-nuclear parties rises; if genuinely infeasible, part of the gap is technical and the structural-injustice claim narrows.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(article_vi_verification_gap, empirical, 'Whether the enforcement gap is technical or structural.').

omega_variable(
    nws_constraint_effect,
    'Under the binding reading, nuclear-weapon-state strategic autonomy — arsenal modernization, doctrine, force posture — is what Article VI legitimately constrains. Does the reading''s normative pressure actually constrain nuclear-weapon-state behavior, or does it remain purely hortatory in effect?',
    'Behavioral tracing: nuclear-weapon-state responses to prohibition-treaty stigmatization (doctrinal shifts, warhead-cap announcements, sole-purpose debates, divestment pressure) set against modernization budgets.',
    'If the binding reading constrains their behavior, nuclear-weapon states acquire nonzero target-directionality and the extraction profile becomes genuinely two-sided; if not, they remain pure beneficiaries and the reading''s leverage claim rests entirely on pressure from the non-nuclear side.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nws_constraint_effect, empirical, 'Whether binding-reading pressure constrains nuclear-weapon-state strategic autonomy in practice.').

omega_variable(
    temporal_urgency_operationalization,
    'The reading asserts temporal urgency, but the treaty text and the ICJ opinion specify no deadline. What timeline would satisfy the obligation — and does urgency without an operationalized date make the binding reading unenforceable?',
    'Negotiated milestones (humanitarian-pledge timelines, prohibition-treaty Article 4 processes, fissile-material cutoff negotiations) or doctrinal acceptance of a reasonable-time standard.',
    'Without an operationalized timeline, breach is unmeasurable and the extraction persists behind unfulfillable vagueness; with one, non-performance becomes a determinable breach and the coalition''s normative leverage converts into a legal claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(temporal_urgency_operationalization, conceptual, 'Whether temporal urgency can be operationalized into measurable breach.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_treaty_1970__reciprocal_disarmament_reading, 0, 55).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt_reciprocal_tr_t0, npt_treaty_1970__reciprocal_disarmament_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement_basis(npt_reciprocal_tr_t0, observed).
narrative_ontology:measurement(npt_reciprocal_tr_t10, npt_treaty_1970__reciprocal_disarmament_reading, theater_ratio, 10, 0.25).
narrative_ontology:measurement_basis(npt_reciprocal_tr_t10, observed).
narrative_ontology:measurement(npt_reciprocal_tr_t20, npt_treaty_1970__reciprocal_disarmament_reading, theater_ratio, 20, 0.28).
narrative_ontology:measurement_basis(npt_reciprocal_tr_t20, observed).
narrative_ontology:measurement(npt_reciprocal_tr_t25, npt_treaty_1970__reciprocal_disarmament_reading, theater_ratio, 25, 0.38).
narrative_ontology:measurement_basis(npt_reciprocal_tr_t25, observed).
narrative_ontology:measurement(npt_reciprocal_tr_t30, npt_treaty_1970__reciprocal_disarmament_reading, theater_ratio, 30, 0.42).
narrative_ontology:measurement_basis(npt_reciprocal_tr_t30, observed).
narrative_ontology:measurement(npt_reciprocal_tr_t40, npt_treaty_1970__reciprocal_disarmament_reading, theater_ratio, 40, 0.44).
narrative_ontology:measurement_basis(npt_reciprocal_tr_t40, observed).
narrative_ontology:measurement(npt_reciprocal_tr_t45, npt_treaty_1970__reciprocal_disarmament_reading, theater_ratio, 45, 0.48).
narrative_ontology:measurement_basis(npt_reciprocal_tr_t45, observed).
narrative_ontology:measurement(npt_reciprocal_tr_t55, npt_treaty_1970__reciprocal_disarmament_reading, theater_ratio, 55, 0.5).
narrative_ontology:measurement_basis(npt_reciprocal_tr_t55, observed).

% Extraction over time
narrative_ontology:measurement(npt_reciprocal_be_t0, npt_treaty_1970__reciprocal_disarmament_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement_basis(npt_reciprocal_be_t0, observed).
narrative_ontology:measurement(npt_reciprocal_be_t10, npt_treaty_1970__reciprocal_disarmament_reading, base_extractiveness, 10, 0.6).
narrative_ontology:measurement_basis(npt_reciprocal_be_t10, observed).
narrative_ontology:measurement(npt_reciprocal_be_t20, npt_treaty_1970__reciprocal_disarmament_reading, base_extractiveness, 20, 0.55).
narrative_ontology:measurement_basis(npt_reciprocal_be_t20, observed).
narrative_ontology:measurement(npt_reciprocal_be_t25, npt_treaty_1970__reciprocal_disarmament_reading, base_extractiveness, 25, 0.6).
narrative_ontology:measurement_basis(npt_reciprocal_be_t25, observed).
narrative_ontology:measurement(npt_reciprocal_be_t30, npt_treaty_1970__reciprocal_disarmament_reading, base_extractiveness, 30, 0.63).
narrative_ontology:measurement_basis(npt_reciprocal_be_t30, observed).
narrative_ontology:measurement(npt_reciprocal_be_t40, npt_treaty_1970__reciprocal_disarmament_reading, base_extractiveness, 40, 0.62).
narrative_ontology:measurement_basis(npt_reciprocal_be_t40, observed).
narrative_ontology:measurement(npt_reciprocal_be_t45, npt_treaty_1970__reciprocal_disarmament_reading, base_extractiveness, 45, 0.67).
narrative_ontology:measurement_basis(npt_reciprocal_be_t45, observed).
narrative_ontology:measurement(npt_reciprocal_be_t55, npt_treaty_1970__reciprocal_disarmament_reading, base_extractiveness, 55, 0.7).
narrative_ontology:measurement_basis(npt_reciprocal_be_t55, observed).

% Suppression requirement over time
narrative_ontology:measurement(npt_reciprocal_su_t0, npt_treaty_1970__reciprocal_disarmament_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement_basis(npt_reciprocal_su_t0, observed).
narrative_ontology:measurement(npt_reciprocal_su_t10, npt_treaty_1970__reciprocal_disarmament_reading, suppression_requirement, 10, 0.34).
narrative_ontology:measurement_basis(npt_reciprocal_su_t10, observed).
narrative_ontology:measurement(npt_reciprocal_su_t20, npt_treaty_1970__reciprocal_disarmament_reading, suppression_requirement, 20, 0.38).
narrative_ontology:measurement_basis(npt_reciprocal_su_t20, observed).
narrative_ontology:measurement(npt_reciprocal_su_t25, npt_treaty_1970__reciprocal_disarmament_reading, suppression_requirement, 25, 0.46).
narrative_ontology:measurement_basis(npt_reciprocal_su_t25, observed).
narrative_ontology:measurement(npt_reciprocal_su_t30, npt_treaty_1970__reciprocal_disarmament_reading, suppression_requirement, 30, 0.5).
narrative_ontology:measurement_basis(npt_reciprocal_su_t30, observed).
narrative_ontology:measurement(npt_reciprocal_su_t40, npt_treaty_1970__reciprocal_disarmament_reading, suppression_requirement, 40, 0.55).
narrative_ontology:measurement_basis(npt_reciprocal_su_t40, observed).
narrative_ontology:measurement(npt_reciprocal_su_t45, npt_treaty_1970__reciprocal_disarmament_reading, suppression_requirement, 45, 0.58).
narrative_ontology:measurement_basis(npt_reciprocal_su_t45, observed).
narrative_ontology:measurement(npt_reciprocal_su_t55, npt_treaty_1970__reciprocal_disarmament_reading, suppression_requirement, 55, 0.6).
narrative_ontology:measurement_basis(npt_reciprocal_su_t55, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_treaty_1970__reciprocal_disarmament_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(npt_treaty_1970__reciprocal_disarmament_reading, npt_treaty_1970__oligopoly_enforcement_reading).
narrative_ontology:affects_constraint(npt_treaty_1970__reciprocal_disarmament_reading, npt_treaty_1970__withdrawal_sovereignty_reading).
narrative_ontology:affects_constraint(npt_treaty_1970__reciprocal_disarmament_reading, tpnw_prohibition_regime).
narrative_ontology:affects_constraint(npt_treaty_1970__reciprocal_disarmament_reading, iaea_additional_protocol_regime).
narrative_ontology:affects_constraint(npt_treaty_1970__reciprocal_disarmament_reading, nato_nuclear_sharing_arrangements).
narrative_ontology:affects_constraint(npt_treaty_1970__reciprocal_disarmament_reading, dprk_withdrawal_precedent).

% DUAL FORMULATION NOTE:
% The natural-language label 'the NPT' covers three structurally distinct constraints — readings of one kernel, npt_treaty_1970. The oligopoly-enforcement reading (binding core = horizontal freeze; low-moderate epsilon; victims = would-be proliferators), this reciprocal-disarmament reading (Article VI binding with temporal urgency; high epsilon; victims = compliant non-nuclear parties), and the withdrawal-sovereignty reading (obligations contingent on the security environment; different persistence structure). Each is a separate story with its own epsilon, beneficiaries, and victims, linked here and via cs_structure.reading_relations rather than merged. The upstream oligopoly reading is cited as evidence in downstream interpretive contests; this reading supplies the legitimacy test on which the withdrawal reading's material-breach claims draw.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
