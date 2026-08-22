% ============================================================================
% CONSTRAINT STORY: qwerty_persistence_mechanism__lock_in_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_qwerty_persistence_mechanism__lock_in_reading, []).

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
 *   constraint_id: qwerty_persistence_mechanism__lock_in_reading
 *   human_readable: QWERTY Lock-In via Path-Dependent Coordination Failure
 *   domain: economic/technological/institutional
 *
 * SUMMARY:
 *   QWERTY keyboard layout persists globally despite documented ergonomic and
 *   speed inferiority to alternatives (Dvorak, Colemak). This constraint
 *   story instantiates the LOCK-IN READING: QWERTY persists through
 *   path-dependent coordination failure, not through active beneficiary
 *   extraction or technical adequacy. The original choice was contingent and
 *   mechanically driven (typewriter design constraints of the 1870s);
 *   alternatives became viable but faced a coordination problem — switching
 *   benefits accrue only if enough users switch together, so each actor
 *   individually prefers the status quo despite collective suboptimality.
 *   This reading differs from the extraction reading (manufacturers actively
 *   defended QWERTY to protect training investments and market lock-in) and
 *   the naturalization reading (QWERTY won fair competition and is adequate).
 *   The lock-in reading attributes persistence to structural coordination
 *   failure, not to beneficiary extraction or settled sufficiency.
 *
 * KEY AGENTS:
 *   - installed_base_users: 1.4 billion touch-typists trained on QWERTY; muscle memory identity-fused; benefit from coordination, bear ergonomic cost
 *   - keyboard_manufacturers: produce QWERTY because demand is shaped by the installed base; benefit from standardization, do not actively extract
 *   - alternative_standard_advocates: powerless to overcome coordination inertia; bear all switching costs if they pioneer; research shows alternatives superior but market does not reward them
 *   - new_system_adopters: children and new professionals learning to type; must learn QWERTY despite inferiority, trapped by coordination on an inherited standard
 *   - economic_historians: analytical seat; study whether QWERTY is an instance of market failure without deliberate beneficiary defense
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qwerty_persistence_mechanism__lock_in_reading, 0.62).
domain_priors:suppression_score(qwerty_persistence_mechanism__lock_in_reading, 0.41).
domain_priors:theater_ratio(qwerty_persistence_mechanism__lock_in_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__lock_in_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__lock_in_reading, suppression_requirement, 0.41).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__lock_in_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__lock_in_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__lock_in_reading, resistance, 0.44).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qwerty_persistence_mechanism__lock_in_reading, tangled_rope).
narrative_ontology:human_readable(qwerty_persistence_mechanism__lock_in_reading, "QWERTY Lock-In via Path-Dependent Coordination Failure").
narrative_ontology:topic_domain(qwerty_persistence_mechanism__lock_in_reading, "economic/technological/institutional").

domain_priors:requires_active_enforcement(qwerty_persistence_mechanism__lock_in_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qwerty_persistence_mechanism__lock_in_reading, '18f44906-9519-46b1-a293-0a055efdae7d').
narrative_ontology:cs_kernel_codification('18f44906-9519-46b1-a293-0a055efdae7d', distributed).
narrative_ontology:cs_authority_grounding('18f44906-9519-46b1-a293-0a055efdae7d', diffuse_epistemic).
narrative_ontology:cs_reading_relation('18f44906-9519-46b1-a293-0a055efdae7d', qwerty_persistence_mechanism__extraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('18f44906-9519-46b1-a293-0a055efdae7d', qwerty_persistence_mechanism__naturalization_reading, coexists_with).
narrative_ontology:cs_axiom('18f44906-9519-46b1-a293-0a055efdae7d', foundational, coordination_failure_without_active_extractor).
narrative_ontology:cs_axiom_status(coordination_failure_without_active_extractor, holdable).
narrative_ontology:cs_axiom_grounding('18f44906-9519-46b1-a293-0a055efdae7d', coordination_failure_without_active_extractor, empirically_contingent).
narrative_ontology:cs_axiom('18f44906-9519-46b1-a293-0a055efdae7d', foundational, path_dependence_contingent_not_inevitable).
narrative_ontology:cs_axiom_status(path_dependence_contingent_not_inevitable, holdable).
narrative_ontology:cs_axiom_grounding('18f44906-9519-46b1-a293-0a055efdae7d', path_dependence_contingent_not_inevitable, empirically_contingent).
narrative_ontology:cs_reference_frame('18f44906-9519-46b1-a293-0a055efdae7d', single_standard_equilibrium).
narrative_ontology:cs_drift_state('18f44906-9519-46b1-a293-0a055efdae7d', contemporary_digital_era, gap(stable, minor, false)).
narrative_ontology:cs_created_at('18f44906-9519-46b1-a293-0a055efdae7d', '').
narrative_ontology:cs_kernel_id(qwerty_persistence_mechanism__lock_in_reading, qwerty_persistence_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(qwerty_persistence_mechanism__lock_in_reading, installed_base_users).
narrative_ontology:constraint_beneficiary(qwerty_persistence_mechanism__lock_in_reading, keyboard_manufacturers).
narrative_ontology:constraint_beneficiary(qwerty_persistence_mechanism__lock_in_reading, typist_trainers).
narrative_ontology:constraint_victim(qwerty_persistence_mechanism__lock_in_reading, alternative_standard_advocates).
narrative_ontology:constraint_victim(qwerty_persistence_mechanism__lock_in_reading, new_system_adopters).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(qwerty_persistence_mechanism__lock_in_reading, installed_base_users).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Have invested decades learning QWERTY touch-typing; their muscle memory and professional identity are fused with the layout. They benefit from the coordination value (everyone types QWERTY; no switching costs for shared devices) but also carry the cost of suboptimal ergonomics and speed constraints. Exit is cognitively framed as impossible despite being technically available — retraining would cost time and professional credibility within a QWERTY-normalized workplace.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__lock_in_reading, installed_base_users, beneficiary,
    moderate, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(qwerty_persistence_mechanism__lock_in_reading, installed_base_users, payer).

% Benefit from coordination on a single standard: no need to produce and inventory multiple layouts or negotiate compatibility. The installed base's learned behavior locks in their production process. They do not actively extract from QWERTY; the arrangement simply reduces their costs. Could arbitrage by producing alternative-layout keyboards at competitive prices, but do not, because the demand signal is shaped by the lock-in itself.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__lock_in_reading, keyboard_manufacturers, beneficiary,
    organized, generational, arbitrage, global).

% Curriculum, textbooks, and professional teaching materials are standardized on QWERTY. Teaching anything else requires creating new materials, retraining themselves, and overcoming student resistance from their typed-before experience. They benefit from the coordination (no need to teach multiple standards) and have structurally weak incentive to switch, though they could collectively pressure for alternatives.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__lock_in_reading, typist_trainers, beneficiary,
    moderate, biographical, constrained, global).

% Include ergonomists, efficiency researchers, and small communities that have developed superior layouts (Dvorak, Colemak, Workman). They advocate for switching despite overwhelming coordination inertia. Switching costs are borne entirely by early adopters: retraining required, isolation from shared keyboards, material and cognitive investment with no guarantee of mainstream adoption. The more superior their alternative, the clearer the social loss — but the coordination failure is structural, not individual.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__lock_in_reading, alternative_standard_advocates, payer,
    powerless, biographical, trapped, global).

% Anyone learning to type in the post-QWERTY era: children, non-native typists, users entering the profession. They have no choice but to learn QWERTY regardless of its inferior ergonomics because every shared device, every training program, every professional expectation assumes QWERTY. The constraint operates as a coordinated standard they did not choose and cannot exit without bearing all switching costs alone.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__lock_in_reading, new_system_adopters, payer,
    powerless, immediate, trapped, global).

% Could design keyboards optimized for non-QWERTY layouts (ergonomic, language-specific, accessibility-focused) but do not, because the demand is shaped by the trained installed base. They would enter the market if lock-in broke, but the lock-in itself is what suppresses the market signal.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__lock_in_reading, computer_hardware_designers, excluded,
    organized, generational, constrained, global).

% Analyze whether QWERTY persists because of market failure (path-dependent lock-in with no individual beneficiary actively defending it) or because of deliberate beneficiary extraction. The reading they adopt changes the constraint's classification and the policy implications of breaking it.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__lock_in_reading, economic_historians, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(qwerty_persistence_mechanism__lock_in_reading, diffuse).
narrative_ontology:fixing_cost_class(qwerty_persistence_mechanism__lock_in_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the multi-party coordination problem of having one shared standard for keyboard layout. Every keyboard produced, every typing curriculum taught, every workplace tool configured assumes a single expected layout — switching costs for any party alone are prohibitive, but if everyone switched together, efficiency gains could be large.
% TRANSFER_FUNCTION: Moves the accumulated learning burden and ergonomic cost from the beneficiary seats (installed-base users, trainers, manufacturers who optimized for QWERTY) to the victim seats (new learners, alternative advocates, and the global economy in aggregate through suboptimal input speed and repetitive strain). The transfer is not rent — no party captures it — but a diffuse collective cost.
% ABSENT_VOICES: The alternative-standard community (Dvorak advocates, ergonomic researchers, accessibility specialists) has seats at the table but faces overwhelming coordination-inertia resistance. Participants who would benefit from switching if it could happen collectively (users with RSI, languages with non-Latin scripts, accessibility-optimized designs) are not organized as a constituency and do not appear in the decision structure.
% DISAPPEARANCE_RATIONALE: If QWERTY lock-in broke overnight (e.g., if a critical mass of devices shipped with remappable defaults or if a major platform defaulted to an alternative), new devices would gradually shift to ergonomically optimized layouts, training programs would diversify, and installed-base muscle memory would remain but cease to determine the standard for new entrants. The economy would reorganize around multiple competing layouts with emerging-user preference driving adoption.
% FOUNDING_PROBLEM: The telegraph, typewriter, and early computing were fragmented — manufacturers used different layouts, typists had to learn multiple standards or were locked to one device. QWERTY (or whichever layout) became the dominant standard through path-dependent adoption during the typewriter era (early mechanical constraints, network effects around training and shared devices), not through any clear technical superiority.
% FOUNDING_PROBLEM_CORROBORATION: Technology historians (David Liebowitz, Stephen Margolin, Paul David) attest that the original fragmentation problem is solved: QWERTY dominates globally. The question is now whether the standard persists because it is adequate (naturalization reading) or because lock-in prevents switching (lock-in reading, this one) or because incumbents defend it for rent (extraction reading). No party claims QWERTY was technically optimal even at the start; the historical consensus is that alternatives were viable but happened to lose the path-dependent race.
narrative_ontology:disappearance_verdict(qwerty_persistence_mechanism__lock_in_reading, world_rearranges).
narrative_ontology:founding_problem_status(qwerty_persistence_mechanism__lock_in_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qwerty_persistence_mechanism__lock_in_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(qwerty_persistence_mechanism__lock_in_reading, 'none', 1).
narrative_ontology:epsilon_provenance(qwerty_persistence_mechanism__lock_in_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(qwerty_persistence_mechanism__lock_in_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(qwerty_persistence_mechanism__lock_in_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(qwerty_persistence_mechanism__lock_in_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.62) because the global economy bears a measurable efficiency cost (typing speed reduction estimated at 5-10%, ergonomic injury burden, language-specific inefficiency), but this cost is NOT collected by any identifiable beneficiary — it accumulates as diffuse collective loss. Suppression is moderate (0.41) not because of active coercion but because the coordination-dependent lock-in makes alternatives inaccessible: every keyboard, every training program, every workplace tool assumes QWERTY; switching costs are borne entirely by pioneers with no guarantee of adoption. Theater is very low (0.12) because there is minimal performative maintenance — the standard is simply assumed and inherited, not defended through narrative or institutional theater. Accessibility collapse is high (0.78) because once the standard is established, alternatives are essentially unavailable at any individual's switching point; the collapse is structural coordination geometry, not enforced suppression. Resistance is moderate (0.44) because alternatives are advocated (Dvorak has had decades of research support, modern layouts like Colemak are empirically designed) but cannot overcome the coordination barrier. The measurement series shows stable metrics over a 120-year interval (0-120), reflecting the persistence of lock-in without accumulating extraction: no rent capture, no intensifying suppression, no rising theater — just coordination failure.
 *
 * DIRECTIONALITY LOGIC:
 *   In the lock-in reading, directionality is paradoxical: there is NO individual beneficiary extracting from the arrangement, yet collective suboptimality emerges. Installed-base users benefit from the coordination value (everyone types QWERTY) but bear the ergonomic cost, placing them near d=0.5 (symmetric). Keyboard manufacturers benefit costlessly from standardization (reduced SKU burden) without bearing any switching cost, placing them near d=0.0-0.2 (beneficiary, but weakly). Alternative advocates and new learners bear the efficiency cost and switching-cost entrapment without any compensating benefit, placing them near d=0.8-0.95 (target). The paradox is that the constraint persists even though no actor has sufficient power or motive to actively defend it — the mechanism is structural coordination geometry, not power asymmetry. An override is not needed because the structural data (beneficiaries without strong power, victims without exit) correctly derives the directionality; what the data reveals is that extraction without an active extractor is possible (a path-dependent institutional failure state). This reading's seat divergence is: from manufacturers' seat, the arrangement is pure coordination (low extraction, equilibrium); from new learners' seat, it is a coordination trap (moderate extraction, suboptimal equilibrium). The engine should compute these divergently.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (fragmented keyboard standards, competing layouts in the typewriter era) is DEAD: QWERTY dominates globally. The constraint persists not because the problem is still being solved but because the path-dependent solution locked in before alternatives could prove superior. This is mandatrophy in the lock-in reading: the original founding function (coordination on a single standard during fragmented market) is solved and no longer operates; what remains is inertia. The distinction from extraction reading is critical: in the extraction reading, beneficiaries MAINTAIN the constraint actively (it is not mandatrophy, it is deliberate rent defense). In the lock-in reading, the constraint is NOT maintained by any actor — it persists through structural default (no actor has enough power to shift to an alternative even if they wanted to). This is textbook mandatrophy: the constraint survives its function through coordination-barrier inertia. The policy implication: fixing extraction requires regulating beneficiaries; fixing lock-in requires solving the coordination problem (e.g., providing default-remappable keyboards, supporting training in alternatives at critical decision points).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    beneficiary_extraction_vs_coordination_failure,
    'Does QWERTY persist because incumbents actively defend it (extraction reading) or because the coordination barrier prevents switching despite no active defender (lock-in reading)?',
    'Historical evidence on manufacturer behavior: Did keyboard manufacturers invest in suppressing alternatives, lobby against alternative-friendly policies, or resist remappable hardware? Did training institutions deliberately exclude alternatives from curriculum? Or did they simply continue standard practice without deliberate defense?',
    'If extraction is verified, the constraint moves closer to snare (active rent-defense required for persistence). If coordination failure is verified, it stays tangled_rope (mixed coordination benefit and efficiency loss without intentional extraction). The policy remedy is entirely different: anti-monopoly enforcement vs. coordination problem-solving.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_extraction_vs_coordination_failure, empirical, 'Whether incumbent active defense or structural coordination failure drives persistence.').

omega_variable(
    switching_cost_distribution_asymmetry,
    'Are switching costs borne equally by all parties, or do they concentrate on a subset while others enjoy benefits without cost?',
    'Cost-benefit audit of switching to an alternative layout: What would current typists lose (retraining time, professional credibility, device incompatibility)? What would manufacturers gain/lose (reduced SKU complexity, potential market differentiation)? What would new learners gain/lose (no sunk cost, but still locked to QWERTY by installed base)?',
    'If costs are symmetric, QWERTY is a symmetric coordination problem with no extraction. If costs are asymmetric (early adopters bear all cost, incumbents gain free benefits), the constraint has distributive injustice even without beneficiary extraction (it is the lock-in structure itself that allocates harm).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(switching_cost_distribution_asymmetry, empirical, 'How switching costs distribute across the party set.').

omega_variable(
    keyboard_remappability_market_failure,
    'Why do hardware manufacturers not produce keyboards with easy remapping or defaults for alternative layouts, if demand for alternatives is genuine?',
    'Market investigation: Do Dvorak/Colemak keyboard sales exist? Are they priced as niche products or absent entirely? Have touchscreen devices (where remapping is costless) shifted layout adoption? If remappable hardware is technologically trivial but not produced, is the barrier demand-side (lock-in suppresses demand) or supply-side (manufacturers refuse)?',
    'If the market failure is in demand suppression (users do not ask for alternatives because QWERTY is the only thing they know how to type), this verifies the lock-in reading. If manufacturers actively refuse to produce alternatives despite demand, the extraction reading gains weight.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(keyboard_remappability_market_failure, empirical, 'Whether alternative-layout keyboards are unavailable due to demand suppression or supply-side refusal.').

omega_variable(
    naturalness_of_path_dependence_vs_constructed_lock_in,
    'Is the coordination problem natural (emerges inevitably from network effects) or constructed (contingent historical choices that could have been different)?',
    'Counterfactual history: Were there critical junctures where a different standard could have emerged? Did mechanical typewriter design FORCE QWERTY layout, or was QWERTY one viable among several layouts that happened to win by path-dependent accident? Did digital devices inherit QWERTY from inertia or from deliberate choice?',
    'If constructed contingency (could have been Dvorak or any other layout), the lock-in is not natural law but an institutional artifact that could theoretically be dismantled. If natural inevitability (network effects guarantee lock-in regardless of which layout wins), the constraint approaches mountain classification (any standard must lock in; QWERTY just happened to arrive first). Lock-in reading assumes CONTINGENT, not NATURAL, path dependence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(naturalness_of_path_dependence_vs_constructed_lock_in, conceptual, 'Whether the path-dependent lock-in is natural coordination geometry or contingent historical accident.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qwerty_persistence_mechanism__lock_in_reading, 0, 120).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qwer_tr_t0, qwerty_persistence_mechanism__lock_in_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(qwer_tr_t0, observed).
narrative_ontology:measurement(qwer_tr_t20, qwerty_persistence_mechanism__lock_in_reading, theater_ratio, 20, 0.09).
narrative_ontology:measurement_basis(qwer_tr_t20, observed).
narrative_ontology:measurement(qwer_tr_t40, qwerty_persistence_mechanism__lock_in_reading, theater_ratio, 40, 0.1).
narrative_ontology:measurement_basis(qwer_tr_t40, observed).
narrative_ontology:measurement(qwer_tr_t60, qwerty_persistence_mechanism__lock_in_reading, theater_ratio, 60, 0.11).
narrative_ontology:measurement_basis(qwer_tr_t60, observed).
narrative_ontology:measurement(qwer_tr_t80, qwerty_persistence_mechanism__lock_in_reading, theater_ratio, 80, 0.12).
narrative_ontology:measurement_basis(qwer_tr_t80, observed).
narrative_ontology:measurement(qwer_tr_t100, qwerty_persistence_mechanism__lock_in_reading, theater_ratio, 100, 0.12).
narrative_ontology:measurement_basis(qwer_tr_t100, observed).
narrative_ontology:measurement(qwer_tr_t120, qwerty_persistence_mechanism__lock_in_reading, theater_ratio, 120, 0.12).
narrative_ontology:measurement_basis(qwer_tr_t120, observed).

% Extraction over time
narrative_ontology:measurement(qwer_be_t0, qwerty_persistence_mechanism__lock_in_reading, base_extractiveness, 0, 0.58).
narrative_ontology:measurement_basis(qwer_be_t0, observed).
narrative_ontology:measurement(qwer_be_t20, qwerty_persistence_mechanism__lock_in_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement_basis(qwer_be_t20, observed).
narrative_ontology:measurement(qwer_be_t40, qwerty_persistence_mechanism__lock_in_reading, base_extractiveness, 40, 0.61).
narrative_ontology:measurement_basis(qwer_be_t40, observed).
narrative_ontology:measurement(qwer_be_t60, qwerty_persistence_mechanism__lock_in_reading, base_extractiveness, 60, 0.62).
narrative_ontology:measurement_basis(qwer_be_t60, observed).
narrative_ontology:measurement(qwer_be_t80, qwerty_persistence_mechanism__lock_in_reading, base_extractiveness, 80, 0.63).
narrative_ontology:measurement_basis(qwer_be_t80, observed).
narrative_ontology:measurement(qwer_be_t100, qwerty_persistence_mechanism__lock_in_reading, base_extractiveness, 100, 0.62).
narrative_ontology:measurement_basis(qwer_be_t100, observed).
narrative_ontology:measurement(qwer_be_t120, qwerty_persistence_mechanism__lock_in_reading, base_extractiveness, 120, 0.62).
narrative_ontology:measurement_basis(qwer_be_t120, observed).

% Suppression requirement over time
narrative_ontology:measurement(qwer_su_t0, qwerty_persistence_mechanism__lock_in_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement_basis(qwer_su_t0, observed).
narrative_ontology:measurement(qwer_su_t20, qwerty_persistence_mechanism__lock_in_reading, suppression_requirement, 20, 0.39).
narrative_ontology:measurement_basis(qwer_su_t20, observed).
narrative_ontology:measurement(qwer_su_t40, qwerty_persistence_mechanism__lock_in_reading, suppression_requirement, 40, 0.4).
narrative_ontology:measurement_basis(qwer_su_t40, observed).
narrative_ontology:measurement(qwer_su_t60, qwerty_persistence_mechanism__lock_in_reading, suppression_requirement, 60, 0.41).
narrative_ontology:measurement_basis(qwer_su_t60, observed).
narrative_ontology:measurement(qwer_su_t80, qwerty_persistence_mechanism__lock_in_reading, suppression_requirement, 80, 0.41).
narrative_ontology:measurement_basis(qwer_su_t80, observed).
narrative_ontology:measurement(qwer_su_t100, qwerty_persistence_mechanism__lock_in_reading, suppression_requirement, 100, 0.41).
narrative_ontology:measurement_basis(qwer_su_t100, observed).
narrative_ontology:measurement(qwer_su_t120, qwerty_persistence_mechanism__lock_in_reading, suppression_requirement, 120, 0.41).
narrative_ontology:measurement_basis(qwer_su_t120, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qwerty_persistence_mechanism__lock_in_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(qwerty_persistence_mechanism__lock_in_reading, 0.18).
narrative_ontology:affects_constraint(qwerty_persistence_mechanism__lock_in_reading, qwerty_persistence_mechanism__extraction_reading).
narrative_ontology:affects_constraint(qwerty_persistence_mechanism__lock_in_reading, qwerty_persistence_mechanism__naturalization_reading).

% DUAL FORMULATION NOTE:
% QWERTY persistence decomposes into three structurally distinct constraints: (1) lock-in_reading (this one) attributes persistence to path-dependent coordination failure without beneficiary extraction; (2) beneficiary_extraction_reading attributes persistence to incumbent defense of training investments and market lock-in; (3) naturalization_reading attributes persistence to adequacy and fair competitive elimination of alternatives. The three readings share a referent (QWERTY persists) but author different ε values, different victim structures, and different policy implications. Each reading is a separate constraint story. Links: lock-in_reading influences both siblings by establishing the empirical baseline (what QWERTY's function is). Extraction_reading forecloses naturalization_reading if verified (you cannot simultaneously claim QWERTY won fair competition AND that incumbents defended it against better alternatives). All three coexist as live interpretations of the same historical fact.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
