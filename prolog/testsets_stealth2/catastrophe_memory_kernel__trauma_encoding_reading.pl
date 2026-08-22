% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_kernel__trauma_encoding_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_kernel__trauma_encoding_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: catastrophe_memory_kernel__trauma_encoding_reading
 *   human_readable: Mourning Practice as Intergenerational Trauma-Transmission Warning System (Trauma-Encoding Reading)
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   A catastrophe-surviving diaspora community maintains an intensive
 *   mourning-practice regime: annual communal fasts and commemorations,
 *   liturgical lament, mandated family testimony, and a
 *   memorial-infrastructure sector, each binding new generations into
 *   structured remembrance of a destruction most never witnessed. This file
 *   instantiates ONE reading of the catastrophe_memory_kernel — the
 *   trauma_encoding_reading — under which the mourning practice is a
 *   transmission mechanism that encodes catastrophe-memory into descendants
 *   as a living warning system: the grief is the encoding, the resulting
 *   hypervigilance is the signal, and the arrangement's point is that the
 *   community never again fails to see a threat coming. Within this reading
 *   the declared beneficiary is the community's threat-vigilance capacity — a
 *   capacity, not an actor, so it is routed to vindicated_propositions per
 *   the OQ-64 split (a capacity collects no rents), while the actor-level
 *   collectors are the custodial office, the survivor generation
 *   (meaning-making), and memorial institutions (material flows). The victim
 *   class is the descendant generations, who inherit obligatory grief and
 *   vigilance they did not choose. Stated assumptions: the story models a
 *   composite post-catastrophe diaspora community rather than one named
 *   community; t=0 is the founding decade immediately after the catastrophe
 *   and t=80 approximates the present. The sibling readings
 *   (symbol_continuity, survival_competence, boundary_maintenance) are
 *   separate constraint stories with their own epsilon values and victim
 *   sets; they are linked through the network, not folded into this one.
 *
 * KEY AGENTS:
 *   - ritual_custodians: agenda-setting collector (institutional / identity_locked) — administers the commemorative calendar, trains successors, enforces participation norms; office, standing, and life-meaning are constituted by the memory-work
 *   - survivor_generation: founding collector (powerless / identity_locked) — direct witnesses for whom the rite expresses lived experience and converts devastation into shared meaning; exits the arrangement by mortality across the interval
 *   - memorial_institutions: secondary collector (organized / constrained) — museums, foundations, and schools whose budgets renew with the annual cycle
 *   - descendant_generations: primary bearer of costs (moderate / identity_locked) — inherits obligatory mourning, hypervigilance, and continuity expectations; shares diffusely in the protection the system provides, which is why refusal reads as betrayal
 *   - assimilated_leavers: excluded voice (moderate / arbitrage) — those who married out, secularized, or emigrated; their revealed judgment that the burden exceeds the warning value is structurally unheard in communal forums
 *   - trauma_researchers: analytical observer (institutional / analytical) — measures intergenerational transmission costs from outside the custodial authority structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_kernel__trauma_encoding_reading, 0.64).
domain_priors:suppression_score(catastrophe_memory_kernel__trauma_encoding_reading, 0.66).
domain_priors:theater_ratio(catastrophe_memory_kernel__trauma_encoding_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__trauma_encoding_reading, extractiveness, 0.64).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__trauma_encoding_reading, suppression_requirement, 0.66).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__trauma_encoding_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__trauma_encoding_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_kernel__trauma_encoding_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_kernel__trauma_encoding_reading, "Mourning Practice as Intergenerational Trauma-Transmission Warning System (Trauma-Encoding Reading)").
narrative_ontology:topic_domain(catastrophe_memory_kernel__trauma_encoding_reading, "religious_studies/collective_memory/ritual_practice").

domain_priors:requires_active_enforcement(catastrophe_memory_kernel__trauma_encoding_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_kernel__trauma_encoding_reading, '33a9603d-c244-4a52-8e5a-20e4c1dceb51').
narrative_ontology:cs_kernel_codification('33a9603d-c244-4a52-8e5a-20e4c1dceb51', distributed).
narrative_ontology:cs_authority_grounding('33a9603d-c244-4a52-8e5a-20e4c1dceb51', lineage).
narrative_ontology:cs_interpretation_layer_present('33a9603d-c244-4a52-8e5a-20e4c1dceb51').
narrative_ontology:cs_reading_relation('33a9603d-c244-4a52-8e5a-20e4c1dceb51', catastrophe_memory_kernel__symbol_continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('33a9603d-c244-4a52-8e5a-20e4c1dceb51', catastrophe_memory_kernel__survival_competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('33a9603d-c244-4a52-8e5a-20e4c1dceb51', catastrophe_memory_kernel__boundary_maintenance_reading, influences).
narrative_ontology:cs_axiom('33a9603d-c244-4a52-8e5a-20e4c1dceb51', foundational, transmitted_trauma_functions_as_adaptive_warning).
narrative_ontology:cs_axiom_status(transmitted_trauma_functions_as_adaptive_warning, holdable).
narrative_ontology:cs_axiom_grounding('33a9603d-c244-4a52-8e5a-20e4c1dceb51', transmitted_trauma_functions_as_adaptive_warning, empirically_contingent).
narrative_ontology:cs_axiom('33a9603d-c244-4a52-8e5a-20e4c1dceb51', foundational, inherited_grief_is_legitimate_payment_for_protection).
narrative_ontology:cs_axiom_status(inherited_grief_is_legitimate_payment_for_protection, holdable).
narrative_ontology:cs_axiom_grounding('33a9603d-c244-4a52-8e5a-20e4c1dceb51', inherited_grief_is_legitimate_payment_for_protection, instrumental).
narrative_ontology:cs_reference_frame('33a9603d-c244-4a52-8e5a-20e4c1dceb51', active_warning_mourning_regime).
narrative_ontology:cs_drift_state('33a9603d-c244-4a52-8e5a-20e4c1dceb51', contemporary_assimilation_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('33a9603d-c244-4a52-8e5a-20e4c1dceb51', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_kernel__trauma_encoding_reading, catastrophe_memory_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__trauma_encoding_reading, ritual_custodians).
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__trauma_encoding_reading, survivor_generation).
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__trauma_encoding_reading, memorial_institutions).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__trauma_encoding_reading, descendant_generations).
narrative_ontology:constraint_vindicates(catastrophe_memory_kernel__trauma_encoding_reading, threat_vigilance_early_warning_capacity).
narrative_ontology:constraint_vindicates(catastrophe_memory_kernel__trauma_encoding_reading, never_again_imperative).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% They set the commemorative calendar, decide which catastrophes are ritually marked and how, train the next cohort of officiants, and uphold participation norms through communal standing, marriage arrangements, and honors. They sincerely hold the warning-function account: the rite exists so the community never again fails to see danger coming. Their office, livelihood, and life's meaning are constituted by the memory-work; stepping away would mean abandoning their role and their community's trust. They also absorb vicarious burden by rehearsing the catastrophe professionally, but they chose the work and are honored for it.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__trauma_encoding_reading, ritual_custodians, agenda_setter,
    institutional, generational, identity_locked, global).

% Direct witnesses of the destruction. For them the mourning practice expresses lived experience rather than imposing one; it converts private devastation into shared meaning and a public promise that it will not happen unmarked again. They cannot exit their memories, and the rite gives those memories communal purpose. Across the interval this generation dies out, and their spoken testimony hardens into liturgical text handled by others.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__trauma_encoding_reading, survivor_generation, beneficiary,
    powerless, biographical, identity_locked, global).

% Museums, memorial foundations, archives, and educational centers funded by and oriented around the commemorative cycle. They receive donations, visitors, school groups, and state recognition in proportion to how central the memory-regime remains to communal life, and they extend the practice's reach beyond the community through exhibits and curricula. Their budgets renew annually with the mourning calendar.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__trauma_encoding_reading, memorial_institutions, beneficiary,
    organized, generational, constrained, global).

% Born after the destruction, they inherit obligatory grief: annual mourning for events they never witnessed, family narratives of annihilation, hypervigilance about signs of renewed persecution, and expectations to continue the line inside the community. Clinicians increasingly document anxiety, depressive symptoms, and narrowed life-scripts among them. Physical exit through assimilation or out-marriage is possible and increasingly taken, but the internalized vigilance travels with those who leave, and many who stay describe the inheritance as inseparable from who they are. They also share diffusely in the protection the system provides — the vigilance is aimed at threats to them — which is precisely why declining to transmit reads as betrayal of the dead.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__trauma_encoding_reading, descendant_generations, payer,
    moderate, generational, identity_locked, global).

% Community members who married out, secularized, or emigrated beyond the communal orbit. They no longer attend commemorations and mostly no longer speak in communal forums; where their departure is discussed at all, it is framed as loss or betrayal rather than as a verdict on the transmission arrangement. They are the population whose revealed judgment — that the inherited burden outweighs the warning value — the arrangement's internal conversation does not hear.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__trauma_encoding_reading, assimilated_leavers, excluded,
    moderate, biographical, arbitrage, global).

% Clinicians and researchers studying intergenerational trauma transmission through family-systems, epidemiological, and psychophysiological methods on survivor-offspring populations. They measure the psychological costs the arrangement imposes and, less frequently, its protective functions. They publish outside the custodial authority structure and have begun supplying descendant advocates with vocabulary the communal leadership cannot easily dismiss.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__trauma_encoding_reading, trauma_researchers, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_kernel__trauma_encoding_reading, ritual_custodians).
narrative_ontology:fixing_cost_class(catastrophe_memory_kernel__trauma_encoding_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: A dispersed, repeatedly persecuted community needs to detect and respond to renewed threats faster than isolated individuals or families could. Shared mourning practice keeps the danger cognitively and emotionally available across generations and geographies, synchronizes the community's threat-assessment, and coordinates commemoration, education, and mutual-defense attention around a single annual rhythm.
% TRANSFER_FUNCTION: Moves psychological burden — grief obligation, hypervigilance, identity weight, continuity expectations — from the catastrophe's aftermath onto each successive generation of descendants; moves authority and standing to the custodial office; moves material support and public attention to memorial institutions.
% ABSENT_VOICES: Assimilated leavers and descendants who declined to transmit would testify that the warning value does not justify the inherited burden; clinicians outside the tradition would testify to the documented pathology side of transmission. They sit outside the communal conversation — in host societies, in clinics, in out-married families — and enter it mainly as external citation rather than as seated voices.
% DISAPPEARANCE_RATIONALE: If the mourning regime vanished overnight, the communal calendar would lose its organizing spine, custodial authority would collapse with nothing to administer, memorial institutions would lose their annual renewal of attention and funding, and the community's threat-knowledge would need re-encoding through some other channel before the next danger — a gap the community has historically filled badly when the practice lapsed.
% FOUNDING_PROBLEM: A catastrophic persecution nearly destroyed the community. Survivors faced a double task: rebuild lives while ensuring the destruction was neither forgotten internally nor repeated unanticipated — keeping the danger vivid enough to mobilize defense without letting memory consume the living.
% FOUNDING_PROBLEM_CORROBORATION: Historians outside the community document the founding catastrophe and the amnesia-risk that motivated early commemoration. Contemporary security assessments and communal defense organizations attest that renewed-threat conditions persist in altered form. Clinicians corroborate the transmission-cost side. No party outside the benefiting set attests that the ORIGINAL founding problem — imminent recurrence of the same catastrophe — remains live in its original form; custodians assert it, descendants and researchers dispute it.
narrative_ontology:disappearance_verdict(catastrophe_memory_kernel__trauma_encoding_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_kernel__trauma_encoding_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_kernel__trauma_encoding_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(catastrophe_memory_kernel__trauma_encoding_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_kernel__trauma_encoding_reading, 0.64, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_kernel__trauma_encoding_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophe_memory_kernel__trauma_encoding_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(catastrophe_memory_kernel__trauma_encoding_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is 0.64 at interval end: the burden is real, imposed on non-consenting descendants, and grows as lived memory recedes — but the arrangement also delivers genuine protection the descendants themselves share in, which caps it below the pure-extraction range. Suppression is 0.66: enforcement is normative-communal (shame, marriage-market effects, standing) plus internalized obligation rather than coercive law — substantial but not total, and it plateaued after mid-interval as assimilation made enforcement costly. Theater ratio is 0.32: the core rites remain functionally loaded (they do transmit), but a growing performative layer — galas, political invocation, heritage tourism — rides the cycle. Accessibility collapse is 0.5: alternatives (secular commemoration, clinical processing, historical education without liturgical obligation) exist and are used by leavers, but inside the community they are discounted as betrayal. Resistance is 0.55: assimilation, out-marriage, and refusal to transmit to one's own children are real, rising, and the main reason enforcement stopped climbing. The claimed type is authored independently from the reading's own structure — a real coordination function plus a named victim class plus active enforcement — while the metrics are authored from the arrangement's observed operation; the engine computes per-seat types from the structural data. All three tracked series share one nine-point grid so no metric row is backfilled from another's end-state.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently. From the custodian seat the arrangement is the community's immune system that they personally maintain — coordination-forward, with their own costs (vicarious burden) voluntarily assumed. From the descendant seat the same structure operates as obligatory grief they did not choose, with exit that fails to release them. The survivor seat experiences neither imposition nor extraction: the practice matches lived experience, sitting near symmetric. Same-nominal-standing divergence appears between descendant_generations and assimilated_leavers — comparable power, opposite exits: those most able to leave did, which reshaped who remains inside the enforcement field and hardened the identity-lock of the stayers.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary/victim declarations drive the derivation: custodians, survivors, and memorial institutions sit near the beneficiary end (low d, damped effective extraction); descendant_generations sit near the target end but not at full-target — they bear the costs yet share diffusely in the vigilance benefit, since the warning system exists to protect them specifically, so their d sits below the trapped-full-target pole despite identity lock. Leavers carry residual d after exit; researchers are analytical. Suppression is authored as a raw structural property and is not scaled; extractiveness is scaled by directionality and scope — the global diaspora scope amplifies verification difficulty in both directions: custodians cannot verify whether distant communities transmit faithfully, and descendants cannot independently verify the threat level that justifies their inherited burden.
 *
 * MANDATROPHY ANALYSIS:
 *   Reading the arrangement as pure coordination (the custodial framing: 'this is simply how we remember') would erase the named victim class and the consent problem; reading it as pure extraction (a flat clinical framing: 'transmitted trauma is pathology, full stop') would erase the genuine warning function this reading posits and the diffuse protection descendants share. The tangled_rope structure holds both halves: threat-information transmission and unchosen burden flow through the same liturgical machinery, held together by active enforcement. On mandatrophy: the founding problem (post-catastrophe amnesia and unpreparedness) was live at t=0 and is now contested — the original imminence is gone while altered threats persist. The mismatch consumer should watch the contested-status x world_rearranges combination: the arrangement still organizes the world, but whether it still solves its founding problem is exactly what the parties dispute.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_underdetermination,
    'This constraint is one reading of the catastrophe_memory_kernel (trauma_encoding_reading); the sibling readings (symbol_continuity, survival_competence, boundary_maintenance) instantiate different constraints with different epsilon values and victim sets. Which function does the mourning practice actually perform dominantly?',
    'Comparative analysis across the four sibling stories: observe which functions the practice performs under stress — whether intensified commemoration precedes improved threat response (this reading), whether symbolic forms persist while vigilance lapses (symbol_continuity), whether practical survival knowledge tracks the rite (survival_competence), or whether enforcement effort concentrates on membership policing (boundary_maintenance).',
    'If the warning function is marginal, this reading''s epsilon overstates functional benefit and the arrangement computes closer to pure extraction; if central, the tangled_rope structure stands. The sibling files'' classifications shift correspondingly in the opposite direction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'Which of the four kernel readings captures the mourning practice''s dominant function.').

omega_variable(
    warning_value_vs_burden_balance,
    'Does transmitted hypervigilance actually improve the community''s threat detection and response enough to offset the documented psychological costs borne by descendants?',
    'Longitudinal outcome comparison of communities with intensive versus attenuated mourning regimes: threat-response metrics (latency of mutual aid to endangered communities, timing of precautionary migration, security-participation rates) set against standardized mental-health burden measures in descendant cohorts.',
    'Net-positive balance supports the coordination half of the tangled_rope and lowers effective extraction; net-negative balance collapses the coordination story toward pure extraction — burden imposed without protective return.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(warning_value_vs_burden_balance, empirical, 'Whether the warning-system benefit is real and sufficient to justify the transmitted burden.').

omega_variable(
    internalized_vs_structural_suppression,
    'Is descendant non-exit driven by structural enforcement (communal sanctions, marriage-market effects, family obligation) or by internalized identity fusion (''this grief is who I am'')?',
    'Post-exit trajectory study of assimilated leavers: if hypervigilance, guilt, and mourning compulsion persist after leaving the communal enforcement field, a substantial share of the measured suppression is internalized and travels with the agent.',
    'If largely internalized, the constraint''s effective suppression exceeds the structural measure — the descendant seat sits deeper in lock than its exit options suggest, and enforcement decay would not relieve the burden on its own.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_vs_structural_suppression, empirical, 'Structural versus internalized mechanism of descendant non-exit.').

omega_variable(
    future_persons_consent_question,
    'Can a legitimate warning system impose psychological costs on people not yet born, who cannot consent to the transmission?',
    'Not resolvable by data: the answer turns on the relative weighting of communal continuity against individual autonomy, resolvable only through the community''s own value deliberation and the descendant generation''s asserted standing to refuse.',
    'An autonomy-weighting resolution pushes the arrangement toward the extraction-dominant classification (imposed burden without consent); a continuity-weighting resolution supports the coordination-dominant reading and stabilizes the tangled_rope verdict.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(future_persons_consent_question, preference, 'Irreducible normative question of imposing costs on non-consenting future generations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_kernel__trauma_encoding_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(catastrophe_memory_trauma_tr_t0, catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(catastrophe_memory_trauma_tr_t10, catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 10, 0.12).
narrative_ontology:measurement(catastrophe_memory_trauma_tr_t20, catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 20, 0.15).
narrative_ontology:measurement(catastrophe_memory_trauma_tr_t30, catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 30, 0.19).
narrative_ontology:measurement(catastrophe_memory_trauma_tr_t40, catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 40, 0.22).
narrative_ontology:measurement(catastrophe_memory_trauma_tr_t50, catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 50, 0.25).
narrative_ontology:measurement(catastrophe_memory_trauma_tr_t60, catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 60, 0.28).
narrative_ontology:measurement(catastrophe_memory_trauma_tr_t70, catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 70, 0.3).
narrative_ontology:measurement(catastrophe_memory_trauma_tr_t80, catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 80, 0.32).

% Extraction over time
narrative_ontology:measurement(catastrophe_memory_trauma_be_t0, catastrophe_memory_kernel__trauma_encoding_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(catastrophe_memory_trauma_be_t10, catastrophe_memory_kernel__trauma_encoding_reading, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(catastrophe_memory_trauma_be_t20, catastrophe_memory_kernel__trauma_encoding_reading, base_extractiveness, 20, 0.48).
narrative_ontology:measurement(catastrophe_memory_trauma_be_t30, catastrophe_memory_kernel__trauma_encoding_reading, base_extractiveness, 30, 0.53).
narrative_ontology:measurement(catastrophe_memory_trauma_be_t40, catastrophe_memory_kernel__trauma_encoding_reading, base_extractiveness, 40, 0.57).
narrative_ontology:measurement(catastrophe_memory_trauma_be_t50, catastrophe_memory_kernel__trauma_encoding_reading, base_extractiveness, 50, 0.6).
narrative_ontology:measurement(catastrophe_memory_trauma_be_t60, catastrophe_memory_kernel__trauma_encoding_reading, base_extractiveness, 60, 0.62).
narrative_ontology:measurement(catastrophe_memory_trauma_be_t70, catastrophe_memory_kernel__trauma_encoding_reading, base_extractiveness, 70, 0.63).
narrative_ontology:measurement(catastrophe_memory_trauma_be_t80, catastrophe_memory_kernel__trauma_encoding_reading, base_extractiveness, 80, 0.64).

% Suppression requirement over time
narrative_ontology:measurement(catastrophe_memory_trauma_su_t0, catastrophe_memory_kernel__trauma_encoding_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(catastrophe_memory_trauma_su_t10, catastrophe_memory_kernel__trauma_encoding_reading, suppression_requirement, 10, 0.55).
narrative_ontology:measurement(catastrophe_memory_trauma_su_t20, catastrophe_memory_kernel__trauma_encoding_reading, suppression_requirement, 20, 0.62).
narrative_ontology:measurement(catastrophe_memory_trauma_su_t30, catastrophe_memory_kernel__trauma_encoding_reading, suppression_requirement, 30, 0.68).
narrative_ontology:measurement(catastrophe_memory_trauma_su_t40, catastrophe_memory_kernel__trauma_encoding_reading, suppression_requirement, 40, 0.7).
narrative_ontology:measurement(catastrophe_memory_trauma_su_t50, catastrophe_memory_kernel__trauma_encoding_reading, suppression_requirement, 50, 0.69).
narrative_ontology:measurement(catastrophe_memory_trauma_su_t60, catastrophe_memory_kernel__trauma_encoding_reading, suppression_requirement, 60, 0.67).
narrative_ontology:measurement(catastrophe_memory_trauma_su_t70, catastrophe_memory_kernel__trauma_encoding_reading, suppression_requirement, 70, 0.66).
narrative_ontology:measurement(catastrophe_memory_trauma_su_t80, catastrophe_memory_kernel__trauma_encoding_reading, suppression_requirement, 80, 0.66).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_kernel__trauma_encoding_reading, attachment_coordination).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__trauma_encoding_reading, catastrophe_memory_kernel__symbol_continuity_reading).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__trauma_encoding_reading, catastrophe_memory_kernel__survival_competence_reading).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__trauma_encoding_reading, catastrophe_memory_kernel__boundary_maintenance_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the catastrophe_memory_kernel per the epsilon-invariance principle: 'ritual mourning practice' is one natural-language label covering four structurally distinct claims, each authored as its own story with its own epsilon, beneficiaries, and victims. This file is the trauma_encoding_reading (mourning as trauma-transmission warning system; victims are descendants bearing inherited burden; moderate-to-high extraction). The upstream sibling symbol_continuity_reading typically shows negligible extraction (preservation of forms is uncontested within the community) and is often cited BY custodians as evidence for this reading's legitimacy; survival_competence_reading shares this reading's transmission logic but flips the valence of what is transmitted (capacity rather than wound); boundary_maintenance_reading relocates the victim class from the bereaved to the excluded. All four files link mutually through network.affects_constraints; classification divergence across the family is the measurement the decomposition exists to take.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
