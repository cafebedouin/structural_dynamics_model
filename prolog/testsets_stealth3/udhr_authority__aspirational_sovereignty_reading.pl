% ============================================================================
% CONSTRAINT STORY: udhr_authority__aspirational_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_udhr_authority__aspirational_sovereignty_reading, []).

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
 *   constraint_id: udhr_authority__aspirational_sovereignty_reading
 *   human_readable: UDHR Authority - Aspirational Sovereignty Reading (Consent-Gated Moral Guidance)
 *   domain: international law / political philosophy / human rights doctrine
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the udhr_authority kernel: the
 *   Universal Declaration of Human Rights as moral guidance whose binding
 *   force requires per-state consent. Under this reading the consent
 *   architecture is a working coordination structure - it is why
 *   near-universal adoption happened at all - while simultaneously
 *   distributing costs asymmetrically: populations under non-consenting
 *   governments hold moral standing without enforcement levers, routed
 *   through the very states most able to fail them. The claim and the metrics
 *   are independent authored facts: the claimed type is what I believe
 *   structurally true from this reading's seat (tangled_rope - genuine
 *   coordination function with asymmetric extraction through the same
 *   structure); the metrics describe how the arrangement actually operates,
 *   reading-indexed to this seat. The sibling readings are separate
 *   constraints with their own epsilon, victims, and classifications; nothing
 *   here averages across them. KEY AGENTS (by structural relationship):
 *   sovereign_member_states: agenda-setter and principal beneficiary
 *   (institutional/arbitrage) - administers the consent gate and collects
 *   preserved autonomy; non_ratifying_governments: concentrated beneficiary
 *   (powerful/arbitrage) - moral standing without binding submission;
 *   populations_under_non_ratifying_states: primary target
 *   (powerless/trapped) - bears enforcement deprivation;
 *   rights_defenders_without_treaty_channels: secondary payer
 *   (organized/constrained); international_tribunals_and_treaty_bodies:
 *   institutionally hobbled payer (institutional/constrained);
 *   stateless_persons: excluded voice (powerless/trapped) - outside the
 *   consent routing entirely; academic_interpreter_community: analytical
 *   observer (analytical/analytical).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(udhr_authority__aspirational_sovereignty_reading, 0.38).
domain_priors:suppression_score(udhr_authority__aspirational_sovereignty_reading, 0.32).
domain_priors:theater_ratio(udhr_authority__aspirational_sovereignty_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(udhr_authority__aspirational_sovereignty_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(udhr_authority__aspirational_sovereignty_reading, suppression_requirement, 0.32).
narrative_ontology:constraint_metric(udhr_authority__aspirational_sovereignty_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(udhr_authority__aspirational_sovereignty_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(udhr_authority__aspirational_sovereignty_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(udhr_authority__aspirational_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(udhr_authority__aspirational_sovereignty_reading, "UDHR Authority - Aspirational Sovereignty Reading (Consent-Gated Moral Guidance)").
narrative_ontology:topic_domain(udhr_authority__aspirational_sovereignty_reading, "international law / political philosophy / human rights doctrine").

domain_priors:requires_active_enforcement(udhr_authority__aspirational_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(udhr_authority__aspirational_sovereignty_reading, '3c54fcb8-7eb4-4434-b94c-5d33cc5b3d3e').
narrative_ontology:cs_kernel_codification('3c54fcb8-7eb4-4434-b94c-5d33cc5b3d3e', fixed_text).
narrative_ontology:cs_authority_grounding('3c54fcb8-7eb4-4434-b94c-5d33cc5b3d3e', lineage).
narrative_ontology:cs_interpretation_layer_present('3c54fcb8-7eb4-4434-b94c-5d33cc5b3d3e').
narrative_ontology:cs_reading_relation('3c54fcb8-7eb4-4434-b94c-5d33cc5b3d3e', udhr_authority__binding_universalism_reading, forecloses).
narrative_ontology:cs_reading_relation('3c54fcb8-7eb4-4434-b94c-5d33cc5b3d3e', udhr_authority__customary_emergence_reading, influences).
narrative_ontology:cs_axiom('3c54fcb8-7eb4-4434-b94c-5d33cc5b3d3e', foundational, binding_requires_state_consent).
narrative_ontology:cs_axiom_status(binding_requires_state_consent, holdable).
narrative_ontology:cs_axiom_grounding('3c54fcb8-7eb4-4434-b94c-5d33cc5b3d3e', binding_requires_state_consent, conventional).
narrative_ontology:cs_axiom('3c54fcb8-7eb4-4434-b94c-5d33cc5b3d3e', secondary, proclamation_confers_moral_not_legal_force).
narrative_ontology:cs_axiom_status(proclamation_confers_moral_not_legal_force, holdable).
narrative_ontology:cs_axiom_grounding('3c54fcb8-7eb4-4434-b94c-5d33cc5b3d3e', proclamation_confers_moral_not_legal_force, deontological).
narrative_ontology:cs_reference_frame('3c54fcb8-7eb4-4434-b94c-5d33cc5b3d3e', consent_gated_sovereign_equality).
narrative_ontology:cs_drift_state('3c54fcb8-7eb4-4434-b94c-5d33cc5b3d3e', contemporary_post_covenant_era, gap(repudiation_pressure, minor, false)).
narrative_ontology:cs_created_at('3c54fcb8-7eb4-4434-b94c-5d33cc5b3d3e', '').
narrative_ontology:cs_kernel_id(udhr_authority__aspirational_sovereignty_reading, udhr_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(udhr_authority__aspirational_sovereignty_reading, sovereign_member_states).
narrative_ontology:constraint_beneficiary(udhr_authority__aspirational_sovereignty_reading, non_ratifying_governments).
narrative_ontology:constraint_victim(udhr_authority__aspirational_sovereignty_reading, populations_under_non_ratifying_states).
narrative_ontology:constraint_victim(udhr_authority__aspirational_sovereignty_reading, rights_defenders_without_treaty_channels).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(udhr_authority__aspirational_sovereignty_reading, international_tribunals_and_treaty_bodies).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Collectively administer the consent architecture: the General Assembly adopts Declarations by consensus, and binding force attaches only where a state separately ratifies a treaty. Each member retains veto over which norms reach it, exercises negative enforcement by withholding consent and blocking institutionalized oversight, and preserves full discretion over its internal conduct. The arrangement costs them nothing they have not agreed to and returns autonomy they would otherwise surrender.
narrative_ontology:constraint_stakeholder(udhr_authority__aspirational_sovereignty_reading, sovereign_member_states, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(udhr_authority__aspirational_sovereignty_reading, sovereign_member_states, beneficiary).

% Governments that invoke the Declaration's language for legitimacy while declining the binding covenants, or ratifying with reservations that hollow the commitments. They receive the moral standing of association with the universal instrument without submitting to any tribunal's judgment. Exit is cheap in the relevant sense: they can never accede, denounce later, or qualify accession, and the architecture imposes no penalty beyond reputation.
narrative_ontology:constraint_stakeholder(udhr_authority__aspirational_sovereignty_reading, non_ratifying_governments, beneficiary,
    powerful, biographical, arbitrage, global).

% People living under governments that have not accepted binding instruments. The Declaration addresses them as 'everyone,' but no enforcement channel reaches them: their protection depends wholly on the goodwill of the very authority most positioned to violate their rights. Leaving the jurisdiction is often impossible, and no external tribunal can act where consent is absent.
narrative_ontology:constraint_stakeholder(udhr_authority__aspirational_sovereignty_reading, populations_under_non_ratifying_states, payer,
    powerless, biographical, trapped, national).

% Advocacy organizations and individual defenders who must pursue every case through moral suasion, documentation, and reputational pressure because the consent gate withholds judicial levers. They bear the recurring cost of persuading states to volunteer for accountability, case by case and decade by decade, where a binding instrument would convert persuasion into procedure.
narrative_ontology:constraint_stakeholder(udhr_authority__aspirational_sovereignty_reading, rights_defenders_without_treaty_channels, payer,
    organized, generational, constrained, global).

% Bodies such as the Human Rights Committee and the regional courts can examine reports and issue findings only inside boundaries that consenting states drew in advance; where consent is absent they have no mandate, no jurisdiction, and no coercive capacity. Their adjudicative function operates perpetually short of enforcement, and they cannot extend it without new rounds of state assent.
narrative_ontology:constraint_stakeholder(udhr_authority__aspirational_sovereignty_reading, international_tribunals_and_treaty_bodies, payer,
    institutional, generational, constrained, global).

% Persons recognized by no state fall outside the architecture's routing entirely: protection flows only along channels that begin in someone's consent, and no one's consent covers them. They would object that a universal declaration routed exclusively through state consent leaves them with nothing, but they hold no seat in any assembly where the architecture is maintained.
narrative_ontology:constraint_stakeholder(udhr_authority__aspirational_sovereignty_reading, stateless_persons, excluded,
    powerless, biographical, trapped, global).

% Jurists and political philosophers who analyze whether the Declaration binds, which provisions have entered custom, and what the consent principle protects or costs. They produce the doctrinal distinctions the other seats argue with, observing the full structure without collecting or paying inside it.
narrative_ontology:constraint_stakeholder(udhr_authority__aspirational_sovereignty_reading, academic_interpreter_community, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(udhr_authority__aspirational_sovereignty_reading, sovereign_member_states).
narrative_ontology:fixing_cost_class(udhr_authority__aspirational_sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the interstate collective-action problem of proclaiming universal moral standards without any state surrendering discretion over internal affairs: because adoption carried no obligation, near-universal endorsement of the Declaration became achievable, and every participant knows what every other participant professes.
% TRANSFER_FUNCTION: Moves enforcement leverage away from individuals toward states: individuals receive moral recognition without practical recourse, while enforcement power pools at the state level, where each government alone decides which obligations bind it.
% ABSENT_VOICES: Stateless persons and populations under non-ratifying regimes have no formal voice in the assemblies that maintain the architecture; they appear only as objects of the Declaration's 'everyone' while their access to protection is mediated by the very governments whose consent gates it.
% DISAPPEARANCE_RATIONALE: If consent-gating vanished overnight - if the Declaration's norms were enforceable without per-state assent - ratification politics would collapse into a different dispute, tribunals would acquire caseloads and coercive questions they currently cannot pose, and governments shielding conduct behind non-bindingness would lose the shield; the interstate order built on sovereign discretion over obligations would reorganize.
% FOUNDING_PROBLEM: Post-war drafters needed maximum adoption of a universal rights standard amid emerging Cold War division, so the Declaration was framed as a common standard of achievement rather than a treaty - proclaiming universal moral authority without requiring, or attempting to secure, state agreement to be bound.
% FOUNDING_PROBLEM_CORROBORATION: International legal historiography and the drafting-era records corroborate the founding rationale as adoption-maximization under 1948 conditions, a contingency rather than a perpetual necessity. Proponents of the sibling readings - universalist jurists and customary-law scholars, writing from outside the benefiting state seats - attest that the binding covenants' later existence dissolved the original excuse for non-bindingness, while state representatives at Vienna (1993) and in General Assembly debate attest the consent problem as perennial. Corroboration exists on both sides, which is why the status is contested rather than dead.
narrative_ontology:disappearance_verdict(udhr_authority__aspirational_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(udhr_authority__aspirational_sovereignty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(udhr_authority__aspirational_sovereignty_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(udhr_authority__aspirational_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(udhr_authority__aspirational_sovereignty_reading, 0.38, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(udhr_authority__aspirational_sovereignty_reading_tests).
:- end_tests(udhr_authority__aspirational_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-low (0.38) because this reading assesses the standing arrangement by its own lights: consent-gating is legitimate constitutional design, and state autonomy bears almost no extraction - but the reading's own lights still register a real bearer of cost, since enforcement deprivation lands on identifiable people rather than diffusing. Suppression is 0.32 and structural, not internalized: the veto architecture and the requirement of per-state ratification are external barriers; no cognitive fusion is needed to sustain them. Suppression is authored as a raw structural property and is NOT scaled by power or scope in this story's arithmetic - only extractiveness scales. Theater is modest (0.28): anniversary proclamations and rhetorical invocation are increasingly performative, but the Declaration retains a real diplomatic and normative-referencing function. Accessibility_collapse is low-moderate (0.35): alternatives do not fully collapse once the arrangement is understood - regional binding systems (the European Convention system) demonstrate consent-plus-enforcement is buildable, and the custom-formation route keeps a live alternative open, which is precisely the sibling reading's wager. Resistance is high (0.60): universalist jurisprudence, NGO campaigns, and treaty-body expansion press continuously against the gate, meeting organized state counter-mobilization at Vienna and in successive General Assembly sessions. On coalition: the powerless payer seats could in principle coalition transnationally, but the architecture routes representation through states, which blunts coalition formation - part of how the gate persists without overt coercion. The measurement series run on one shared time grid; every tracked metric is authored at every examined point.
 *
 * PERSPECTIVAL GAP:
 *   The payer and agenda-setter seats compute differently and should. From the sovereign_member_states seat the arrangement is legitimate constitutional order: nobody bound without consent, everyone sovereign, the Declaration a common reference point freely honored. From the populations_under_non_ratifying_states seat the same structure operates as enforced helplessness: a universal promise addressed to them personally whose fulfillment requires a signature their own abuser controls. The tribunals sit between - they experience the gate as a permanent mandate shortfall. The engine computes these divergent per-seat classifications from the structural data (power, exit, roles); the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive derivation. sovereign_member_states and non_ratifying_governments sit near the beneficiary end (d near 0.0): the constraint subsidizes their autonomy, and arbitrage-grade exit (selective ratification, reservation, denunciation) pushes them toward the subsidy pole. populations_under_non_ratifying_states sit near the full-target end (d near 1.0): trapped exit, all cost. rights_defenders_without_treaty_channels carry high d but with organized power and constrained mobility damping effective extraction below the trapped-population seat. international_tribunals_and_treaty_bodies derive high d from their victim listing - the gate strips their coercive function - moderated by institutional power. stateless_persons are structurally maximally targeted with no channel at all. The academic observer seat is neutral. Larger spatial scope (global) modestly amplifies effective extraction for the payer seats through verification difficulty, per the engine's scope handling.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem - how to win near-universal adoption when binding was impossible - belongs to 1948 conditions; the covenants' existence (1966/1976) removed the excuse, so the adoption-maximization rationale is contested-dead while the arrangement persists serving a successor function: sovereignty protection and selective moral association. The tangled_rope classification prevents both mislabelings. Read as pure coordination (rope), the enforcement deprivation imposed on the consentless would be laundered as neutral constitutional design. Read as pure extraction (snare), the genuine and load-bearing coordination function - the reason the postwar universal vocabulary exists at all - would be erased, and with it the honest answer to why dismantling the gate is prohibitively costly for the seats that could dismantle it. It is neither scaffold (there is no sunset: consent-gating is steady-state by design, not transitional support) nor piton (the coordination function is live, not atrophied performance).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This story instantiates the aspirational_sovereignty_reading of the udhr_authority kernel; how would the sibling readings restructure the constraint''s beneficiary/victim sets and classification?',
    'Generate the sibling stories (binding_universalism_reading, customary_emergence_reading) as separate epsilon-invariant files with their own structural data, and compare per-seat classifications; the engine computes foreclosure between axioms from grounding types and drift states.',
    'Under binding_universalism the victim set expands to include state autonomy itself and extraction on the state seats rises sharply; under customary_emergence the consent gate erodes provision by provision and this reading progressively loses territory to empirically accumulated obligation. Either way this file''s classification is not the kernel''s verdict - it is one seat''s.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer-frame position: this constraint is one of three readings of the UDHR-authority kernel.').

omega_variable(
    customary_status_inventory,
    'Which specific UDHR provisions have actually attained customary international law status through state practice and opinio juris?',
    'Per-provision surveys by international law commissions and systematic scholarly reviews of state practice, reservations, and persistent-objector records, distinguishing rhetorical invocation from behavioral compliance.',
    'Every provision certified as custom migrates out of this reading''s non-binding territory into the sibling reading''s domain, raising measured extraction on non-consenting states and shrinking the coordination-only core; a near-empty inventory would confirm this reading as the operative truth.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(customary_status_inventory, empirical, 'Empirical extent of custom formation eroding the consent gate provision by provision.').

omega_variable(
    consent_gate_price_allocation,
    'Is the protection deficit borne by populations under non-consenting states a legitimate constitutional price of sovereign equality, or an extractive asymmetry that the beneficiary seats decline to acknowledge?',
    'Normative-theoretical adjudication combined with outcome comparison across consent regimes: regions operating binding instruments versus regions left inside the consent gate, holding violation propensity roughly constant.',
    'If judged extractive, effective extraction for the payer seats exceeds this reading''s self-authored epsilon and the classification shifts toward the snare-flavored pole despite the genuine coordination function; if judged a legitimate price, the tangled_rope reading stands with the current metric profile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_gate_price_allocation, preference, 'Whether the enforcement gap is a fair constitutional cost or unacknowledged extraction - the value question this reading''s low epsilon quietly assumes.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(udhr_authority__aspirational_sovereignty_reading, 1948, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(udhr_tr_t1948, udhr_authority__aspirational_sovereignty_reading, theater_ratio, 1948, 0.1).
narrative_ontology:measurement_basis(udhr_tr_t1948, observed).
narrative_ontology:measurement(udhr_tr_t1966, udhr_authority__aspirational_sovereignty_reading, theater_ratio, 1966, 0.16).
narrative_ontology:measurement_basis(udhr_tr_t1966, observed).
narrative_ontology:measurement(udhr_tr_t1985, udhr_authority__aspirational_sovereignty_reading, theater_ratio, 1985, 0.2).
narrative_ontology:measurement_basis(udhr_tr_t1985, observed).
narrative_ontology:measurement(udhr_tr_t2005, udhr_authority__aspirational_sovereignty_reading, theater_ratio, 2005, 0.25).
narrative_ontology:measurement_basis(udhr_tr_t2005, observed).
narrative_ontology:measurement(udhr_tr_t2025, udhr_authority__aspirational_sovereignty_reading, theater_ratio, 2025, 0.28).
narrative_ontology:measurement_basis(udhr_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(udhr_be_t1948, udhr_authority__aspirational_sovereignty_reading, base_extractiveness, 1948, 0.14).
narrative_ontology:measurement_basis(udhr_be_t1948, observed).
narrative_ontology:measurement(udhr_be_t1966, udhr_authority__aspirational_sovereignty_reading, base_extractiveness, 1966, 0.24).
narrative_ontology:measurement_basis(udhr_be_t1966, observed).
narrative_ontology:measurement(udhr_be_t1985, udhr_authority__aspirational_sovereignty_reading, base_extractiveness, 1985, 0.3).
narrative_ontology:measurement_basis(udhr_be_t1985, observed).
narrative_ontology:measurement(udhr_be_t2005, udhr_authority__aspirational_sovereignty_reading, base_extractiveness, 2005, 0.34).
narrative_ontology:measurement_basis(udhr_be_t2005, observed).
narrative_ontology:measurement(udhr_be_t2025, udhr_authority__aspirational_sovereignty_reading, base_extractiveness, 2025, 0.38).
narrative_ontology:measurement_basis(udhr_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(udhr_su_t1948, udhr_authority__aspirational_sovereignty_reading, suppression_requirement, 1948, 0.12).
narrative_ontology:measurement_basis(udhr_su_t1948, observed).
narrative_ontology:measurement(udhr_su_t1966, udhr_authority__aspirational_sovereignty_reading, suppression_requirement, 1966, 0.21).
narrative_ontology:measurement_basis(udhr_su_t1966, observed).
narrative_ontology:measurement(udhr_su_t1985, udhr_authority__aspirational_sovereignty_reading, suppression_requirement, 1985, 0.26).
narrative_ontology:measurement_basis(udhr_su_t1985, observed).
narrative_ontology:measurement(udhr_su_t2005, udhr_authority__aspirational_sovereignty_reading, suppression_requirement, 2005, 0.29).
narrative_ontology:measurement_basis(udhr_su_t2005, observed).
narrative_ontology:measurement(udhr_su_t2025, udhr_authority__aspirational_sovereignty_reading, suppression_requirement, 2025, 0.32).
narrative_ontology:measurement_basis(udhr_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(udhr_authority__aspirational_sovereignty_reading, identity_coordination).
narrative_ontology:affects_constraint(udhr_authority__aspirational_sovereignty_reading, udhr_authority__binding_universalism_reading).
narrative_ontology:affects_constraint(udhr_authority__aspirational_sovereignty_reading, udhr_authority__customary_emergence_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'UDHR authority' per the epsilon-invariance principle. The single natural-language question 'does the Declaration bind?' conflates three structurally distinct claims: consent-gated moral guidance (this file - the founding default reading, upstream and most empirically settled about what the document literally is), consent-independent justiciability (binding_universalism_reading), and accrued custom (customary_emergence_reading). This upstream reading influences both downstream siblings: the invocation-without-commitment state practice this arrangement generates is precisely the practice-and-opinio-juris record the customary reading harvests, and the durability of the gate defines the bar the universalist reading must clear. Each member carries its own epsilon, beneficiaries, and victims; no member averages across the family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
