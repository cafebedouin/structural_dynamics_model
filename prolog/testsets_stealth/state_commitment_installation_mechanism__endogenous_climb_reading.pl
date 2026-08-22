% ============================================================================
% CONSTRAINT STORY: state_commitment_installation_mechanism__endogenous_climb_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_commitment_installation_mechanism__endogenous_climb_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: state_commitment_installation_mechanism__endogenous_climb_reading
 *   human_readable: Endogenous Climb Pathway for Commitment Legitimacy
 *   domain: historical sociology / state formation / cultural authority
 *
 * SUMMARY:
 *   This story instantiates the endogenous_climb_reading of the kernel
 *   state_commitment_installation_mechanism: the claim that new commitments
 *   gain legitimacy by climbing from institutional fringes through
 *   demonstrated superiority. The constraint under classification is the
 *   standing climb pathway itself, the operative regime through which
 *   challenger commitments must pass, as this reading sees it. The reading
 *   credits the pathway's genuine filtering function (peaceful, decentralized
 *   legitimacy transfer) and simultaneously records its asymmetric costs
 *   (challengers bear the entire demonstration burden; apexes collect the
 *   vetting surplus; the demonstration standard is set and interpreted by the
 *   very authorities the challengers petition). Sibling readings,
 *   exogenous_imposition_reading and hybrid_cascade_reading, are separate
 *   constraints with their own epsilon values, beneficiary sets, and
 *   classifications; they are linked through the network, not folded into
 *   this story. KEY AGENTS (by structural relationship):
 *   fringe_advocates_reform_movements: primary beneficiary with heavy payer
 *   overlay ([moderate]/[identity_locked]); apex_gatekeeper_elites: agenda
 *   setter and vetting-surplus collector ([institutional]/[arbitrage]);
 *   populations_under_legacy_arrangements: primary target
 *   ([powerless]/[trapped]); failed_superior_challengers: secondary target
 *   ([powerless]/[trapped]); early_adopter_institutions: secondary
 *   beneficiary ([organized]/[mobile]); rival_installation_coalitions:
 *   excluded actor ([powerful]/[constrained]); comparative_historians:
 *   analytical observer.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_commitment_installation_mechanism__endogenous_climb_reading, 0.55).
domain_priors:suppression_score(state_commitment_installation_mechanism__endogenous_climb_reading, 0.36).
domain_priors:theater_ratio(state_commitment_installation_mechanism__endogenous_climb_reading, 0.33).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__endogenous_climb_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__endogenous_climb_reading, suppression_requirement, 0.36).
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__endogenous_climb_reading, theater_ratio, 0.33).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__endogenous_climb_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__endogenous_climb_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_commitment_installation_mechanism__endogenous_climb_reading, tangled_rope).
narrative_ontology:human_readable(state_commitment_installation_mechanism__endogenous_climb_reading, "Endogenous Climb Pathway for Commitment Legitimacy").
narrative_ontology:topic_domain(state_commitment_installation_mechanism__endogenous_climb_reading, "historical sociology / state formation / cultural authority").

domain_priors:requires_active_enforcement(state_commitment_installation_mechanism__endogenous_climb_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_commitment_installation_mechanism__endogenous_climb_reading, '3f0e8a04-6b61-48e2-b574-a4f22283c634').
narrative_ontology:cs_kernel_codification('3f0e8a04-6b61-48e2-b574-a4f22283c634', distributed).
narrative_ontology:cs_authority_grounding('3f0e8a04-6b61-48e2-b574-a4f22283c634', distributed).
narrative_ontology:cs_reading_relation('3f0e8a04-6b61-48e2-b574-a4f22283c634', state_commitment_installation_mechanism__exogenous_imposition_reading, coexists_with).
narrative_ontology:cs_reading_relation('3f0e8a04-6b61-48e2-b574-a4f22283c634', state_commitment_installation_mechanism__hybrid_cascade_reading, coexists_with).
narrative_ontology:cs_axiom('3f0e8a04-6b61-48e2-b574-a4f22283c634', foundational, legitimacy_originates_at_institutional_periphery).
narrative_ontology:cs_axiom_status(legitimacy_originates_at_institutional_periphery, holdable).
narrative_ontology:cs_axiom_grounding('3f0e8a04-6b61-48e2-b574-a4f22283c634', legitimacy_originates_at_institutional_periphery, empirically_contingent).
narrative_ontology:cs_axiom('3f0e8a04-6b61-48e2-b574-a4f22283c634', foundational, demonstrated_superiority_drives_adoption_without_mandate).
narrative_ontology:cs_axiom_status(demonstrated_superiority_drives_adoption_without_mandate, holdable).
narrative_ontology:cs_axiom_grounding('3f0e8a04-6b61-48e2-b574-a4f22283c634', demonstrated_superiority_drives_adoption_without_mandate, empirically_contingent).
narrative_ontology:cs_reference_frame('3f0e8a04-6b61-48e2-b574-a4f22283c634', demonstrated_superiority_legitimacy_order).
narrative_ontology:cs_drift_state('3f0e8a04-6b61-48e2-b574-a4f22283c634', contemporary_revisionist_scholarship, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('3f0e8a04-6b61-48e2-b574-a4f22283c634', '').
narrative_ontology:cs_kernel_id(state_commitment_installation_mechanism__endogenous_climb_reading, state_commitment_installation_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_commitment_installation_mechanism__endogenous_climb_reading, fringe_advocates_reform_movements).
narrative_ontology:constraint_beneficiary(state_commitment_installation_mechanism__endogenous_climb_reading, early_adopter_institutions).
narrative_ontology:constraint_victim(state_commitment_installation_mechanism__endogenous_climb_reading, populations_under_legacy_arrangements).
narrative_ontology:constraint_victim(state_commitment_installation_mechanism__endogenous_climb_reading, failed_superior_challengers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(state_commitment_installation_mechanism__endogenous_climb_reading, apex_gatekeeper_elites).
narrative_ontology:constraint_victim(state_commitment_installation_mechanism__endogenous_climb_reading, fringe_advocates_reform_movements).
narrative_ontology:constraint_vindicates(state_commitment_installation_mechanism__endogenous_climb_reading, demonstrated_superiority_standard).
narrative_ontology:constraint_vindicates(state_commitment_installation_mechanism__endogenous_climb_reading, gradualist_legitimation_theory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Organize at the edges of institutional life to advance a candidate commitment: they build working demonstrations, compile performance records, and petition apex bodies for recognition. The climb pathway is their principal route to influence, since they hold no coercive or budgetary power. They fund years of unrecompensed demonstration work out of conviction and donations, and each cycle of apex skepticism raises the evidence they must produce next. Leaving the pathway means abandoning a commitment their organizations, reputations, and identities are built around.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__endogenous_climb_reading, fringe_advocates_reform_movements, beneficiary,
    moderate, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(state_commitment_installation_mechanism__endogenous_climb_reading, fringe_advocates_reform_movements, payer).

% Hold recognized authority over adoption decisions at the center: they convene review bodies, define what counts as adequate demonstration, and time recognition. They resist challenges that arrive before the standard is met, and they capture the advantage of adopting only commitments whose performance is already established, shifting risk onto the challengers who bore it. When a climb succeeds, part of their authority transfers to the arriving commitment and its sponsors. Their position lets them tighten standards, delay recognition, or endorse late without losing standing.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__endogenous_climb_reading, apex_gatekeeper_elites, agenda_setter,
    institutional, generational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(state_commitment_installation_mechanism__endogenous_climb_reading, apex_gatekeeper_elites, beneficiary).

% Live under the incumbent arrangements while candidate replacements spend years proving themselves. They bear the harms the new commitment would remedy, continued exposure to the legacy practice, and have no seat in the demonstration process: their experience is cited as motivation, but they neither design the pilots nor time the adoption. Their options are endurance, exit where geography allows, or agitation that the pathway channels back into more demonstration.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__endogenous_climb_reading, populations_under_legacy_arrangements, payer,
    powerless, biographical, trapped, local).

% Mounted demonstration campaigns that never reached recognition. Some carried performance advantages that later arrivals reproduced successfully; others were simply outlasted by incumbent patience. Their archives are thin, their organizations dissolved, and their leaders absorbed into other causes. They represent the pathway's unrecorded costs: effort spent proving a case the surviving record no longer contains.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__endogenous_climb_reading, failed_superior_challengers, payer,
    powerless, biographical, trapped, regional).

% Mid-tier bodies that adopt climbing commitments ahead of the apex: a provincial court taking up a novel doctrine, a regional board piloting a new protocol. Adoption ahead of consensus buys competitive and reputational advantage, and their results become the demonstration record the apex later reviews. They can revert if results disappoint, which makes their position materially safer than the challengers' position.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__endogenous_climb_reading, early_adopter_institutions, beneficiary,
    organized, biographical, mobile, national).

% Factions convinced that legitimate change runs through authoritative mandate rather than patient demonstration. The climb pathway's cultural dominance marks their strategy as illegitimate coercion before it is argued on merits, so they operate at the discursive margin: they can act in crises or through captured offices, but they cannot table mandate-based installation in ordinary adoption debates without paying a legitimacy penalty upfront.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__endogenous_climb_reading, rival_installation_coalitions, excluded,
    powerful, generational, constrained, continental).

% Reconstruct adoption curves across cases and centuries: religious toleration, abolition, professional licensure, administrative reform. They attest the gradual shape of climbs and the resistance encountered at apexes, and their archives are the main independent check on movement self-narratives. They collect nothing from any outcome and can see both the recorded successes and the silences in the record.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__endogenous_climb_reading, comparative_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(state_commitment_installation_mechanism__endogenous_climb_reading, apex_gatekeeper_elites).
narrative_ontology:fixing_cost_class(state_commitment_installation_mechanism__endogenous_climb_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a decentralized, comparatively low-violence procedure for transferring legitimacy to new commitments: innovations prove themselves in limited settings, results are observed by third parties, and adoption spreads on demonstrated performance rather than coercive mandate.
% TRANSFER_FUNCTION: Moves legitimacy, deference, and eventually resources from incumbent arrangements to challenger commitments contingent on demonstrated performance; moves the costs of demonstration (time, money, reputational risk) onto challengers; moves the vetting surplus (risk-free adoption of already-proven winners) to apex adopters who bear little of the proving cost.
% ABSENT_VOICES: Failed challengers cannot testify: dissolved organizations leave no seat at the review table, so the record of climbs is curated by survivors. Populations bearing the delay costs appear in the record only as motivation, never as participants. Proponents of mandate-based installation are excluded preemptively, their route dismissed as coercion before argument. All three groups would object to the pathway's self-description as cost-free meritocracy.
% DISAPPEARANCE_RATIONALE: If the climb pathway vanished overnight, legitimacy transfers would have to run through imposition mandates or stall in contested stalemate. Fringe movements would lose their only nonviolent route to influence; apex bodies would face unfiltered adoption demands with no vetting buffer; adoption curves would either steepen abruptly or turn violent, and the demonstration infrastructure (pilots, review bodies, performance records) would lose its organizing purpose.
% FOUNDING_PROBLEM: How can a society replace entrenched commitments without fighting a legitimacy war over each one: how to make normative change possible without making it arbitrary or purely coercive?
% FOUNDING_PROBLEM_CORROBORATION: Comparative-historical scholarship outside any benefiting party documents recurring gradual adoption curves and apex resistance across domains, and archival reconstructions of failed climbs, compiled by historians with no stake in the climb narrative, independently attest both the pathway's operation and its casualties. No corroborating source attests that the pathway is cost-free: the delay-cost burden is documented by demographic and economic historians rather than by the movement beneficiaries themselves.
narrative_ontology:disappearance_verdict(state_commitment_installation_mechanism__endogenous_climb_reading, world_rearranges).
narrative_ontology:founding_problem_status(state_commitment_installation_mechanism__endogenous_climb_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_commitment_installation_mechanism__endogenous_climb_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(state_commitment_installation_mechanism__endogenous_climb_reading, 'none', 1).
narrative_ontology:epsilon_provenance(state_commitment_installation_mechanism__endogenous_climb_reading, 0.55, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_commitment_installation_mechanism__endogenous_climb_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(state_commitment_installation_mechanism__endogenous_climb_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(state_commitment_installation_mechanism__endogenous_climb_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.55: the pathway solves a real coordination problem, so this reading does not score it as pure extraction, but the burden asymmetry is structural, challengers pay all proving costs, apexes adopt proven winners at discount, and each accumulated success inflates the evidentiary standard for the next challenger (moving goalposts), which is why the series rises across the interval. Suppression is 0.36 and is a raw structural property, unscaled by power or scope: the pathway coerces little directly; its force is evidentiary and reputational (a challenger who quits is merely unheard, not punished). Theater ratio is 0.33: a growing share of demonstration activity consists of pilots staged for recognition optics rather than functional tests, tracked temporally below. Accessibility collapse is 0.50: rival routes (mandate politics, crisis-window installation) remain visible and occasionally exercised, so the climb doctrine dominates without erasing alternatives. Resistance is 0.60, reflecting this reading's signature dynamic of apex resistance. The measurement series run on one shared time grid (t=0..30, six points) for both tracked metrics; suppression_requirement series are deliberately omitted because the enforcement picture is static across the interval, the scalar in base_properties carries it, and the traced dynamics are standard-inflation and theater growth, not enforcement-capacity change.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda-setter seat compute differently by construction. From the apex seat the pathway is prudent stewardship: why adopt what has not proven itself? From the fringe-advocate seat it is the only fair route available to the powerless, worth its tolls. From the populations' seat it is a toll paid in years of continued harm while someone else's case is proven. From the failed-challenger seat it is a wager that consumed everything and left no record. The latent coalition of the powerless payers (populations plus the memory of failed climbs) rarely forms precisely because failed challengers exit the record, which is itself a pathway effect the engine should see in the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary and victim declarations drive the derivation: fringe advocates and early adopters sit toward the beneficiary end; populations and failed challengers sit toward the target end with trapped exits amplifying their effective burden. Two overrides are declared because the derivation from role listings alone would misplace two dual-positioned seats. First, fringe_advocates_reform_movements (moderate): listed as beneficiaries, but their situation carries the full demonstration-cost burden, so the derived near-beneficiary d understates what they pay; the override to 0.40 places them near-symmetric, leaning beneficiary, which matches their counterfactual position (without the pathway they would have no route at all). Second, apex_gatekeeper_elites (institutional): agenda-setter-plus-beneficiary derivation would push them strongly toward the subsidized end, but successful climbs erode apex authority and they bear displacement costs, so the override to 0.35 records their genuinely mixed position rather than a pure-subsidy reading.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (change without legitimacy war) remains live, so there is no zombie mismatch: status live crossed with verdict world_rearranges is the consistent cell. The tangled_rope classification is what prevents mislabeling in both directions: a pure-coordination reading would erase the burden asymmetry and the apex's vetting surplus; a pure-extraction reading would erase the genuine filtering function that makes the pathway preferable to mandate war for nearly every seat. The temporal series watches the degradation mode specific to this constraint: if recognition became ceremonial while real adoption ran through patronage, theater_ratio would climb past 0.5 and the pathway would drift toward piton dynamics, coordination in name, inertia in fact. The rising extractiveness series is the early signature of that drift, not yet the arrival.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This constraint is one reading (endogenous_climb_reading) of the kernel state_commitment_installation_mechanism; what would the sibling readings change structurally if adopted as the operative account?',
    'Compare compiled sibling stories: exogenous_imposition_reading relocates beneficiary status to the apex and converts fringe actors into targets; hybrid_cascade_reading splits the flow so both apex and fringe hold partial beneficiary positions. The disagreement resolves only by adjudicating which account best fits the adoption record, never by editing this story.',
    'Classification is reading-indexed: the same historical episode classifies differently under each sibling, so cross-reading comparisons must join on the kernel, not on this story''s metrics.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer-frame indexicality: one reading of a three-reading kernel; sibling readings are separate constraints.').

omega_variable(
    survivorship_of_failed_climbs,
    'Does demonstrated superiority actually drive adoption, or do patronage, timing, and luck drive adoption while superiority narratives are retrofitted by victors?',
    'Archival reconstruction of failed climbs with performance metrics comparable to contemporaneous successes: if equally-performing challengers failed where better-connected ones succeeded, the filtering function is substantially patronage, not merit.',
    'If patronage drives adoption, challenger demonstration labor is wasted rent, epsilon rises materially, and the classification drifts from tangled_rope toward snare; if performance predicts adoption, the coordination credit stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(survivorship_of_failed_climbs, empirical, 'Whether the climb pathway filters on merit or on patronage, given that failures leave thin records.').

omega_variable(
    delay_cost_attribution,
    'Are the harms populations suffer during gradual adoption curves the necessary price of safe legitimation, or artifacts of apex gatekeeping that could be compressed without instability?',
    'Compare adoption-curve lengths across regimes with different gatekeeping intensities at matched hazard levels: if curves compress under weaker apex gatekeeping without raising reversal rates, the delay is gatekeeping artifact, not coordination cost.',
    'If delay is artifact, populations_under_legacy_arrangements are victims of the pathway itself and the victim set strengthens; if delay is irreducible, their harm belongs to the incumbent arrangements and the pathway''s epsilon falls.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(delay_cost_attribution, conceptual, 'Attribution of adoption-delay harms between the pathway and the incumbents it displaces.').

omega_variable(
    constructed_standard_vs_selection_law,
    'Is the climb pathway a structural feature of how legitimacy forms in large societies, or a constructed standard actively maintained by apex gatekeeping and movement self-discipline?',
    'Search for societies or periods where legitimacy transferred without fringe demonstration and without mandate: sustained absence of such cases supports law-like status; documented cases support constructed status.',
    'If constructed, requires_active_enforcement carries the classification weight and the tangled_rope reading is confirmed; if law-like, the constraint drifts rope-ward or mountain-ward and naturality claims gain force, inviting false-summit scrutiny.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constructed_standard_vs_selection_law, conceptual, 'Naturality ambiguity: selection law versus maintained standard.').

omega_variable(
    demonstration_theater_boundary,
    'What share of demonstration activity is functional testing versus pilots staged for recognition optics?',
    'Audit pilot designs against their published use: pilots whose results fed adoption decisions count as functional; pilots whose results were never consulted count as theater.',
    'A theater share above 0.5 would indicate proxy displacement (Goodhart drift) and push the pathway toward piton dynamics; the current 0.33 estimate carries wide uncertainty.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(demonstration_theater_boundary, empirical, 'Functional-versus-staged composition of the demonstration record underlying theater_ratio.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_commitment_installation_mechanism__endogenous_climb_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(scim_endog_climb_tr_t0, state_commitment_installation_mechanism__endogenous_climb_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(scim_endog_climb_tr_t0, observed).
narrative_ontology:measurement(scim_endog_climb_tr_t6, state_commitment_installation_mechanism__endogenous_climb_reading, theater_ratio, 6, 0.21).
narrative_ontology:measurement_basis(scim_endog_climb_tr_t6, observed).
narrative_ontology:measurement(scim_endog_climb_tr_t12, state_commitment_installation_mechanism__endogenous_climb_reading, theater_ratio, 12, 0.24).
narrative_ontology:measurement_basis(scim_endog_climb_tr_t12, observed).
narrative_ontology:measurement(scim_endog_climb_tr_t18, state_commitment_installation_mechanism__endogenous_climb_reading, theater_ratio, 18, 0.27).
narrative_ontology:measurement_basis(scim_endog_climb_tr_t18, observed).
narrative_ontology:measurement(scim_endog_climb_tr_t24, state_commitment_installation_mechanism__endogenous_climb_reading, theater_ratio, 24, 0.3).
narrative_ontology:measurement_basis(scim_endog_climb_tr_t24, observed).
narrative_ontology:measurement(scim_endog_climb_tr_t30, state_commitment_installation_mechanism__endogenous_climb_reading, theater_ratio, 30, 0.33).
narrative_ontology:measurement_basis(scim_endog_climb_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(scim_endog_climb_be_t0, state_commitment_installation_mechanism__endogenous_climb_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement_basis(scim_endog_climb_be_t0, observed).
narrative_ontology:measurement(scim_endog_climb_be_t6, state_commitment_installation_mechanism__endogenous_climb_reading, base_extractiveness, 6, 0.42).
narrative_ontology:measurement_basis(scim_endog_climb_be_t6, observed).
narrative_ontology:measurement(scim_endog_climb_be_t12, state_commitment_installation_mechanism__endogenous_climb_reading, base_extractiveness, 12, 0.46).
narrative_ontology:measurement_basis(scim_endog_climb_be_t12, observed).
narrative_ontology:measurement(scim_endog_climb_be_t18, state_commitment_installation_mechanism__endogenous_climb_reading, base_extractiveness, 18, 0.49).
narrative_ontology:measurement_basis(scim_endog_climb_be_t18, observed).
narrative_ontology:measurement(scim_endog_climb_be_t24, state_commitment_installation_mechanism__endogenous_climb_reading, base_extractiveness, 24, 0.52).
narrative_ontology:measurement_basis(scim_endog_climb_be_t24, observed).
narrative_ontology:measurement(scim_endog_climb_be_t30, state_commitment_installation_mechanism__endogenous_climb_reading, base_extractiveness, 30, 0.55).
narrative_ontology:measurement_basis(scim_endog_climb_be_t30, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(state_commitment_installation_mechanism__endogenous_climb_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_commitment_installation_mechanism__endogenous_climb_reading, resource_allocation).
narrative_ontology:affects_constraint(state_commitment_installation_mechanism__endogenous_climb_reading, state_commitment_installation_mechanism__exogenous_imposition_reading).
narrative_ontology:affects_constraint(state_commitment_installation_mechanism__endogenous_climb_reading, state_commitment_installation_mechanism__hybrid_cascade_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial question 'how do new commitments gain legitimacy?' decomposes per the epsilon-invariance principle into three structurally distinct claims. The endogenous climb reading (this story) authors epsilon for the fringe-demonstration pathway; the exogenous imposition reading authors epsilon for mandate installation with the apex as beneficiary and fringe actors as targets; the hybrid cascade reading authors epsilon for a split flow requiring both apex action and fringe validation. Each member links the others via network.affects_constraints. The endogenous reading functions as the baseline account that the other two position against: exogenous cites the climb account's slowness as its warrant, hybrid cites both as partial accounts. Their epsilon values differ because their beneficiary/victim structures differ, not because one observable is measured two ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(state_commitment_installation_mechanism__endogenous_climb_reading, moderate, 0.4).
constraint_indexing:directionality_override(state_commitment_installation_mechanism__endogenous_climb_reading, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
