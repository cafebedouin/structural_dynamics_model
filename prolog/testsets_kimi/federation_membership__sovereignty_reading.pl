% ============================================================================
% CONSTRAINT STORY: federation_membership__sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_membership__sovereignty_reading, []).

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
 *   constraint_id: federation_membership__sovereignty_reading
 *   human_readable: Federation Membership as Conditional Treaty: Retained National Border Authority (Sovereignty Reading)
 *   domain: political_economy/federalism/migration_policy
 *
 * SUMMARY:
 *   Under this reading, the federation is a treaty among sovereign states:
 *   members confer limited competences, retain authority over their own
 *   borders, and treat free movement as negotiated policy that national
 *   decision can tighten, condition, or suspend. Movement terms are
 *   administered through visas, quotas, recognition rules, and removal
 *   machinery; national governments run that machinery and keep renegotiation
 *   and exit on the table. Resident workers are shielded from incoming labor
 *   competition; would-be movers hold access as a revocable grant rather than
 *   an entitlement. The claim/metric gap is deliberate: the constraint is
 *   CLAIMED as rope (this reading's own frame — border control is legitimate
 *   retained competence, and conditional membership is genuine coordination
 *   among consenting states) while the authored metrics describe substantial,
 *   actively enforced extraction borne by mobile citizens and shortage
 *   employers. The engine measures that divergence per seat; the claim is not
 *   reconciled to the metrics.
 *
 * KEY AGENTS:
 *   - national_government_incumbents: Agenda setter and beneficiary (institutional/mobile) — administer border authority and collect discretion and treaty leverage
 *   - local_labor_market_insiders: Primary beneficiary (organized/constrained) — wage and job-access shielding, delivered through the movement rules
 *   - mobile_federation_citizens: Primary payer (moderate/constrained) — foregone earnings, revocable access, removal risk
 *   - destination_state_employers: Secondary payer (powerful/constrained) — unfilled vacancies and sponsorship compliance costs
 *   - supranational_federation_institutions: Excluded seat — competent elsewhere, not seated on border legitimacy
 *   - migration_policy_analysts: Analytical observer — sees the full structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership__sovereignty_reading, 0.68).
domain_priors:suppression_score(federation_membership__sovereignty_reading, 0.72).
domain_priors:theater_ratio(federation_membership__sovereignty_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership__sovereignty_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(federation_membership__sovereignty_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(federation_membership__sovereignty_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership__sovereignty_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(federation_membership__sovereignty_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership__sovereignty_reading, rope).
narrative_ontology:human_readable(federation_membership__sovereignty_reading, "Federation Membership as Conditional Treaty: Retained National Border Authority (Sovereignty Reading)").
narrative_ontology:topic_domain(federation_membership__sovereignty_reading, "political_economy/federalism/migration_policy").

domain_priors:requires_active_enforcement(federation_membership__sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership__sovereignty_reading, 'af5c4030-e52a-46d8-a6a9-046eb414cb73').
narrative_ontology:cs_kernel_codification('af5c4030-e52a-46d8-a6a9-046eb414cb73', formalized).
narrative_ontology:cs_authority_grounding('af5c4030-e52a-46d8-a6a9-046eb414cb73', lineage).
narrative_ontology:cs_interpretation_layer_present('af5c4030-e52a-46d8-a6a9-046eb414cb73').
narrative_ontology:cs_reading_relation('af5c4030-e52a-46d8-a6a9-046eb414cb73', federation_membership__integration_reading, forecloses).
narrative_ontology:cs_axiom('af5c4030-e52a-46d8-a6a9-046eb414cb73', foundational, unconferred_competence_is_retained).
narrative_ontology:cs_axiom_status(unconferred_competence_is_retained, holdable).
narrative_ontology:cs_axiom_grounding('af5c4030-e52a-46d8-a6a9-046eb414cb73', unconferred_competence_is_retained, conventional).
narrative_ontology:cs_axiom('af5c4030-e52a-46d8-a6a9-046eb414cb73', secondary, movement_terms_bind_by_state_consent).
narrative_ontology:cs_axiom_status(movement_terms_bind_by_state_consent, holdable).
narrative_ontology:cs_axiom_grounding('af5c4030-e52a-46d8-a6a9-046eb414cb73', movement_terms_bind_by_state_consent, conventional).
narrative_ontology:cs_reference_frame('af5c4030-e52a-46d8-a6a9-046eb414cb73', treaty_among_sovereign_states).
narrative_ontology:cs_drift_state('af5c4030-e52a-46d8-a6a9-046eb414cb73', contemporary_federation_politics, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('af5c4030-e52a-46d8-a6a9-046eb414cb73', '2026-06-19T00:00:00Z').
narrative_ontology:cs_kernel_id(federation_membership__sovereignty_reading, federation_membership).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership__sovereignty_reading, local_labor_market_insiders).
narrative_ontology:constraint_beneficiary(federation_membership__sovereignty_reading, national_government_incumbents).
narrative_ontology:constraint_victim(federation_membership__sovereignty_reading, mobile_federation_citizens).
narrative_ontology:constraint_victim(federation_membership__sovereignty_reading, destination_state_employers).
narrative_ontology:constraint_vindicates(federation_membership__sovereignty_reading, retained_sovereignty_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Negotiate and renew federation membership as a treaty among states. Set the terms on which citizens of other member states may enter, work, and settle: quotas, visas, recognition rules, emergency suspensions. Run the border and removals machinery directly. Retain the option to renegotiate or leave, and use that option as leverage in every other federation negotiation. Domestic electorates reward visible control of the border.
narrative_ontology:constraint_stakeholder(federation_membership__sovereignty_reading, national_government_incumbents, agenda_setter,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(federation_membership__sovereignty_reading, national_government_incumbents, beneficiary).

% Resident workers and their households whose job access and wage levels are shielded from incoming member-state labor competition by the movement rules. They are tied to place by housing, pensions, and family; they do not move, the rules keep competitors from arriving. They vote, and border enforcement is one of the things they reward.
narrative_ontology:constraint_stakeholder(federation_membership__sovereignty_reading, local_labor_market_insiders, beneficiary,
    organized, biographical, constrained, local).

% Citizens of member states who would take work, residence, or family life elsewhere in the federation. Their access is a policy grant, not an entitlement: it can be conditioned, delayed, quota-limited, or revoked. Those who stay home forgo the wage differential; those who go carry administrative burden, recognition barriers, and the standing possibility of removal. Their fallback is not moving.
narrative_ontology:constraint_stakeholder(federation_membership__sovereignty_reading, mobile_federation_citizens, payer,
    moderate, biographical, constrained, continental).

% Firms in member states with persistent vacancies who cannot simply hire from the federation labor pool. They sponsor within quotas, absorb compliance costs, delay expansion, or automate. They lobby for liberalization but accept the regime because relocation of the whole enterprise is costlier than compliance.
narrative_ontology:constraint_stakeholder(federation_membership__sovereignty_reading, destination_state_employers, payer,
    powerful, biographical, constrained, national).

% Administer the competences member states have conferred: trade, standards, program funding. On the border question they are not seated — movement terms are reserved to national decision under the treaty frame, and their attempts to speak on border legitimacy are answered with competence objections and renegotiation threats.
narrative_ontology:constraint_stakeholder(federation_membership__sovereignty_reading, supranational_federation_institutions, excluded,
    institutional, generational, constrained, continental).

% Track flows, enforcement operations, wage effects, and treaty practice across the federation. They describe who gains and who bears costs without setting or paying either, and their comparisons of restricted and open corridors are the main external check on the arrangement's self-description.
narrative_ontology:constraint_stakeholder(federation_membership__sovereignty_reading, migration_policy_analysts, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(federation_membership__sovereignty_reading, local_labor_market_insiders).
narrative_ontology:fixing_cost_class(federation_membership__sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates deep cooperation among states that will not surrender sovereign discretion: trade, standards, and residency terms are negotiated as treaty articles, and each state calibrates labor inflows to its domestic conditions so that membership remains politically sustainable at home.
% TRANSFER_FUNCTION: Moves the terms of labor-market access and residence across internal borders out of the realm of entitlement and into the realm of conditional national policy. Concretely it transfers wage-competition shielding to resident workers and discretion-and-leverage to national governments, paid for by would-be movers in foregone earnings, family separation, and administrative burden, and by shortage-sector employers in unfilled vacancies.
% ABSENT_VOICES: Mobile federation citizens and the supranational institutions are not in the room when border legitimacy is asserted. Their claim — that movement within the federation is an entitlement of membership rather than a policy grant — is exactly the claim the treaty frame keeps off the agenda.
% DISAPPEARANCE_RATIONALE: If national discretion over intra-federation movement vanished overnight, labor would reallocate across the federation within years, wage differentials would compress, shortage employers would recruit freely, and national governments would lose one of their primary levers in every other federation negotiation. The membership bargain itself would have to be re-struck.
% FOUNDING_PROBLEM: The federation was founded among states that refused to cede sovereignty: the founding problem was enabling durable, deep cooperation while leaving each member's control over its own territory and membership conditions intact, so that joining did not mean dissolving into the whole.
% FOUNDING_PROBLEM_CORROBORATION: Attested outside the benefiting parties by the treaty texts themselves, which reserve border competence to members; by the recorded practice of renegotiations, opt-outs, and an actual member exit invoked under domestic authority; and by comparative-federalism scholarship documenting that no member state has formally surrendered the competence. No corroboration rests solely on the governments that benefit.
narrative_ontology:disappearance_verdict(federation_membership__sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership__sovereignty_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership__sovereignty_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-18',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k3', 'max_tokens=32000,temperature=default,reasoning=max').
narrative_ontology:story_seed(federation_membership__sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership__sovereignty_reading, 0.68, 'kimi-k3', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership__sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(federation_membership__sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(federation_membership__sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68) because conditioning movement on revocable national policy denies movers wage differentials, residence, and family life they would otherwise realize as members of the same federation — and the denial is the mechanism, not a side effect. Suppression (0.72) is a raw structural property: the regime persists only through active border enforcement, document regimes, and removals; it is not scaled by power or scope in this authoring — the engine scales only extractiveness, via directionality and scope. Theater ratio (0.30) reflects real enforcement with a growing share of sovereignty-signaling performance. Accessibility_collapse (0.55) is partial: a blocked mover retains third-country options and the staying-home fallback, so alternatives narrow rather than vanish. Resistance (0.60) is real: employer lobbying, mover evasion, litigation, and advocacy contest the regime continuously. The measurement series runs on one shared grid (0–40, six points); extractiveness and suppression requirement rise together over the interval as enlargement and widening wage differentials increased mobility demand while restriction persisted and its enforcement machinery matured — more people with more reason to move, held at a harder border. Theater drifts up slowly as symbolic border gestures multiply faster than functional ones.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat the arrangement reads as legitimate coordination: a competence deliberately retained, exercised under treaty, sustaining a membership bargain that would otherwise collapse. From the payer seats the same structure reads as conditioned, revocable access to their own federation's labor market, backed by removals — extraction with a coordination story attached. The engine computes this divergence from the declared beneficiary/victim structure; the authored rope claim records the reading's self-understanding and does not adjudicate the payer seats' experience.
 *
 * DIRECTIONALITY LOGIC:
 *   National government incumbents and local labor market insiders are structural beneficiaries: governments collect discretion, leverage, and electoral reward; insiders collect wage-competition shielding — both sit near the beneficiary end (low d, damped or inverted effective extraction). Mobile federation citizens are the full targets: the regime's costs land on them as foregone earnings and revocable status (high d). Destination-state employers are secondary targets — powerful but constrained, absorbing vacancy and compliance costs without leverage to change the terms. Supranational institutions are excluded rather than coordinated; their non-seating is itself an enforcement product. The derivation chain (beneficiary/victim + exit) produces these values directly; no directionality overrides are authored.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — cooperation among states that refuse to cede sovereignty — remains live, corroborated by treaty text and by the continued exercise of renegotiation and exit, so the arrangement is not a zombie mandate and declaring mandatrophy resolved would misclassify retained discretion as inertia. The classification discipline cuts the other way too: the rising theater series is authored precisely so that, if border activity ever decouples from actual movement control and persists as pure sovereignty signaling, the piton signature has the temporal data to fire. Coordination is not mislabeled as extraction (the treaty function is declared and the coordination type is set), and extraction is not mislabeled as coordination (victims and enforcement are declared, and the metrics are authored high).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_disclosure,
    'This constraint is one reading of the federation_membership kernel — the sovereignty reading. Would the sibling integration reading instantiate a structurally different constraint rather than a different evaluation of this one?',
    'Not empirically resolvable from within this story: the readings differ in what membership IS (conditional treaty versus irreversible integration), a commitment choice under-determined by treaty text and practice alike. The sibling is authored as a separate constraint (federation_membership__integration_reading) and linked; comparison proceeds file-to-file, never by hedging ε inside this one.',
    'Under the sibling reading the structure inverts: mobile citizens become beneficiaries of a movement right, restrictionist incumbents bear the constraint''s costs, and ε measured from mobility restriction is substantially lower because movement is guaranteed rather than conditioned. Classification, gain_flow, and the six-questions answers would all change with the constraint, not with the observer.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_disclosure, conceptual, 'Committer disclosure: this story is the sovereignty reading of the federation_membership kernel; the sibling reading is a different constraint.').

omega_variable(
    disagreement_locus_amendment_practice,
    'Where is the disagreement located — in the legal character of membership (treaty versus constitution) or merely in the policy content of movement rules? Can a member state alter movement terms by ordinary legislation, or only by constitutional-level revision?',
    'Constitutional adjudication and amendment practice: if member states have in fact tightened, conditioned, or suspended movement by ordinary national decision and the federation has accommodated it, the kernel sits at treaty level and this reading stands; if such changes require constitutional-level revision, the sovereignty reading misdescribes the operative constraint.',
    'A finding that movement terms are constitutionally entrenched would retire this story in favor of the sibling; a finding of ordinary-legislation control confirms this reading''s reference frame.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(disagreement_locus_amendment_practice, empirical, 'Locating the kernel contest in amendment practice rather than in rhetoric.').

omega_variable(
    insider_wage_protection_efficacy,
    'Does the mobility restriction actually deliver the wage protection to local labor market insiders that its beneficiary structure presumes, or do capital mobility, informal cross-border work, and labor-demand adjustment nullify the protection while the costs to movers persist?',
    'Long-run wage and employment studies comparing restricted and open internal corridors, plus audits of informal cross-border labor under the restriction.',
    'If the protection proves illusory, the coordination story thins toward performance, gain_flow misattributes receipt, and the constraint drifts toward piton dynamics — costs carried by movers, benefits captured by no one, maintained as sovereignty signaling. If the protection is real, the declared beneficiary structure is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(insider_wage_protection_efficacy, empirical, 'Whether the extraction purchases the coordination benefit its beneficiaries claim.').

omega_variable(
    exit_credibility_condition,
    'Does the conditionality of membership remain load-bearing — is exit or forced renegotiation a credible, exercised option — or has membership become de facto irreversible while the sovereignty frame persists as rhetoric?',
    'Track actual exits, formal exit-clause invocations, and renegotiation outcomes over the interval; measure whether conditionality is exercised or only invoked in speech.',
    'If conditionality is structurally unavailable in practice, the reference frame (treaty among sovereign states) has collapsed, drift_state magnitude should be revised toward severe, and the constraint''s classification must be re-derived on the operative rather than the professed structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exit_credibility_condition, empirical, 'Whether conditional consent remains an operative fact of the arrangement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership__sovereignty_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t0, federation_membership__sovereignty_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(fede_tr_t8, federation_membership__sovereignty_reading, theater_ratio, 8, 0.18).
narrative_ontology:measurement(fede_tr_t16, federation_membership__sovereignty_reading, theater_ratio, 16, 0.2).
narrative_ontology:measurement(fede_tr_t24, federation_membership__sovereignty_reading, theater_ratio, 24, 0.22).
narrative_ontology:measurement(fede_tr_t32, federation_membership__sovereignty_reading, theater_ratio, 32, 0.26).
narrative_ontology:measurement(fede_tr_t40, federation_membership__sovereignty_reading, theater_ratio, 40, 0.3).

% Extraction over time
narrative_ontology:measurement(fede_be_t0, federation_membership__sovereignty_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(fede_be_t8, federation_membership__sovereignty_reading, base_extractiveness, 8, 0.55).
narrative_ontology:measurement(fede_be_t16, federation_membership__sovereignty_reading, base_extractiveness, 16, 0.58).
narrative_ontology:measurement(fede_be_t24, federation_membership__sovereignty_reading, base_extractiveness, 24, 0.62).
narrative_ontology:measurement(fede_be_t32, federation_membership__sovereignty_reading, base_extractiveness, 32, 0.65).
narrative_ontology:measurement(fede_be_t40, federation_membership__sovereignty_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t0, federation_membership__sovereignty_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(fede_su_t8, federation_membership__sovereignty_reading, suppression_requirement, 8, 0.54).
narrative_ontology:measurement(fede_su_t16, federation_membership__sovereignty_reading, suppression_requirement, 16, 0.58).
narrative_ontology:measurement(fede_su_t24, federation_membership__sovereignty_reading, suppression_requirement, 24, 0.63).
narrative_ontology:measurement(fede_su_t32, federation_membership__sovereignty_reading, suppression_requirement, 32, 0.68).
narrative_ontology:measurement(fede_su_t40, federation_membership__sovereignty_reading, suppression_requirement, 40, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership__sovereignty_reading, resource_allocation).
narrative_ontology:affects_constraint(federation_membership__sovereignty_reading, federation_membership__integration_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the federation_membership kernel decomposes into two readings that instantiate structurally distinct constraints. This file is the sovereignty reading (conditional treaty, retained border legitimacy, movement as policy): local labor market insiders and national governments are beneficiaries, mobile citizens and shortage employers are victims, ε is high because movement access is conditioned and revocable. The integration reading (irreversible integration, movement as constitutional right) inverts the beneficiary/victim structure and carries a substantially lower ε. The two are not one constraint from two angles — their ε values, failure modes, and enforcement structures differ — so they are authored as separate ε-invariant files and linked, per the ε-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
