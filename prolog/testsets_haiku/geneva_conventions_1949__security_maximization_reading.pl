% ============================================================================
% CONSTRAINT STORY: geneva_conventions_1949__security_maximization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_geneva_conventions_1949__security_maximization_reading, []).

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
 *   constraint_id: geneva_conventions_1949__security_maximization_reading
 *   human_readable: Geneva Conventions 1949 — Security Maximization Reading
 *   domain: international_humanitarian_law/political_philosophy
 *
 * SUMMARY:
 *   This constraint instantiates the security-maximization reading of the
 *   Geneva Conventions (1949), one of three contested interpretations. The
 *   reading claims that humanitarian protections designed for peacetime
 *   reciprocal conflict are incompatible with asymmetric irregular warfare,
 *   and that state security imperatives therefore justify suspending most
 *   protections to maximize counterinsurgency effectiveness. The reading
 *   transfers legal status, habeas corpus, civilian immunity, and freedom
 *   from coercive interrogation from combatants, detained persons, and
 *   civilian populations to the state security apparatus. It justifies this
 *   transfer via discretionary categories ('unlawful combatant,' 'operational
 *   necessity,' 'human shields doctrine') that the state controls
 *   unilaterally. The measurement series shows extraction rising sharply over
 *   the first 12 time points and then plateauing — a pattern consistent with
 *   doctrine hardening and normalization, followed by institutional
 *   entrenchment (high theater ratio indicates performative compliance with
 *   Geneva framework while substance is hollowed out).
 *
 * KEY AGENTS:
 *   - state_security_apparatus: Institutional beneficiary; controls threat definitions and doctrine authority
 *   - military_command: Institutional beneficiary; reduced operational friction; collateral damage normalized
 *   - irregular_combatants: Powerless victims; denied POW status, legal process, habeas corpus
 *   - detained_persons_without_status: Powerless victims; indefinite detention; coercive interrogation normalized
 *   - civilian_populations: Constrained victims; degraded civilian immunity; collateral damage accepted
 *   - humanitarian_organizations: Partially excluded; restricted access to detention; monitoring neutered
 *   - humanitarian_ceiling_advocates: Excluded; foundational axiom negated by this reading
 *   - state_leadership: Beneficiary; political credit for security; faces international legal jeopardy
 *   - international_courts: Observer; excluded from necessity adjudication; authority non-recognized
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(geneva_conventions_1949__security_maximization_reading, 0.82).
domain_priors:suppression_score(geneva_conventions_1949__security_maximization_reading, 0.91).
domain_priors:theater_ratio(geneva_conventions_1949__security_maximization_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(geneva_conventions_1949__security_maximization_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(geneva_conventions_1949__security_maximization_reading, suppression_requirement, 0.91).
narrative_ontology:constraint_metric(geneva_conventions_1949__security_maximization_reading, theater_ratio, 0.68).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(geneva_conventions_1949__security_maximization_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(geneva_conventions_1949__security_maximization_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(geneva_conventions_1949__security_maximization_reading, tangled_rope).
narrative_ontology:human_readable(geneva_conventions_1949__security_maximization_reading, "Geneva Conventions 1949 — Security Maximization Reading").
narrative_ontology:topic_domain(geneva_conventions_1949__security_maximization_reading, "international_humanitarian_law/political_philosophy").

domain_priors:requires_active_enforcement(geneva_conventions_1949__security_maximization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(geneva_conventions_1949__security_maximization_reading, '5200d60d-df56-4a6c-a462-2188a53458da').
narrative_ontology:cs_kernel_codification('5200d60d-df56-4a6c-a462-2188a53458da', fixed_text).
narrative_ontology:cs_authority_grounding('5200d60d-df56-4a6c-a462-2188a53458da', extraction).
narrative_ontology:cs_interpretation_layer_present('5200d60d-df56-4a6c-a462-2188a53458da').
narrative_ontology:cs_reading_relation('5200d60d-df56-4a6c-a462-2188a53458da', geneva_conventions_1949__humanitarian_ceiling_reading, forecloses).
narrative_ontology:cs_reading_relation('5200d60d-df56-4a6c-a462-2188a53458da', geneva_conventions_1949__conditional_reciprocity_reading, coexists_with).
narrative_ontology:cs_axiom('5200d60d-df56-4a6c-a462-2188a53458da', foundational, necessity_unilateral_determination).
narrative_ontology:cs_axiom_status(necessity_unilateral_determination, holdable).
narrative_ontology:cs_axiom_grounding('5200d60d-df56-4a6c-a462-2188a53458da', necessity_unilateral_determination, instrumental).
narrative_ontology:cs_axiom('5200d60d-df56-4a6c-a462-2188a53458da', foundational, asymmetric_threat_justifies_suspension).
narrative_ontology:cs_axiom_status(asymmetric_threat_justifies_suspension, holdable).
narrative_ontology:cs_axiom_grounding('5200d60d-df56-4a6c-a462-2188a53458da', asymmetric_threat_justifies_suspension, empirically_contingent).
narrative_ontology:cs_reference_frame('5200d60d-df56-4a6c-a462-2188a53458da', security_imperative_supremacy).
narrative_ontology:cs_drift_state('5200d60d-df56-4a6c-a462-2188a53458da', post_2001_counterinsurgency_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('5200d60d-df56-4a6c-a462-2188a53458da', '').
narrative_ontology:cs_kernel_id(geneva_conventions_1949__security_maximization_reading, geneva_conventions_1949).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__security_maximization_reading, state_security_apparatus).
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__security_maximization_reading, military_command).
narrative_ontology:constraint_victim(geneva_conventions_1949__security_maximization_reading, irregular_combatants).
narrative_ontology:constraint_victim(geneva_conventions_1949__security_maximization_reading, detained_persons_without_status).
narrative_ontology:constraint_victim(geneva_conventions_1949__security_maximization_reading, civilian_populations_in_conflict_zones).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__security_maximization_reading, state_leadership).
narrative_ontology:constraint_victim(geneva_conventions_1949__security_maximization_reading, international_humanitarian_organizations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets doctrine that interprets Conventions through security-maximization lens. Controls threat definitions, detention classification, interrogation protocols. Maintains authority to declare necessity. Retains ability to exit international treaties and adjust doctrine unilaterally. Collects the extraction (reduced constraints on counterinsurgency operations, enhanced intelligence-gathering, faster threat neutralization, no international oversight of detention or interrogation).
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__security_maximization_reading, state_security_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% Operates under expanded interpretation. Benefits from reduced detention-procedure requirements, accepted collateral damage, 'human shields' doctrine that lowers targeting thresholds, normalized coercive interrogation. Cannot unilaterally change doctrine but operates within its latitude. Primary beneficiary from daily operations.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__security_maximization_reading, military_command, beneficiary,
    institutional, generational, constrained, national).

% Deny POW status. Classified as 'unlawful combatants' by state authority. Subject to indefinite detention without trial, coercive interrogation, execution without legal process. Distributed cell structure and civilian cover trigger the security reading's justification. No legal status, no habeas corpus, no right to counsel. No path to release that is not capitulation or death.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__security_maximization_reading, irregular_combatants, payer,
    powerless, immediate, trapped, local).

% Persons captured near conflict zones, neither clearly combatants nor civilians. Held without charge, trial date, or release plan. Coercive interrogation normalized as security measure. Family contact restricted. State discretion over their classification means no certain legal status. Indefinite detention is lawful under the reading.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__security_maximization_reading, detained_persons_without_status, payer,
    powerless, biographical, trapped, local).

% Experience degraded civilian immunity. 'Human shields' doctrine permits wider targeting latitude where insurgents commingle with civilian infrastructure. Collateral-damage acceptance thresholds are set by military calculation. Civilian protection shrinks as operational-necessity framing expands. Displacement and destruction are accepted costs of counter-irregular operations.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__security_maximization_reading, civilian_populations_in_conflict_zones, payer,
    powerless, immediate, constrained, local).

% Denied access to detention facilities during interrogation. Forbidden from publicizing abuse. Pressured to de-prioritize documenting violations. Monitoring mandate neutered by classification restrictions. Work within highly constrained space while trying to maintain humanitarian presence. Bear the cost of compromised mandate.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__security_maximization_reading, international_humanitarian_organizations, payer,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(geneva_conventions_1949__security_maximization_reading, international_humanitarian_organizations, observer).

% Argue for absolute humanitarian minimums regardless of threat or reciprocity. This reading's core premise directly negates their foundational axiom. Excluded from security-doctrine design. Advocacy treated as naïve or dangerous by adopting states. International courts, NGOs, academics holding this position are institutionally marginalized. Their alternative reading is the sibling constraint.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__security_maximization_reading, humanitarian_ceiling_advocates, excluded,
    organized, generational, constrained, global).

% Gains political credit for security effectiveness and reduced national-force casualties. Reading permits faster intelligence, faster targeting, fewer operational constraints. Accountable domestically for attacks on nationals but (under the reading) not internationally accountable for overseas detention or interrogation. Faces legal jeopardy if courts challenge the reading; can exit international instruments if challenged.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__security_maximization_reading, state_leadership, beneficiary,
    institutional, biographical, mobile, national).

% Hold middle position: Conventions apply fully unless adversaries violate; non-compliance permits proportional (but bounded) degradation. This reading subsumes their position by removing reciprocity ceiling — permits suspension based on asymmetry alone, not violation-for-violation.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__security_maximization_reading, conditional_reciprocity_advocates, observer,
    organized, generational, constrained, global).

% Review cases arising from the reading's application. The reading's authority premise excludes courts from adjudicating necessity claims. States adopting it do not recognize court jurisdiction over security determinations. Courts retain theoretical authority but face non-compliance or withdrawal from international instruments if they attempt review.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__security_maximization_reading, international_courts, observer,
    organized, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(geneva_conventions_1949__security_maximization_reading, state_security_apparatus).
narrative_ontology:fixing_cost_class(geneva_conventions_1949__security_maximization_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared framework (the Geneva Conventions) within which states can define the bounds of permissible violence in armed conflict. The reading instantiates ONE possible coordination solution: harmonize state security interests by permitting suspension of humanitarian constraints under asymmetric-threat conditions, with the state as the arbiter of necessity. This allows states to coordinate on shared threat definitions and mutual tolerance for expanded detention/interrogation regimes.
% TRANSFER_FUNCTION: Transfers humanitarian protections (legal status, habeas corpus, freedom from coercive interrogation, civilian immunity) from irregular combatants, detained persons of ambiguous status, and civilian populations in conflict zones to the state security apparatus and military command. The transfer is mediated by discretionary categories ('unlawful combatant,' 'human shields,' 'operational necessity') that the state controls.
% ABSENT_VOICES: Irregular combatants have no seat at the table where security doctrine is designed — they are the parties to be controlled, not consulted. Humanitarian organizations are excluded from detention-facility access and documentation. Humanitarian-ceiling advocates (the sibling reading's constituency) are institutionally marginalized. Civilian populations in conflict zones have no voice in targeting-threshold decisions. International courts are excluded from necessity adjudication.
% DISAPPEARANCE_RATIONALE: If this reading vanished and the humanitarian-ceiling reading took institutional dominance, detention regimes would shorten (trial or release mandates), coercive interrogation would cease, civilian immunity would be restored to near-absolute status, and irregular combatants would be afforded POW status or at minimum trial rights. The organizational capacity of states to conduct counterinsurgency would be substantially constrained. Conversely, the constraint currently persists only through active enforcement (threat of state violence against detainees, exclusion of monitors, classification systems that the state controls).
% FOUNDING_PROBLEM: Early Geneva Conventions (1949) were negotiated in peacetime, assuming reciprocal state adversaries who wore uniforms and respected protections. Post-WWII irregular warfare and non-state actors do not fit this model. The security-maximization reading claims the founding constraints are obsolete against asymmetric threats: insurgents hide among civilians, do not wear uniforms, and do not recognize humanitarian law themselves. Therefore, suspending protections is justified as adaptation to a new threat class.
% FOUNDING_PROBLEM_CORROBORATION: State security establishments attest that asymmetric threats render traditional protections operationally impossible and civilians unsafe if constraints persist. Humanitarian organizations and international courts attest that the threat, while real, does not necessitate abandoning absolute minimums — civilian immunity and prohibition on torture are maintainable even under asymmetric pressure. Academic literature in law of armed conflict is divided; military strategists tend to endorse the reading, while humanitarian law scholars tend to reject it. No consensus outside the security-apparatus constituency exists on whether the founding problem justifies the solution.
narrative_ontology:disappearance_verdict(geneva_conventions_1949__security_maximization_reading, world_rearranges).
narrative_ontology:founding_problem_status(geneva_conventions_1949__security_maximization_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(geneva_conventions_1949__security_maximization_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(geneva_conventions_1949__security_maximization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(geneva_conventions_1949__security_maximization_reading, 0.82, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(geneva_conventions_1949__security_maximization_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(geneva_conventions_1949__security_maximization_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(geneva_conventions_1949__security_maximization_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.82 at interval end) because the reading systematically transfers protections to the state without reciprocal constraint — the state defines threat, decides detentions, and sets interrogation boundaries. Suppression is very high (0.91) because the reading's persistence depends on active enforcement: classification systems are state-controlled, documentation is restricted, international monitors are excluded, and dissenting voices (humanitarian advocates, international courts) are marginalized or ignored. Theater is moderately high (0.68) because the reading maintains formal adherence to Geneva Conventions while redefining their operative scope — detention is 'lawful' under the new reading, interrogation is 'non-torture' by definitional authority, and collateral damage is 'proportional' by state calculation. The measurement trajectory shows steady extraction increase (t0→t15) followed by plateauing (t15→t25), indicating doctrine consolidation and institutional normalization. Theater ratio rises and levels off in parallel, consistent with initial boundary-pushing followed by new 'normal' establishment — a piton-adjacent dynamic where the state invests heavily in defending its interpretation but faces diminishing resistance as the reading becomes institutionalized and alternative interpretations are pushed to the margins.
 *
 * PERSPECTIVAL GAP:
 *   From the state security apparatus seat, this reading is a rational coordination solution to a real asymmetric-threat problem: it permits efficient intelligence gathering, faster threat neutralization, and protection of national civilians. From the detained-person and irregular-combatant seats, it is pure extraction with theatrical consent — the Conventions are invoked in form but negated in substance. From the humanitarian-organizations seat, it is a destructive precedent that erodes the absolute minimums they were created to protect. From the international-courts seat, it is an illegitimate unilateral reinterpretation that bypasses adjudication. The engine computes these divergent types from the structural data: beneficiary/victim, power/exit, enforceability. The security apparatus will compute as beneficiary with high d; detained persons will compute as targets with high d; humanitarian organizations will compute as trapped or constrained with asymmetric extraction. The state leadership computes as beneficiary with arbitrage-grade exit (can withdraw from international instruments if courts challenge the reading). The perspectival gap is structural, not evaluative.
 *
 * DIRECTIONALITY LOGIC:
 *   State security apparatus: role=agenda_setter, power=institutional, exit=arbitrage → low d (full beneficiary); can redefine doctrine at will and exit international oversight. Military command: role=beneficiary, power=institutional, exit=constrained → moderate d (constrained beneficiary); benefits from reduced friction but cannot unilaterally reshape the doctrine. Irregular combatants: role=payer, power=powerless, exit=trapped → high d (full target); no legal status, no habeas corpus, classified by state authority, no path to release. Detained persons without status: role=payer, power=powerless, exit=trapped → high d (full target); indefinite detention, state discretion over their category. Civilian populations: role=payer, power=powerless, exit=constrained → high d (near-target); no control over targeting thresholds, collateral-damage acceptance constrains their exit. Humanitarian organizations: role=payer+observer, power=organized, exit=constrained → moderate-high d (constrained target); restricted from detention access, monitored themselves, but retain some international platform. International courts: role=observer, power=organized, exit=analytical → low-moderate d (beneficiary at theory level, but powerless in practice due to non-recognition). The directionality overrides would be necessary only if domain knowledge suggested the power or exit derivation was missing a critical institutional nuance — in this case, the structural derivation captures the asymmetry directly.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (asymmetric irregular warfare incompatible with reciprocal-state Conventions) is contested: security apparatus claims it is live and growing; humanitarian advocates claim it is solvable within absolute-minimum frameworks; courts claim it does not justify unilateral suspension. The disappearance verdict is world_rearranges: if this reading vanished and the humanitarian-ceiling reading took hold, detention regimes would transform, interrogation would cease, civilian immunity would be restored. This indicates the constraint is not mandatropic — it persists because it is actively maintained and enforced, not because its founding problem has died while the structure persists. However, the high theater ratio (0.68) and the measurement pattern suggest a piton-like dynamic: as the reading becomes normalized and institutionalized, the initial functional work (threat assessment, operational efficiency) is supplemented by theatrical maintenance (demonstrating compliance with Conventions while hollowing their substance). The constraint is not dead-mandate, but it is increasingly performative. The mandatrophy analysis resolves via the founding_problem_status x disappearance_verdict mismatch: status=contested + verdict=world_rearranges flags a live but increasingly theatrical enforcement picture. The engine would flag this for review as a potential late-stage piton (a tangled_rope sliding toward piton via normalization and theater accumulation).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    asymmetric_threat_authenticity,
    'Does the asymmetric irregular-warfare threat genuinely require suspension of humanitarian protections, or is the threat level separable from the protection question?',
    'Comparative analysis of counterinsurgency efficacy across regimes that maintain humanitarian protections vs. those that suspend them; documentation of intelligence-gathering outcomes and operational success rates under each framework.',
    'If threat and protection are separable (efficacy data shows no significant difference), the suspension is revealed as extraction camouflaged by threat inflation; if suspension is necessary for efficacy, the reading''s core justification is sustained. If mixed (some operations require suspension, others do not), the boundary between necessary and extractive becomes the live question.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(asymmetric_threat_authenticity, empirical, 'Whether asymmetric threat necessitates humanitarian suspension or if threat and protection are analytically separable.').

omega_variable(
    unilateral_necessity_authority,
    'Can a state legitimately determine ''necessity'' unilaterally, or does necessity adjudication require external oversight?',
    'Evolution of international law doctrine and state practice: do states converge on self-adjudication, or do treaty amendments or customary law establish independent review mechanisms? Precedent analysis from national courts reviewing state-security claims.',
    'If unilateral authority is accepted as custom, the reading becomes legitimate governance doctrine; if external review is reasserted, the reading is reclassified as unauthorized reinterpretation. If hybrid (states retain unilateral authority in practice but face jeopardy from courts), institutional instability persists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unilateral_necessity_authority, conceptual, 'Who adjudicates necessity: the state or external authority?').

omega_variable(
    protection_vs_efficacy_false_choice,
    'Is the dichotomy between absolute humanitarian protection and operational efficacy real, or do states use it rhetorically to justify extraction?',
    'Systematic review of detention and interrogation practices under this reading vs. humanitarian-ceiling regimes; measurement of intelligence yield, interrogation reliability, and operational success independent of protection levels. Post-conflict analysis of whether interrogations under coercion produced reliable intelligence vs. unreliable confessions.',
    'If protection and efficacy are negatively correlated (protection reduces efficacy), the reading is justified on pragmatic grounds; if uncorrelated or positively correlated (protection enhances reliability), the reading is revealed as extractive cover for institutional preference. If the relationship is context-dependent, the necessary boundary becomes analyzable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(protection_vs_efficacy_false_choice, empirical, 'Whether humanitarian protections and counterinsurgency efficacy are actually in tension.').

omega_variable(
    reading_authority_legitimacy_grounding,
    'On what authority does the state claim the right to reinterpret the Geneva Conventions unilaterally?',
    'Analysis of the state''s cited legal basis (original-intent doctrine, evolutionary interpretation, state-sovereignty doctrine, or emergency powers). Comparison to other treaty interpretations by the same state and international consensus on treaty evolution.',
    'If the reinterpretation is authorized by the treaty''s own amendment clauses or recognized custom, it is structurally legitimate; if it is unilateral rewriting outside authorized amendment, it is ultra vires. The legitimacy grounding feeds into whether the reading can sustain institutional authority.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_authority_legitimacy_grounding, conceptual, 'What grounds the state''s authority to reinterpret the Conventions?').

omega_variable(
    civilian_immunity_separability,
    'Is degraded civilian immunity (via ''human shields'' doctrine and collateral-damage acceptance) a necessary component of irregular-warfare adaptation, or could states maintain absolute civilian immunity while suspending combatant protections?',
    'Operational analysis of asymmetric conflicts under different civilian-immunity regimes; documentation of whether insurgent behavior changes under absolute-immunity vs. degraded-immunity rules; civilian casualty rates and strategic outcomes.',
    'If civilian immunity is separable from combatant-protection suspension, the reading is partly disaggregable — some suspensions might be justified while civilian immunity is not. If inseparable, the reading stands as a package. If civilian immunity can be maintained without degrading operational effectiveness, the reading''s coherence is challenged.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(civilian_immunity_separability, empirical, 'Can absolute civilian immunity be maintained while combatant protections are suspended?').

omega_variable(
    institutional_identity_lock_suppression,
    'Is the high suppression (0.91) structural (external barriers, legal restrictions) or internalized (states have absorbed the reading as legitimate, believe their own necessity narrative)?',
    'Post-suppression trajectory: if states that adopt this reading later abandon it (due to external pressure, regime change, or treaty renegotiation), does the suppression persist or dissolve? Do individuals trained under this doctrine retain belief in it after institutional context changes?',
    'If suppression is structural, removing external barriers (sanctions, court authority) would alter the reading''s persistence; if internalized, states or officers would carry the reading forward even without external enforcement. If mixed, the mechanisms differ between state institutions and individual agents, and deprogramming efforts would differ accordingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_identity_lock_suppression, empirical, 'Is measured suppression structural or internalized (or both, and in what proportion)?').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(geneva_conventions_1949__security_maximization_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gene_tr_t0, geneva_conventions_1949__security_maximization_reading, theater_ratio, 0, 0.55).
narrative_ontology:measurement_basis(gene_tr_t0, observed).
narrative_ontology:measurement(gene_tr_t3, geneva_conventions_1949__security_maximization_reading, theater_ratio, 3, 0.58).
narrative_ontology:measurement_basis(gene_tr_t3, observed).
narrative_ontology:measurement(gene_tr_t6, geneva_conventions_1949__security_maximization_reading, theater_ratio, 6, 0.61).
narrative_ontology:measurement_basis(gene_tr_t6, observed).
narrative_ontology:measurement(gene_tr_t9, geneva_conventions_1949__security_maximization_reading, theater_ratio, 9, 0.64).
narrative_ontology:measurement_basis(gene_tr_t9, observed).
narrative_ontology:measurement(gene_tr_t12, geneva_conventions_1949__security_maximization_reading, theater_ratio, 12, 0.66).
narrative_ontology:measurement_basis(gene_tr_t12, observed).
narrative_ontology:measurement(gene_tr_t15, geneva_conventions_1949__security_maximization_reading, theater_ratio, 15, 0.67).
narrative_ontology:measurement_basis(gene_tr_t15, observed).
narrative_ontology:measurement(gene_tr_t18, geneva_conventions_1949__security_maximization_reading, theater_ratio, 18, 0.68).
narrative_ontology:measurement_basis(gene_tr_t18, observed).
narrative_ontology:measurement(gene_tr_t21, geneva_conventions_1949__security_maximization_reading, theater_ratio, 21, 0.68).
narrative_ontology:measurement_basis(gene_tr_t21, observed).
narrative_ontology:measurement(gene_tr_t25, geneva_conventions_1949__security_maximization_reading, theater_ratio, 25, 0.68).
narrative_ontology:measurement_basis(gene_tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(gene_be_t0, geneva_conventions_1949__security_maximization_reading, base_extractiveness, 0, 0.68).
narrative_ontology:measurement_basis(gene_be_t0, observed).
narrative_ontology:measurement(gene_be_t3, geneva_conventions_1949__security_maximization_reading, base_extractiveness, 3, 0.71).
narrative_ontology:measurement_basis(gene_be_t3, observed).
narrative_ontology:measurement(gene_be_t6, geneva_conventions_1949__security_maximization_reading, base_extractiveness, 6, 0.74).
narrative_ontology:measurement_basis(gene_be_t6, observed).
narrative_ontology:measurement(gene_be_t9, geneva_conventions_1949__security_maximization_reading, base_extractiveness, 9, 0.77).
narrative_ontology:measurement_basis(gene_be_t9, observed).
narrative_ontology:measurement(gene_be_t12, geneva_conventions_1949__security_maximization_reading, base_extractiveness, 12, 0.79).
narrative_ontology:measurement_basis(gene_be_t12, observed).
narrative_ontology:measurement(gene_be_t15, geneva_conventions_1949__security_maximization_reading, base_extractiveness, 15, 0.8).
narrative_ontology:measurement_basis(gene_be_t15, observed).
narrative_ontology:measurement(gene_be_t18, geneva_conventions_1949__security_maximization_reading, base_extractiveness, 18, 0.81).
narrative_ontology:measurement_basis(gene_be_t18, observed).
narrative_ontology:measurement(gene_be_t21, geneva_conventions_1949__security_maximization_reading, base_extractiveness, 21, 0.82).
narrative_ontology:measurement_basis(gene_be_t21, observed).
narrative_ontology:measurement(gene_be_t25, geneva_conventions_1949__security_maximization_reading, base_extractiveness, 25, 0.82).
narrative_ontology:measurement_basis(gene_be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(gene_su_t0, geneva_conventions_1949__security_maximization_reading, suppression_requirement, 0, 0.85).
narrative_ontology:measurement_basis(gene_su_t0, observed).
narrative_ontology:measurement(gene_su_t3, geneva_conventions_1949__security_maximization_reading, suppression_requirement, 3, 0.87).
narrative_ontology:measurement_basis(gene_su_t3, observed).
narrative_ontology:measurement(gene_su_t6, geneva_conventions_1949__security_maximization_reading, suppression_requirement, 6, 0.88).
narrative_ontology:measurement_basis(gene_su_t6, observed).
narrative_ontology:measurement(gene_su_t9, geneva_conventions_1949__security_maximization_reading, suppression_requirement, 9, 0.9).
narrative_ontology:measurement_basis(gene_su_t9, observed).
narrative_ontology:measurement(gene_su_t12, geneva_conventions_1949__security_maximization_reading, suppression_requirement, 12, 0.9).
narrative_ontology:measurement_basis(gene_su_t12, observed).
narrative_ontology:measurement(gene_su_t15, geneva_conventions_1949__security_maximization_reading, suppression_requirement, 15, 0.91).
narrative_ontology:measurement_basis(gene_su_t15, observed).
narrative_ontology:measurement(gene_su_t18, geneva_conventions_1949__security_maximization_reading, suppression_requirement, 18, 0.91).
narrative_ontology:measurement_basis(gene_su_t18, observed).
narrative_ontology:measurement(gene_su_t21, geneva_conventions_1949__security_maximization_reading, suppression_requirement, 21, 0.91).
narrative_ontology:measurement_basis(gene_su_t21, observed).
narrative_ontology:measurement(gene_su_t25, geneva_conventions_1949__security_maximization_reading, suppression_requirement, 25, 0.91).
narrative_ontology:measurement_basis(gene_su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(geneva_conventions_1949__security_maximization_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(geneva_conventions_1949__security_maximization_reading, 0.12).
narrative_ontology:affects_constraint(geneva_conventions_1949__security_maximization_reading, geneva_conventions_1949__humanitarian_ceiling_reading).
narrative_ontology:affects_constraint(geneva_conventions_1949__security_maximization_reading, geneva_conventions_1949__conditional_reciprocity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the Geneva Conventions (1949) kernel. The security_maximization_reading asserts that humanitarian protections must yield to state security in asymmetric conflict. The humanitarian_ceiling_reading (sibling) asserts absolute humanitarian minimums regardless of threat or reciprocity. The conditional_reciprocity_reading (sibling) asserts reciprocal application with proportional degradation for non-compliance. Each reading has distinct ε values: security-maximization ~0.82 (high extraction), humanitarian-ceiling ~0.15 (minimal extraction), conditional-reciprocity ~0.45 (moderate/contested). The readings compete for institutional dominance; which reading is adopted determines the scope of permissible state violence. All three must be authored as separate constraints linked via network.affects_constraints to capture the kernel contest structure. The ethical reading is not a variant measurement of a single constraint — it is a different constraint instantiated by a different reading of the same kernel commitment.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(geneva_conventions_1949__security_maximization_reading, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
