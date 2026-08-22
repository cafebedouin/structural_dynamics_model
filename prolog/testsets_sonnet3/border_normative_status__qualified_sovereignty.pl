% ============================================================================
% CONSTRAINT STORY: border_normative_status__qualified_sovereignty
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_border_normative_status__qualified_sovereignty, []).

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
 *   constraint_id: border_normative_status__qualified_sovereignty
 *   human_readable: Proportionality-Constrained Border Control Authority
 *   domain: political/legal/migration
 *
 * SUMMARY:
 *   This constraint instantiates the qualified_sovereignty reading of the
 *   border_normative_status kernel: states retain the underlying authority to
 *   control entry, but that authority is conditioned on proportionality,
 *   necessity, and consistency with human rights obligations. Unlike
 *   sovereignty_primary (which treats exclusion as a foundational prerogative
 *   requiring no external justification) or freedom_primary (which treats
 *   exclusion as presumptively wrongful absent extraordinary justification),
 *   this reading builds a mixed structure: real coordination function
 *   (organized, accountable border administration) layered with real
 *   extraction (exclusion still falls disproportionately on powerless
 *   claimants, and the adjudication apparatus itself generates new harms —
 *   prolonged limbo, inconsistent proportionality findings, uneven protection
 *   for displaced citizens). The rising theater_ratio and
 *   suppression_requirement track a structure that increasingly performs
 *   rights-compliance review (hearings, tribunals, published criteria) while
 *   enforcement capacity and exclusionary outcomes continue to harden.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_normative_status__qualified_sovereignty, 0.52).
domain_priors:suppression_score(border_normative_status__qualified_sovereignty, 0.58).
domain_priors:theater_ratio(border_normative_status__qualified_sovereignty, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_normative_status__qualified_sovereignty, extractiveness, 0.52).
narrative_ontology:constraint_metric(border_normative_status__qualified_sovereignty, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(border_normative_status__qualified_sovereignty, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_normative_status__qualified_sovereignty, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(border_normative_status__qualified_sovereignty, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_normative_status__qualified_sovereignty, tangled_rope).
narrative_ontology:human_readable(border_normative_status__qualified_sovereignty, "Proportionality-Constrained Border Control Authority").
narrative_ontology:topic_domain(border_normative_status__qualified_sovereignty, "political/legal/migration").

domain_priors:requires_active_enforcement(border_normative_status__qualified_sovereignty).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_normative_status__qualified_sovereignty, '7bed077f-7db3-4474-8888-4439900c8444').
narrative_ontology:cs_kernel_codification('7bed077f-7db3-4474-8888-4439900c8444', distributed).
narrative_ontology:cs_authority_grounding('7bed077f-7db3-4474-8888-4439900c8444', distributed).
narrative_ontology:cs_reading_relation('7bed077f-7db3-4474-8888-4439900c8444', border_normative_status__sovereignty_primary, coexists_with).
narrative_ontology:cs_reading_relation('7bed077f-7db3-4474-8888-4439900c8444', border_normative_status__freedom_primary, coexists_with).
narrative_ontology:cs_axiom('7bed077f-7db3-4474-8888-4439900c8444', foundational, sovereign_authority_conditioned_on_justification).
narrative_ontology:cs_axiom_status(sovereign_authority_conditioned_on_justification, holdable).
narrative_ontology:cs_axiom_grounding('7bed077f-7db3-4474-8888-4439900c8444', sovereign_authority_conditioned_on_justification, conventional).
narrative_ontology:cs_axiom('7bed077f-7db3-4474-8888-4439900c8444', foundational, human_rights_obligations_bind_border_discretion).
narrative_ontology:cs_axiom_status(human_rights_obligations_bind_border_discretion, holdable).
narrative_ontology:cs_axiom_grounding('7bed077f-7db3-4474-8888-4439900c8444', human_rights_obligations_bind_border_discretion, deontological).
narrative_ontology:cs_reference_frame('7bed077f-7db3-4474-8888-4439900c8444', post_ww2_refugee_convention_framework).
narrative_ontology:cs_drift_state('7bed077f-7db3-4474-8888-4439900c8444', contemporary_mass_displacement_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7bed077f-7db3-4474-8888-4439900c8444', '').
narrative_ontology:cs_kernel_id(border_normative_status__qualified_sovereignty, border_normative_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_normative_status__qualified_sovereignty, receiving_state_institutions).
narrative_ontology:constraint_beneficiary(border_normative_status__qualified_sovereignty, citizen_polity_of_receiving_states).
narrative_ontology:constraint_victim(border_normative_status__qualified_sovereignty, excluded_migrants).
narrative_ontology:constraint_victim(border_normative_status__qualified_sovereignty, displaced_citizens_denied_reentry_or_protection).
narrative_ontology:constraint_victim(border_normative_status__qualified_sovereignty, asylum_seekers_in_prolonged_limbo).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets and administers border control policy, adjudicates individual claims for entry or protection, and must justify each exclusion or admission decision against a proportionality standard drawn from human rights law. Retains the underlying authority to exclude, but the exercise of that authority is subject to review, litigation, and international scrutiny. Bears the administrative cost of the adjudication regime but also captures the benefit of sovereignty being formally recognized as legitimate when properly exercised.
narrative_ontology:constraint_stakeholder(border_normative_status__qualified_sovereignty, receiving_state_institutions, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(border_normative_status__qualified_sovereignty, receiving_state_institutions, beneficiary).

% Benefits from a border regime that filters entry according to negotiated, rights-constrained criteria rather than either unlimited admission or unconstrained exclusion. Retains a democratic claim to shape membership rules, tempered by the state's human rights obligations. Largely insulated from the constraint's direct costs; experiences it mainly through policy debate and litigation outcomes.
narrative_ontology:constraint_stakeholder(border_normative_status__qualified_sovereignty, citizen_polity_of_receiving_states, beneficiary,
    organized, generational, mobile, national).

% Seek entry or protection and are denied, sometimes despite meeting substantive criteria, because the proportionality standard is applied inconsistently, under political pressure, or with weak procedural safeguards. Have limited capacity to contest exclusion decisions from outside the receiving state's territory or jurisdiction; the 'qualified' character of sovereignty offers them a legal argument but rarely a reliable remedy.
narrative_ontology:constraint_stakeholder(border_normative_status__qualified_sovereignty, excluded_migrants, payer,
    powerless, biographical, trapped, global).

% Citizens or long-term residents caught by border-adjacent enforcement — denied re-entry, stripped of documentation, or refused consular protection abroad — who find that the proportionality standard, meant to constrain state power over outsiders, offers uneven protection to insiders caught in the same enforcement machinery.
narrative_ontology:constraint_stakeholder(border_normative_status__qualified_sovereignty, displaced_citizens_denied_reentry_or_protection, payer,
    powerless, biographical, trapped, national).

% Await adjudication of protection claims for years under detention, restricted movement, or precarious temporary status. The proportionality and necessity tests that are supposed to prevent arbitrary exclusion instead generate a lengthy, resource-intensive review apparatus whose delay itself becomes a form of harm, distinct from either outright admission or outright exclusion.
narrative_ontology:constraint_stakeholder(border_normative_status__qualified_sovereignty, asylum_seekers_in_prolonged_limbo, payer,
    powerless, biographical, trapped, national).

% Reviews individual border decisions against proportionality and necessity standards, issues rulings that bind or persuade states, and thereby co-administers the qualification on sovereignty this reading asserts. Has no enforcement power of its own and depends on state compliance, giving it real but bounded influence over how the constraint operates in practice.
narrative_ontology:constraint_stakeholder(border_normative_status__qualified_sovereignty, human_rights_courts_and_treaty_bodies, observer,
    institutional, civilizational, analytical, continental).
narrative_ontology:stakeholder_secondary_role(border_normative_status__qualified_sovereignty, human_rights_courts_and_treaty_bodies, agenda_setter).

% Operate in the gap between formal legal channels and the practical inaccessibility of proportionate, rights-respecting admission processes. Not party to the legal contest over sovereignty's scope, but structurally dependent on its unresolved tension — the harder proportionality is to invoke in practice, the more migrants turn to unauthorized channels.
narrative_ontology:constraint_stakeholder(border_normative_status__qualified_sovereignty, smuggling_and_trafficking_networks, excluded,
    organized, immediate, arbitrage, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(border_normative_status__qualified_sovereignty, receiving_state_institutions).
narrative_ontology:fixing_cost_class(border_normative_status__qualified_sovereignty, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allows states to maintain organized, rule-governed control over territorial entry — necessary for administering public services, labor markets, and security — while embedding a check that prevents that control from operating as unconstrained exclusion of people with legitimate claims to protection or membership.
% TRANSFER_FUNCTION: Moves the burden of justifying border decisions from migrants (who under sovereignty_primary would bear the full burden of persuading a state to admit them) toward the state (which must now justify exclusion), while leaving enforcement discretion, detention capacity, and adjudication resources concentrated in state institutions.
% ABSENT_VOICES: Excluded migrants and displaced citizens denied re-entry are structurally outside the venues — domestic courts, legislatures, treaty body sessions — where the proportionality standard is defined and applied; they experience the standard's operation but rarely participate in setting its content. Smuggling networks are excluded from the legal conversation entirely despite being a direct structural product of its gaps.
% DISAPPEARANCE_RATIONALE: If the proportionality/human-rights qualification vanished overnight, states would revert to something closer to unconstrained sovereign discretion (sovereignty_primary) — courts would lose jurisdiction to review exclusion decisions, and migrants and displaced citizens would lose their principal legal lever. States would experience this as removal of an adjudication burden; rights advocates and affected populations would experience it as removal of the only check on arbitrary exclusion. Whether the world 'rearranges' or 'stays the same' depends entirely on which party's baseline expectation you take as given — hence contested rather than a clean verdict.
% FOUNDING_PROBLEM: Twentieth-century mass displacement (post-WWII refugee flows, Cold War asylum politics) exposed that unconstrained state discretion over entry could produce catastrophic, arbitrary exclusion of people with nowhere else to go; the qualified-sovereignty reading was built to preserve state control while foreclosing the worst of that arbitrariness through binding legal standards.
% FOUNDING_PROBLEM_CORROBORATION: Refugee law scholars and UNHCR monitoring reports (external to receiving states) attest the underlying displacement problem remains fully live — global forced displacement is at record levels — and argue the proportionality standard is honored more in doctrine than in border practice. Receiving states' own foreign ministries attest the standard functions as intended, citing formal compliance with treaty review mechanisms. Independent litigation data from regional human rights courts shows a substantial gap between formal proportionality doctrine and outcomes for excluded claimants, supporting the reading that the founding problem is live but inconsistently addressed.
narrative_ontology:disappearance_verdict(border_normative_status__qualified_sovereignty, contested).
narrative_ontology:founding_problem_status(border_normative_status__qualified_sovereignty, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_normative_status__qualified_sovereignty, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(border_normative_status__qualified_sovereignty, 'none', 1).
narrative_ontology:epsilon_provenance(border_normative_status__qualified_sovereignty, 0.52, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(border_normative_status__qualified_sovereignty_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(border_normative_status__qualified_sovereignty, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(border_normative_status__qualified_sovereignty_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52) is moderate rather than high because a genuine coordination function is present — states legitimately need workable entry regimes, and the qualification does produce real remedies in a meaningful share of cases. Suppression (0.58) reflects that migrants and displaced citizens denied reentry still face binding, coercively enforced exclusion; the qualification narrows but does not eliminate the coercive core. Theater ratio (0.42) captures a growing gap between the formal proportionality apparatus (tribunals, published standards, treaty reporting) and outcomes, especially for asylum seekers in prolonged limbo, where the review process itself becomes a mechanism of delay rather than resolution. All three temporal series share the single time grid (0/8/16/24/32/40) as required.
 *
 * PERSPECTIVAL GAP:
 *   From the receiving state's seat, qualified sovereignty looks like principled, rule-bound coordination: real authority, real constraint, real legitimacy earned through compliance. From the excluded migrant's seat, the same structure looks like sovereignty_primary wearing procedural clothing — exclusion proceeds on largely the same practical terms, now accompanied by a legal argument the excluded party rarely has resources to pursue. The engine's per-seat computation is expected to diverge sharply between the agenda_setter and payer seats even though both are described by the same structural facts.
 *
 * DIRECTIONALITY LOGIC:
 *   Receiving state institutions and the citizen polity sit near the beneficiary end: they retain control, gain legal legitimacy for that control, and bear the administrative cost of adjudication as a manageable overhead rather than an extraction. Excluded migrants, displaced citizens, and asylum seekers in limbo sit near the target end: trapped exit options, powerless standing, and the practical unavailability of remedy despite formal entitlement to it push their derived directionality high even though the reading's doctrine nominally protects them. Human rights courts occupy an unusual dual position — institutional power but analytical exit, since they administer part of the qualification but cannot themselves enforce compliance.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (catastrophic arbitrary exclusion during mass displacement crises) remains live by external, non-state attestation, which prevents this reading from being dismissed as obsolete scaffolding. But the founding_problem_status is authored as contested rather than simply live, because the mismatch between formal proportionality doctrine and adjudication outcomes (documented by litigation data, not self-report) suggests the qualification is partially functioning as legitimating theater for continued exclusion rather than as the check it was designed to be. This is exactly the divergence the framework is built to surface: claiming tangled_rope while the metrics show rising theater and suppression is not an error to reconcile — it is the finding.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_kernel_reading_location,
    'This story is one reading (qualified_sovereignty) of the border_normative_status kernel. Where exactly does it diverge from the sibling readings sovereignty_primary and freedom_primary, and what would adopting a sibling reading change structurally?',
    'Compare the three constraint stories'' victim sets, beneficiary sets, and required enforcement structure. sovereignty_primary would drop the proportionality/necessity adjudication burden entirely, converting displaced citizens and excluded migrants from qualified-remedy holders to non-parties with no legal claim. freedom_primary would invert the justificatory burden further, making exclusion itself presumptively wrongful and shifting the coordination function''s cost structure onto the state far more heavily, likely reclassifying the state''s position from agenda_setter/beneficiary toward payer for many enforcement actions.',
    'The disagreement is located specifically in WHO bears the burden of justification and WHERE the adjudication apparatus sits: qualified_sovereignty splits the burden (state must justify exclusion, but retains ultimate authority), sovereignty_primary places no burden on the state, freedom_primary places near-total burden on the state. This determines whether the constraint reads as tangled_rope (this story), rope-leaning-snare (sovereignty_primary, likely with sharper victim concentration and less coordination cover), or a scaffold-toward-rope reading (freedom_primary, if movement rights are treated as the eventual steady state).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_kernel_reading_location, conceptual, 'Committer-frame location of this reading relative to its siblings in the border_normative_status kernel.').

omega_variable(
    proportionality_standard_capture,
    'Is the proportionality/necessity standard a genuine external check on state discretion, or has it been substantially captured by the states that are supposed to be constrained by it (via deferential judicial review, national-security carve-outs, and resource-starved adjudication systems)?',
    'Longitudinal analysis of human rights court rulings on border cases: track the rate at which proportionality challenges succeed against state exclusion decisions over time, and whether success rates track formal doctrine or track state compliance incentives.',
    'If substantially captured, the reading''s real-world operation drifts toward sovereignty_primary despite its qualified_sovereignty doctrine — meaning the tangled_rope classification is generous relative to actual practice and a snare classification may be closer to descriptive truth in specific jurisdictions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_standard_capture, empirical, 'Whether the proportionality check constrains states in practice or has been captured by them.').

omega_variable(
    displaced_citizen_inclusion_boundary,
    'How far does the qualified_sovereignty reading''s protective scope actually extend to citizens caught in border-adjacent enforcement (denial of re-entry, passport revocation, consular abandonment), versus being doctrinally limited to non-citizen migrants?',
    'Survey comparative constitutional and human rights case law on citizen re-entry denial cases to determine whether courts apply the same proportionality framework to citizens as to migrants, or a distinct and typically stronger standard.',
    'If citizens receive meaningfully stronger protection than migrants under the same doctrinal framework, the ''both excluded migrants and displaced citizens in the victim set'' structural delta this story is built on may overstate symmetry between the two victim groups, understating how much weaker migrants'' position is relative to citizens'' even within this reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(displaced_citizen_inclusion_boundary, empirical, 'Whether displaced citizens and excluded migrants are genuinely similarly positioned victims under this reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_normative_status__qualified_sovereignty, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bord_tr_t0, border_normative_status__qualified_sovereignty, theater_ratio, 0, 0.25).
narrative_ontology:measurement(bord_tr_t8, border_normative_status__qualified_sovereignty, theater_ratio, 8, 0.3).
narrative_ontology:measurement(bord_tr_t16, border_normative_status__qualified_sovereignty, theater_ratio, 16, 0.35).
narrative_ontology:measurement(bord_tr_t24, border_normative_status__qualified_sovereignty, theater_ratio, 24, 0.38).
narrative_ontology:measurement(bord_tr_t32, border_normative_status__qualified_sovereignty, theater_ratio, 32, 0.4).
narrative_ontology:measurement(bord_tr_t40, border_normative_status__qualified_sovereignty, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(bord_be_t0, border_normative_status__qualified_sovereignty, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(bord_be_t8, border_normative_status__qualified_sovereignty, base_extractiveness, 8, 0.42).
narrative_ontology:measurement(bord_be_t16, border_normative_status__qualified_sovereignty, base_extractiveness, 16, 0.46).
narrative_ontology:measurement(bord_be_t24, border_normative_status__qualified_sovereignty, base_extractiveness, 24, 0.49).
narrative_ontology:measurement(bord_be_t32, border_normative_status__qualified_sovereignty, base_extractiveness, 32, 0.51).
narrative_ontology:measurement(bord_be_t40, border_normative_status__qualified_sovereignty, base_extractiveness, 40, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(bord_su_t0, border_normative_status__qualified_sovereignty, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(bord_su_t8, border_normative_status__qualified_sovereignty, suppression_requirement, 8, 0.5).
narrative_ontology:measurement(bord_su_t16, border_normative_status__qualified_sovereignty, suppression_requirement, 16, 0.53).
narrative_ontology:measurement(bord_su_t24, border_normative_status__qualified_sovereignty, suppression_requirement, 24, 0.56).
narrative_ontology:measurement(bord_su_t32, border_normative_status__qualified_sovereignty, suppression_requirement, 32, 0.57).
narrative_ontology:measurement(bord_su_t40, border_normative_status__qualified_sovereignty, suppression_requirement, 40, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_normative_status__qualified_sovereignty, enforcement_mechanism).
narrative_ontology:affects_constraint(border_normative_status__qualified_sovereignty, border_normative_status__sovereignty_primary).
narrative_ontology:affects_constraint(border_normative_status__qualified_sovereignty, border_normative_status__freedom_primary).

% DUAL FORMULATION NOTE:
% This is one of three constraint stories decomposing the colloquial 'border control legitimacy' kernel (border_normative_status) into structurally distinct readings, each with its own stakeholder set, victim set, and ε. qualified_sovereignty (this story) authors a moderate ε (0.52) reflecting genuine mixed coordination/extraction. sovereignty_primary is expected to author a lower ε with a thinner victim set (state authority treated as near-foundational, adjudication burden absent). freedom_primary is expected to author the exclusion arrangement's ε from an abolitionist-of-exclusion vantage, likely higher, with the coordination function itself treated as substantially illegitimate. All three should be read as siblings, not as three measurements of one constraint — per the ε-invariance principle, differing ε across the readings signals three distinct constraints, not observer noise on one.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
