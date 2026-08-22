% ============================================================================
% CONSTRAINT STORY: july_charter_sovereign_legitimacy__guided_nationalism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_july_charter_sovereign_legitimacy__guided_nationalism_reading, []).

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
 *   constraint_id: july_charter_sovereign_legitimacy__guided_nationalism_reading
 *   human_readable: July Charter — Islamic-Nationalist Sovereign Legitimacy Reading
 *   domain: constitutional_law/political_transitions/post_revolutionary_state_building
 *
 * SUMMARY:
 *   This story instantiates ONE of three contested readings of the July
 *   Charter's sovereignty clause: the guided-nationalism reading, in which
 *   the Charter's grounding of sovereign legitimacy in religious-national
 *   identity is the operative constitutional fact. Under this reading, the
 *   clause reallocates authority from secular civil institutions and
 *   legal-equality frameworks toward the religious-nationalist coalition, the
 *   clerical establishment, and an aligned judiciary, while constraining
 *   secular opposition, religious minorities, and civil society. This is a
 *   distinct constraint from the secular_democratic_reading (which reads the
 *   same Charter as mandating secular institutions with military subordinate
 *   to civilian rule) and the military_custodian_reading (which reads it as
 *   ratifying military guardianship). Each reading has its own ε, its own
 *   beneficiary/victim structure, and its own classification; they are linked
 *   only via network edges, never merged.
 *
 * KEY AGENTS:
 *   - religious_nationalist_party_leadership: agenda_setter/beneficiary (institutional/arbitrage) — drafted and administers the clause
 *   - clerical_establishment: beneficiary (organized/arbitrage) — newly empowered co-sovereign body
 *   - secular_civil_society_organizations: payer (moderate/constrained) — loses legal standing and operating space
 *   - religious_minority_communities: payer (powerless/trapped) — loses formal parity
 *   - international_human_rights_bodies: observer (institutional/analytical) — external corroborating seat
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(july_charter_sovereign_legitimacy__guided_nationalism_reading, 0.68).
domain_priors:suppression_score(july_charter_sovereign_legitimacy__guided_nationalism_reading, 0.72).
domain_priors:theater_ratio(july_charter_sovereign_legitimacy__guided_nationalism_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__guided_nationalism_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__guided_nationalism_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__guided_nationalism_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__guided_nationalism_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__guided_nationalism_reading, resistance, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(july_charter_sovereign_legitimacy__guided_nationalism_reading, tangled_rope).
narrative_ontology:human_readable(july_charter_sovereign_legitimacy__guided_nationalism_reading, "July Charter — Islamic-Nationalist Sovereign Legitimacy Reading").
narrative_ontology:topic_domain(july_charter_sovereign_legitimacy__guided_nationalism_reading, "constitutional_law/political_transitions/post_revolutionary_state_building").

domain_priors:requires_active_enforcement(july_charter_sovereign_legitimacy__guided_nationalism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(july_charter_sovereign_legitimacy__guided_nationalism_reading, '9370efe8-bb18-4e7a-949e-8e175b312d90').
narrative_ontology:cs_kernel_codification('9370efe8-bb18-4e7a-949e-8e175b312d90', formalized).
narrative_ontology:cs_authority_grounding('9370efe8-bb18-4e7a-949e-8e175b312d90', extraction).
narrative_ontology:cs_interpretation_layer_present('9370efe8-bb18-4e7a-949e-8e175b312d90').
narrative_ontology:cs_reading_relation('9370efe8-bb18-4e7a-949e-8e175b312d90', july_charter_sovereign_legitimacy__secular_democratic_reading, forecloses).
narrative_ontology:cs_reading_relation('9370efe8-bb18-4e7a-949e-8e175b312d90', july_charter_sovereign_legitimacy__military_custodian_reading, coexists_with).
narrative_ontology:cs_axiom('9370efe8-bb18-4e7a-949e-8e175b312d90', foundational, religious_identity_grounds_sovereign_legitimacy).
narrative_ontology:cs_axiom_status(religious_identity_grounds_sovereign_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('9370efe8-bb18-4e7a-949e-8e175b312d90', religious_identity_grounds_sovereign_legitimacy, theological).
narrative_ontology:cs_axiom('9370efe8-bb18-4e7a-949e-8e175b312d90', secondary, secular_civil_equality_subordinate_to_religious_norm).
narrative_ontology:cs_axiom_status(secular_civil_equality_subordinate_to_religious_norm, holdable).
narrative_ontology:cs_axiom_grounding('9370efe8-bb18-4e7a-949e-8e175b312d90', secular_civil_equality_subordinate_to_religious_norm, conventional).
narrative_ontology:cs_reference_frame('9370efe8-bb18-4e7a-949e-8e175b312d90', religious_national_founding_covenant).
narrative_ontology:cs_drift_state('9370efe8-bb18-4e7a-949e-8e175b312d90', post_ratification_consolidation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('9370efe8-bb18-4e7a-949e-8e175b312d90', '').
narrative_ontology:cs_kernel_id(july_charter_sovereign_legitimacy__guided_nationalism_reading, july_charter_sovereign_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__guided_nationalism_reading, religious_nationalist_party_leadership).
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__guided_nationalism_reading, clerical_establishment).
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__guided_nationalism_reading, islamist_aligned_judiciary).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__guided_nationalism_reading, secular_civil_society_organizations).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__guided_nationalism_reading, religious_minority_communities).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__guided_nationalism_reading, secular_opposition_parties).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__guided_nationalism_reading, women_rights_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__guided_nationalism_reading, general_public).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__guided_nationalism_reading, general_public).
narrative_ontology:constraint_vindicates(july_charter_sovereign_legitimacy__guided_nationalism_reading, religious_identity_as_national_essence).
narrative_ontology:constraint_vindicates(july_charter_sovereign_legitimacy__guided_nationalism_reading, post_revolutionary_moral_restoration_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Drafted and pushed through the Charter provision naming religious identity as the ground of sovereign legitimacy. Controls appointments to the constitutional court and the religious-affairs ministry that interprets what the provision requires in practice. Frames the clause as restoring the nation's authentic character after a corrupt or foreign-aligned prior order; in practice it entrenches this faction's veto over legislation framed as un-Islamic or anti-national.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__guided_nationalism_reading, religious_nationalist_party_leadership, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(july_charter_sovereign_legitimacy__guided_nationalism_reading, religious_nationalist_party_leadership, beneficiary).

% Gains a constitutionally guaranteed advisory or veto role over legislation for the first time. Receives state funding, deference in courts, and a formal channel to strike down or delay laws it deems inconsistent with religious norms. Was a subordinate social institution before the Charter; is now a co-sovereign body.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__guided_nationalism_reading, clerical_establishment, beneficiary,
    organized, generational, arbitrage, national).

% Newly empowered constitutional and religious-affairs benches review ordinary legislation against the religious-identity clause. Appointments run through the same nationalist coalition that wrote the Charter, so review functions as a second veto layer over anything the coalition dislikes, dressed as neutral constitutional compliance.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__guided_nationalism_reading, islamist_aligned_judiciary, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(july_charter_sovereign_legitimacy__guided_nationalism_reading, islamist_aligned_judiciary, beneficiary).

% NGOs, secular associations, and independent media that operated under the prior legal order now face registration reviews, funding restrictions, and legal challenges framed in terms of the religious-identity clause. Domestic legal recourse is foreclosed because the same clause structures the courts that would hear their appeals; exit means operating underground or relocating operations abroad.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__guided_nationalism_reading, secular_civil_society_organizations, payer,
    moderate, biographical, constrained, national).

% Communities outside the majority confession lose the formal parity they held under the prior secular framework. Personal-status law, public employment access, and blasphemy-adjacent speech protections shift against them. Emigration is the only real exit and is costly, identity-severing, and unavailable to most.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__guided_nationalism_reading, religious_minority_communities, payer,
    powerless, generational, trapped, national).

% Family law, inheritance, and public-conduct regulation are increasingly referred to religious-normative standards under the new clause. Advocates who litigated equality claims under the prior civil code now find the constitutional ground for those claims removed; the exit options are private accommodation, informal circumvention, or leaving the jurisdiction.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__guided_nationalism_reading, women_rights_advocates, payer,
    moderate, biographical, constrained, national).

% Parties that campaigned on secular governance now face disqualification risk if their platforms are read as contravening the religious-identity clause. They retain organizational capacity and international allies but cannot contest the legitimacy ground itself without appearing to reject the Charter's foundational claim, which the ruling coalition frames as rejecting the nation.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__guided_nationalism_reading, secular_opposition_parties, payer,
    organized, biographical, constrained, national).

% Majority-confession citizens experience some symbolic and cultural validation from the Charter's framing and may benefit from services the clerical establishment now administers, but also lose secular civil protections and face the same constrained political competition as everyone else if their views diverge from the coalition line.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__guided_nationalism_reading, general_public, beneficiary,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(july_charter_sovereign_legitimacy__guided_nationalism_reading, general_public, payer).

% Monitor compliance with treaty obligations on minority rights, freedom of association, and gender equality. Issue reports and rulings that carry reputational but limited enforcement weight against the ruling coalition; can condition aid or diplomatic recognition on findings.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__guided_nationalism_reading, international_human_rights_bodies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(july_charter_sovereign_legitimacy__guided_nationalism_reading, religious_nationalist_party_leadership).
narrative_ontology:fixing_cost_class(july_charter_sovereign_legitimacy__guided_nationalism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides the new order with a unifying legitimacy narrative after a revolutionary rupture — a shared symbolic ground (religious-national identity) that the coalition argues prevents renewed fragmentation and gives the state a coherent basis for authority distinct from the discredited prior regime.
% TRANSFER_FUNCTION: Moves formal legal deference, judicial veto power, and public authority away from secular civil institutions, religious minorities, and legal-equality frameworks, toward the religious-nationalist leadership, the clerical establishment, and the aligned judiciary that interprets the identity clause.
% ABSENT_VOICES: Religious minority communities and secular civil society had limited or no seat in the Charter's drafting process; secular opposition parties participated but under threat of exclusion if they contested the legitimacy premise itself. Diaspora and exiled secular voices are structurally absent from the domestic ratification process entirely.
% DISAPPEARANCE_RATIONALE: If the religious-identity legitimacy clause were removed, the clerical establishment's veto channel and the aligned judiciary's review basis would both lose their constitutional footing; secular civil society and minority communities would regain the legal ground for equality claims that the clause currently forecloses; the ruling coalition would lose its principal legitimacy narrative and likely face electoral or coalition realignment.
% FOUNDING_PROBLEM: The prior order was framed by the coalition as having lost genuine national and moral legitimacy — as either foreign-aligned, corrupt, or secularizing in a way that alienated the religious majority — creating (in this reading) a vacuum that only a religiously-grounded sovereign claim could fill after the revolutionary break.
% FOUNDING_PROBLEM_CORROBORATION: The religious-nationalist leadership and clerical establishment attest the problem (legitimacy vacuum, moral drift) was real and remains live, justifying continued religious grounding. Secular civil society organizations, international human rights monitors, and independent constitutional scholars outside the coalition dispute that a legitimacy vacuum required a religious-identity solution at all, and argue the clause functions primarily to entrench the drafting coalition rather than to solve any genuinely shared founding problem.
narrative_ontology:disappearance_verdict(july_charter_sovereign_legitimacy__guided_nationalism_reading, world_rearranges).
narrative_ontology:founding_problem_status(july_charter_sovereign_legitimacy__guided_nationalism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(july_charter_sovereign_legitimacy__guided_nationalism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(july_charter_sovereign_legitimacy__guided_nationalism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(july_charter_sovereign_legitimacy__guided_nationalism_reading, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(july_charter_sovereign_legitimacy__guided_nationalism_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(july_charter_sovereign_legitimacy__guided_nationalism_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(july_charter_sovereign_legitimacy__guided_nationalism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.42 to 0.68 over the interval as the clause moves from declaratory text to operative judicial and administrative practice — early ambiguity about how the identity clause would be applied gives way to a settled pattern of disqualifications, registration denials, and personal-status rulings. Suppression tracks upward similarly (0.50 to 0.72) as enforcement infrastructure — vetting boards, compliance review panels, registration authorities — is built out to give the clause teeth. Theater ratio is moderate and rising (0.25 to 0.40): some genuine constitutional deliberation occurs, but an increasing share of judicial and administrative activity is oriented toward performing fidelity to the identity clause rather than adjudicating substantive disputes on their merits.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter and clerical-establishment seats, the clause is experienced as a coordination achievement — a settled, legitimate foundation ending the ambiguity of the revolutionary interregnum. From the payer seats, the identical clause is experienced as an actively enforced mechanism that closes off legal avenues they relied on. The engine computes this divergence from the structural data (power, exit options, beneficiary/victim declarations); the claimed_type (tangled_rope) is authored independently of these metrics and is not tuned to force any particular seat's computed result.
 *
 * DIRECTIONALITY LOGIC:
 *   The religious-nationalist leadership and clerical establishment sit at the beneficiary end: they authored the clause, control its interpretive apparatus, and collect formal authority and resources through it — near-full beneficiary directionality. Secular civil society, religious minorities, women's rights advocates, and secular opposition parties sit at the target end: the same clause that empowers the coalition removes the legal ground on which their prior claims to protection or standing rested, and their exit options (relocation, informal circumvention, underground operation) are costly and identity-severing rather than genuine alternatives. The general public is treated as split-role: majority-confession citizens receive some diffuse symbolic and administrative benefit while still bearing the political-competition constraints everyone under the Charter faces.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding-problem interview is answered directly in mismatch form: the coalition and clerical establishment attest the legitimacy-vacuum problem remains live, while civil society and external monitors attest it either was never the real driver of the clause or has long since been resolved, and the clause now persists primarily to entrench the drafting coalition's veto power. This mismatch (status=contested, disappearance_verdict=world_rearranges) is exactly the signal the mandatrophy check is designed to surface — a coordination-function narrative (national unity, moral restoration) riding on a structure that redistributes concrete legal authority toward the parties who wrote it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indeterminacy,
    'Which of the three readings (guided_nationalism, secular_democratic, military_custodian) reflects the Charter''s operative legal meaning, and is that meaning even singular, or does the text remain genuinely underdetermined pending future constitutional-court practice?',
    'Track which reading''s institutional apparatus (clerical review boards vs. civilian legislative supremacy vs. military oversight council) actually gains enforcement capacity and case volume over the next several years of constitutional-court rulings.',
    'If the guided-nationalism apparatus (clerical review, identity-clause judicial review) is the one that consolidates enforcement power while the others remain dormant, this reading''s classification is the operative one; if a different apparatus consolidates, this story remains a valid but non-dominant reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_indeterminacy, conceptual, 'Which sibling reading of the Charter kernel becomes the operative constitutional practice.').

omega_variable(
    religious_identity_natural_vs_constructed,
    'Is the Charter''s grounding of sovereignty in religious-national identity a genuine expression of pre-existing majority identity commitments, or a constructed legitimacy claim engineered by the drafting coalition to secure its own authority?',
    'Comparative study of pre-Charter public opinion on religious-identity governance versus post-Charter coalition messaging and enforcement patterns; degree of correlation between the clause''s content and the specific interests of the drafting coalition''s leadership.',
    'If genuinely reflective of broad pre-existing commitment, the coordination-function claim carries more weight and the classification moves toward a hybrid with real coordination content; if substantially engineered, the constraint is closer to pure extraction dressed in identity language.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(religious_identity_natural_vs_constructed, empirical, 'Whether the identity-legitimacy claim is organic or coalition-constructed.').

omega_variable(
    minority_exit_feasibility,
    'How feasible is emigration or informal accommodation actually for religious minority communities and secular civil society, versus how feasible does the Charter''s defenders claim it to be?',
    'Emigration data, asylum claims, and documented instances of informal legal circumvention (e.g., private religious-court alternatives, unregistered NGO operation) compared against stated exit costs.',
    'If exit is substantially less feasible than assumed, directionality for these victim groups should sit even closer to full-target than currently modeled, raising effective extraction further under the engine''s scope/directionality scaling.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(minority_exit_feasibility, empirical, 'Real-world feasibility of exit options for trapped and constrained victim groups.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(july_charter_sovereign_legitimacy__guided_nationalism_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(july_tr_t0, july_charter_sovereign_legitimacy__guided_nationalism_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(july_tr_t4, july_charter_sovereign_legitimacy__guided_nationalism_reading, theater_ratio, 4, 0.28).
narrative_ontology:measurement(july_tr_t8, july_charter_sovereign_legitimacy__guided_nationalism_reading, theater_ratio, 8, 0.32).
narrative_ontology:measurement(july_tr_t12, july_charter_sovereign_legitimacy__guided_nationalism_reading, theater_ratio, 12, 0.35).
narrative_ontology:measurement(july_tr_t16, july_charter_sovereign_legitimacy__guided_nationalism_reading, theater_ratio, 16, 0.37).
narrative_ontology:measurement(july_tr_t20, july_charter_sovereign_legitimacy__guided_nationalism_reading, theater_ratio, 20, 0.39).
narrative_ontology:measurement(july_tr_t24, july_charter_sovereign_legitimacy__guided_nationalism_reading, theater_ratio, 24, 0.4).

% Extraction over time
narrative_ontology:measurement(july_be_t0, july_charter_sovereign_legitimacy__guided_nationalism_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(july_be_t4, july_charter_sovereign_legitimacy__guided_nationalism_reading, base_extractiveness, 4, 0.5).
narrative_ontology:measurement(july_be_t8, july_charter_sovereign_legitimacy__guided_nationalism_reading, base_extractiveness, 8, 0.56).
narrative_ontology:measurement(july_be_t12, july_charter_sovereign_legitimacy__guided_nationalism_reading, base_extractiveness, 12, 0.61).
narrative_ontology:measurement(july_be_t16, july_charter_sovereign_legitimacy__guided_nationalism_reading, base_extractiveness, 16, 0.64).
narrative_ontology:measurement(july_be_t20, july_charter_sovereign_legitimacy__guided_nationalism_reading, base_extractiveness, 20, 0.66).
narrative_ontology:measurement(july_be_t24, july_charter_sovereign_legitimacy__guided_nationalism_reading, base_extractiveness, 24, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(july_su_t0, july_charter_sovereign_legitimacy__guided_nationalism_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(july_su_t4, july_charter_sovereign_legitimacy__guided_nationalism_reading, suppression_requirement, 4, 0.58).
narrative_ontology:measurement(july_su_t8, july_charter_sovereign_legitimacy__guided_nationalism_reading, suppression_requirement, 8, 0.63).
narrative_ontology:measurement(july_su_t12, july_charter_sovereign_legitimacy__guided_nationalism_reading, suppression_requirement, 12, 0.67).
narrative_ontology:measurement(july_su_t16, july_charter_sovereign_legitimacy__guided_nationalism_reading, suppression_requirement, 16, 0.69).
narrative_ontology:measurement(july_su_t20, july_charter_sovereign_legitimacy__guided_nationalism_reading, suppression_requirement, 20, 0.71).
narrative_ontology:measurement(july_su_t24, july_charter_sovereign_legitimacy__guided_nationalism_reading, suppression_requirement, 24, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(july_charter_sovereign_legitimacy__guided_nationalism_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(july_charter_sovereign_legitimacy__guided_nationalism_reading, 0.08).
narrative_ontology:affects_constraint(july_charter_sovereign_legitimacy__guided_nationalism_reading, july_charter_sovereign_legitimacy__secular_democratic_reading).
narrative_ontology:affects_constraint(july_charter_sovereign_legitimacy__guided_nationalism_reading, july_charter_sovereign_legitimacy__military_custodian_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the july_charter_sovereign_legitimacy kernel, each authored as a separate constraint per the ε-invariance principle: guided_nationalism_reading (this file, tangled_rope, religious-nationalist beneficiaries and secular/minority victims), secular_democratic_reading (civilian-supremacy framing, different beneficiary/victim structure), and military_custodian_reading (military-guardian framing, different beneficiary/victim structure again). The three do not share ε; they are linked via network edges to enable contamination and coupling analysis across the contested kernel, not merged into one averaged constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
