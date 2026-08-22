% ============================================================================
% CONSTRAINT STORY: july_charter_sovereign_legitimacy__guided_nationalism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   This story instantiates the guided-nationalism reading of the contested
 *   July Charter kernel: the Charter grounds sovereign legitimacy in
 *   Islamic-nationalist identity, elevating religious law and norms to
 *   constitutional status and displacing the secular constitutional order it
 *   replaced. In this reading's own terms, the arrangement solves a genuine
 *   legitimacy vacuum left by a discredited, foreign-associated secular order
 *   — but the coordination story rides alongside concentrated benefit for the
 *   religious-nationalist coalition and clerical establishment, and
 *   identifiable, structural cost to secular civil society, religious
 *   minorities, and the pre-existing judiciary. This is ONE of three readings
 *   of the same kernel (july_charter_sovereign_legitimacy); the
 *   secular_democratic_reading and military_custodian_reading are separate
 *   constraints with their own ε, beneficiaries, and victims — they are not
 *   blended into this story.
 *
 * KEY AGENTS:
 *   - religious_nationalist_coalition: agenda_setter (institutional/arbitrage) — drafts and enforces the legitimacy ground
 *   - clerical_establishment: beneficiary (organized/arbitrage) — gains constitutionally entrenched review authority
 *   - secular_civil_society: payer (moderate/trapped) — bears doctrinal review of prior civic activity
 *   - religious_minorities: payer (powerless/trapped) — bears downgraded legal status
 *   - independent_judiciary: payer (institutional/trapped) — displaced by doctrinal review hierarchy
 *   - diaspora_and_international_observers: excluded (moderate/analytical) — sees the structure but has no domestic voice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(july_charter_sovereign_legitimacy__guided_nationalism_reading, 0.71).
domain_priors:suppression_score(july_charter_sovereign_legitimacy__guided_nationalism_reading, 0.78).
domain_priors:theater_ratio(july_charter_sovereign_legitimacy__guided_nationalism_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__guided_nationalism_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__guided_nationalism_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__guided_nationalism_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__guided_nationalism_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__guided_nationalism_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(july_charter_sovereign_legitimacy__guided_nationalism_reading, tangled_rope).
narrative_ontology:human_readable(july_charter_sovereign_legitimacy__guided_nationalism_reading, "July Charter — Islamic-Nationalist Sovereign Legitimacy Reading").
narrative_ontology:topic_domain(july_charter_sovereign_legitimacy__guided_nationalism_reading, "constitutional_law/political_transitions/post_revolutionary_state_building").

domain_priors:requires_active_enforcement(july_charter_sovereign_legitimacy__guided_nationalism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(july_charter_sovereign_legitimacy__guided_nationalism_reading, '13ce9acf-855c-472f-a6f1-9cbcc0b6fe8f').
narrative_ontology:cs_kernel_codification('13ce9acf-855c-472f-a6f1-9cbcc0b6fe8f', formalized).
narrative_ontology:cs_authority_grounding('13ce9acf-855c-472f-a6f1-9cbcc0b6fe8f', extraction).
narrative_ontology:cs_interpretation_layer_present('13ce9acf-855c-472f-a6f1-9cbcc0b6fe8f').
narrative_ontology:cs_reading_relation('13ce9acf-855c-472f-a6f1-9cbcc0b6fe8f', july_charter_sovereign_legitimacy__secular_democratic_reading, forecloses).
narrative_ontology:cs_reading_relation('13ce9acf-855c-472f-a6f1-9cbcc0b6fe8f', july_charter_sovereign_legitimacy__military_custodian_reading, coexists_with).
narrative_ontology:cs_axiom('13ce9acf-855c-472f-a6f1-9cbcc0b6fe8f', foundational, religious_national_identity_is_sovereign_ground).
narrative_ontology:cs_axiom_status(religious_national_identity_is_sovereign_ground, holdable).
narrative_ontology:cs_axiom_grounding('13ce9acf-855c-472f-a6f1-9cbcc0b6fe8f', religious_national_identity_is_sovereign_ground, conventional).
narrative_ontology:cs_axiom('13ce9acf-855c-472f-a6f1-9cbcc0b6fe8f', secondary, secular_popular_sovereignty_subordinate_to_religious_identity).
narrative_ontology:cs_axiom_status(secular_popular_sovereignty_subordinate_to_religious_identity, holdable).
narrative_ontology:cs_axiom_grounding('13ce9acf-855c-472f-a6f1-9cbcc0b6fe8f', secular_popular_sovereignty_subordinate_to_religious_identity, conventional).
narrative_ontology:cs_reference_frame('13ce9acf-855c-472f-a6f1-9cbcc0b6fe8f', pre_revolutionary_secular_constitutional_order).
narrative_ontology:cs_drift_state('13ce9acf-855c-472f-a6f1-9cbcc0b6fe8f', post_ratification_enforcement_maturity, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('13ce9acf-855c-472f-a6f1-9cbcc0b6fe8f', '').
narrative_ontology:cs_kernel_id(july_charter_sovereign_legitimacy__guided_nationalism_reading, july_charter_sovereign_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__guided_nationalism_reading, religious_nationalist_coalition).
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__guided_nationalism_reading, clerical_establishment).
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__guided_nationalism_reading, guided_nationalist_political_bloc).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__guided_nationalism_reading, secular_civil_society).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__guided_nationalism_reading, religious_minorities).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__guided_nationalism_reading, women_rights_advocates).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__guided_nationalism_reading, independent_judiciary).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Drafted and pushed through the Charter provisions grounding sovereign legitimacy in religious-national identity. Controls the legislative supermajority and appointment machinery needed to enforce compliance of state institutions with the new legitimacy ground. Frames the arrangement as restoring authentic national identity after a period of foreign-imposed secularism.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__guided_nationalism_reading, religious_nationalist_coalition, agenda_setter,
    institutional, generational, arbitrage, national).

% Gains constitutionally entrenched authority to certify legislation and public conduct against religious-national norms, a role it did not hold under the prior order. Sits on new charter-created review bodies with veto power over statutes deemed contrary to the sovereign legitimacy ground. Its institutional standing and funding grow directly from the Charter's operation.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__guided_nationalism_reading, clerical_establishment, beneficiary,
    organized, generational, arbitrage, national).

% Political parties and patronage networks that campaigned on the guided-nationalist identity platform now hold disproportionate access to state contracts, media licensing, and civil-service appointments tied to demonstrated alignment with the Charter's legitimacy claim.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__guided_nationalism_reading, guided_nationalist_political_bloc, beneficiary,
    powerful, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(july_charter_sovereign_legitimacy__guided_nationalism_reading, guided_nationalist_political_bloc, agenda_setter).

% NGOs, universities, and secular political parties that operated under the prior constitutional order now find core activities — curriculum content, association registration, public assembly — subject to review against the new religious-national legitimacy standard. Legal challenges are heard by tribunals whose composition was reshaped by the same Charter. Emigration is the only reliable exit, and it forfeits standing, assets, and community.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__guided_nationalism_reading, secular_civil_society, payer,
    moderate, biographical, trapped, national).

% Communities whose faith traditions fall outside the Charter's designated national-religious identity now face downgraded legal status for family law, inheritance, and worship-site protections. They bear the costs of a legitimacy ground that was never theirs to claim, with no meaningful path to constitutional redress since the interpreting bodies are staffed by the majority tradition.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__guided_nationalism_reading, religious_minorities, payer,
    powerless, biographical, trapped, national).

% Organizations advocating for gender-equal family law and labor protections now confront a constitutional order that subordinates statutory equality guarantees to religious-national interpretive review. Advocacy that once operated through ordinary legislative channels must now also survive doctrinal certification, narrowing what reforms are achievable.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__guided_nationalism_reading, women_rights_advocates, payer,
    moderate, biographical, constrained, national).

% Judges committed to secular constitutional interpretation find their rulings subject to override or annulment by new religious-legitimacy review bodies created by the Charter. Judicial independence as previously understood is displaced by a hierarchy in which doctrinal conformity outranks precedent; resistant judges face removal proceedings.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__guided_nationalism_reading, independent_judiciary, payer,
    institutional, generational, trapped, national).
narrative_ontology:stakeholder_secondary_role(july_charter_sovereign_legitimacy__guided_nationalism_reading, independent_judiciary, excluded).

% Exiled dissidents, foreign governments, and human-rights monitors document the Charter's effects and would argue for restoring the secular-democratic framework, but have no seat in the domestic ratification or interpretive process and can only exert pressure through sanctions, asylum policy, or diplomatic leverage.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__guided_nationalism_reading, diaspora_and_international_observers, excluded,
    moderate, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(july_charter_sovereign_legitimacy__guided_nationalism_reading, diffuse).
narrative_ontology:fixing_cost_class(july_charter_sovereign_legitimacy__guided_nationalism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The Charter coordinates a fractured post-revolutionary polity around a single, legible source of sovereign legitimacy — religious-national identity — replacing a contested and previously foreign-associated secular constitutional order with a locally-rooted claim that (from this reading's perspective) unifies competing factions under one legitimating narrative.
% TRANSFER_FUNCTION: Moves interpretive and enforcement authority over public life — education, family law, association rights, judicial review — from secular civil institutions and pluralist legal frameworks to religious-nationalist political and clerical bodies; moves legal standing and constitutional protection away from religious minorities and secular civil society toward the majority religious-national bloc.
% ABSENT_VOICES: Religious minorities, secular civil society organizations, and the pre-existing independent judiciary were present during ratification only as objects of the process, not as co-drafters; diaspora communities and international human-rights observers who would argue for the secular-democratic reading have no domestic institutional voice at all.
% DISAPPEARANCE_RATIONALE: If this reading's legitimacy ground were removed, the review bodies staffed under it would lose their constitutional mandate, patronage networks tied to doctrinal conformity would collapse, secular civil society and minority communities would regain contestable legal standing, and the judiciary would revert to precedent-based rather than doctrine-based review — a substantial reorganization of state authority, not a cosmetic change.
% FOUNDING_PROBLEM: The post-revolutionary state faced a legitimacy vacuum: the prior secular constitutional order was associated with an ousted regime and, in this reading's account, with foreign imposition, leaving no locally-rooted, widely-recognized ground for sovereign authority during the transition.
% FOUNDING_PROBLEM_CORROBORATION: The religious-nationalist coalition and clerical establishment attest the legitimacy vacuum was real and remains live, citing continued factional fragmentation. Secular civil society representatives, independent judiciary members removed under the new review bodies, and international human-rights monitors attest from outside the benefiting coalition that the vacuum was addressable through pluralist means and that the Islamic-nationalist framing was adopted to entrench a specific faction's power rather than to solve the legitimacy problem generically — no source outside the beneficiary coalition corroborates that religious-national identity was the only available legitimacy ground.
narrative_ontology:disappearance_verdict(july_charter_sovereign_legitimacy__guided_nationalism_reading, world_rearranges).
narrative_ontology:founding_problem_status(july_charter_sovereign_legitimacy__guided_nationalism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(july_charter_sovereign_legitimacy__guided_nationalism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(july_charter_sovereign_legitimacy__guided_nationalism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(july_charter_sovereign_legitimacy__guided_nationalism_reading, 0.71, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness rises across the interval (0.40 to 0.71) as the doctrinal review apparatus matures from a ratified text into an operating enforcement bureaucracy — early years show mostly declaratory constitutional language; later years show active statute annulments, minority-status downgrades, and judicial removals. Suppression rises in parallel (0.50 to 0.78) tracking the buildup of the review bodies' enforcement capacity. Theater ratio is moderate and rising (0.20 to 0.42): some genuine identity-coordination function is real (a shared national narrative did partly stabilize factional conflict), but an increasing share of enforcement activity is performative assertion of doctrinal purity rather than functional dispute resolution. All three metrics are authored on one shared time grid.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat, the Charter reads as legitimate coordination restoring authentic national identity after a legitimacy vacuum — a rope-like story of unification. From the payer seats (secular civil society, religious minorities, displaced judiciary), the same structure computes as enforced extraction of legal standing and institutional power, requiring active suppression to hold. This divergence is exactly the tangled-rope signature: a genuine coordination narrative (ending factional fragmentation) riding alongside asymmetric extraction (concentrated religious-nationalist benefit, diffuse minority and secularist cost) sustained by active enforcement — the review bodies, judicial removal proceedings, and association-registration review.
 *
 * DIRECTIONALITY LOGIC:
 *   The religious-nationalist coalition and clerical establishment sit at the beneficiary end: they authored the legitimacy ground, staff its enforcement bodies, and derive expanding institutional power from its operation — d near zero. Secular civil society, religious minorities, and the independent judiciary sit at the target end: they bear the doctrinal review, lose prior legal standing, and have trapped or constrained exit — d near one. Religious minorities in particular carry powerless/trapped status, which the derivation chain correctly pushes toward the full-target end even before any override.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — legitimacy vacuum after regime collapse — is genuinely contested as to whether it remains live. The religious-nationalist coalition asserts it is still live (ongoing factional threat); outside corroborators (removed judges, minority advocates, diaspora observers) assert the vacuum was addressable pluralistically and that the doctrinal framework now persists primarily to entrench the coalition's power rather than to solve any remaining legitimacy problem. Classifying this as tangled_rope rather than snare preserves the genuine (if contested) coordination claim in the record, while the requires_active_enforcement flag and victim declarations ensure the extraction is not laundered as pure coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest_locus,
    'Where exactly does the guided_nationalism_reading''s core premise conflict with its sibling readings — is the conflict over WHO holds sovereign authority (religious-national identity vs. secular democratic institutions vs. military guardianship), or over HOW that authority is exercised once granted?',
    'Textual and drafting-history analysis of the Charter''s sovereignty clauses: does the text name religious-national identity as the SOURCE of legitimacy (foreclosing the secular-democratic reading''s premise that legitimacy derives from popular sovereignty exercised through secular institutions), or merely as one legitimating narrative alongside institutional guarantees the military-custodian reading also claims?',
    'If the religious-national identity clause is drafted as the exclusive legitimacy ground, it forecloses the secular_democratic_reading within any single constitutional framework (the two premises cannot both be operative law). If it is one narrative among several institutional guarantees, the readings coexist as competing political claims rather than logically incompatible framework readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest_locus, conceptual, 'Whether the guided-nationalism and secular-democratic readings are logically incompatible or merely politically competing.').

omega_variable(
    religious_nationalist_naturalness_vs_construction,
    'Is the Islamic-nationalist legitimacy ground a rediscovery of pre-existing national identity (as the coalition claims), or a constructed framework engineered by identifiable political and clerical beneficiaries to entrench post-revolutionary power?',
    'Historical analysis of pre-revolutionary constitutional practice and public opinion polling on national identity across the transition period; comparison with the drafting coalition''s documented institutional interests before and after ratification.',
    'If the identity ground substantially predates the coalition''s rise and reflects genuine popular consensus, the coordination claim is stronger and the tangled_rope classification''s coordination half is better supported. If the framing was substantially engineered by the coalition itself, the coordination story is closer to pure cover and a snare classification would be more accurate.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(religious_nationalist_naturalness_vs_construction, empirical, 'Whether the religious-national identity claim is a genuine antecedent consensus or a post-hoc construction serving identifiable beneficiaries.').

omega_variable(
    founding_problem_persistence,
    'Has the post-revolutionary legitimacy vacuum this reading claims to solve actually persisted through the measured interval, or was it substantially resolved early while the enforcement apparatus continued to expand for its own institutional reasons?',
    'Track factional violence, secessionist activity, and institutional legitimacy polling across the interval; compare against the timing of review-body enforcement expansion shown in the measurements.',
    'If the vacuum closed early (say by time_point 12) while enforcement continued intensifying afterward, this supports the founding_problem_status of ''dead-but-persisting'' (mandatrophy) rather than genuinely contested — strengthening the outside corroborators'' account over the coalition''s account.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_persistence, empirical, 'Whether the legitimacy vacuum this reading was built to solve remains live or has been overtaken by self-sustaining enforcement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(july_charter_sovereign_legitimacy__guided_nationalism_reading, 0, 36).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(july_tr_t0, july_charter_sovereign_legitimacy__guided_nationalism_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(july_tr_t6, july_charter_sovereign_legitimacy__guided_nationalism_reading, theater_ratio, 6, 0.25).
narrative_ontology:measurement(july_tr_t12, july_charter_sovereign_legitimacy__guided_nationalism_reading, theater_ratio, 12, 0.3).
narrative_ontology:measurement(july_tr_t18, july_charter_sovereign_legitimacy__guided_nationalism_reading, theater_ratio, 18, 0.34).
narrative_ontology:measurement(july_tr_t24, july_charter_sovereign_legitimacy__guided_nationalism_reading, theater_ratio, 24, 0.37).
narrative_ontology:measurement(july_tr_t30, july_charter_sovereign_legitimacy__guided_nationalism_reading, theater_ratio, 30, 0.4).
narrative_ontology:measurement(july_tr_t36, july_charter_sovereign_legitimacy__guided_nationalism_reading, theater_ratio, 36, 0.42).

% Extraction over time
narrative_ontology:measurement(july_be_t0, july_charter_sovereign_legitimacy__guided_nationalism_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(july_be_t6, july_charter_sovereign_legitimacy__guided_nationalism_reading, base_extractiveness, 6, 0.48).
narrative_ontology:measurement(july_be_t12, july_charter_sovereign_legitimacy__guided_nationalism_reading, base_extractiveness, 12, 0.55).
narrative_ontology:measurement(july_be_t18, july_charter_sovereign_legitimacy__guided_nationalism_reading, base_extractiveness, 18, 0.61).
narrative_ontology:measurement(july_be_t24, july_charter_sovereign_legitimacy__guided_nationalism_reading, base_extractiveness, 24, 0.66).
narrative_ontology:measurement(july_be_t30, july_charter_sovereign_legitimacy__guided_nationalism_reading, base_extractiveness, 30, 0.69).
narrative_ontology:measurement(july_be_t36, july_charter_sovereign_legitimacy__guided_nationalism_reading, base_extractiveness, 36, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(july_su_t0, july_charter_sovereign_legitimacy__guided_nationalism_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(july_su_t6, july_charter_sovereign_legitimacy__guided_nationalism_reading, suppression_requirement, 6, 0.58).
narrative_ontology:measurement(july_su_t12, july_charter_sovereign_legitimacy__guided_nationalism_reading, suppression_requirement, 12, 0.64).
narrative_ontology:measurement(july_su_t18, july_charter_sovereign_legitimacy__guided_nationalism_reading, suppression_requirement, 18, 0.69).
narrative_ontology:measurement(july_su_t24, july_charter_sovereign_legitimacy__guided_nationalism_reading, suppression_requirement, 24, 0.73).
narrative_ontology:measurement(july_su_t30, july_charter_sovereign_legitimacy__guided_nationalism_reading, suppression_requirement, 30, 0.76).
narrative_ontology:measurement(july_su_t36, july_charter_sovereign_legitimacy__guided_nationalism_reading, suppression_requirement, 36, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(july_charter_sovereign_legitimacy__guided_nationalism_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(july_charter_sovereign_legitimacy__guided_nationalism_reading, 0.08).
narrative_ontology:affects_constraint(july_charter_sovereign_legitimacy__guided_nationalism_reading, secular_democratic_reading).
narrative_ontology:affects_constraint(july_charter_sovereign_legitimacy__guided_nationalism_reading, military_custodian_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked constraints decomposing the natural-language label 'July Charter sovereign legitimacy' per the ε-invariance principle. guided_nationalism_reading (this story) authors ε=0.71 with religious-nationalist beneficiaries and secular/minority victims. secular_democratic_reading authors a structurally distinct claim — secular institutions and civilian-military subordination as the legitimacy ground — with a much smaller or absent victim set among secularists (the roles invert: religious-nationalist actors become the excluded/payer seats there). military_custodian_reading authors a third distinct claim locating legitimacy in military guardianship rather than religious or democratic identity at all, with its own beneficiary (the military establishment) and victim (civilian political actors) structure. The three are not the same constraint measured three ways; they are three constraints sharing one contested kernel text, linked here for contamination-propagation analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
