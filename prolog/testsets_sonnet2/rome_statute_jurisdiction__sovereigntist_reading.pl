% ============================================================================
% CONSTRAINT STORY: rome_statute_jurisdiction__sovereigntist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rome_statute_jurisdiction__sovereigntist_reading, []).

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
 *   constraint_id: rome_statute_jurisdiction__sovereigntist_reading
 *   human_readable: Rome Statute Jurisdiction — Sovereigntist (Consent-Conditional) Reading
 *   domain: international_law/treaty_interpretation/institutional_authority
 *
 * SUMMARY:
 *   This story instantiates the sovereigntist reading of the Rome Statute
 *   jurisdiction kernel: the ICC's authority runs strictly from state
 *   consent, non-party nationals are immune absent a Security Council
 *   referral that any permanent member can block, and complementarity is read
 *   as deference to national proceedings rather than as a substantive
 *   override standard. Under this reading the Statute is a treaty like any
 *   other — binding only those who ratified, expanding jurisdiction only
 *   through mechanisms (UNSC referral) that themselves route through
 *   sovereign veto power. This is NOT the same constraint as the universalist
 *   reading (which treats the Statute as establishing jurisdiction
 *   transcending consent) or the hybrid_complementarity_reading (which treats
 *   complementarity as a substantive balancing mechanism rather than pure
 *   deference) — those are separate constraints with their own epsilon
 *   values, linked via network.affects_constraints.
 *
 * KEY AGENTS:
 *   - non_ratifying_great_powers: primary beneficiary (institutional/arbitrage) — categorical immunity absent UNSC referral
 *   - unsc_permanent_members: structural beneficiary and gatekeeper (institutional/arbitrage) — control the only jurisdictional bridge to non-parties
 *   - icc_prosecutorial_office: institutional payer (institutional/trapped) — bears the jurisdictional constraint directly, cannot self-expand mandate
 *   - victims_of_atrocities_in_non_party_states: primary victim class (powerless/trapped) — categorically excluded absent a vetoable referral
 *   - populations_of_weak_state_parties: secondary victim class (powerless/constrained) — nominally covered but structurally disadvantaged by deference standard
 *   - international_law_scholars: analytical observer (analytical/analytical) — documents the aspiration/mechanics gap
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rome_statute_jurisdiction__sovereigntist_reading, 0.42).
domain_priors:suppression_score(rome_statute_jurisdiction__sovereigntist_reading, 0.38).
domain_priors:theater_ratio(rome_statute_jurisdiction__sovereigntist_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rome_statute_jurisdiction__sovereigntist_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__sovereigntist_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__sovereigntist_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rome_statute_jurisdiction__sovereigntist_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__sovereigntist_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rome_statute_jurisdiction__sovereigntist_reading, tangled_rope).
narrative_ontology:human_readable(rome_statute_jurisdiction__sovereigntist_reading, "Rome Statute Jurisdiction — Sovereigntist (Consent-Conditional) Reading").
narrative_ontology:topic_domain(rome_statute_jurisdiction__sovereigntist_reading, "international_law/treaty_interpretation/institutional_authority").

domain_priors:requires_active_enforcement(rome_statute_jurisdiction__sovereigntist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rome_statute_jurisdiction__sovereigntist_reading, '7e5e9bd1-72fd-49f3-8136-32f1171c1736').
narrative_ontology:cs_kernel_codification('7e5e9bd1-72fd-49f3-8136-32f1171c1736', fixed_text).
narrative_ontology:cs_authority_grounding('7e5e9bd1-72fd-49f3-8136-32f1171c1736', lineage).
narrative_ontology:cs_interpretation_layer_present('7e5e9bd1-72fd-49f3-8136-32f1171c1736').
narrative_ontology:cs_reading_relation('7e5e9bd1-72fd-49f3-8136-32f1171c1736', rome_statute_jurisdiction__universalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('7e5e9bd1-72fd-49f3-8136-32f1171c1736', rome_statute_jurisdiction__hybrid_complementarity_reading, influences).
narrative_ontology:cs_axiom('7e5e9bd1-72fd-49f3-8136-32f1171c1736', foundational, state_consent_as_exclusive_basis_of_jurisdiction).
narrative_ontology:cs_axiom_status(state_consent_as_exclusive_basis_of_jurisdiction, holdable).
narrative_ontology:cs_axiom_grounding('7e5e9bd1-72fd-49f3-8136-32f1171c1736', state_consent_as_exclusive_basis_of_jurisdiction, conventional).
narrative_ontology:cs_axiom('7e5e9bd1-72fd-49f3-8136-32f1171c1736', secondary, complementarity_as_deference_not_override).
narrative_ontology:cs_axiom_status(complementarity_as_deference_not_override, holdable).
narrative_ontology:cs_axiom_grounding('7e5e9bd1-72fd-49f3-8136-32f1171c1736', complementarity_as_deference_not_override, conventional).
narrative_ontology:cs_reference_frame('7e5e9bd1-72fd-49f3-8136-32f1171c1736', westphalian_treaty_consent_framework).
narrative_ontology:cs_drift_state('7e5e9bd1-72fd-49f3-8136-32f1171c1736', post_2016_withdrawal_wave, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('7e5e9bd1-72fd-49f3-8136-32f1171c1736', '').
narrative_ontology:cs_kernel_id(rome_statute_jurisdiction__sovereigntist_reading, rome_statute_jurisdiction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__sovereigntist_reading, non_ratifying_great_powers).
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__sovereigntist_reading, domestic_judiciaries_of_state_parties).
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__sovereigntist_reading, unsc_permanent_members).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__sovereigntist_reading, victims_of_atrocities_in_non_party_states).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__sovereigntist_reading, icc_prosecutorial_office).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__sovereigntist_reading, populations_of_weak_state_parties).
narrative_ontology:constraint_vindicates(rome_statute_jurisdiction__sovereigntist_reading, state_consent_as_basis_of_treaty_obligation).
narrative_ontology:constraint_vindicates(rome_statute_jurisdiction__sovereigntist_reading, sovereign_equality_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% States such as the US, Russia, China, and India never ratified or withdrew signature; under this reading their nationals are categorically immune from ICC jurisdiction absent a Security Council referral. They actively promote this reading in diplomatic and legal fora because it insulates their military and political personnel from prosecution while still allowing them to invoke the Statute selectively against adversaries via the Security Council.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__sovereigntist_reading, non_ratifying_great_powers, beneficiary,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(rome_statute_jurisdiction__sovereigntist_reading, non_ratifying_great_powers, agenda_setter).

% The five permanent Security Council members hold veto power over referrals, meaning the only jurisdictional bridge to non-consenting states under this reading runs through an institution they individually control. They can refer rivals' situations while shielding allies and themselves, converting the consent requirement into a selectively deployable political tool rather than a neutral limiting principle.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__sovereigntist_reading, unsc_permanent_members, beneficiary,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(rome_statute_jurisdiction__sovereigntist_reading, unsc_permanent_members, agenda_setter).

% National courts in ratifying states retain first-priority authority to investigate and prosecute their own nationals under the complementarity-as-deference reading; the ICC may act only where national systems are unwilling or unable in the strictest sense. This preserves domestic judicial sovereignty and shields national institutions from being second-guessed except in extreme cases.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__sovereigntist_reading, domestic_judiciaries_of_state_parties, beneficiary,
    institutional, generational, constrained, national).

% Bears the structural cost of the consent-conditional reading directly: cannot open investigations into non-party nationals' conduct on non-party territory without a UNSC referral that permanent members can block, and must defer heavily to national proceedings even where those proceedings are pretextual. The office cannot exit the framework — its authority is defined entirely by the treaty and the reading applied to it.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__sovereigntist_reading, icc_prosecutorial_office, payer,
    institutional, generational, trapped, global).

% Civilians harmed by conduct occurring on the territory of a non-ratifying state, or by nationals of a non-ratifying state, have no path to ICC jurisdiction under this reading unless the Security Council refers the situation — a referral any permanent member can veto. They have no direct standing, no alternative international forum with comparable authority, and cannot compel their own state's cooperation if that state is itself the perpetrator or an ally of the perpetrator's patron.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__sovereigntist_reading, victims_of_atrocities_in_non_party_states, payer,
    powerless, biographical, trapped, local).

% Citizens of states that DID ratify but lack the institutional capacity or political will to prosecute their own elites bear a double cost: their state is bound by the Statute (so ICC jurisdiction nominally exists) yet the deference-heavy complementarity reading raises the bar for admissibility, and their state's national judiciary may be captured by the very actors under investigation, leaving accountability gaps the strict-consent framework does not address.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__sovereigntist_reading, populations_of_weak_state_parties, payer,
    powerless, biographical, constrained, regional).

% The Assembly of States Parties administers amendments, budget, and judicial appointments, operating within whatever jurisdictional reading prevails. Under the sovereigntist reading it has limited ability to expand jurisdiction over non-parties without triggering mass withdrawal threats, so it manages the Statute's operation around the consent limitation rather than against it.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__sovereigntist_reading, icc_state_parties_assembly, agenda_setter,
    organized, generational, constrained, global).

% Academic and practitioner commentators analyze how the consent-conditional reading interacts with customary international law claims, jus cogens exceptions, and the Statute's own preambular language about ending impunity. They document the gap between the Statute's stated aspiration and its consent-limited operative mechanics without themselves controlling jurisdictional outcomes.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__sovereigntist_reading, international_law_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(rome_statute_jurisdiction__sovereigntist_reading, non_ratifying_great_powers).
narrative_ontology:fixing_cost_class(rome_statute_jurisdiction__sovereigntist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides states with a predictable, opt-in mechanism for delegating a narrow slice of criminal jurisdiction to a permanent international tribunal, preserving each state's sovereign choice over whether and how far to be bound — solving the coordination problem of establishing a standing court without requiring universal, non-consensual submission to it.
% TRANSFER_FUNCTION: Moves accountability risk away from nationals of non-ratifying and powerful states (who remain shielded absent a UNSC referral their patrons can block) and concentrates residual jurisdictional exposure onto nationals of ratifying, typically weaker states and onto victims in situations where no permanent-member interest motivates a referral.
% ABSENT_VOICES: Victims in non-party states have no seat in the consent architecture at all — their access to justice depends entirely on the political calculus of Security Council permanent members, none of whom answer to them. Weaker state parties' own citizens are formally covered but structurally disadvantaged by the deference standard, and they have no forum to contest how 'unwillingness' or 'inability' is assessed by the Court against their national elites.
% DISAPPEARANCE_RATIONALE: If the strict-consent reading disappeared and jurisdiction extended universally regardless of ratification, non-ratifying great powers would face genuinely novel exposure and would likely respond with withdrawal threats, non-cooperation, or attempts to defund/delegitimize the Court — a major rearrangement for them. For victims currently excluded, the world would rearrange favorably (new access to justice). For domestic judiciaries of state parties, little would change since their national proceedings would still take priority in most ordinary cases. The verdict is genuinely contested because the reading's disappearance affects different stakeholders in opposite directions and by different magnitudes.
% FOUNDING_PROBLEM: The 1998 Rome Conference needed to secure enough state ratifications to bring a permanent international criminal court into existence at all, after the ad hoc tribunal model (ICTY, ICTR) proved politically and financially unsustainable as a template for every future atrocity. Strict consent limitations were the price of getting major and mid-sized powers to sign rather than boycott the entire project.
% FOUNDING_PROBLEM_CORROBORATION: Non-ratifying powers and their aligned legal scholars attest the founding problem — the need for broad state buy-in to establish institutional legitimacy — remains live and justifies continued consent-gating. Independent international law historians and several former ICC officials, corroborating from outside the beneficiary set, argue the founding problem (ending impunity for atrocity crimes) has been substantially frustrated by the consent architecture itself, and that the bargain struck in 1998 to secure signatures has calcified into a permanent immunity structure for the powerful rather than a transitional compromise.
narrative_ontology:disappearance_verdict(rome_statute_jurisdiction__sovereigntist_reading, contested).
narrative_ontology:founding_problem_status(rome_statute_jurisdiction__sovereigntist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rome_statute_jurisdiction__sovereigntist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(rome_statute_jurisdiction__sovereigntist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(rome_statute_jurisdiction__sovereigntist_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rome_statute_jurisdiction__sovereigntist_reading_tests).
:- end_tests(rome_statute_jurisdiction__sovereigntist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42) because the sovereigntist reading is not primarily an extraction mechanism in the classic rent-seeking sense — it is a genuine consent-based coordination structure that ALSO produces a systematic asymmetry: the states most capable of committing atrocities at scale (major military powers) enjoy the most complete immunity, while accountability exposure concentrates on weaker ratifying states and their populations. Suppression (0.38) reflects the vetoable-referral mechanism, which is a real, structural, non-scaled barrier — it does not require case-by-case coercion, the veto is dispositive by design. Theater ratio (0.30) reflects that the complementarity apparatus performs meaningful judicial review in most cases even as its deference standard is calibrated to rarely override national proceedings from powerful or well-connected states. Accessibility collapse (0.45) and resistance (0.58) are mid-range and deliberately NOT mountain-shaped: victims and scholars actively contest the strict-consent framing, and alternative readings (universalist, hybrid) remain live in the same institutional space — this is exactly the profile of a contested treaty-interpretation kernel, not a natural law.
 *
 * PERSPECTIVAL GAP:
 *   From the non-ratifying great power seat, this reading looks like a rope — a sensible, sovereignty-respecting coordination mechanism that lets states opt into international justice without surrendering blanket authority over their own nationals. From the seat of victims in non-party states or the ICC prosecutorial office, the identical structural arrangement computes as something closer to tangled_rope-shaded-toward-snare: real coordination exists (many states did ratify and do submit to jurisdiction) but the coordination function is bought at the price of asymmetric extraction — accountability risk is systematically displaced onto those with the least power to resist it. The engine should compute this divergence from the stakeholder power/exit data, not from any authored claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Non-ratifying great powers and UNSC permanent members sit at the beneficiary end: the consent-conditional reading is the mechanism by which their nationals' immunity is secured, and they actively promote this reading in diplomatic practice (US Bilateral Immunity Agreements, Russian and Chinese non-ratification postures). Domestic judiciaries of state parties benefit from deference but bear some residual constraint. The ICC prosecutorial office is structurally a target: the reading directly constrains what it can do, and it has no exit — dissolution or radical reform are its only alternatives to living inside the constraint. Victims of atrocities in non-party states are the clearest full-target seat: trapped, powerless, and excluded from the consent architecture entirely except through a referral mechanism controlled by the very powers most likely to shield the perpetrator or its patron.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — assembling enough state consent to launch a permanent court at all in 1998 — was genuinely live at the time and the consent-gating was a defensible transitional bargain. Whether that problem remains live in 2024 is exactly the contested question: non-ratifying powers argue continued consent-gating is still necessary to preserve institutional legitimacy and avoid mass defection; independent scholars and former ICC officials argue the bargain has calcified into a permanent immunity structure that no longer serves expansion of legitimacy (few new major-power ratifications have followed) and now primarily serves incumbent-power shielding. This is precisely a founding_problem_status: contested case rather than a clean live/dead call — the mismatch between corroborating sources outside the beneficiary set (historians, former officials) and the self-interested account from non-ratifying powers is the signal the R5 interview is designed to surface.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    consent_as_genuine_limit_or_selective_shield,
    'Is the strict-consent jurisdictional gate a principled expression of sovereign equality, or a mechanism that, in practice, shields the most powerful states while binding weaker ones who lack the leverage to withhold or withdraw consent?',
    'Comparative analysis of ratification patterns weighted by military/economic capability, and tracking of actual UNSC referral outcomes (which situations get referred, which are blocked, and by whom) over the Statute''s operating history.',
    'If the pattern shows referrals systematically targeting weaker or geopolitically disfavored states while shielding permanent-member allies, this supports reading the consent architecture as functionally asymmetric extraction dressed as sovereign equality — pushing the classification toward tangled_rope or snare. If referral patterns show no such asymmetry, the coordination framing is better supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_as_genuine_limit_or_selective_shield, empirical, 'Whether consent-gating functions as principled sovereignty or selective shielding in practice.').

omega_variable(
    kernel_reading_disagreement_locus,
    'This constraint is one reading (sovereigntist) of the rome_statute_jurisdiction kernel; sibling readings are universalist_reading and hybrid_complementarity_reading. Where exactly does the disagreement between readings live — in the interpretation of Article 12 (preconditions to exercise of jurisdiction), in the preambular language on ending impunity, or in the customary-international-law status of jus cogens crimes independent of treaty consent?',
    'Textual and travaux preparatoires analysis of Article 12''s drafting history, cross-referenced against ICJ and ICC Appeals Chamber jurisprudence on the relationship between treaty-based and customary jurisdictional bases.',
    'If the disagreement is purely about Article 12''s textual scope, the readings may be reconcilable through interpretive practice (moving toward hybrid_complementarity_reading). If the disagreement is about whether customary international law independently authorizes jurisdiction over jus cogens crimes regardless of consent, the sovereigntist and universalist readings are foreclosing rather than merely competing — a structurally different relationship than currently declared (coexists_with).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_locus, conceptual, 'Locating the precise textual/doctrinal site of disagreement between the three kernel readings.').

omega_variable(
    unsc_referral_as_coordination_or_capture,
    'Is the UNSC-referral bridge to non-party jurisdiction a genuine coordination mechanism (allowing the international community collectively to extend accountability in extreme cases) or is it structurally captured by permanent-member veto power such that it functions as a beneficiary-controlled valve rather than a coordination device?',
    'Track record analysis: how many referral attempts have been vetoed or threatened with veto versus successfully adopted, and whether veto use correlates with permanent-member alliance structures rather than case merits.',
    'A high veto-blocking rate correlated with alliance protection would support treating the referral mechanism as captured, strengthening the case that the sovereigntist reading''s ''coordination'' function is substantially a beneficiary-control mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unsc_referral_as_coordination_or_capture, empirical, 'Whether the sole jurisdictional bridge to non-consenting states functions as coordination or capture.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rome_statute_jurisdiction__sovereigntist_reading, 1998, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rome_tr_t1998, rome_statute_jurisdiction__sovereigntist_reading, theater_ratio, 1998, 0.18).
narrative_ontology:measurement(rome_tr_t2002, rome_statute_jurisdiction__sovereigntist_reading, theater_ratio, 2002, 0.2).
narrative_ontology:measurement(rome_tr_t2008, rome_statute_jurisdiction__sovereigntist_reading, theater_ratio, 2008, 0.24).
narrative_ontology:measurement(rome_tr_t2014, rome_statute_jurisdiction__sovereigntist_reading, theater_ratio, 2014, 0.27).
narrative_ontology:measurement(rome_tr_t2019, rome_statute_jurisdiction__sovereigntist_reading, theater_ratio, 2019, 0.29).
narrative_ontology:measurement(rome_tr_t2024, rome_statute_jurisdiction__sovereigntist_reading, theater_ratio, 2024, 0.3).

% Extraction over time
narrative_ontology:measurement(rome_be_t1998, rome_statute_jurisdiction__sovereigntist_reading, base_extractiveness, 1998, 0.28).
narrative_ontology:measurement(rome_be_t2002, rome_statute_jurisdiction__sovereigntist_reading, base_extractiveness, 2002, 0.32).
narrative_ontology:measurement(rome_be_t2008, rome_statute_jurisdiction__sovereigntist_reading, base_extractiveness, 2008, 0.36).
narrative_ontology:measurement(rome_be_t2014, rome_statute_jurisdiction__sovereigntist_reading, base_extractiveness, 2014, 0.38).
narrative_ontology:measurement(rome_be_t2019, rome_statute_jurisdiction__sovereigntist_reading, base_extractiveness, 2019, 0.4).
narrative_ontology:measurement(rome_be_t2024, rome_statute_jurisdiction__sovereigntist_reading, base_extractiveness, 2024, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(rome_su_t1998, rome_statute_jurisdiction__sovereigntist_reading, suppression_requirement, 1998, 0.25).
narrative_ontology:measurement(rome_su_t2002, rome_statute_jurisdiction__sovereigntist_reading, suppression_requirement, 2002, 0.28).
narrative_ontology:measurement(rome_su_t2008, rome_statute_jurisdiction__sovereigntist_reading, suppression_requirement, 2008, 0.31).
narrative_ontology:measurement(rome_su_t2014, rome_statute_jurisdiction__sovereigntist_reading, suppression_requirement, 2014, 0.34).
narrative_ontology:measurement(rome_su_t2019, rome_statute_jurisdiction__sovereigntist_reading, suppression_requirement, 2019, 0.36).
narrative_ontology:measurement(rome_su_t2024, rome_statute_jurisdiction__sovereigntist_reading, suppression_requirement, 2024, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rome_statute_jurisdiction__sovereigntist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(rome_statute_jurisdiction__sovereigntist_reading, 0.12).
narrative_ontology:affects_constraint(rome_statute_jurisdiction__sovereigntist_reading, rome_statute_jurisdiction__universalist_reading).
narrative_ontology:affects_constraint(rome_statute_jurisdiction__sovereigntist_reading, rome_statute_jurisdiction__hybrid_complementarity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three linked stories decomposing the natural-language label 'the Rome Statute's jurisdictional scope' per the epsilon-invariance principle. sovereigntist_reading (this file) authors epsilon=0.42 with beneficiaries concentrated among non-ratifying great powers and UNSC permanent members. universalist_reading and hybrid_complementarity_reading are separate files with their own epsilon values, beneficiary/victim structures, and classifications, reflecting that the three readings instantiate structurally distinct constraints sharing only the treaty text as a common kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
