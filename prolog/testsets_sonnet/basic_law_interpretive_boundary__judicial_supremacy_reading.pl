% ============================================================================
% CONSTRAINT STORY: basic_law_interpretive_boundary__judicial_supremacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_basic_law_interpretive_boundary__judicial_supremacy_reading, []).

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
 *   constraint_id: basic_law_interpretive_boundary__judicial_supremacy_reading
 *   human_readable: Basic Laws as Judicially-Enforced Higher Law (Judicial Supremacy Reading)
 *   domain: constitutional_law/comparative_constitutionalism/judicial_review
 *
 * SUMMARY:
 *   This story instantiates the judicial-supremacy reading of the contested
 *   Basic Law interpretive kernel: the claim that the Basic Laws are a
 *   higher-order legal framework the Supreme Court must interpret and
 *   enforce, with its invalidation power binding on the Knesset. Under this
 *   reading, the 1995 Bank Mizrahi ruling did not merely interpret existing
 *   law but effectively completed the constitution the 1950 Harari decision
 *   deferred, establishing judicial review of primary legislation without a
 *   formally entrenched, super-majority-amendable constitutional text. The
 *   coordination function (protecting rights and minorities against
 *   unconstrained majoritarianism) is real; so is the asymmetric extraction
 *   (elected coalitions repeatedly losing legislative priorities to an
 *   unelected body they cannot straightforwardly override). The rising
 *   extractiveness and suppression-requirement series through 2023 track the
 *   escalating judicial-reform confrontation; the 2024 dip reflects partial
 *   de-escalation after the reform effort stalled amid the war's political
 *   disruption, not resolution of the underlying structural tension.
 *
 * KEY AGENTS:
 *   - supreme_court_justices: agenda_setter (institutional/analytical) — administers and enforces the boundary
 *   - rights_claimant_litigants: beneficiary (moderate/constrained) — primary users of the judicial lever
 *   - minority_groups_protected_by_courts: beneficiary (powerless/trapped) — depend on courts absent electoral leverage
 *   - knesset_majority_coalition: payer (powerful/constrained) — bears repeated invalidation of legislative priorities
 *   - settler_movement_legislators & ultra_orthodox_political_parties: payer (organized/constrained) — electorally successful but judicially blocked constituencies
 *   - executive_ministers_pursuing_reform: payer (powerful/constrained) — caught in a self-referential reform loop
 *   - comparative_constitutional_scholars: observer (analytical/global) — sees the structural comparison to codified systems
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(basic_law_interpretive_boundary__judicial_supremacy_reading, 0.58).
domain_priors:suppression_score(basic_law_interpretive_boundary__judicial_supremacy_reading, 0.47).
domain_priors:theater_ratio(basic_law_interpretive_boundary__judicial_supremacy_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__judicial_supremacy_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__judicial_supremacy_reading, suppression_requirement, 0.47).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__judicial_supremacy_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__judicial_supremacy_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__judicial_supremacy_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(basic_law_interpretive_boundary__judicial_supremacy_reading, tangled_rope).
narrative_ontology:human_readable(basic_law_interpretive_boundary__judicial_supremacy_reading, "Basic Laws as Judicially-Enforced Higher Law (Judicial Supremacy Reading)").
narrative_ontology:topic_domain(basic_law_interpretive_boundary__judicial_supremacy_reading, "constitutional_law/comparative_constitutionalism/judicial_review").

domain_priors:requires_active_enforcement(basic_law_interpretive_boundary__judicial_supremacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(basic_law_interpretive_boundary__judicial_supremacy_reading, '96e9946f-f629-4b5f-b7c1-fd66ed49c5d9').
narrative_ontology:cs_kernel_codification('96e9946f-f629-4b5f-b7c1-fd66ed49c5d9', distributed).
narrative_ontology:cs_authority_grounding('96e9946f-f629-4b5f-b7c1-fd66ed49c5d9', practice).
narrative_ontology:cs_interpretation_layer_present('96e9946f-f629-4b5f-b7c1-fd66ed49c5d9').
narrative_ontology:cs_reading_relation('96e9946f-f629-4b5f-b7c1-fd66ed49c5d9', basic_law_interpretive_boundary__parliamentary_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('96e9946f-f629-4b5f-b7c1-fd66ed49c5d9', basic_law_interpretive_boundary__balanced_contestation_reading, coexists_with).
narrative_ontology:cs_axiom('96e9946f-f629-4b5f-b7c1-fd66ed49c5d9', foundational, judicial_invalidation_binds_legislature).
narrative_ontology:cs_axiom_status(judicial_invalidation_binds_legislature, holdable).
narrative_ontology:cs_axiom_grounding('96e9946f-f629-4b5f-b7c1-fd66ed49c5d9', judicial_invalidation_binds_legislature, conventional).
narrative_ontology:cs_axiom('96e9946f-f629-4b5f-b7c1-fd66ed49c5d9', secondary, basic_laws_constitute_completed_constitutional_tier).
narrative_ontology:cs_axiom_status(basic_laws_constitute_completed_constitutional_tier, holdable).
narrative_ontology:cs_axiom_grounding('96e9946f-f629-4b5f-b7c1-fd66ed49c5d9', basic_laws_constitute_completed_constitutional_tier, conventional).
narrative_ontology:cs_reference_frame('96e9946f-f629-4b5f-b7c1-fd66ed49c5d9', harari_deferred_constitution_compromise).
narrative_ontology:cs_drift_state('96e9946f-f629-4b5f-b7c1-fd66ed49c5d9', post_bank_mizrahi_judicial_reform_era, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('96e9946f-f629-4b5f-b7c1-fd66ed49c5d9', '').
narrative_ontology:cs_kernel_id(basic_law_interpretive_boundary__judicial_supremacy_reading, basic_law_interpretive_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__judicial_supremacy_reading, rights_claimant_litigants).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__judicial_supremacy_reading, minority_groups_protected_by_courts).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__judicial_supremacy_reading, supreme_court_justices).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__judicial_supremacy_reading, liberal_civil_society_organizations).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__judicial_supremacy_reading, knesset_majority_coalition).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__judicial_supremacy_reading, settler_movement_legislators).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__judicial_supremacy_reading, ultra_orthodox_political_parties).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__judicial_supremacy_reading, executive_ministers_pursuing_reform).
narrative_ontology:constraint_vindicates(basic_law_interpretive_boundary__judicial_supremacy_reading, constitutional_supremacy_doctrine).
narrative_ontology:constraint_vindicates(basic_law_interpretive_boundary__judicial_supremacy_reading, reasonableness_standard_legitimacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret the Basic Laws as a constitutional ceiling on legislation, applying doctrines such as reasonableness and proportionality to strike down or narrow Knesset enactments. They administer the boundary itself — deciding what counts as a Basic Law violation — and their institutional authority and case-law-building function both depend on that boundary being enforced and respected.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__judicial_supremacy_reading, supreme_court_justices, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(basic_law_interpretive_boundary__judicial_supremacy_reading, supreme_court_justices, beneficiary).

% Individuals and NGOs who bring petitions against legislation or executive action, using the Basic Law framework to obtain judicial protection they could not secure through ordinary political majorities. Their access to relief depends entirely on the Court retaining and exercising the power of invalidation.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__judicial_supremacy_reading, rights_claimant_litigants, beneficiary,
    moderate, biographical, constrained, national).

% Populations without durable legislative majorities — including Arab citizens, asylum seekers, and other minorities — rely on judicial review as their primary recourse against majoritarian legislation. They cannot exit the polity and have limited capacity to build electoral coalitions large enough to protect their interests through ordinary politics.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__judicial_supremacy_reading, minority_groups_protected_by_courts, beneficiary,
    powerless, generational, trapped, national).

% The elected governing coalition passes legislation reflecting its electoral mandate, only to have portions invalidated or narrowed by judicial interpretation of the Basic Laws. It can pass laws but cannot guarantee they survive judicial review, and amending a Basic Law to foreclose review requires supermajorities or coalition unity it frequently lacks.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__judicial_supremacy_reading, knesset_majority_coalition, payer,
    powerful, immediate, constrained, national).

% Legislators and organizations advancing settlement expansion and land-transfer policy have repeatedly seen preferred legislation struck down or conditioned by the Court under Basic Law: Human Dignity and Liberty. They experience the constraint as an unelected veto on policies with electoral backing.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__judicial_supremacy_reading, settler_movement_legislators, payer,
    organized, biographical, constrained, national).

% Parties seeking military-service exemptions and religious-status legislation have had core legislative priorities invalidated by the Court's Basic Law jurisprudence (e.g. the Haredi draft exemption rulings), despite securing repeated electoral mandates for those priorities.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__judicial_supremacy_reading, ultra_orthodox_political_parties, payer,
    organized, generational, constrained, national).

% Ministers attempting judicial-reform legislation (limiting the reasonableness doctrine, altering judicial-appointment composition) find their own reform efforts subject to review by the very court whose power they seek to curb, producing a self-referential loop the executive cannot easily escape through ordinary legislation.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__judicial_supremacy_reading, executive_ministers_pursuing_reform, payer,
    powerful, immediate, constrained, national).

% NGOs and advocacy groups that litigate strategically use the judicial supremacy framework as their primary lever for policy change, filing petitions that would have no equivalent path through ordinary legislative advocacy given their minority political weight.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__judicial_supremacy_reading, liberal_civil_society_organizations, beneficiary,
    organized, generational, mobile, national).

% Voters who elect the Knesset have no direct mechanism to ratify or reject specific judicial interpretations of the Basic Laws; their electoral mandate can be substantially reshaped by rulings they did not vote on and cannot easily reverse.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__judicial_supremacy_reading, general_electorate, excluded,
    powerless, generational, trapped, national).

% Scholars comparing Israel's uncodified constitutional order to codified systems (US, Germany) analyze whether judicial supremacy without an entrenched constitutional text and without a constitutionally-specified override mechanism produces a stable settlement or a standing legitimacy crisis.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__judicial_supremacy_reading, comparative_constitutional_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(basic_law_interpretive_boundary__judicial_supremacy_reading, diffuse).
narrative_ontology:fixing_cost_class(basic_law_interpretive_boundary__judicial_supremacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a check against majoritarian legislation that would otherwise be unconstrained by any codified constitution, protecting rights and minority interests that lack durable electoral majorities, and supplying predictable legal doctrine (proportionality, reasonableness) that other institutions and private actors can plan around.
% TRANSFER_FUNCTION: Moves effective policy-making power from the elected legislative majority to the unelected judiciary and, derivatively, to the litigants and organizations able to invoke the Court's jurisdiction — at the cost of the enacting coalition's ability to convert electoral mandates into durable law.
% ABSENT_VOICES: The general electorate that produced the invalidated legislation has no institutionalized voice in the judicial process itself; their remedy is indirect (electing legislators who might eventually alter Basic Law procedure or Court composition), and that remedy has historically been slow and contested. Religious and settler constituencies who experience repeated invalidation describe themselves as structurally locked out of an interpretive process they cannot access on equal terms with litigating NGOs.
% DISAPPEARANCE_RATIONALE: If judicial invalidation power over Knesset legislation disappeared overnight, the Knesset would become the unconstrained final word on all legislation; military-exemption laws, settlement-related enactments, and reasonableness-doctrine-constrained executive decisions currently blocked or narrowed by the Court would proceed unmodified. Rights-claimant litigation as a policy lever would collapse, and minority groups without electoral leverage would lose their primary institutional protection. This is not a null structure — an entire body of case law, litigation strategy, and coalition-formation calculation is built on the assumption that the Court can and will exercise this power.
% FOUNDING_PROBLEM: Israel lacks a single codified constitution; the Basic Laws were enacted piecemeal, several explicitly described as building blocks toward a future constitution, and the 1992 Human Dignity and Liberty and Freedom of Occupation laws were read by the 1995 Bank Mizrahi ruling as furnishing the missing higher-law tier, filling the gap left by the 1950 Harari decision's deferred constitution-writing.
% FOUNDING_PROBLEM_CORROBORATION: Supreme Court justices and liberal legal scholars attest the founding problem (an unconstrained majoritarian legislature with no rights ceiling) remains live and cite ongoing legislative threats to minority and civil rights as evidence. Knesset legal advisors, some comparative constitutional scholars, and dissenting justices from the founding era (including members of the original Basic Law drafting committees) have stated on the record that the Basic Laws were never intended by their drafters to constitute a full constitution enforceable by judicial invalidation, and that the 1995 judicial move exceeded what the enacting Knesset understood itself to be doing — corroboration exists on both sides, with no single outside-the-benefiting-parties consensus resolving it.
narrative_ontology:disappearance_verdict(basic_law_interpretive_boundary__judicial_supremacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(basic_law_interpretive_boundary__judicial_supremacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(basic_law_interpretive_boundary__judicial_supremacy_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(basic_law_interpretive_boundary__judicial_supremacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(basic_law_interpretive_boundary__judicial_supremacy_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(basic_law_interpretive_boundary__judicial_supremacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(basic_law_interpretive_boundary__judicial_supremacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(basic_law_interpretive_boundary__judicial_supremacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.58 (moderate-high, not extreme) because the coordination function — rights protection absent a codified constitution — is genuine and not merely cover; but the asymmetric cost to repeatedly-blocked electoral majorities (settler and Haredi legislators especially) is substantial and structurally recurring, not incidental. Suppression sits near 0.47: the Court cannot jail or fine non-compliant legislators, but its invalidation is binding and backed by the state's enforcement apparatus once a law is struck, and the absence of any codified override mechanism means resistance must occur through slow constitutional politics (amending Basic Laws, altering Court composition) rather than direct legislative correction. Accessibility collapse (0.40) and resistance (0.72) reflect that alternatives to accepting judicial supremacy are NOT foreclosed — override legislation, Basic Law amendment, and the 2023 reform effort all remain live avenues, and resistance to the constraint is vigorous and organized, distinguishing this from a mountain-like or near-total-collapse structure.
 *
 * PERSPECTIVAL GAP:
 *   From the justices' seat, this is principled constitutional guardianship filling a deliberate gap left by the 1950 Harari compromise. From the seat of a settler-movement legislator or Haredi party whose electorally-mandated bill was struck down, the same structure operates as an unelected veto with no comparable check running the other direction. The engine's per-seat computation should reflect this asymmetry directly from the structural data (power, exit, beneficiary/victim role) rather than from either party's self-description.
 *
 * DIRECTIONALITY LOGIC:
 *   Justices sit at the agenda-setting/beneficiary pole: they administer the boundary and their institutional authority is constituted by its continued exercise. Rights-claimant litigants and structurally powerless minorities derive real, low-d benefit because the mechanism is often their only effective recourse. The Knesset majority coalition, settler legislators, and ultra-Orthodox parties sit at the high-d target end: they hold electoral mandates that the same structure repeatedly nullifies, and their exit options are constrained (they can attempt Basic Law amendment or Court-composition reform, but these require supermajorities or sustained coalition unity difficult to sustain). Executive ministers pursuing judicial reform experience a distinctive bind: the tool they need to change (judicial review) is the tool that reviews their attempt to change it.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (absence of a rights ceiling on legislative majoritarianism) plausibly remains live in the abstract, but the specific mechanism by which the Court filled that gap — self-conferred authority via interpretive ruling rather than a text specifically drafted and ratified for that purpose — is exactly the contested feature that the sibling readings (parliamentary sovereignty, balanced contestation) dispute. Treating this as a tangled_rope rather than either a pure rope (understating the extraction on electorally-successful blocked constituencies) or a pure snare (understating the genuine minority-protection function) is the classification that avoids both mislabeling errors.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    constitutional_completion_vs_judicial_self_authorization,
    'Did the 1995 Bank Mizrahi ruling correctly interpret the Basic Laws as already constituting a higher-order constitutional tier (a discovery of latent constitutional structure), or did the Court self-authorize a supremacy power the enacting Knesset never granted (a construction serving the Court''s own institutional authority)?',
    'Historical analysis of the Basic Law drafting committees'' contemporaneous statements and the Harari decision''s original deferral language, cross-referenced against subsequent Knesset attempts to legislate around or override specific rulings; a sustained failure of the political branches to successfully override the Court over multiple electoral cycles despite repeated attempts would support the self-authorization reading empirically.',
    'If self-authorization, the beneficiary structure (justices as agenda_setter/beneficiary) is not incidental to the mechanism but constitutive of it — the Court both created and administers its own binding authority, which is the paradigmatic tangled_rope signature. If genuine constitutional completion, the coordination function dominates and the extraction reads as a legitimate byproduct of rights protection rather than an appropriated power.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constitutional_completion_vs_judicial_self_authorization, conceptual, 'Whether judicial supremacy discovered or constructed constitutional authority.').

omega_variable(
    kernel_reading_selection_evidence,
    'What in the current institutional record most supports selecting the judicial_supremacy_reading over the parliamentary_sovereignty_reading or balanced_contestation_reading as the operative structural account, given that Israeli constitutional practice does not cleanly settle this at the level of enacted text?',
    'The outcome of the 2023-2024 judicial reform confrontation and reasonableness-standard legislation dispute functions as a live natural experiment: if the Knesset''s override attempt succeeds and durably reduces the Court''s invalidation power, the parliamentary_sovereignty_reading gains empirical support; if the Court''s invalidation authority persists functionally unchanged despite legislative attempts to curb it, the judicial_supremacy_reading is better supported as the operative account.',
    'Selecting this reading over the siblings determines the entire beneficiary/victim structure and the χ computation; the story''s ε (0.58) and directionality assignments would look substantially different under the parliamentary_sovereignty_reading, where the same events would register mainly as intra-institutional political conflict rather than as recurring extraction from electorally-successful legislative coalitions.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_evidence, conceptual, 'Which kernel reading the current record actually supports, and how contested that selection remains.').

omega_variable(
    override_mechanism_absence,
    'Does the absence of any formally codified override mechanism for Supreme Court rulings on Basic Law compliance (unlike, e.g., Canada''s notwithstanding clause) make the judicial_supremacy_reading structurally more entrenched than comparable systems, or does the practical availability of Basic Law amendment by ordinary Knesset majority (since Basic Laws themselves are not specially entrenched) mean the override path exists but is politically, not legally, difficult?',
    'Comparative analysis of actual amendment attempts: track how many Basic Law amendments proposed specifically to overturn or preempt Court rulings have succeeded versus failed, and why (coalition instability vs. genuine supermajority requirement vs. Court subsequently striking down the amendment itself).',
    'If amendment is legally easy but politically hard, the suppression metric may be overstated relative to genuine legal entrenchment — this bears directly on whether accessibility_collapse (0.40) is calibrated correctly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(override_mechanism_absence, empirical, 'Whether the override path is legally open but politically difficult, affecting suppression calibration.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(basic_law_interpretive_boundary__judicial_supremacy_reading, 1992, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(basi_tr_t1992, basic_law_interpretive_boundary__judicial_supremacy_reading, theater_ratio, 1992, 0.05).
narrative_ontology:measurement(basi_tr_t1995, basic_law_interpretive_boundary__judicial_supremacy_reading, theater_ratio, 1995, 0.08).
narrative_ontology:measurement(basi_tr_t2005, basic_law_interpretive_boundary__judicial_supremacy_reading, theater_ratio, 2005, 0.12).
narrative_ontology:measurement(basi_tr_t2015, basic_law_interpretive_boundary__judicial_supremacy_reading, theater_ratio, 2015, 0.16).
narrative_ontology:measurement(basi_tr_t2020, basic_law_interpretive_boundary__judicial_supremacy_reading, theater_ratio, 2020, 0.2).
narrative_ontology:measurement(basi_tr_t2023, basic_law_interpretive_boundary__judicial_supremacy_reading, theater_ratio, 2023, 0.24).
narrative_ontology:measurement(basi_tr_t2024, basic_law_interpretive_boundary__judicial_supremacy_reading, theater_ratio, 2024, 0.22).

% Extraction over time
narrative_ontology:measurement(basi_be_t1992, basic_law_interpretive_boundary__judicial_supremacy_reading, base_extractiveness, 1992, 0.15).
narrative_ontology:measurement(basi_be_t1995, basic_law_interpretive_boundary__judicial_supremacy_reading, base_extractiveness, 1995, 0.32).
narrative_ontology:measurement(basi_be_t2005, basic_law_interpretive_boundary__judicial_supremacy_reading, base_extractiveness, 2005, 0.42).
narrative_ontology:measurement(basi_be_t2015, basic_law_interpretive_boundary__judicial_supremacy_reading, base_extractiveness, 2015, 0.5).
narrative_ontology:measurement(basi_be_t2020, basic_law_interpretive_boundary__judicial_supremacy_reading, base_extractiveness, 2020, 0.55).
narrative_ontology:measurement(basi_be_t2023, basic_law_interpretive_boundary__judicial_supremacy_reading, base_extractiveness, 2023, 0.6).
narrative_ontology:measurement(basi_be_t2024, basic_law_interpretive_boundary__judicial_supremacy_reading, base_extractiveness, 2024, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(basi_su_t1992, basic_law_interpretive_boundary__judicial_supremacy_reading, suppression_requirement, 1992, 0.2).
narrative_ontology:measurement(basi_su_t1995, basic_law_interpretive_boundary__judicial_supremacy_reading, suppression_requirement, 1995, 0.35).
narrative_ontology:measurement(basi_su_t2005, basic_law_interpretive_boundary__judicial_supremacy_reading, suppression_requirement, 2005, 0.4).
narrative_ontology:measurement(basi_su_t2015, basic_law_interpretive_boundary__judicial_supremacy_reading, suppression_requirement, 2015, 0.44).
narrative_ontology:measurement(basi_su_t2020, basic_law_interpretive_boundary__judicial_supremacy_reading, suppression_requirement, 2020, 0.48).
narrative_ontology:measurement(basi_su_t2023, basic_law_interpretive_boundary__judicial_supremacy_reading, suppression_requirement, 2023, 0.52).
narrative_ontology:measurement(basi_su_t2024, basic_law_interpretive_boundary__judicial_supremacy_reading, suppression_requirement, 2024, 0.47).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(basic_law_interpretive_boundary__judicial_supremacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(basic_law_interpretive_boundary__judicial_supremacy_reading, basic_law_interpretive_boundary__parliamentary_sovereignty_reading).
narrative_ontology:affects_constraint(basic_law_interpretive_boundary__judicial_supremacy_reading, basic_law_interpretive_boundary__balanced_contestation_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the basic_law_interpretive_boundary kernel. The parliamentary_sovereignty_reading and balanced_contestation_reading are separate constraint stories with their own ε values, beneficiary/victim structures, and classifications — they are NOT alternative measurements of this same constraint but structurally distinct constraints emitted from different readings of the same underlying kernel (the uncodified, ambiguous status of the Basic Laws as either ordinary legislation, a completed constitution, or a contested intermediate). Per the ε-invariance principle, decomposition into three linked files rather than one story with a measurement parameter is required because the three readings assign different power to the Supreme Court, different beneficiary sets, and would produce different ε under a single-story attempt to average across them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(basic_law_interpretive_boundary__judicial_supremacy_reading, organized, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
