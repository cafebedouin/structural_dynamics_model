% ============================================================================
% CONSTRAINT STORY: speech_protection_kernel__democratic_participation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_protection_kernel__democratic_participation_reading, []).

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
 *   constraint_id: speech_protection_kernel__democratic_participation_reading
 *   human_readable: Two-Tier Speech Protection Hierarchy — Democratic Participation Reading
 *   domain: constitutional law/political philosophy/communication rights
 *
 * SUMMARY:
 *   The democratic-participation reading of speech protection builds an
 *   internal hierarchy: expression serving collective self-governance
 *   receives the strongest available protection, while expression classified
 *   as non-political sits in a lower tier where restriction is materially
 *   easier. The arrangement has a genuine coordination function — it
 *   entrenches the dissent and criticism that electoral self-correction
 *   requires against majoritarian and wartime suppression — and it
 *   simultaneously carries an asymmetric burden: commercial, cultural, and
 *   boundary-case speakers bear the restriction latitude that the top tier's
 *   shield implies, and the power to draw the political/non-political line
 *   accumulates to the courts that administer it. This file instantiates ONE
 *   reading of the speech_protection_kernel; the sibling readings
 *   (absolutist, harm-threshold, marketplace, dignity) are separate
 *   constraints with different victim and beneficiary sets and therefore
 *   different epsilon values — e.g., the dignity reading adds targeted-group
 *   members to the victim set and strips the top tier's shield from
 *   subordinating speech, while the absolutist reading deletes the lower
 *   tier's restriction latitude entirely. The epsilon here refers to the
 *   standing two-tier arrangement as this reading holds it, assessed by this
 *   reading's own lights.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_kernel__democratic_participation_reading, 0.7).
domain_priors:suppression_score(speech_protection_kernel__democratic_participation_reading, 0.62).
domain_priors:theater_ratio(speech_protection_kernel__democratic_participation_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_kernel__democratic_participation_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(speech_protection_kernel__democratic_participation_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(speech_protection_kernel__democratic_participation_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_protection_kernel__democratic_participation_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(speech_protection_kernel__democratic_participation_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_kernel__democratic_participation_reading, tangled_rope).
narrative_ontology:human_readable(speech_protection_kernel__democratic_participation_reading, "Two-Tier Speech Protection Hierarchy — Democratic Participation Reading").
narrative_ontology:topic_domain(speech_protection_kernel__democratic_participation_reading, "constitutional law/political philosophy/communication rights").

domain_priors:requires_active_enforcement(speech_protection_kernel__democratic_participation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_kernel__democratic_participation_reading, '427b197e-f35e-4f89-a55b-f5ba37701092').
narrative_ontology:cs_kernel_codification('427b197e-f35e-4f89-a55b-f5ba37701092', fixed_text).
narrative_ontology:cs_authority_grounding('427b197e-f35e-4f89-a55b-f5ba37701092', lineage).
narrative_ontology:cs_interpretation_layer_present('427b197e-f35e-4f89-a55b-f5ba37701092').
narrative_ontology:cs_reading_relation('427b197e-f35e-4f89-a55b-f5ba37701092', speech_protection_kernel__absolutist_reading, forecloses).
narrative_ontology:cs_reading_relation('427b197e-f35e-4f89-a55b-f5ba37701092', speech_protection_kernel__harm_threshold_reading, coexists_with).
narrative_ontology:cs_reading_relation('427b197e-f35e-4f89-a55b-f5ba37701092', speech_protection_kernel__marketplace_reading, coexists_with).
narrative_ontology:cs_reading_relation('427b197e-f35e-4f89-a55b-f5ba37701092', speech_protection_kernel__dignity_reading, coexists_with).
narrative_ontology:cs_axiom('427b197e-f35e-4f89-a55b-f5ba37701092', foundational, self_governance_requires_uninhibited_political_speech).
narrative_ontology:cs_axiom_status(self_governance_requires_uninhibited_political_speech, holdable).
narrative_ontology:cs_axiom_grounding('427b197e-f35e-4f89-a55b-f5ba37701092', self_governance_requires_uninhibited_political_speech, instrumental).
narrative_ontology:cs_axiom('427b197e-f35e-4f89-a55b-f5ba37701092', secondary, protection_graded_by_governance_contribution).
narrative_ontology:cs_axiom_status(protection_graded_by_governance_contribution, holdable).
narrative_ontology:cs_axiom_grounding('427b197e-f35e-4f89-a55b-f5ba37701092', protection_graded_by_governance_contribution, instrumental).
narrative_ontology:cs_reference_frame('427b197e-f35e-4f89-a55b-f5ba37701092', meiklejohnian_townhall_deliberation_frame).
narrative_ontology:cs_drift_state('427b197e-f35e-4f89-a55b-f5ba37701092', contemporary_campaign_finance_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('427b197e-f35e-4f89-a55b-f5ba37701092', '').
narrative_ontology:cs_kernel_id(speech_protection_kernel__democratic_participation_reading, speech_protection_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_kernel__democratic_participation_reading, political_dissidents_and_reformers).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__democratic_participation_reading, electoral_candidates_and_campaigns).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__democratic_participation_reading, investigative_press).
narrative_ontology:constraint_victim(speech_protection_kernel__democratic_participation_reading, nonpolitical_commercial_speakers).
narrative_ontology:constraint_victim(speech_protection_kernel__democratic_participation_reading, unorthodox_cultural_expressives).
narrative_ontology:constraint_victim(speech_protection_kernel__democratic_participation_reading, marginalized_protest_communities).
narrative_ontology:constraint_victim(speech_protection_kernel__democratic_participation_reading, noncitizen_resident_speakers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__democratic_participation_reading, constitutional_courts).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__democratic_participation_reading, marginalized_protest_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Draws and polices the line between expression that serves self-governance and expression that does not, case by case, through doctrines of varying scrutiny. Every classification dispute enlarges its gatekeeping role, and its standing as guardian of democratic deliberation rests on administering this hierarchy. Departing from it would require repudiating the body of precedent that constitutes its modern institutional identity.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__democratic_participation_reading, constitutional_courts, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(speech_protection_kernel__democratic_participation_reading, constitutional_courts, beneficiary).

% Enact and enforce restrictions on commercially oriented, indecent, or offensive expression that the hierarchy places outside the protected core, and periodically attempt to reach political expression, where they are rebuffed by the strongest tier of protection. The asymmetry between what they may regulate and what they may not defines their working environment; their latitude is ultimately bounded by judicial review.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__democratic_participation_reading, national_legislatures, agenda_setter,
    institutional, biographical, constrained, national).

% Antiwar activists, civil-rights organizers, and whistleblowers whose criticism of officeholders is the paradigm of what the top tier shields. They receive the strongest available protection against prosecution and suppression; without it they face the prosecution histories of the sedition era. Speaking is their only lever, so leaving the arrangement means forfeiting their purpose.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__democratic_participation_reading, political_dissidents_and_reformers, beneficiary,
    moderate, biographical, trapped, national).

% Collect an expanding share of the top tier's shield as the category of political expression grows to cover campaign spending and organizational messaging. They can substitute wealth for speech through media purchases and committee structures, giving them more room to maneuver than any other protected seat.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__democratic_participation_reading, electoral_candidates_and_campaigns, beneficiary,
    powerful, biographical, arbitrage, national).

% Publishes official misconduct under the top tier's protection, including strong insulation from defamation liability for coverage of public officials. Accountability journalism is jurisdiction-bound: relocating abroad forfeits the subject matter, so the press depends on continued domestic protection and bears exposure whenever coverage is relabeled as commercial or entertainment content.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__democratic_participation_reading, investigative_press, beneficiary,
    organized, generational, constrained, national).

% Advertisers and sellers of goods and services assigned to the lower tier. They accept content restrictions, disclosure mandates, and outright bans that political speakers escape, and their principal recourse is arguing that their expression is 'really' political, which concedes the premise that protection should vary by category.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__democratic_participation_reading, nonpolitical_commercial_speakers, payer,
    organized, biographical, constrained, national).

% Erotic artists, radical performers, and countercultural publishers whose work is least likely to be certified as contributing to self-governance. They bear obscenity and decency regimes administered under the lower tier, and their realistic exits are silence or self-censorship.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__democratic_participation_reading, unorthodox_cultural_expressives, payer,
    powerless, biographical, trapped, national).

% Street protesters and symbolic demonstrators who are nominally the top tier's intended clients but whose expression arrives in forms — civil disobedience, symbol, occupation — that classifiers can push into conduct or disorder exceptions. They gain when classification succeeds and pay when it fails; the gap between the ideal of protected dissent and the administered line lands on them.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__democratic_participation_reading, marginalized_protest_communities, payer,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(speech_protection_kernel__democratic_participation_reading, marginalized_protest_communities, beneficiary).

% Resident noncitizens whose expression the self-governance rationale protects weakest, because the grounding premise ties full protection to membership in the self-governing community. Deportation leverage further narrows what they can safely say. They would contest the membership boundary itself but hold no seat in the doctrine's formation.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__democratic_participation_reading, noncitizen_resident_speakers, excluded,
    powerless, biographical, trapped, national).

% Map the line-drawing record, test whether classifications track stated principle or speaker status, and generate rival readings of the underlying commitment. They hold no enforcement stake and bear none of the arrangement's burdens.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__democratic_participation_reading, first_amendment_scholars, observer,
    analytical, civilizational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(speech_protection_kernel__democratic_participation_reading, constitutional_courts).
narrative_ontology:fixing_cost_class(speech_protection_kernel__democratic_participation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Channels scarce adjudicative protection toward the expression on which electoral self-government depends, assuring citizens that criticism of officeholders and governing institutions cannot be suppressed by the temporary holders of power, and solving the problem of how a polity entrenches the speech its own correction mechanism requires.
% TRANSFER_FUNCTION: Moves immunity from restriction toward speakers classified as contributing to self-governance, and moves restriction latitude toward expression classified as non-political; with each classification dispute it also moves line-drawing authority toward the courts that administer the hierarchy.
% ABSENT_VOICES: Noncitizen residents, children and future members of the polity, and speakers whose expression does not register as 'politics' to elite classifiers would contest the membership-boundary and category-boundary premises, but they were never in the room where the doctrine took shape; the unanimity of the protected core reflects who was seated at its construction.
% DISAPPEARANCE_RATIONALE: If the two-tier hierarchy vanished overnight, the distribution of protection across speech categories would immediately reorganize — either toward uniform near-categorical protection, toward harm- or dignity-conditioned protection, or toward unconstrained legislative discretion — and every speaker's legal position would change with it, while the courts would lose the organizing taxonomy of modern expressive-freedom law.
% FOUNDING_PROBLEM: Early twentieth-century suppression of dissent — wartime sedition prosecutions, criminal syndicalism statutes, and loyalty panics — demonstrated that majorities and executives would silence the criticism democratic self-correction requires; the arrangement was built to entrench that speech constitutionally before the next panic.
% FOUNDING_PROBLEM_CORROBORATION: Legal-historical scholarship documenting pre-doctrine prosecution rates (Chafee's contemporary studies of the sedition era), civil-liberties litigation archives, and comparative evidence from jurisdictions lacking an entrenched political-speech tier, where recurrent suppression of dissent is documented — all sources outside the arrangement's benefiting parties corroborate that the founding problem predates the doctrine and recurs.
narrative_ontology:disappearance_verdict(speech_protection_kernel__democratic_participation_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_protection_kernel__democratic_participation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_protection_kernel__democratic_participation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(speech_protection_kernel__democratic_participation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_protection_kernel__democratic_participation_reading, 0.7, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_protection_kernel__democratic_participation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(speech_protection_kernel__democratic_participation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(speech_protection_kernel__democratic_participation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.70 because the arrangement's costs are real and have accumulated: the lower tier licenses restrictions on commercial and cultural expression, the classification power imposes recurring risk on boundary-case speakers, and the top tier's expansion to concentrated campaign spending has shifted much of its subsidy toward already-powerful actors. Suppression is authored at 0.62 as a raw structural property — the enforcement machinery (obscenity and decency regimes, commercial-speech regulation, campaign-finance administration, conduct exceptions applied to protest) is unscaled by power or scope in the engine's computation, and this note reflects that. Theater ratio 0.40: the protective function is substantially real, but a growing share of doctrinal activity is the performance of principled line-drawing that in operation tracks judicial judgment and speaker status. Accessibility collapse is low (0.35) because rival readings remain fully live in scholarship and comparative law — understanding this arrangement does not close off alternatives. Resistance is moderate-high (0.60): persistent scholarly critique, dissents, and litigation over the line. Claim and metrics are independent authored facts: tangled_rope is claimed because the structure possesses both a genuine coordination function and asymmetric extraction requiring continuous judicial enforcement; the metric values describe observed operation without being tuned to any predicted output. The temporal series run on one shared eight-point grid (1948–2026) so every tracked metric is authored at every examined time point; the rising base_extractiveness trajectory documents extraction accumulation as the political category widened to include money, and the rising suppression_requirement trajectory documents the maturing enforcement infrastructure for policing the lower tier.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from the same structure. From the dissident seat the hierarchy is a shield — the difference between publishing and prosecution. From the commercial-speaker seat it is a license others hold to restrict them. From the unorthodox-expressive seat it is a regime that certifies other people's speech as important and theirs as disposable. From the candidate seat it is an expanding subsidy. From the court's seat it is neutral administration of a principled distinction. The engine computes these per-seat classifications from the structural data; the divergence between the shield experience and the license experience is the perspectival fact this story exists to record.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries sit near the subsidized end: dissidents (trapped exit, wholly dependent on the shield), the press (constrained, jurisdiction-bound), and candidates (arbitrage-capable, the least dependent beneficiaries). Declared victims sit near the target end: commercial speakers (constrained), unorthodox expressives (trapped), and noncitizen residents (trapped, and excluded from the grounding premise itself). Marginalized protest communities are declared victims because the classification risk dominates their net position, even though they are nominal intended clients of the top tier — their dual position is recorded via secondary_role rather than a directionality override, since the override surface keys on power atom and both powerless seats require different treatment. The courts are the structural anomaly: an agenda setter that also collects adjudicative authority from every dispute, placing them near the beneficiary end despite administering rather than receiving the arrangement's burdens directly.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — entrenching self-governance-critical speech against suppression — remains live, so no mandatrophy is declared and no sunset applies. The classification guards against mislabeling in both directions: the genuine coordination function (assuring the speech democracy runs on) blocks a pure-extraction reading, while the asymmetric burden on lower-tier speakers, the accumulating capture of the top tier by concentrated spenders, and the active enforcement requirement block a pure-coordination reading. The R5 mismatch check runs clean: founding_problem_status=live crossed with disappearance_verdict=world_rearranges raises no zombie flag, and the corroborated genealogy (sedition-era prosecution records, comparative suppression data) is sourced outside the benefiting parties.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sibling_reading_redistribution,
    'This constraint is one reading of the speech_protection_kernel (democratic_participation_reading); which reading governs, and how does the choice redistribute protection across speakers?',
    'Doctrinal adoption events — court majorities expressly re-grounding protection, constitutional amendment, or sustained cross-jurisdictional convergence on a single reading.',
    'Under the dignity reading, targeted-group members enter the victim set and the top tier loses its shield for subordinating speech; under the absolutist reading the lower tier''s restriction latitude disappears and measured extraction falls toward coordination cost; under the harm-threshold reading the category axis is replaced by a victim-harm axis with a different beneficiary/victim split entirely.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_redistribution, conceptual, 'Committer structure: which reading of the speech kernel is instantiated changes the constraint''s victim and beneficiary sets.').

omega_variable(
    line_drawing_principled_vs_status_aligned,
    'Is the political/non-political line drawn by principled criteria, or does it systematically track speaker status and alignment with incumbent interests?',
    'Systematic coding of classification decisions against speaker ideology, resources, and proximity to governing coalitions, controlling for doctrinally stated criteria.',
    'If classifications track status, the hierarchy operates as enforcement of discursive dominance and the arrangement shifts toward pure extraction riding a coordination cover story; if principled, the coordination framing holds and the measured extraction is the price of administrability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(line_drawing_principled_vs_status_aligned, empirical, 'Whether the administered line matches the announced principle.').

omega_variable(
    political_category_capture_by_concentrated_wealth,
    'Has the expansion of the political category to cover concentrated campaign spending converted the top tier from a shield for the powerless into a subsidy for the powerful?',
    'Distributional analysis of whose expression gains protection as the category widens — comparing protection outcomes for wealthy organizational speakers versus marginal individual speakers post-campaign-finance decisions.',
    'Confirmation would establish the rising extractiveness trajectory as capture-driven accumulation rather than neutral growth, supporting remedies that separate spending from the protected core of citizen deliberation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(political_category_capture_by_concentrated_wealth, empirical, 'Whether the top tier''s widening shield accrues disproportionately to concentrated economic power.').

omega_variable(
    membership_boundary_exclusion,
    'Does grounding full protection in membership in the self-governing community build a structural exclusion of noncitizens, children, and future members into the arrangement?',
    'Comparative doctrine on the expressive rights of noncitizen residents, plus philosophical analysis of whether the constituency of self-governance can be drawn without excluding affected nonmembers.',
    'If the exclusion is structural, the reading carries a built-in blind spot invisible in the political-tier analysis — a class of affected speakers is weakened by the very premise that protects everyone else — and sibling readings without a membership premise would not carry it.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(membership_boundary_exclusion, conceptual, 'Whether the self-governance grounding premise excludes nonmembers from full protection.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_kernel__democratic_participation_reading, 1948, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spk_dempart_tr_t1948, speech_protection_kernel__democratic_participation_reading, theater_ratio, 1948, 0.2).
narrative_ontology:measurement(spk_dempart_tr_t1964, speech_protection_kernel__democratic_participation_reading, theater_ratio, 1964, 0.24).
narrative_ontology:measurement(spk_dempart_tr_t1976, speech_protection_kernel__democratic_participation_reading, theater_ratio, 1976, 0.28).
narrative_ontology:measurement(spk_dempart_tr_t1990, speech_protection_kernel__democratic_participation_reading, theater_ratio, 1990, 0.31).
narrative_ontology:measurement(spk_dempart_tr_t2003, speech_protection_kernel__democratic_participation_reading, theater_ratio, 2003, 0.34).
narrative_ontology:measurement(spk_dempart_tr_t2010, speech_protection_kernel__democratic_participation_reading, theater_ratio, 2010, 0.36).
narrative_ontology:measurement(spk_dempart_tr_t2018, speech_protection_kernel__democratic_participation_reading, theater_ratio, 2018, 0.38).
narrative_ontology:measurement(spk_dempart_tr_t2026, speech_protection_kernel__democratic_participation_reading, theater_ratio, 2026, 0.4).

% Extraction over time
narrative_ontology:measurement(spk_dempart_be_t1948, speech_protection_kernel__democratic_participation_reading, base_extractiveness, 1948, 0.38).
narrative_ontology:measurement(spk_dempart_be_t1964, speech_protection_kernel__democratic_participation_reading, base_extractiveness, 1964, 0.44).
narrative_ontology:measurement(spk_dempart_be_t1976, speech_protection_kernel__democratic_participation_reading, base_extractiveness, 1976, 0.52).
narrative_ontology:measurement(spk_dempart_be_t1990, speech_protection_kernel__democratic_participation_reading, base_extractiveness, 1990, 0.56).
narrative_ontology:measurement(spk_dempart_be_t2003, speech_protection_kernel__democratic_participation_reading, base_extractiveness, 2003, 0.61).
narrative_ontology:measurement(spk_dempart_be_t2010, speech_protection_kernel__democratic_participation_reading, base_extractiveness, 2010, 0.66).
narrative_ontology:measurement(spk_dempart_be_t2018, speech_protection_kernel__democratic_participation_reading, base_extractiveness, 2018, 0.68).
narrative_ontology:measurement(spk_dempart_be_t2026, speech_protection_kernel__democratic_participation_reading, base_extractiveness, 2026, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(spk_dempart_su_t1948, speech_protection_kernel__democratic_participation_reading, suppression_requirement, 1948, 0.34).
narrative_ontology:measurement(spk_dempart_su_t1964, speech_protection_kernel__democratic_participation_reading, suppression_requirement, 1964, 0.4).
narrative_ontology:measurement(spk_dempart_su_t1976, speech_protection_kernel__democratic_participation_reading, suppression_requirement, 1976, 0.47).
narrative_ontology:measurement(spk_dempart_su_t1990, speech_protection_kernel__democratic_participation_reading, suppression_requirement, 1990, 0.51).
narrative_ontology:measurement(spk_dempart_su_t2003, speech_protection_kernel__democratic_participation_reading, suppression_requirement, 2003, 0.55).
narrative_ontology:measurement(spk_dempart_su_t2010, speech_protection_kernel__democratic_participation_reading, suppression_requirement, 2010, 0.58).
narrative_ontology:measurement(spk_dempart_su_t2018, speech_protection_kernel__democratic_participation_reading, suppression_requirement, 2018, 0.6).
narrative_ontology:measurement(spk_dempart_su_t2026, speech_protection_kernel__democratic_participation_reading, suppression_requirement, 2026, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_protection_kernel__democratic_participation_reading, resource_allocation).
narrative_ontology:affects_constraint(speech_protection_kernel__democratic_participation_reading, speech_protection_kernel__absolutist_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__democratic_participation_reading, speech_protection_kernel__harm_threshold_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__democratic_participation_reading, speech_protection_kernel__marketplace_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__democratic_participation_reading, speech_protection_kernel__dignity_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'speech protection' decomposes, per the epsilon-invariance principle, into five structurally distinct constraints — one per reading of the speech_protection_kernel. Each reading yields a different epsilon because each yields a different beneficiary/victim structure: this democratic-participation reading grades protection by governance contribution (victims: lower-tier and boundary-case speakers); the absolutist reading removes the lower tier (no category-based victims); the harm-threshold reading replaces the category axis with a victim-harm axis; the marketplace reading grounds protection in truth-discovery; the dignity reading adds targeted groups as victims and strips the top tier's shield from subordinating speech. The files are linked pairwise through affects_constraints; the upstream/downstream citation pattern runs primarily from this reading and the marketplace reading (the two rationales absorbed into mainstream doctrine) toward the dignity and harm-threshold readings, which arose partly as responses to this reading's blind spots.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
