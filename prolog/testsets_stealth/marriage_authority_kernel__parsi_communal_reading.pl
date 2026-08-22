% ============================================================================
% CONSTRAINT STORY: marriage_authority_kernel__parsi_communal_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_authority_kernel__parsi_communal_reading, []).

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
 *   constraint_id: marriage_authority_kernel__parsi_communal_reading
 *   human_readable: Parsi Communal Marriage Authority (Parsi Marriage and Divorce Act 1936)
 *   domain: comparative_law/constitutional_pluralism/religious_governance
 *
 * SUMMARY:
 *   In Indian personal-law pluralism, the Parsi communal reading holds that
 *   matrimonial authority for Parsis derives from community custom as
 *   codified in the Parsi Marriage and Divorce Act 1936: marriage validity
 *   requires two Parsi Zoroastrian parties, divorce and matrimonial relief
 *   are tried by delegate juries under the statute, and the Panchayat
 *   institutions administer the communal boundary. The arrangement
 *   coordinates a tiny, shrinking minority's identity continuity and provides
 *   culturally consonant dispute resolution — with statutory matrimonial
 *   rights for women that were progressive for 1936 and extended by
 *   mutual-consent divorce in 1988 — while its endogamy enforcement imposes
 *   severe costs on a minority of members: women who marry non-Parsis, their
 *   children, non-Parsi spouses, and reform-minded members whose exclusion
 *   the same machinery maintains. Epsilon is authored for the standing Parsi
 *   communal arrangement as this reading holds it — the arrangement the story
 *   is about — never for the secular alternative it coexists with. The claim
 *   and the metrics are authored independently: claimed_type is tangled_rope
 *   because the structure demonstrably carries both a genuine coordination
 *   function and asymmetric extraction under active enforcement; the metrics
 *   describe the operation as the historical record shows it, and the engine
 *   computes each seat's type from the structural data.
 *
 * KEY AGENTS:
 *   - parsi_panchayat_institutions: primary agenda-setter and institutional beneficiary (institutional/arbitrage) — administers trusts, delegate panels, and the communal boundary
 *   - indian_legislature: formal enactor (institutional/arbitrage) — codified the custom in 1936, amended in 1988, retains amendment power while deferring administration
 *   - parsi_matrimonial_delegates: enforcement-beneficiary with adjudicative role (moderate/identity_locked) — the jury class that keeps the communal forum running
 *   - parsi_priesthood: ritual beneficiary (moderate/identity_locked) — hereditary priesthood bound to endogamous continuity
 *   - orthodox_community_members: coordination beneficiaries (moderate/identity_locked) — the majority for whom the arrangement is self-governance
 *   - parsi_women_marrying_out: primary targets (moderate/identity_locked) — lose standing and ritual access while insisting on remaining Parsi
 *   - children_of_exogamous_unions: inherited targets (powerless/trapped) — contested membership assigned at birth
 *   - non_parsi_spouses: excluded payers (moderate/mobile) — barred from communal religious life but able to disengage
 *   - reformist_parsis: dual-positioned internal opposition (organized/identity_locked) — bear the boundary's costs while fighting to reform the community from inside
 *   - secular_high_courts: analytical observer (institutional/analytical) — see the statutory shell, not the communal sanction that does the boundary work
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority_kernel__parsi_communal_reading, 0.58).
domain_priors:suppression_score(marriage_authority_kernel__parsi_communal_reading, 0.66).
domain_priors:theater_ratio(marriage_authority_kernel__parsi_communal_reading, 0.36).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__parsi_communal_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(marriage_authority_kernel__parsi_communal_reading, suppression_requirement, 0.66).
narrative_ontology:constraint_metric(marriage_authority_kernel__parsi_communal_reading, theater_ratio, 0.36).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__parsi_communal_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(marriage_authority_kernel__parsi_communal_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority_kernel__parsi_communal_reading, tangled_rope).
narrative_ontology:human_readable(marriage_authority_kernel__parsi_communal_reading, "Parsi Communal Marriage Authority (Parsi Marriage and Divorce Act 1936)").
narrative_ontology:topic_domain(marriage_authority_kernel__parsi_communal_reading, "comparative_law/constitutional_pluralism/religious_governance").

domain_priors:requires_active_enforcement(marriage_authority_kernel__parsi_communal_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority_kernel__parsi_communal_reading, '6fc5f76d-26b3-423c-bca1-f1bde6d31e22').
narrative_ontology:cs_kernel_codification('6fc5f76d-26b3-423c-bca1-f1bde6d31e22', formalized).
narrative_ontology:cs_authority_grounding('6fc5f76d-26b3-423c-bca1-f1bde6d31e22', lineage).
narrative_ontology:cs_interpretation_layer_present('6fc5f76d-26b3-423c-bca1-f1bde6d31e22').
narrative_ontology:cs_reading_relation('6fc5f76d-26b3-423c-bca1-f1bde6d31e22', marriage_authority_kernel__hindu_codified_reading, coexists_with).
narrative_ontology:cs_reading_relation('6fc5f76d-26b3-423c-bca1-f1bde6d31e22', marriage_authority_kernel__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('6fc5f76d-26b3-423c-bca1-f1bde6d31e22', marriage_authority_kernel__christian_canonical_reading, coexists_with).
narrative_ontology:cs_reading_relation('6fc5f76d-26b3-423c-bca1-f1bde6d31e22', marriage_authority_kernel__secular_civil_reading, influences).
narrative_ontology:cs_axiom('6fc5f76d-26b3-423c-bca1-f1bde6d31e22', foundational, parsi_ethnoreligious_endogamy_constitutive).
narrative_ontology:cs_axiom_status(parsi_ethnoreligious_endogamy_constitutive, holdable).
narrative_ontology:cs_axiom_grounding('6fc5f76d-26b3-423c-bca1-f1bde6d31e22', parsi_ethnoreligious_endogamy_constitutive, deontological).
narrative_ontology:cs_axiom('6fc5f76d-26b3-423c-bca1-f1bde6d31e22', secondary, communal_delegate_adjudication_right).
narrative_ontology:cs_axiom_status(communal_delegate_adjudication_right, holdable).
narrative_ontology:cs_axiom_grounding('6fc5f76d-26b3-423c-bca1-f1bde6d31e22', communal_delegate_adjudication_right, conventional).
narrative_ontology:cs_reference_frame('6fc5f76d-26b3-423c-bca1-f1bde6d31e22', parsi_custom_codified_settlement).
narrative_ontology:cs_drift_state('6fc5f76d-26b3-423c-bca1-f1bde6d31e22', post_goolrokh_gupta_demographic_decline, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('6fc5f76d-26b3-423c-bca1-f1bde6d31e22', '').
narrative_ontology:cs_kernel_id(marriage_authority_kernel__parsi_communal_reading, marriage_authority_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__parsi_communal_reading, parsi_panchayat_institutions).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__parsi_communal_reading, parsi_priesthood).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__parsi_communal_reading, orthodox_community_members).
narrative_ontology:constraint_victim(marriage_authority_kernel__parsi_communal_reading, parsi_women_marrying_out).
narrative_ontology:constraint_victim(marriage_authority_kernel__parsi_communal_reading, children_of_exogamous_unions).
narrative_ontology:constraint_victim(marriage_authority_kernel__parsi_communal_reading, non_parsi_spouses).
narrative_ontology:constraint_victim(marriage_authority_kernel__parsi_communal_reading, reformist_parsis).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__parsi_communal_reading, parsi_matrimonial_delegates).
narrative_ontology:constraint_vindicates(marriage_authority_kernel__parsi_communal_reading, parsi_ethnoreligious_continuity_doctrine).
narrative_ontology:constraint_vindicates(marriage_authority_kernel__parsi_communal_reading, communal_personal_law_pluralism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The Bombay Parsi Panchayat and regional anjumans administer community trusts, fire temples, charitable endowments, and the panels from which matrimonial delegates are drawn. They articulate the endogamy requirement as the community's survival condition, control access to communal institutions, and defend the 1936 settlement in litigation and public argument. If the statutory anchor disappeared they could reconstitute as voluntary associations running the same institutions, which is why their exit is arbitrage-grade.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__parsi_communal_reading, parsi_panchayat_institutions, agenda_setter,
    institutional, generational, arbitrage, national).

% Enacted the 1936 codification and amended it in 1988 to add divorce by mutual consent; retains formal power to amend or displace the Act but has deferred administration to communal institutions and treats reopening personal law as constitutionally and politically fraught. Its agenda-setting is episodic — codification and amendment — while day-to-day rule administration sits with the Panchayats.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__parsi_communal_reading, indian_legislature, agenda_setter,
    institutional, generational, arbitrage, national).

% Lay members of the community appointed to the delegate juries that try Parsi matrimonial suits under the Act. The role carries standing and honor within the community and exists only because the communal forum does; they adjudicate divorce and matrimonial relief applying codified custom, and their service keeps the communal adjudication function running.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__parsi_communal_reading, parsi_matrimonial_delegates, beneficiary,
    moderate, biographical, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(marriage_authority_kernel__parsi_communal_reading, parsi_matrimonial_delegates, agenda_setter).

% Hereditary priests whose orthodox ordination requires Parsi parentage and whose ritual calendar — weddings, navjote initiations, jashans — depends on a continuous endogamous community. Their livelihood and the validity of the rites they perform are bound to the communal boundary; exit would mean the end of their ritual role, not a change of employer.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__parsi_communal_reading, parsi_priesthood, beneficiary,
    moderate, generational, identity_locked, regional).

% The majority membership of a shrinking community, for whom endogamous communal life supplies identity, social fabric, and a sense of continuity with ancestors who survived persecution. They experience the communal forum as self-governance and the endogamy rule as the community's survival condition; leaving would mean dissolving the identity itself, which they do not want to do.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__parsi_communal_reading, orthodox_community_members, beneficiary,
    moderate, generational, identity_locked, national).

% Parsi women who marry non-Parsi men and insist on remaining Parsi. They are typically educated and economically independent, but marrying out costs them communal standing: orthodox practice denies them ritual access and treats their membership as forfeit or conditional, and their children's initiation is contested. Civil marriage does not end the community's claim on them, and they litigate (Goolrokh Gupta) to remain inside.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__parsi_communal_reading, parsi_women_marrying_out, payer,
    moderate, biographical, identity_locked, national).

% Children of Parsi and non-Parsi marriages whose communal membership is contested from birth: orthodox practice admits children of Parsi fathers and non-Parsi mothers while excluding children of Parsi mothers and non-Parsi fathers. They did not choose the boundary that governs them, have no standing in the delegate system, and cannot exit a status assigned at birth.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__parsi_communal_reading, children_of_exogamous_unions, payer,
    powerless, biographical, trapped, national).

% Non-Parsi husbands and wives married to Parsis under civil law. They are excluded from communal religious life and institutional access and bear the social costs of the boundary; they can and do disengage from community institutions, since membership was never theirs to keep. Their stake is real but their exit is mobile.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__parsi_communal_reading, non_parsi_spouses, payer,
    moderate, biographical, mobile, national).

% Community members organized to reform the endogamy regime: admitting the children of Parsi women married to non-Parsis, broadening the delegate panels, and modernizing the Act. They litigate, publish, and petition from inside the community, bearing the social cost of internal conflict; leaving the community would defeat their own project, which is why their exit is identity-locked.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__parsi_communal_reading, reformist_parsis, payer,
    organized, generational, identity_locked, national).

% The High Courts and the Supreme Court hear appeals from Parsi matrimonial courts and adjudicate the constitutional boundary of communal personal law, including the Goolrokh Gupta litigation on out-married women's status. They see the statutory shell of the arrangement — a functioning matrimonial forum — while the communal sanction that does the boundary work occurs outside their docket.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__parsi_communal_reading, secular_high_courts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_authority_kernel__parsi_communal_reading, parsi_panchayat_institutions).
narrative_ontology:fixing_cost_class(marriage_authority_kernel__parsi_communal_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a state-recognized matrimonial jurisdiction for a small religious minority: marriage validity, divorce, and matrimonial relief are adjudicated through community delegate juries applying codified Parsi custom, keeping dispute resolution culturally and religiously consonant and preserving communal identity continuity across a shrinking, diaspora-scattered population.
% TRANSFER_FUNCTION: Moves adjudicative authority and communal recognition from the state to community institutions; extracts conformity to endogamy norms — and the communal membership costs that follow from out-marriage — primarily from Parsi women who marry non-Parsis, their children, and reform-minded members, in exchange for access to religious rites, communal institutions, and trust-governed welfare.
% ABSENT_VOICES: Parsi women married to non-Parsi men, children of exogamous unions, and non-Parsi spouses are absent from the delegate juries (delegates must themselves be Parsis) and from the Panchayat panels that shaped the custom; their interests entered the record only through litigation and reformist advocacy, decades after the 1936 codification.
% DISAPPEARANCE_RATIONALE: If the communal matrimonial jurisdiction vanished overnight, Parsi marriages and divorces would route entirely through the secular civil courts; the delegate jury system, the Panchayats' boundary-maintenance role, and the community's identity-transmission machinery would lose their legal anchor; endogamous marriage norms would persist socially but lose their enforcement instrument, and the community's institutional life would reorganize around voluntary association.
% FOUNDING_PROBLEM: Nineteenth- and early twentieth-century Parsis lacked a stable, state-recognized matrimonial law: disputes were resolved through ad hoc custom and colonial courts with no community participation, and Parsi wives had weak statutory matrimonial relief. The 1936 Act codified community custom to secure a participatory matrimonial forum, strengthen women's divorce rights, and preserve communal identity.
% FOUNDING_PROBLEM_CORROBORATION: The dispute-resolution function's persistence is corroborated from outside the benefiting parties by High Court and Supreme Court records adjudicating appeals from Parsi matrimonial courts, and by the 1988 legislative amendment treating the forum as live. The identity-preservation function's status is contested: Parsi reformist organizations and community scholars attest that endogamy enforcement now costs more than it preserves, while orthodox Panchayat trustees attest the opposite; the Goolrokh Gupta litigation record documents the dispute from both sides. No party outside the dispute denies that the participatory forum was necessary at codification.
narrative_ontology:disappearance_verdict(marriage_authority_kernel__parsi_communal_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority_kernel__parsi_communal_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority_kernel__parsi_communal_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(marriage_authority_kernel__parsi_communal_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_authority_kernel__parsi_communal_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority_kernel__parsi_communal_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_authority_kernel__parsi_communal_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_authority_kernel__parsi_communal_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.58) is concentrated rather than diffuse: for the majority of members the arrangement delivers identity continuity and culturally consonant adjudication at low personal cost, but for the out-married minority the extraction is severe — loss of communal standing, denial of ritual access, contested status for children — and the Goolrokh Gupta litigation record shows those costs are actively maintained, not vestigial. Suppression (0.66) is the end state of a rising enforcement trajectory: as civil exit widened (Special Marriage Act 1954, urban mobility), the community's sanction against out-marriage became more explicit and organized rather than less, which is why suppression_requirement is tracked on the shared grid — the story's dynamic is enforcement intensification, not extraction drift alone. Theater (0.36) rises across the interval because demographic decline shrinks the caseload and cohort that give the delegate system and ritual infrastructure their function, leaving a growing share of activity as tradition maintenance. Accessibility collapse (0.42): the civil alternative is genuinely available and used, so alternatives are only partly collapsed, but communal sanction persists after exit, so exit does not fully dissolve the constraint's hold. Resistance (0.48): litigation and organized reformism are real and continuous, but a shrinking, aging loyal membership broadly acquiesces. All three tracked series share one time grid (1936, 1950, 1965, 1988, 2000, 2012, 2025) with every metric authored at every point. The claimed type is stated from structure — genuine coordination plus asymmetric extraction requiring active enforcement — independently of the metric values.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute divergent types from the same structural data. The Panchayat and orthodox-member seats sit at the beneficiary end of directionality with identity-locked commitment: from those positions the arrangement is self-governance and the endogamy boundary is the community's survival condition. The out-married-women and children seats sit near the full-target end with locked or trapped exit: from those positions the same structure operates as enforced exclusion with no adequate forum, and the exclusion is administered by the same institutions that claim to serve them. The reformist seat is deliberately mid-directionality (see directionality_overrides rationale): it bears the boundary's costs and simultaneously presupposes the community it would reform. The secular courts observe only the statutory layer — a functioning matrimonial forum — because the sanction that does the extractive work occurs in communal practice outside their docket, which is why an observer seat reading the statute alone would under-measure the constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (panchayat institutions, priesthood, orthodox members) derive low d: the arrangement subsidizes their identity, ritual authority, and institutional control, and their identity-locked exit anchors them near the beneficiary end rather than allowing arbitrage. Victim declarations (out-married women, exogamous children, non-Parsi spouses, reformists) derive high d; the identity-locked exit of out-married women pushes them toward the full-target end because civil exit does not end the community's claim on them. The single override corrects the organized-power derivation for reformist Parsis: victims-array membership would derive a near-full-target d, but reformists are simultaneously principal beneficiaries of communal existence — their project is to reform the community, not leave it — so d is set to 0.55 to reflect the dual position. Delegates are authored at moderate power (appointed jurors, not an organized bloc) so the organized-atom override does not misfire on them. Gain receipt: the arrangement's gains — preserved boundary control, delegate appointment power, trust governance — demonstrably accrue to the Panchayat seat, so gain_flow names that seat rather than asserting diffuse receipt.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope claim is what prevents mislabeling in both directions. Reading the arrangement as pure extraction would erase the genuine coordination function — a state-recognized, participatory matrimonial forum with unusually strong statutory rights for women in 1936 — that most members affirm and that the 1988 amendment extended. Reading it as pure coordination would erase the gendered extraction the endogamy machinery imposes on precisely the members with the least voice in the delegate system. The R5 interview locates the drift risk precisely: the founding dispute-resolution problem is corroborated as live by the courts' continuing appellate docket and the 1988 amendment, while the identity-preservation function is contested between orthodox trustees and reformists. If demographic decline continues, the coordination function atrophies into performance — tracked by the rising theater_ratio series and demographic_viability_of_coordination_function — and the arrangement drifts piton-ward, with no concentrated beneficiary able to profit from fixing it and the cost of fixing exceeding what the administrator bears. The classification keeps that trajectory visible as data instead of collapsing it into a static label.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'This constraint is one reading (parsi_communal_reading) of the contested marriage_authority_kernel; which structural elements would change under the sibling readings, and where exactly is the disagreement located?',
    'Comparative structural analysis across the five sibling readings'' authority seats, victim sets, and enforcement machinery; the disagreement localizes to who holds adjudicative authority over marriage (communal institutions vs. civil courts vs. religious boards) and whose identity claims constitute a valid marriage (communal boundary vs. individual consent).',
    'Under the secular_civil_reading the victim set shifts from exogamous community members to no communal victims and enforcement becomes ordinary civil process; under muslim_shariat_reading the tribunal structure is analogous but the gender-equity profile differs. Classification is per-reading; no averaging across readings is performed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committer structure: this story instantiates one reading of a contested kernel; sibling readings are separate constraints with separate epsilon values.').

omega_variable(
    endogamy_asymmetry_statute_vs_practice,
    'Is the gender-asymmetric cost of endogamy enforcement (women who marry out lose more standing than men who marry out) a feature of the 1936 codification itself or of orthodox Panchayat practice layered above the statute?',
    'Doctrinal comparison of the Act''s text (which conditions Parsi marriage validity on both parties being Parsis, without gender-differentiated consequences) against documented Panchayat practice on membership, navjote initiation, and ritual access for out-married women versus out-married men.',
    'If the asymmetry is practice rather than statute, the extractive component is attributable to the communal administration seat rather than the codified kernel, shifting which agenda-setter seat bears reform responsibility and lowering effective extraction at the statutory layer.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(endogamy_asymmetry_statute_vs_practice, conceptual, 'Whether the gendered extraction is statutory or practice-layered above the codified kernel.').

omega_variable(
    demographic_viability_of_coordination_function,
    'Does the Parsi demographic decline (community population roughly halved since 1941) make the constraint''s coordination function demographically self-terminating, converting maintenance into performance?',
    'Census and Jiyo Parsi program trajectory analysis: if the marriage-eligible cohort keeps shrinking, the delegate jury system and ritual infrastructure will lack the cases and participants that give them function, regardless of reform outcomes.',
    'If self-terminating, the constraint drifts toward piton — theatrical maintenance of a shrinking jurisdiction with no party able to profit from fixing it — within roughly a generation, independent of the gender-equity reform question.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(demographic_viability_of_coordination_function, empirical, 'Whether demographic decline converts the coordination function into theatrical maintenance.').

omega_variable(
    suppression_structural_vs_social,
    'Is the measured suppression structural (the statutory validity condition tying Parsi marriage to two Parsi parties) or social/internalized (community sanction, denial of rites, internalized communal duty that persists after civil exit)?',
    'Post-exit trajectory analysis: Parsis who marry under the Special Marriage Act retain or lose communal standing — the Goolrokh Gupta record shows civil validity does not restore religious standing, indicating the social component persists independently of the statutory one.',
    'If the dominant component is social/internalized, civil-law exit does not dissolve the constraint''s hold on the target: effective suppression is higher than the structural measure suggests, and the target carries the community''s claim after formal exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_social, empirical, 'Structural versus internalized suppression mechanism in communal endogamy enforcement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority_kernel__parsi_communal_reading, 1936, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t1936, marriage_authority_kernel__parsi_communal_reading, theater_ratio, 1936, 0.1).
narrative_ontology:measurement_basis(marr_tr_t1936, observed).
narrative_ontology:measurement(marr_tr_t1950, marriage_authority_kernel__parsi_communal_reading, theater_ratio, 1950, 0.12).
narrative_ontology:measurement_basis(marr_tr_t1950, observed).
narrative_ontology:measurement(marr_tr_t1965, marriage_authority_kernel__parsi_communal_reading, theater_ratio, 1965, 0.17).
narrative_ontology:measurement_basis(marr_tr_t1965, observed).
narrative_ontology:measurement(marr_tr_t1988, marriage_authority_kernel__parsi_communal_reading, theater_ratio, 1988, 0.22).
narrative_ontology:measurement_basis(marr_tr_t1988, observed).
narrative_ontology:measurement(marr_tr_t2000, marriage_authority_kernel__parsi_communal_reading, theater_ratio, 2000, 0.28).
narrative_ontology:measurement_basis(marr_tr_t2000, observed).
narrative_ontology:measurement(marr_tr_t2012, marriage_authority_kernel__parsi_communal_reading, theater_ratio, 2012, 0.33).
narrative_ontology:measurement_basis(marr_tr_t2012, observed).
narrative_ontology:measurement(marr_tr_t2025, marriage_authority_kernel__parsi_communal_reading, theater_ratio, 2025, 0.36).
narrative_ontology:measurement_basis(marr_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(marr_be_t1936, marriage_authority_kernel__parsi_communal_reading, base_extractiveness, 1936, 0.28).
narrative_ontology:measurement_basis(marr_be_t1936, observed).
narrative_ontology:measurement(marr_be_t1950, marriage_authority_kernel__parsi_communal_reading, base_extractiveness, 1950, 0.33).
narrative_ontology:measurement_basis(marr_be_t1950, observed).
narrative_ontology:measurement(marr_be_t1965, marriage_authority_kernel__parsi_communal_reading, base_extractiveness, 1965, 0.4).
narrative_ontology:measurement_basis(marr_be_t1965, observed).
narrative_ontology:measurement(marr_be_t1988, marriage_authority_kernel__parsi_communal_reading, base_extractiveness, 1988, 0.46).
narrative_ontology:measurement_basis(marr_be_t1988, observed).
narrative_ontology:measurement(marr_be_t2000, marriage_authority_kernel__parsi_communal_reading, base_extractiveness, 2000, 0.52).
narrative_ontology:measurement_basis(marr_be_t2000, observed).
narrative_ontology:measurement(marr_be_t2012, marriage_authority_kernel__parsi_communal_reading, base_extractiveness, 2012, 0.57).
narrative_ontology:measurement_basis(marr_be_t2012, observed).
narrative_ontology:measurement(marr_be_t2025, marriage_authority_kernel__parsi_communal_reading, base_extractiveness, 2025, 0.58).
narrative_ontology:measurement_basis(marr_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t1936, marriage_authority_kernel__parsi_communal_reading, suppression_requirement, 1936, 0.44).
narrative_ontology:measurement_basis(marr_su_t1936, observed).
narrative_ontology:measurement(marr_su_t1950, marriage_authority_kernel__parsi_communal_reading, suppression_requirement, 1950, 0.47).
narrative_ontology:measurement_basis(marr_su_t1950, observed).
narrative_ontology:measurement(marr_su_t1965, marriage_authority_kernel__parsi_communal_reading, suppression_requirement, 1965, 0.53).
narrative_ontology:measurement_basis(marr_su_t1965, observed).
narrative_ontology:measurement(marr_su_t1988, marriage_authority_kernel__parsi_communal_reading, suppression_requirement, 1988, 0.58).
narrative_ontology:measurement_basis(marr_su_t1988, observed).
narrative_ontology:measurement(marr_su_t2000, marriage_authority_kernel__parsi_communal_reading, suppression_requirement, 2000, 0.62).
narrative_ontology:measurement_basis(marr_su_t2000, observed).
narrative_ontology:measurement(marr_su_t2012, marriage_authority_kernel__parsi_communal_reading, suppression_requirement, 2012, 0.66).
narrative_ontology:measurement_basis(marr_su_t2012, observed).
narrative_ontology:measurement(marr_su_t2025, marriage_authority_kernel__parsi_communal_reading, suppression_requirement, 2025, 0.66).
narrative_ontology:measurement_basis(marr_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority_kernel__parsi_communal_reading, identity_coordination).
narrative_ontology:affects_constraint(marriage_authority_kernel__parsi_communal_reading, hindu_codified_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__parsi_communal_reading, muslim_shariat_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__parsi_communal_reading, christian_canonical_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__parsi_communal_reading, secular_civil_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'Parsi marriage law' decomposes, per the epsilon-invariance principle, into one reading of the marriage_authority_kernel: this file instantiates the parsi_communal_reading with its own epsilon (authored for the standing Parsi communal arrangement as this reading holds it), its own victim set (out-married women, exogamous children, non-Parsi spouses, reformists), and its own enforcement machinery (delegate juries under the 1936 Act). The sibling readings are separate constraints with separate epsilon values; the secular_civil_reading additionally serves as the exit route that keeps this reading's accessibility_collapse partial rather than total. Family links run through network.affects_constraints and cs_structure.reading_relations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(marriage_authority_kernel__parsi_communal_reading, organized, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
