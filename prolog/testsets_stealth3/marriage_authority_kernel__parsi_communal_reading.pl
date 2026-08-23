% ============================================================================
% CONSTRAINT STORY: marriage_authority_kernel__parsi_communal_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
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
 *   human_readable: Parsi Communal Marriage and Family Law Settlement (Parsi Marriage and Divorce Act 1936)
 *   domain: comparative_law/constitutional_pluralism/religious_governance
 *
 * SUMMARY:
 *   A micro-minority of roughly fifty to sixty thousand Parsis, concentrated
 *   in Mumbai and Gujarat, governs marriage and family life through communal
 *   custom codified in the Parsi Marriage and Divorce Act 1936: validity
 *   requires the Ashirvad ceremony before a priest and two witnesses;
 *   matrimonial disputes are reserved to courts on which community-elected
 *   Parsi delegates sit as assessors; the Act's deeming rules decide which
 *   children of mixed marriages count as Parsis; and priestly practice —
 *   outside the statute's text — rations fire-temple access and dokhma burial
 *   along the endogamy line. The arrangement solves a real coordination
 *   problem (legal certainty and self-governance for a community too small to
 *   sustain any other legal order of its own) while imposing asymmetric,
 *   actively enforced costs on those whose marriages or conversions cross the
 *   communal boundary. This file is ONE reading of the
 *   marriage_authority_kernel — the parsi_communal_reading; its siblings
 *   (hindu_codified_reading, muslim_shariat_reading,
 *   christian_canonical_reading, secular_civil_reading) are separate
 *   constraints authored separately, per the epsilon-invariance principle:
 *   extractiveness here is authored over the standing Parsi communal
 *   settlement, assessed by this reading's own lights, and does not transfer
 *   to any sibling's arrangement.
 *
 * KEY AGENTS:
 *   - parsi_communal_institutions: agenda-setter (institutional / identity_locked) — administers the trusts, the delegate panel, and the priestly boundary; their authority is constituted by the settlement they maintain
 *   - parsi_matrimonial_delegates: beneficiary (institutional / identity_locked) — community-elected assessors holding an adjudicative seat no other Indian community has
 *   - endogamy_observing_parsis: beneficiary with payer exposure (moderate / identity_locked) — receive identity assurance and communal goods; bear a marriage pool that shrinks every year
 *   - intermarried_parsis: primary target (moderate / constrained) — live outside the settlement's protection while inside its sanctions
 *   - children_of_mixed_marriages: primary target (powerless / constrained) — inherit standing from decisions made before they could speak; hold no seat in the bodies that decide their status
 *   - parsi_religious_dissenters: target (moderate / constrained) — conversion out is a statutory divorce ground; exit is priced in kinship
 *   - non_parsi_spouses: excluded (moderate / trapped) — no standing anywhere in the settlement; their presence in a Parsi's life is what triggers the sanctions
 *   - indian_legislature_and_courts: agenda-setter at amendment moments (institutional / analytical) — codified the custom in 1936, amended in 1949 and 1988, drew the Gupta boundary; priestly gatekeeping sits beyond their reach
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority_kernel__parsi_communal_reading, 0.58).
domain_priors:suppression_score(marriage_authority_kernel__parsi_communal_reading, 0.6).
domain_priors:theater_ratio(marriage_authority_kernel__parsi_communal_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__parsi_communal_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(marriage_authority_kernel__parsi_communal_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(marriage_authority_kernel__parsi_communal_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__parsi_communal_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(marriage_authority_kernel__parsi_communal_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority_kernel__parsi_communal_reading, tangled_rope).
narrative_ontology:human_readable(marriage_authority_kernel__parsi_communal_reading, "Parsi Communal Marriage and Family Law Settlement (Parsi Marriage and Divorce Act 1936)").
narrative_ontology:topic_domain(marriage_authority_kernel__parsi_communal_reading, "comparative_law/constitutional_pluralism/religious_governance").

domain_priors:requires_active_enforcement(marriage_authority_kernel__parsi_communal_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority_kernel__parsi_communal_reading, 'd2ed30bd-8fe0-4bad-994f-29c3164da282').
narrative_ontology:cs_kernel_codification('d2ed30bd-8fe0-4bad-994f-29c3164da282', fixed_text).
narrative_ontology:cs_authority_grounding('d2ed30bd-8fe0-4bad-994f-29c3164da282', lineage).
narrative_ontology:cs_interpretation_layer_present('d2ed30bd-8fe0-4bad-994f-29c3164da282').
narrative_ontology:cs_reading_relation('d2ed30bd-8fe0-4bad-994f-29c3164da282', marriage_authority_kernel__hindu_codified_reading, influences).
narrative_ontology:cs_reading_relation('d2ed30bd-8fe0-4bad-994f-29c3164da282', marriage_authority_kernel__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('d2ed30bd-8fe0-4bad-994f-29c3164da282', marriage_authority_kernel__christian_canonical_reading, coexists_with).
narrative_ontology:cs_reading_relation('d2ed30bd-8fe0-4bad-994f-29c3164da282', marriage_authority_kernel__secular_civil_reading, coexists_with).
narrative_ontology:cs_axiom('d2ed30bd-8fe0-4bad-994f-29c3164da282', foundational, parsi_family_authority_from_codified_custom).
narrative_ontology:cs_axiom_status(parsi_family_authority_from_codified_custom, holdable).
narrative_ontology:cs_axiom_grounding('d2ed30bd-8fe0-4bad-994f-29c3164da282', parsi_family_authority_from_codified_custom, conventional).
narrative_ontology:cs_axiom('d2ed30bd-8fe0-4bad-994f-29c3164da282', foundational, parsi_endogamy_constitutes_communal_existence).
narrative_ontology:cs_axiom_status(parsi_endogamy_constitutes_communal_existence, holdable).
narrative_ontology:cs_axiom_grounding('d2ed30bd-8fe0-4bad-994f-29c3164da282', parsi_endogamy_constitutes_communal_existence, empirically_contingent).
narrative_ontology:cs_reference_frame('d2ed30bd-8fe0-4bad-994f-29c3164da282', codified_communal_custom_1936).
narrative_ontology:cs_drift_state('d2ed30bd-8fe0-4bad-994f-29c3164da282', post_gupta_demographic_decline_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('d2ed30bd-8fe0-4bad-994f-29c3164da282', '').
narrative_ontology:cs_kernel_id(marriage_authority_kernel__parsi_communal_reading, marriage_authority_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__parsi_communal_reading, parsi_communal_institutions).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__parsi_communal_reading, parsi_matrimonial_delegates).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__parsi_communal_reading, endogamy_observing_parsis).
narrative_ontology:constraint_victim(marriage_authority_kernel__parsi_communal_reading, intermarried_parsis).
narrative_ontology:constraint_victim(marriage_authority_kernel__parsi_communal_reading, children_of_mixed_marriages).
narrative_ontology:constraint_victim(marriage_authority_kernel__parsi_communal_reading, parsi_religious_dissenters).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(marriage_authority_kernel__parsi_communal_reading, endogamy_observing_parsis).
narrative_ontology:constraint_vindicates(marriage_authority_kernel__parsi_communal_reading, communal_self_governance_doctrine).
narrative_ontology:constraint_vindicates(marriage_authority_kernel__parsi_communal_reading, personal_law_pluralism_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The Bombay Parsi Punchayat, the community trusts, and the priestly bodies administer the communal settlement the 1936 Act codified: they maintain the panel from which matrimonial delegates are drawn, control access to fire temples and dokhma burial through priestly practice, and operate subsidized housing and welfare whose allocation tracks community standing. Their authority is constituted by the boundary they maintain; as the community shrinks, the goods they ration grow scarcer and the boundary's value to them rises. Leaving this position would mean dissolving the institutions' own function.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__parsi_communal_reading, parsi_communal_institutions, agenda_setter,
    institutional, generational, identity_locked, national).

% Community-elected Parsi delegates sit with the judge in every Parsi matrimonial case, as the Act requires; their opinions are recorded and weighed before the judge decides. The seat confers standing, adjudicative influence, and a role no other Indian community's members hold. Delegates are drawn from, and answerable to, the constituency that elects them; a delegate who pressed to liberalize the intermarriage rules would face the panel at the next election.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__parsi_communal_reading, parsi_matrimonial_delegates, beneficiary,
    institutional, biographical, identity_locked, national).

% Members whose own marriages and lineage conform to the communal norm receive what the settlement protects: recognized ritual status, burial rights, access to communal institutions, and the assurance that their identity transmits to their children. The same settlement narrows their own marriage pool every year as the community declines, and holds their children's choices to the same boundary they accepted.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__parsi_communal_reading, endogamy_observing_parsis, beneficiary,
    moderate, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority_kernel__parsi_communal_reading, endogamy_observing_parsis, payer).

% Parsis who marry non-Parsis live outside the settlement's protection while remaining inside its sanctions. Priestly practice denies their households temple access and, in contested cases, dokhma burial for relatives; the Act's deeming rules decide their children's standing by the parent's sex. Reversing course would require dissolving the marriage; remaining in it carries the exclusion. Litigation — the Goolrokh Gupta case, which reached the Supreme Court — is the main avenue for contesting it.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__parsi_communal_reading, intermarried_parsis, payer,
    moderate, biographical, constrained, national).

% These children inherit their standing from decisions made before they could speak: under the Act's deeming provisions, a child of a Parsi mother and a non-Zoroastrian father is deemed a Parsi, while a child of a Parsi father and a non-Parsi mother is not — and priestly practice in many cases withholds ritual goods from both. They hold no seats in the bodies that decide their status; their remedies are litigation or departure.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__parsi_communal_reading, children_of_mixed_marriages, payer,
    powerless, generational, constrained, national).

% Members who convert away from Zoroastrianism or publicly reject priestly authority meet the settlement's harshest formal hook: conversion out is a statutory ground for divorce, and communal practice strips ritual standing. Their families often remain inside the community, so departure is paid in kinship as much as in membership.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__parsi_communal_reading, parsi_religious_dissenters, payer,
    moderate, biographical, constrained, national).

% The non-Parsi husbands and wives of Parsis have no standing anywhere in the settlement: they cannot be delegates, cannot invoke its courts, and their presence in a Parsi's life is precisely what triggers the sanctions on the household. They would contest the exclusion of their spouses and children if the conversation admitted them.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__parsi_communal_reading, non_parsi_spouses, excluded,
    moderate, biographical, trapped, national).

% Parliament codified the communal custom in 1936 at the community's request, amended the Act in 1949 and 1988, and could amend it again; the courts, up to the Supreme Court in the Gupta litigation, decide the settlement's boundaries against Articles 25 and 29. They hold the procedural keys to every statutory element but have left the priestly gatekeeping outside the statute's text untouched.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__parsi_communal_reading, indian_legislature_and_courts, agenda_setter,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_authority_kernel__parsi_communal_reading, parsi_communal_institutions).
narrative_ontology:fixing_cost_class(marriage_authority_kernel__parsi_communal_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: For a micro-minority dispersed inside a plural legal order, the settlement provides a single codified framework for marriage validity, divorce, and family status: the Ashirvad ceremony fixes validity, delegate benches give the community a voice in its own matrimonial adjudication, and the codification spares a fifty-thousand-person community the cost of litigating its custom from scratch before general courts.
% TRANSFER_FUNCTION: Moves adjudicative authority over Parsi family life from general civil courts to benches on which community-elected delegates sit; moves access to communal goods (temple, dokhma, trust housing and welfare) only to households whose marriages conform to endogamy; moves the cost of communal continuity onto those whose marriages or conversions cross the boundary.
% ABSENT_VOICES: Non-Parsi spouses have no standing anywhere in the settlement; children of mixed marriages are argued for by litigants and reformers but hold no seat in the delegate panel, the panchayat, or the priesthood; reformist Parsis are marginalized in communal elections dominated by orthodox constituencies. The unanimity with which the settlement's terms are affirmed inside its institutions is partly produced by these absences.
% DISAPPEARANCE_RATIONALE: If the settlement vanished overnight, Parsi marriages would fall to the Special Marriage Act or general civil law, the delegate benches would dissolve, and the statutory hooks for the endogamy sanctions would disappear — but priestly gatekeeping would persist as informal religious authority, so the world would rearrange rather than simply normalize: a micro-minority would lose its codified self-governance while its sanction structure went underground.
% FOUNDING_PROBLEM: In the nineteenth and early twentieth centuries, Parsi family disputes were governed by unwritten custom applied by general courts unfamiliar with Zoroastrian ritual validity; the community sought — and obtained in 1865 and again in 1936 — a codification that fixed marriage validity (the Ashirvad ceremony before a priest and two witnesses), reserved matrimonial adjudication to benches including community-elected delegates, and kept the community's family law in communal hands within the colonial and then republican plural order.
% FOUNDING_PROBLEM_CORROBORATION: The Indian state corroborates the founding problem from outside the beneficiary set: it codified the custom at the community's request, amended the Act in 1949 and 1988, and the Supreme Court in the Goolrokh Gupta litigation treated Parsi communal identity and the delegates system as live law. Comparative-family-law scholarship outside the community documents the settlement's function. Most tellingly, the settlement's own targets corroborate both halves at once: intermarried Parsis litigate for readmission to communal goods rather than walking away — attesting that the communal framework delivers real goods while its endogamy terms impose real costs.
narrative_ontology:disappearance_verdict(marriage_authority_kernel__parsi_communal_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority_kernel__parsi_communal_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority_kernel__parsi_communal_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
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
 *   Extraction (0.58) is substantial but concentrated: the settlement's general terms (ceremonial validity, delegate adjudication, liberalized divorce grounds after 1988) serve the community broadly, while the asymmetric costs fall on intermarried households, mixed-descent children, and dissenters — for whom they approach totality (loss of ritual standing, burial rights, and children's status). Suppression (0.60) is authored as a raw structural property — the engine scales only extractiveness, by directionality and scope; this value reflects priestly gatekeeping and statutory deeming rules as they stand, not any amplified figure. Theater (0.38) is rising: adjudication and ceremony remain real, but a growing share of communal activity is boundary performance — purity discourse, fertility schemes conditioned on endogamous marriage — as the demographic base contracts. Accessibility collapse (0.55): alternatives exist (civil marriage under the Special Marriage Act, exit, litigation) but collapse to exit for anyone who wants to remain Parsi, since no alternative delivers the communal goods on non-endogamous terms. Resistance (0.45): the Goolrokh Gupta litigation and reformist challenges to the delegate panel are real but bounded by the community's size and cohesion, and demographic panic frames reform as anti-survival. The measurement series run on one shared eight-point grid (t=0,13,27,40,52,64,77,88): the 1949 and 1988 reform windows show as dips in extraction and suppression, after which both ratchet upward as demographic scarcity raises the boundary's value to its administrators — a ratchet with reform windows, not intermittent reinforcement. Base properties were measured at the interval end. The payer seats' coalition potential runs through litigation and delegate-panel elections rather than numbers; the children's seat is that coalition's strongest future constituency.
 *
 * PERSPECTIVAL GAP:
 *   Seat divergence is structural, not notional. From the institutions' seat the settlement is self-governance: the one legal order a fifty-thousand-person community can actually run, with the delegates' bench as its proudest institution. From the intermarried and dissenter seats the same settlement is enforced exclusion priced in ritual goods and children's status. Between those poles, endogamy-observing members experience both faces at once — identity assurance delivered, and a marriage pool that shrinks every year. Same-level differentiation: observing members and intermarried members hold the same nominal power (moderate) in the same community but fall on opposite sides of one line — whose marriage crossed the boundary — and their exit options differ accordingly (identity-locked versus constrained). Inter-institutionally, the state and the community institutions hold different keys: Parliament codified and amended the text (1936, 1949, 1988) and the Supreme Court drew the Gupta boundary, but the harshest sanctions — temple and dokhma access — sit in priestly practice beyond legislative reach, so the state reforms what it can reach while the boundary hardens where it cannot. The identity-lock binding the beneficiary seats is both professional (the priesthood's function is the boundary) and relational-ideological (membership is constituted through communal continuity); if 'Parsi' became a voluntary affiliation rather than a birthright boundary, the enforcement constituency for endogamy would dissolve and most families would drift toward the secular reading's terms.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to real collection: the institutions collect authority and administer the trust and ritual boundary; the delegates collect the adjudicative seat; observing members collect identity assurance and communal goods. Victim declarations map to real cost-bearing: intermarried households, mixed-descent children, and dissenters bear the sanctions. The engine derives directionality from these declarations plus exit atoms: identity-locked beneficiaries (the institutions cannot exit their own function) sit at the beneficiary end; the powerless, constrained children sit nearest the full-target end; the dual-positioned observing members carry secondary_role payer rather than a directionality override, because their position is genuinely two-sided — the same settlement that assures their identity narrows their children's marriage pool. No directionality overrides are authored: the structural declarations and exit options produce the right d for every seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled-rope claim is what keeps both mislabelings out. Reading the settlement as pure extraction would erase why a shrinking community defends it — the coordination function (family-law certainty, communal adjudication, identity transmission) is real and is the founding problem, which is still live. Reading it as pure coordination would erase the payer seats — the endogamy sanctions are asymmetric, actively enforced, and fall hardest on those with no seat in the bodies that decide their status. The mandatrophy risk is specific and instrumented: the founding problem is live, but the enforcement dimension is inverting as demographics decline — a mechanism built to serve communal life is increasingly maintained because the community fears its disappearance, the signature of a mandate converting into a persistence mechanism. The endogamy_survival_tradeoff omega detects that conversion; if enforcement accelerates the decline it is meant to prevent, the settlement drifts toward piton dynamics on its endogamy dimension while the adjudicative core remains functional.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_status,
    'This constraint is the parsi_communal_reading of marriage_authority_kernel — what structurally changes if a sibling reading (hindu_codified, muslim_shariat, christian_canonical, secular_civil) governs the same families instead?',
    'Cross-reading comparison of victim sets and enforcement structures: the secular reading dissolves the delegates system and the statutory endogamy deeming into individual civil choice; the hindu, muslim, and christian readings substitute their own communal codifications. The disagreement is located in the source-of-authority premise (codified communal custom versus individual right versus scripture) and in who counts as bearing costs (intermarried households versus no one).',
    'Under the secular reading the endogamy sanctions lose their statutory hooks and the burden measured here migrates to informal religious sanction; under this reading retained, the authored values stand. The reading choice, not any measurement, sets the victim set.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_status, conceptual, 'Committer structure: one reading of the marriage-authority kernel among five.').

omega_variable(
    endogamy_survival_tradeoff,
    'Does endogamy enforcement sustain the Parsi community or accelerate its demographic decline — and is the burden measured here the price of coordination or a death-bound maintenance cost?',
    'Demographic modeling against the community''s own record (census series 1941-2011, Jiyo Parsi uptake, intermarriage rates): compare projected community size under endogamy-optional versus endogamy-enforced scenarios.',
    'If enforcement accelerates decline, the coordination function is being consumed by boundary maintenance and the extractiveness assessment should rise with mandatrophy risk; if it sustains identity, part of the measured burden is genuine coordination cost and the tangled-rope reading strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(endogamy_survival_tradeoff, empirical, 'Whether endogamy enforcement serves or undermines communal survival.').

omega_variable(
    statutory_priestly_force_split,
    'Where does the binding force of the settlement actually sit — the 1936 Act''s statutory text, or the priestly gatekeeping over temple access, dokhma burial, and ceremony performance that the statute leaves informal?',
    'Doctrinal mapping of each sanction to its legal source: which exclusions a statutory amendment could reach, and which rest on priestly practice beyond legislative reach.',
    'If most binding force is extra-statutory, legislative reform alone cannot relieve the payer seats and the settlement''s effective hold exceeds its statutory surface; if the statute carries the force, amendment is a real remedy and the cost of fixing drops.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(statutory_priestly_force_split, conceptual, 'Statute versus priestly practice as the seat of binding force.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the suppression measured on community members structural (priestly gatekeeping, trust allocation, statutory deeming rules) or internalized (endogamy absorbed as identity duty, exit experienced as betrayal)?',
    'Post-exit trajectory of intermarried and converted members: whether sanction pressure persists after they leave the community''s reach, and whether second-generation attitudes track the statutory rules or the internalized norm independently of enforcement.',
    'If internalized, effective suppression exceeds the structural measure and would persist through statutory liberalization; if structural, amending the statute and opening trust allocation would release most of the measured suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural versus internalized suppression mechanism in a communal identity settlement.').

omega_variable(
    gender_equity_extraction_paradox,
    'The community''s internal gender settlement is unusually equitable (women''s property and education rights), yet the heaviest costs fall on women who marry out and their children — is that asymmetry a residue the 1949 amendment formally cured while priestly practice retains it?',
    'Track post-Gupta access outcomes: whether daughters of Parsi mothers, Parsi women married out, and their children gain temple and dokhma access in practice after the ruling, or whether priestly refusal persists unchanged.',
    'If practice converges on the statute, the gendered burden is transitional and the extractiveness series drifts down; if practice holds, the statutory gender settlement is a formal shell and the burden is carried entirely by extra-statutory enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gender_equity_extraction_paradox, empirical, 'Whether the gendered concentration of costs is statutory residue or priestly holdover.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority_kernel__parsi_communal_reading, 0, 88).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(parsi_communal_reading_tr_t0, marriage_authority_kernel__parsi_communal_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement_basis(parsi_communal_reading_tr_t0, observed).
narrative_ontology:measurement(parsi_communal_reading_tr_t13, marriage_authority_kernel__parsi_communal_reading, theater_ratio, 13, 0.18).
narrative_ontology:measurement_basis(parsi_communal_reading_tr_t13, observed).
narrative_ontology:measurement(parsi_communal_reading_tr_t27, marriage_authority_kernel__parsi_communal_reading, theater_ratio, 27, 0.22).
narrative_ontology:measurement_basis(parsi_communal_reading_tr_t27, observed).
narrative_ontology:measurement(parsi_communal_reading_tr_t40, marriage_authority_kernel__parsi_communal_reading, theater_ratio, 40, 0.25).
narrative_ontology:measurement_basis(parsi_communal_reading_tr_t40, observed).
narrative_ontology:measurement(parsi_communal_reading_tr_t52, marriage_authority_kernel__parsi_communal_reading, theater_ratio, 52, 0.24).
narrative_ontology:measurement_basis(parsi_communal_reading_tr_t52, observed).
narrative_ontology:measurement(parsi_communal_reading_tr_t64, marriage_authority_kernel__parsi_communal_reading, theater_ratio, 64, 0.3).
narrative_ontology:measurement_basis(parsi_communal_reading_tr_t64, observed).
narrative_ontology:measurement(parsi_communal_reading_tr_t77, marriage_authority_kernel__parsi_communal_reading, theater_ratio, 77, 0.36).
narrative_ontology:measurement_basis(parsi_communal_reading_tr_t77, observed).
narrative_ontology:measurement(parsi_communal_reading_tr_t88, marriage_authority_kernel__parsi_communal_reading, theater_ratio, 88, 0.38).
narrative_ontology:measurement_basis(parsi_communal_reading_tr_t88, observed).

% Extraction over time
narrative_ontology:measurement(parsi_communal_reading_be_t0, marriage_authority_kernel__parsi_communal_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(parsi_communal_reading_be_t0, observed).
narrative_ontology:measurement(parsi_communal_reading_be_t13, marriage_authority_kernel__parsi_communal_reading, base_extractiveness, 13, 0.38).
narrative_ontology:measurement_basis(parsi_communal_reading_be_t13, observed).
narrative_ontology:measurement(parsi_communal_reading_be_t27, marriage_authority_kernel__parsi_communal_reading, base_extractiveness, 27, 0.4).
narrative_ontology:measurement_basis(parsi_communal_reading_be_t27, observed).
narrative_ontology:measurement(parsi_communal_reading_be_t40, marriage_authority_kernel__parsi_communal_reading, base_extractiveness, 40, 0.43).
narrative_ontology:measurement_basis(parsi_communal_reading_be_t40, observed).
narrative_ontology:measurement(parsi_communal_reading_be_t52, marriage_authority_kernel__parsi_communal_reading, base_extractiveness, 52, 0.44).
narrative_ontology:measurement_basis(parsi_communal_reading_be_t52, observed).
narrative_ontology:measurement(parsi_communal_reading_be_t64, marriage_authority_kernel__parsi_communal_reading, base_extractiveness, 64, 0.5).
narrative_ontology:measurement_basis(parsi_communal_reading_be_t64, observed).
narrative_ontology:measurement(parsi_communal_reading_be_t77, marriage_authority_kernel__parsi_communal_reading, base_extractiveness, 77, 0.56).
narrative_ontology:measurement_basis(parsi_communal_reading_be_t77, observed).
narrative_ontology:measurement(parsi_communal_reading_be_t88, marriage_authority_kernel__parsi_communal_reading, base_extractiveness, 88, 0.58).
narrative_ontology:measurement_basis(parsi_communal_reading_be_t88, observed).

% Suppression requirement over time
narrative_ontology:measurement(parsi_communal_reading_su_t0, marriage_authority_kernel__parsi_communal_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement_basis(parsi_communal_reading_su_t0, observed).
narrative_ontology:measurement(parsi_communal_reading_su_t13, marriage_authority_kernel__parsi_communal_reading, suppression_requirement, 13, 0.44).
narrative_ontology:measurement_basis(parsi_communal_reading_su_t13, observed).
narrative_ontology:measurement(parsi_communal_reading_su_t27, marriage_authority_kernel__parsi_communal_reading, suppression_requirement, 27, 0.46).
narrative_ontology:measurement_basis(parsi_communal_reading_su_t27, observed).
narrative_ontology:measurement(parsi_communal_reading_su_t40, marriage_authority_kernel__parsi_communal_reading, suppression_requirement, 40, 0.5).
narrative_ontology:measurement_basis(parsi_communal_reading_su_t40, observed).
narrative_ontology:measurement(parsi_communal_reading_su_t52, marriage_authority_kernel__parsi_communal_reading, suppression_requirement, 52, 0.48).
narrative_ontology:measurement_basis(parsi_communal_reading_su_t52, observed).
narrative_ontology:measurement(parsi_communal_reading_su_t64, marriage_authority_kernel__parsi_communal_reading, suppression_requirement, 64, 0.54).
narrative_ontology:measurement_basis(parsi_communal_reading_su_t64, observed).
narrative_ontology:measurement(parsi_communal_reading_su_t77, marriage_authority_kernel__parsi_communal_reading, suppression_requirement, 77, 0.63).
narrative_ontology:measurement_basis(parsi_communal_reading_su_t77, observed).
narrative_ontology:measurement(parsi_communal_reading_su_t88, marriage_authority_kernel__parsi_communal_reading, suppression_requirement, 88, 0.6).
narrative_ontology:measurement_basis(parsi_communal_reading_su_t88, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority_kernel__parsi_communal_reading, identity_coordination).
narrative_ontology:affects_constraint(marriage_authority_kernel__parsi_communal_reading, marriage_authority_kernel__hindu_codified_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__parsi_communal_reading, marriage_authority_kernel__muslim_shariat_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__parsi_communal_reading, marriage_authority_kernel__christian_canonical_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__parsi_communal_reading, marriage_authority_kernel__secular_civil_reading).

% DUAL FORMULATION NOTE:
% The natural-language label 'Parsi marriage and family law' decomposes, per the epsilon-invariance principle, into one constraint per authority source; this story is the communal-custom reading, and family links run to all four sibling readings. The influences edge toward the hindu_codified reading records two real dependencies: the Parsi codifications (1865, 1936) were the earliest full statutory codifications of communal family law in India and served as drafting references, and the delegates system is the standing counter-example cited in uniform-civil-code debates — it changes the legitimacy conditions under which the Hindu codified arrangement (courts applying codified Hindu law without communal assessors) is evaluated, without foreclosing it. The sibling readings are other files, not parts of this one.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
