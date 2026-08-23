% ============================================================================
% CONSTRAINT STORY: marriage_authority_kernel__muslim_shariat_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_authority_kernel__muslim_shariat_reading, []).

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
 *   constraint_id: marriage_authority_kernel__muslim_shariat_reading
 *   human_readable: Shariat-Based Muslim Personal Law Authority in Family Matters
 *   domain: comparative_law/constitutional_pluralism/religious_governance
 *
 * SUMMARY:
 *   This story authors ONE reading of the marriage_authority_kernel: the
 *   claim that marriage and family law authority for the Muslim community
 *   derives from Shariat as interpreted by Muslim personal law boards (e.g.,
 *   the All India Muslim Personal Law Board) and qazi-run tribunals
 *   (dar-ul-qaza, darul ifta), with the state's role contested and largely
 *   deferential since the 1986 reversal of the Shah Bano maintenance
 *   extension. The epsilon referent is the standing arrangement as actually
 *   operated — communal adjudication plus gender-asymmetric rule content
 *   (unilateral divorce forms beyond the now-banned instant triple talaq,
 *   plural marriage permission, unequal inheritance shares) — not the
 *   reading's endorsed ideal of divinely balanced governance. Sibling
 *   readings (hindu_codified_reading, christian_canonical_reading,
 *   parsi_communal_reading, secular_civil_reading) are separate constraints
 *   with their own epsilon values; the contest among them is routed to omega
 *   variables, not folded into this story's classification. KEY AGENTS (by
 *   structural relationship): - muslim_personal_law_boards: agenda-setter
 *   (institutional / identity_locked) — sets interpretive policy, operates
 *   tribunals, defends the boundary against codification - qazi_adjudicators:
 *   agenda-setter and beneficiary (organized / constrained) — staff the
 *   tribunals, collect fees and standing - male_community_members: primary
 *   beneficiary (organized / constrained) — hold divorce discretion,
 *   plural-marriage permission, larger inheritance shares -
 *   muslim_women_in_community: primary target (moderate / constrained) — bear
 *   the rules' asymmetric costs; objections arrive only from outside the
 *   decision structure - reformist_womens_advocates: excluded challenger
 *   (organized / trapped) — litigate and campaign for gender-just readings
 *   from outside - state_legislature: episodic external intervener
 *   (institutional / analytical) — deference shields the arrangement,
 *   intervention shocks it - constitutional_courts: analytical observer
 *   (institutional / analytical) — police the
 *   religious-freedom/fundamental-rights boundary
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority_kernel__muslim_shariat_reading, 0.58).
domain_priors:suppression_score(marriage_authority_kernel__muslim_shariat_reading, 0.6).
domain_priors:theater_ratio(marriage_authority_kernel__muslim_shariat_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__muslim_shariat_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(marriage_authority_kernel__muslim_shariat_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(marriage_authority_kernel__muslim_shariat_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__muslim_shariat_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(marriage_authority_kernel__muslim_shariat_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority_kernel__muslim_shariat_reading, tangled_rope).
narrative_ontology:human_readable(marriage_authority_kernel__muslim_shariat_reading, "Shariat-Based Muslim Personal Law Authority in Family Matters").
narrative_ontology:topic_domain(marriage_authority_kernel__muslim_shariat_reading, "comparative_law/constitutional_pluralism/religious_governance").

domain_priors:requires_active_enforcement(marriage_authority_kernel__muslim_shariat_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority_kernel__muslim_shariat_reading, '8f1e4416-97db-45e5-b826-023eecd78c62').
narrative_ontology:cs_kernel_codification('8f1e4416-97db-45e5-b826-023eecd78c62', fixed_text).
narrative_ontology:cs_authority_grounding('8f1e4416-97db-45e5-b826-023eecd78c62', lineage).
narrative_ontology:cs_interpretation_layer_present('8f1e4416-97db-45e5-b826-023eecd78c62').
narrative_ontology:cs_reading_relation('8f1e4416-97db-45e5-b826-023eecd78c62', marriage_authority_kernel__hindu_codified_reading, coexists_with).
narrative_ontology:cs_reading_relation('8f1e4416-97db-45e5-b826-023eecd78c62', marriage_authority_kernel__christian_canonical_reading, coexists_with).
narrative_ontology:cs_reading_relation('8f1e4416-97db-45e5-b826-023eecd78c62', marriage_authority_kernel__parsi_communal_reading, coexists_with).
narrative_ontology:cs_reading_relation('8f1e4416-97db-45e5-b826-023eecd78c62', marriage_authority_kernel__secular_civil_reading, influences).
narrative_ontology:cs_axiom('8f1e4416-97db-45e5-b826-023eecd78c62', foundational, divine_revelation_grounds_family_authority).
narrative_ontology:cs_axiom_status(divine_revelation_grounds_family_authority, holdable).
narrative_ontology:cs_axiom_grounding('8f1e4416-97db-45e5-b826-023eecd78c62', divine_revelation_grounds_family_authority, theological).
narrative_ontology:cs_axiom('8f1e4416-97db-45e5-b826-023eecd78c62', foundational, qualified_scholars_exclusive_interpreters).
narrative_ontology:cs_axiom_status(qualified_scholars_exclusive_interpreters, holdable).
narrative_ontology:cs_axiom_grounding('8f1e4416-97db-45e5-b826-023eecd78c62', qualified_scholars_exclusive_interpreters, theological).
narrative_ontology:cs_reference_frame('8f1e4416-97db-45e5-b826-023eecd78c62', shariat_supremacy_in_family_matters).
narrative_ontology:cs_drift_state('8f1e4416-97db-45e5-b826-023eecd78c62', post_shayara_bano_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('8f1e4416-97db-45e5-b826-023eecd78c62', '').
narrative_ontology:cs_kernel_id(marriage_authority_kernel__muslim_shariat_reading, marriage_authority_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__muslim_shariat_reading, muslim_personal_law_boards).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__muslim_shariat_reading, male_community_members).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__muslim_shariat_reading, qazi_adjudicators).
narrative_ontology:constraint_victim(marriage_authority_kernel__muslim_shariat_reading, muslim_women_in_community).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__muslim_shariat_reading, muslim_women_in_community).
narrative_ontology:constraint_vindicates(marriage_authority_kernel__muslim_shariat_reading, shariat_supremacy_doctrine).
narrative_ontology:constraint_vindicates(marriage_authority_kernel__muslim_shariat_reading, minority_personal_law_pluralism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets interpretive policy for the community's family law through pronouncements, guidance to tribunals, and political mobilization; operates dar-ul-qaza dispute resolution; litigates and lobbies to hold the boundary against state codification of family law. Institutional authority, adjudication revenue, and political relevance flow to it from the arrangement's persistence. Its organizational identity is constituted by administering Shariat in family matters — stepping outside the arrangement would mean the body's own dissolution, so exit is not a live option it can take.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__muslim_shariat_reading, muslim_personal_law_boards, agenda_setter,
    institutional, generational, identity_locked, national).

% Staff the community's dispute-resolution tribunals, issuing rulings on marriage validity, divorce, maintenance, and inheritance that bind socially if not formally. Adjudication fees, social standing, and religious authority flow to them from the arrangement. Their vocation exists inside it; outside, their interpretive skills have thinner markets, so leaving is costly though not impossible.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__muslim_shariat_reading, qazi_adjudicators, agenda_setter,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(marriage_authority_kernel__muslim_shariat_reading, qazi_adjudicators, beneficiary).

% Hold the arrangement's rule advantages: divorce available to them by pronouncement (instant triple talaq now criminalized, other unilateral forms intact), permission for plural marriage, and larger fixed inheritance shares. They bear little of the arrangement's costs and would see their family-law position narrow under a codified gender-neutral regime, so most defend its continuity; a minority opt into civil marriage at some social cost.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__muslim_shariat_reading, male_community_members, beneficiary,
    organized, biographical, constrained, national).

% Bear the rules' asymmetric burdens: divorce runs through the tribunals or civil litigation for them while their husbands hold pronouncement capacity; plural marriage exposes them to co-wife competition; inheritance shares are fixed smaller by rule; maintenance claims in community practice are bounded and hard to enforce. They use the tribunals too — for maintenance and marriage legitimacy, often because civil courts are slower and costlier — so the arrangement delivers them a service while charging them the asymmetries. Their objections reach the decision structure only from outside, through constitutional petitions and campaigns; exit to civil marriage exists but carries social sanction and, for many, the felt cost of leaving the community's religious framework.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__muslim_shariat_reading, muslim_women_in_community, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority_kernel__muslim_shariat_reading, muslim_women_in_community, beneficiary).

% Organize gender-just readings from within the tradition: draft model nikahnamas with delegated divorce rights, campaign against unilateral divorce, and petition the courts. The boards' decision structure has no seat for them, so their leverage runs entirely through constitutional litigation and public opinion rather than through the arrangement's own institutions; they are kept outside by the same interpretive-authority structure they contest.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__muslim_shariat_reading, reformist_womens_advocates, excluded,
    organized, generational, trapped, national).

% Holds formal power to codify or reform family law and uses it episodically: it reversed the Shah Bano maintenance extension in 1986 under mobilized opposition, then criminalized instant triple talaq in 2019. Between interventions it defers to community adjudication, and that deference is the political shield under which the arrangement persists. Electoral cycles make its posture oscillate rather than settle.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__muslim_shariat_reading, state_legislature, observer,
    institutional, biographical, analytical, national).

% Adjudicate the boundary between religious freedom and fundamental rights: struck down instant triple talaq in 2017 while declining to dismantle the wider arrangement, and repeatedly note the unenforced uniform-civil-code directive. They administer nothing and collect nothing from the arrangement; their interventions are incremental and arrive case by case.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__muslim_shariat_reading, constitutional_courts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_authority_kernel__muslim_shariat_reading, male_community_members).
narrative_ontology:fixing_cost_class(marriage_authority_kernel__muslim_shariat_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves marriage, divorce, maintenance, and inheritance disputes inside the community through religiously legitimated tribunals and qazi adjudication, supplies a marriage-legitimacy framework the state registry does not, and keeps family-law norm-setting in communal hands rather than state courts.
% TRANSFER_FUNCTION: Moves adjudication authority and normative control over family life to male-led religious institutions; moves adjudication fees and institutional standing to boards and qazis; moves divorce discretion, remarriage freedom, and larger inheritance shares toward men, and the corresponding burdens — bounded maintenance, co-wife exposure, smaller shares — toward women.
% ABSENT_VOICES: Muslim women pressing gender-just readings sit outside the boards' interpretive and adjudicative seats; their objections arrive only through constitutional litigation (Shayara Bano) and public campaigns (model nikahnamas). Secular constitutionalists who would subject family law to uniform civil norms are likewise outside the arrangement's deliberative bodies. Both would contest the arrangement's terms if seated; their exclusion is maintained by the same interpretive-authority structure the arrangement runs on.
% DISAPPEARANCE_RATIONALE: Family dispute resolution for the community would shift to civil courts; marriage legitimacy would reorganize around state registration; the boards would lose their institutional function and political constituency, and the qazi tribunals their docket; gender-asymmetric allocations would give way to codified gender-neutral rules. A major rearrangement of communal governance, not a continuation as before.
% FOUNDING_PROBLEM: Preserving communal religious autonomy in family law: colonial codification had absorbed other communities' family regimes, and post-independence Muslim communal leadership sought to keep Shariat application in family matters as an expression of religious freedom and minority continuity, later defended against assimilation pressure as a minority-identity question.
% FOUNDING_PROBLEM_CORROBORATION: Historical scholarship on colonial personal-law policy and the Constituent Assembly debates on the uniform-civil-code directive corroborate that the autonomy problem was real at founding. From outside the benefiting parties: the Shayara Bano bench, across its opinions, acknowledged the religious-freedom stake even while striking instant talaq, and minority-rights scholarship documents genuine assimilation pressure. The boards attest the problem remains live; women's groups and much of the bench dispute that the founding problem, however real, still justifies the arrangement's current gender-asymmetric form. The problem's historical reality is corroborated; its present justificatory force is what is contested, and no party outside the boards attests that the current form is required by it.
narrative_ontology:disappearance_verdict(marriage_authority_kernel__muslim_shariat_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority_kernel__muslim_shariat_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority_kernel__muslim_shariat_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(marriage_authority_kernel__muslim_shariat_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_authority_kernel__muslim_shariat_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority_kernel__muslim_shariat_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_authority_kernel__muslim_shariat_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_authority_kernel__muslim_shariat_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58 at interval end) is substantial because the arrangement's rule content allocates family-law entitlements asymmetrically — unilateral divorce capacity, plural marriage permission, larger inheritance shares — and the 1986 maintenance rollback pushed it higher (0.68 at T=45) before the triple-talaq ban removed one vector (0.58 at T=75). Suppression (0.60) reflects the enforcement mix holding the arrangement in place: community sanction, the social and identity cost of exit to civil marriage, and the boards' mobilization capacity, which peaked after 1986 and has been partially checked by judicial intervention since 2017. Suppression is authored as the raw structural property — it is not scaled; the engine scales only extractiveness, by directionality and scope. Theater (0.32) is rising: the tribunals' dispute-resolution work remains real, but a growing share of board activity is political defense of the boundary rather than adjudication. Accessibility collapse (0.45): alternatives exist — civil marriage under the Special Marriage Act, constitutional litigation — but each carries social sanction and, for many, the felt cost of stepping outside the community's religious framework, so alternatives are costly rather than collapsed. Resistance (0.55): Shayara Bano, the 2019 Act, reformist nikahnama campaigns, and the Uttarakhand uniform civil code are real resistance that has reshaped but not displaced the arrangement. All three series run on one shared six-point grid (T=0..75); the 1986 Act and the 2017-2019 interventions are the interval's structural shocks. Coordination type identity_coordination is declared because the arrangement's dominant function is boundary maintenance and membership legitimacy — who is validly married, who is inside the community's normative order; the identity framing is not mere cover, the functions are real and used, but the coupling concentrates the asymmetric rule content on women at national scope, which the identity-type offset does not excuse.
 *
 * PERSPECTIVAL GAP:
 *   Inter-institutional: the boards, the courts, and the legislature hold the same arrangement under three different frames — divine-law governance, a rights boundary to police, an electoral and constitutional question — and each frame assigns different legitimacy to the same enforcement acts. Same-level lateral: men and women inside the same community, at the same nominal standing, face opposite structures — his divorce is a pronouncement, hers is a tribunal petition or a civil suit; his remarriage is unrestricted, hers bounded by iddat and social cost; his inheritance share is fixed larger by rule. Exit differentiates them further: exit to civil marriage is socially cheaper for a man than for a woman. Identity-lock: the boards' identity_locked exit means the arrangement cannot be reformed from its administrative center without the center dissolving — reform pressure therefore routes around it, through courts and campaigns, which is why resistance takes litigation form.
 *
 * DIRECTIONALITY LOGIC:
 *   The boards sit nearest the beneficiary end: they collect institutional authority, adjudication revenue, and political relevance, and their identity is constituted by administering the arrangement — the derivation runs toward full beneficiary, amplified by identity_locked exit. Qazis likewise collect fees and standing, with constrained exit keeping them inside. Male community members hold the rule content's advantages (low-to-moderate d). Muslim women are the structural targets (high d): they bear the asymmetric allocations, and constrained exit — litigation cost, community sanction, identity pressure — keeps them from arbitrage. The engine computes per-seat types from this structure: from the boards' seat the arrangement computes as legitimate identity governance it administers; from women's seat the same structure computes as enforced asymmetric burden. The state legislature and constitutional courts are observers — neither collects nor pays — but the legislature's deference is the enforcement shield and the courts' interventions the principal erosion vector, so the arrangement's persistence is jointly held by community sanction and state abstention.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — communal autonomy in family law after colonial codification absorbed other communities' regimes — is historically real (corroborated by Assembly debates and personal-law scholarship) and still contested as a justification. Mandatrophy risk runs in both directions. Reading the arrangement as pure extraction erases the genuine coordination community members receive: dispute resolution that is cheaper, faster, and culturally legible than civil litigation, and marriage legitimacy the state framework does not supply. Reading it as pure coordination erases the asymmetric allocation the same rules enact and the enforcement needed to hold it. The tangled_rope classification preserves both halves: coordination function plus asymmetric extraction plus active enforcement. It is not a piton: beneficiaries are concentrated (boards, qazis, men as a class), enforcement is active, and the administrator could change the rules — the cost asymmetry that defines a piton does not hold, since the boards bear little of the arrangement's costs. The classification prevents mislabeling in both directions and locates the live question where it belongs: whether the interpretive layer can reform the asymmetric rule content internally (omega gender_asymmetry_kernel_vs_interpretation), and whether the political shield holds (omega state_deference_durability).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sibling_reading_structural_delta,
    'This constraint is one reading of the marriage_authority_kernel — how would the structure (victim sets, enforcement, extraction profile) change under the sibling readings (hindu_codified_reading, christian_canonical_reading, parsi_communal_reading, secular_civil_reading)?',
    'Author each sibling reading as its own epsilon-invariant constraint story and compare computed classifications across the family; the deltas locate what each reading''s premises structurally commit it to.',
    'The secular_civil_reading would dissolve communal adjudication (removing the boards'' agenda-setter seat and changing the burdened set to all marrying parties against a state framework); the hindu_codified_reading relocates interpretation to civil courts (raising state integration, lowering communal enforcement). Classification divergence across the family is the measurement the kernel contest exists to take.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_structural_delta, conceptual, 'Committer structure: one of five readings of the marriage-authority kernel; sibling deltas define the contest.').

omega_variable(
    gender_asymmetry_kernel_vs_interpretation,
    'Is the gender-asymmetric rule content (unilateral divorce forms beyond the banned instant triple talaq, plural marriage permission, unequal inheritance shares) intrinsic to this reading''s structure, or contingent on the boards'' particular interpretive choices, which the tradition could revise internally?',
    'Track whether internal reform instruments (delegated-talaq nikahnamas, model marriage contracts, gender-just fiqh arguments) achieve uptake without state coercion; compare burden metrics in communities where they do.',
    'If the asymmetry is interpretively contingent, the burden is a property of the interpretive layer rather than the kernel, and this reading could in principle soften toward a coordination-dominant profile; if intrinsic, it persists under any interpreter and the tangled_rope classification is stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gender_asymmetry_kernel_vs_interpretation, empirical, 'Whether the asymmetric rule content lives in the kernel or in the interpretive layer.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the suppression holding women inside the arrangement structural (tribunal docket monopoly in practice, litigation cost, community sanction) or internalized (religious identity making exit unthinkable)?',
    'Post-exit trajectory of women who move to civil marriage: if rights-claiming and dissent persist after exit, the suppression was structural; if women re-enter community adjudication or abandon claims, internalization carries it.',
    'If substantially internalized, effective suppression exceeds the structural measure — the arrangement travels with its targets after exit — and reform strategies that merely widen the exit door will underperform.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural vs internalized suppression mechanism in a communal religious framework.').

omega_variable(
    state_deference_durability,
    'How durable is the state''s deference that shields the arrangement — does the uniform-civil-code trajectory (Article 44 advocacy, Uttarakhand 2024) erode the political protection its persistence depends on?',
    'Track legislative and judicial intervention frequency and board mobilization success over the coming decade; a sustained intervention trend would show the shield failing.',
    'If deference collapses, enforcement reverts to purely communal sanction — the suppression profile changes and the arrangement drifts toward theatrical persistence or a purely coercive form; if deference holds, the current profile is stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_deference_durability, empirical, 'Durability of the political shield sustaining the arrangement''s enforcement.').

omega_variable(
    tribunal_participation_voluntariness,
    'Do women participate in dar-ul-qaza proceedings as a genuine forum choice (cheaper, faster, culturally legible) or under community pressure that makes the choice nominal?',
    'Survey and ethnographic data on tribunal usage; compare outcomes for women who litigate in civil courts versus accept tribunal rulings on comparable facts.',
    'If participation is substantially coerced, the coordination function''s genuineness is overstated and the burden on women is heavier than the service framing suggests; if genuinely voluntary, part of what the metrics register is the price of a service women actually choose.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(tribunal_participation_voluntariness, empirical, 'Voluntariness of women''s participation in communal adjudication.').

omega_variable(
    kernel_framing_underdetermination,
    'Is the contested kernel best framed as ''what grounds marriage/family law authority'' (making the secular civil reading a sibling reading of the same kernel), or as ''communal autonomy versus constitutional individual rights'' (making the secular reading an external alternative rather than a sibling)?',
    'Test whether parties to the dispute treat the civil code as another way of grounding the same commitment (sibling) or as the rejection of the commitment itself (external alternative); the Constituent Assembly record and uniform-civil-code litigation framing are the evidence.',
    'Under the second framing, the relation to the secular reading shifts from influences toward foreclosure-within-a-single-framework for any party holding communal autonomy as non-negotiable, and the declared reading_relations would need revision.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Framing under-determination: sibling reading versus external alternative.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority_kernel__muslim_shariat_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t0, marriage_authority_kernel__muslim_shariat_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(marr_tr_t0, observed).
narrative_ontology:measurement(marr_tr_t15, marriage_authority_kernel__muslim_shariat_reading, theater_ratio, 15, 0.15).
narrative_ontology:measurement_basis(marr_tr_t15, observed).
narrative_ontology:measurement(marr_tr_t30, marriage_authority_kernel__muslim_shariat_reading, theater_ratio, 30, 0.18).
narrative_ontology:measurement_basis(marr_tr_t30, observed).
narrative_ontology:measurement(marr_tr_t45, marriage_authority_kernel__muslim_shariat_reading, theater_ratio, 45, 0.25).
narrative_ontology:measurement_basis(marr_tr_t45, observed).
narrative_ontology:measurement(marr_tr_t60, marriage_authority_kernel__muslim_shariat_reading, theater_ratio, 60, 0.28).
narrative_ontology:measurement_basis(marr_tr_t60, observed).
narrative_ontology:measurement(marr_tr_t75, marriage_authority_kernel__muslim_shariat_reading, theater_ratio, 75, 0.32).
narrative_ontology:measurement_basis(marr_tr_t75, observed).

% Extraction over time
narrative_ontology:measurement(marr_be_t0, marriage_authority_kernel__muslim_shariat_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement_basis(marr_be_t0, observed).
narrative_ontology:measurement(marr_be_t15, marriage_authority_kernel__muslim_shariat_reading, base_extractiveness, 15, 0.52).
narrative_ontology:measurement_basis(marr_be_t15, observed).
narrative_ontology:measurement(marr_be_t30, marriage_authority_kernel__muslim_shariat_reading, base_extractiveness, 30, 0.58).
narrative_ontology:measurement_basis(marr_be_t30, observed).
narrative_ontology:measurement(marr_be_t45, marriage_authority_kernel__muslim_shariat_reading, base_extractiveness, 45, 0.68).
narrative_ontology:measurement_basis(marr_be_t45, observed).
narrative_ontology:measurement(marr_be_t60, marriage_authority_kernel__muslim_shariat_reading, base_extractiveness, 60, 0.64).
narrative_ontology:measurement_basis(marr_be_t60, observed).
narrative_ontology:measurement(marr_be_t75, marriage_authority_kernel__muslim_shariat_reading, base_extractiveness, 75, 0.58).
narrative_ontology:measurement_basis(marr_be_t75, observed).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t0, marriage_authority_kernel__muslim_shariat_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement_basis(marr_su_t0, observed).
narrative_ontology:measurement(marr_su_t15, marriage_authority_kernel__muslim_shariat_reading, suppression_requirement, 15, 0.48).
narrative_ontology:measurement_basis(marr_su_t15, observed).
narrative_ontology:measurement(marr_su_t30, marriage_authority_kernel__muslim_shariat_reading, suppression_requirement, 30, 0.55).
narrative_ontology:measurement_basis(marr_su_t30, observed).
narrative_ontology:measurement(marr_su_t45, marriage_authority_kernel__muslim_shariat_reading, suppression_requirement, 45, 0.65).
narrative_ontology:measurement_basis(marr_su_t45, observed).
narrative_ontology:measurement(marr_su_t60, marriage_authority_kernel__muslim_shariat_reading, suppression_requirement, 60, 0.62).
narrative_ontology:measurement_basis(marr_su_t60, observed).
narrative_ontology:measurement(marr_su_t75, marriage_authority_kernel__muslim_shariat_reading, suppression_requirement, 75, 0.6).
narrative_ontology:measurement_basis(marr_su_t75, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority_kernel__muslim_shariat_reading, identity_coordination).
narrative_ontology:affects_constraint(marriage_authority_kernel__muslim_shariat_reading, marriage_authority_kernel__hindu_codified_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__muslim_shariat_reading, marriage_authority_kernel__christian_canonical_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__muslim_shariat_reading, marriage_authority_kernel__parsi_communal_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__muslim_shariat_reading, marriage_authority_kernel__secular_civil_reading).

% DUAL FORMULATION NOTE:
% The colloquial concept 'who governs marriage and family law in India' is a single contested kernel (marriage_authority_kernel) that decomposes into five structurally distinct readings — Shariat communal adjudication, codified Hindu law, Christian canonical law, Parsi communal custom, and a secular civil code. Each reading instantiates a different constraint with its own epsilon, beneficiary/victim structure, and enforcement profile; this story authors only the Shariat reading. The three codified communal readings (Hindu, Christian, Parsi) coexist with this one as parallel regimes; the secular reading is downstream of this one in the specific sense that the Shariat reading's political mobilization (the 1986 reversal) is what keeps the civil code optional rather than compulsory. Epsilon differs across the family: this reading carries communal enforcement plus gender-asymmetric rule content; the secular reading carries state enforcement with gender-neutral rules. Classification divergence across the family is the measurement.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
