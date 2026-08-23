% ============================================================================
% CONSTRAINT STORY: family_law_authority__parsi_zoroastrian_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_family_law_authority__parsi_zoroastrian_reading, []).

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
 *   constraint_id: family_law_authority__parsi_zoroastrian_reading
 *   human_readable: Parsi Zoroastrian Endogamous Marriage Regime
 *   domain: comparative_law/political_theory/religious_governance
 *
 * SUMMARY:
 *   The Parsi community of the Indian subcontinent — descendants of
 *   Zoroastrians who fled to Gujarat roughly in the eighth to tenth
 *   centuries, now numbering roughly fifty to sixty thousand in India within
 *   a larger global diaspora — governs marriage through a codified communal
 *   regime. Ritual validity requires the ashirwad ceremony performed by
 *   ordained priesthood under the Parsi Marriage and Divorce Act 1936;
 *   membership is bounded by endogamy; and intermarriage triggers status
 *   consequences that fall asymmetrically on women (the Davar Beamon line of
 *   1908 held children of Parsi women married to non-Parsi men to be outside
 *   the community; the Goolrokh Gupta litigation contested whether an
 *   outmarried woman ceases to be Zoroastrian at all). Fire temple entry,
 *   dokhma burial, and welfare-asset eligibility track the same edge. This
 *   file instantiates ONE reading of the family_law_authority kernel; the
 *   sibling readings are separate constraint files, not positions inside this
 *   one. Epsilon's referent is the standing endogamy-plus-priestly-authority
 *   arrangement assessed from the analytical seat — never the reformers'
 *   preferred open-membership arrangement, which would score near zero by
 *   construction.
 *
 * KEY AGENTS:
 *   - bombay_parsi_panchayat_trustees: agenda-setting seat (institutional / identity_locked) — administers membership rolls, welfare assets, and access policy
 *   - zoroastrian_mobed_priesthood: agenda-setting seat (organized / identity_locked) — hereditary ritual authority over navjote and ashirvad validity
 *   - endogamous_parsi_lineages: primary beneficiary (organized / identity_locked) — identity continuity and children's enrollment secured by the community edge
 *   - intermarried_parsi_women: primary target (moderate / identity_locked) — status revoked on outmarriage; birth identity persists after revocation
 *   - children_of_outmarried_women: primary target (powerless / trapped) — enrollment denied by a marital choice made before they could speak
 *   - non_parsi_spouses: excluded seat (moderate / constrained) — governed by rules their spouses' community writes without their standing
 *   - marriage_eligible_parsi_youth: dual-positioned seat (moderate / constrained) — bears the shrinking-pool cost, receives the identity goods
 *   - indian_judiciary: analytical observer (institutional / analytical) — adjudicates the constitutional collision (Goolrokh Gupta; the Valsad fire temple cases)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(family_law_authority__parsi_zoroastrian_reading, 0.63).
domain_priors:suppression_score(family_law_authority__parsi_zoroastrian_reading, 0.58).
domain_priors:theater_ratio(family_law_authority__parsi_zoroastrian_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(family_law_authority__parsi_zoroastrian_reading, extractiveness, 0.63).
narrative_ontology:constraint_metric(family_law_authority__parsi_zoroastrian_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(family_law_authority__parsi_zoroastrian_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(family_law_authority__parsi_zoroastrian_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(family_law_authority__parsi_zoroastrian_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(family_law_authority__parsi_zoroastrian_reading, tangled_rope).
narrative_ontology:human_readable(family_law_authority__parsi_zoroastrian_reading, "Parsi Zoroastrian Endogamous Marriage Regime").
narrative_ontology:topic_domain(family_law_authority__parsi_zoroastrian_reading, "comparative_law/political_theory/religious_governance").

domain_priors:requires_active_enforcement(family_law_authority__parsi_zoroastrian_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(family_law_authority__parsi_zoroastrian_reading, '32d002ad-2b25-4626-ba9e-33dc2b28c6a1').
narrative_ontology:cs_kernel_codification('32d002ad-2b25-4626-ba9e-33dc2b28c6a1', formalized).
narrative_ontology:cs_authority_grounding('32d002ad-2b25-4626-ba9e-33dc2b28c6a1', lineage).
narrative_ontology:cs_interpretation_layer_present('32d002ad-2b25-4626-ba9e-33dc2b28c6a1').
narrative_ontology:cs_reading_relation('32d002ad-2b25-4626-ba9e-33dc2b28c6a1', family_law_authority__hindu_dharmashastra_reading, coexists_with).
narrative_ontology:cs_reading_relation('32d002ad-2b25-4626-ba9e-33dc2b28c6a1', family_law_authority__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('32d002ad-2b25-4626-ba9e-33dc2b28c6a1', family_law_authority__christian_canonical_reading, coexists_with).
narrative_ontology:cs_reading_relation('32d002ad-2b25-4626-ba9e-33dc2b28c6a1', family_law_authority__secular_contractual_reading, influences).
narrative_ontology:cs_axiom('32d002ad-2b25-4626-ba9e-33dc2b28c6a1', foundational, endogamy_preserves_covenant_identity).
narrative_ontology:cs_axiom_status(endogamy_preserves_covenant_identity, holdable).
narrative_ontology:cs_axiom_grounding('32d002ad-2b25-4626-ba9e-33dc2b28c6a1', endogamy_preserves_covenant_identity, instrumental).
narrative_ontology:cs_axiom('32d002ad-2b25-4626-ba9e-33dc2b28c6a1', foundational, mobed_ordination_constitutes_ritual_validity).
narrative_ontology:cs_axiom_status(mobed_ordination_constitutes_ritual_validity, holdable).
narrative_ontology:cs_axiom_grounding('32d002ad-2b25-4626-ba9e-33dc2b28c6a1', mobed_ordination_constitutes_ritual_validity, conventional).
narrative_ontology:cs_axiom('32d002ad-2b25-4626-ba9e-33dc2b28c6a1', secondary, outmarriage_revokes_community_status).
narrative_ontology:cs_axiom_status(outmarriage_revokes_community_status, holdable).
narrative_ontology:cs_axiom_grounding('32d002ad-2b25-4626-ba9e-33dc2b28c6a1', outmarriage_revokes_community_status, conventional).
narrative_ontology:cs_reference_frame('32d002ad-2b25-4626-ba9e-33dc2b28c6a1', anjuman_endogamous_covenant).
narrative_ontology:cs_drift_state('32d002ad-2b25-4626-ba9e-33dc2b28c6a1', contemporary_diaspora_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('32d002ad-2b25-4626-ba9e-33dc2b28c6a1', '').
narrative_ontology:cs_kernel_id(family_law_authority__parsi_zoroastrian_reading, family_law_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(family_law_authority__parsi_zoroastrian_reading, bombay_parsi_panchayat_trustees).
narrative_ontology:constraint_beneficiary(family_law_authority__parsi_zoroastrian_reading, zoroastrian_mobed_priesthood).
narrative_ontology:constraint_beneficiary(family_law_authority__parsi_zoroastrian_reading, endogamous_parsi_lineages).
narrative_ontology:constraint_victim(family_law_authority__parsi_zoroastrian_reading, intermarried_parsi_women).
narrative_ontology:constraint_victim(family_law_authority__parsi_zoroastrian_reading, children_of_outmarried_women).
narrative_ontology:constraint_victim(family_law_authority__parsi_zoroastrian_reading, non_parsi_spouses).
narrative_ontology:constraint_victim(family_law_authority__parsi_zoroastrian_reading, marriage_eligible_parsi_youth).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(family_law_authority__parsi_zoroastrian_reading, marriage_eligible_parsi_youth).
narrative_ontology:constraint_vindicates(family_law_authority__parsi_zoroastrian_reading, anjuman_covenant_doctrine).
narrative_ontology:constraint_vindicates(family_law_authority__parsi_zoroastrian_reading, purity_boundary_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Elected trustees of the community's principal charitable trust. They maintain the membership rolls, allocate housing and welfare funds, set policy on fire temple and dokhma access, and defend the membership rules in litigation. Their authority exists only inside the community whose edges they keep; abandoning the boundary-keeping role would dissolve the position itself.
narrative_ontology:constraint_stakeholder(family_law_authority__parsi_zoroastrian_reading, bombay_parsi_panchayat_trustees, agenda_setter,
    institutional, generational, identity_locked, national).

% Hereditary priestly families who perform the navjote initiation and the marriage ashirwad ceremony. A marriage is ritually valid only when they perform it, and under orthodox rulings they decline to perform it for intermarried couples. Their vocation, lineage standing, and livelihood are constituted by serving a closed community.
narrative_ontology:constraint_stakeholder(family_law_authority__parsi_zoroastrian_reading, zoroastrian_mobed_priesthood, agenda_setter,
    organized, generational, identity_locked, national).

% Households whose members marry within the community. Their children are enrolled, initiated, and eligible for temple, burial, and welfare access without dispute. The edge of the community secures their identity continuity and their children's membership, and they supply the electoral and financial base that sustains the trustees and the priesthood.
narrative_ontology:constraint_stakeholder(family_law_authority__parsi_zoroastrian_reading, endogamous_parsi_lineages, beneficiary,
    organized, generational, identity_locked, global).

% Women born into the community who marry non-Parsi men. Under orthodox rulings their community status is revoked and their children are not enrolled. Many continue to identify as Zoroastrian and litigate for temple access, as in the Goolrokh Gupta line of cases; the identity they were born into persists after the community withdraws recognition, so revocation does not release them from it.
narrative_ontology:constraint_stakeholder(family_law_authority__parsi_zoroastrian_reading, intermarried_parsi_women, payer,
    moderate, biographical, identity_locked, global).

% Born to a Parsi mother and a non-Parsi father. Under the community's rule they are not enrolled regardless of their own identification or their wish to be initiated. Their status was fixed by a marital choice made before they could speak, and no act of their own admits them.
narrative_ontology:constraint_stakeholder(family_law_authority__parsi_zoroastrian_reading, children_of_outmarried_women, payer,
    powerless, biographical, trapped, global).

% Unmarried adult members facing a shrinking and geographically scattered pool of eligible partners. Marrying within preserves full membership for their children but is increasingly impractical; marrying out triggers status consequences for spouse and children. They also receive the identity continuity and welfare goods the community edge maintains, and most comply voluntarily while bearing the pool constraint as a real cost.
narrative_ontology:constraint_stakeholder(family_law_authority__parsi_zoroastrian_reading, marriage_eligible_parsi_youth, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(family_law_authority__parsi_zoroastrian_reading, marriage_eligible_parsi_youth, beneficiary).

% Married to community members but barred from initiation, from entry to most fire temple trusts, and from dokhma rites. They have no vote in panchayat elections and no seat in any body that writes the rules governing their access. Their presence is the fact the community's edge exists to manage, yet they are governed entirely without standing in it.
narrative_ontology:constraint_stakeholder(family_law_authority__parsi_zoroastrian_reading, non_parsi_spouses, excluded,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(family_law_authority__parsi_zoroastrian_reading, non_parsi_spouses, payer).

% Adjudicates the collision between the community's codified matrimonial authority under the Parsi Marriage and Divorce Act and constitutional equality claims. The Goolrokh Gupta decision and the Valsad fire temple litigation came through this seat, and its rulings determine what the community's institutions may continue to enforce.
narrative_ontology:constraint_stakeholder(family_law_authority__parsi_zoroastrian_reading, indian_judiciary, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(family_law_authority__parsi_zoroastrian_reading, diffuse).
narrative_ontology:fixing_cost_class(family_law_authority__parsi_zoroastrian_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a distinct three-thousand-year-old religious identity for a micro-minority dispersed inside a religiously absorbing environment: endogamy coordinates the reproduction of membership across generations, keeps priestly lineages and ritual practice viable by guaranteeing a closed congregation, and pools communal welfare resources behind a stable edge of belonging.
% TRANSFER_FUNCTION: Moves community status, ritual access (fire temple entry, dokhma burial), and welfare-asset eligibility according to marital choice: marriages inside the edge move ritual fees, demographic continuity, and enrollment rights into the community; a woman's marriage outside moves her own status and her children's membership out of it, revoked rather than transferred, and asymmetrically by sex.
% ABSENT_VOICES: Non-Parsi spouses and children of outmarried women have no standing in the panchayat general body or trustee elections; the rules that govern their access are written entirely by those the edge protects. The community's demographic future — the youth who will inherit a contracted membership decades out — likewise has no seat. Both are outside the community's governance, petitioning through Indian courts instead.
% DISAPPEARANCE_RATIONALE: If the endogamy-plus-status regime vanished overnight, intermarried families would gain ritual access, the membership rolls would open to outmarried women's children, fire temple and dokhma policy would change, and the trusteeship's gatekeeping function over membership would dissolve. The community would persist — its religion, institutions, and welfare assets would not evaporate — but it would reorganize around voluntary affiliation rather than a policed edge of birth and marriage.
% FOUNDING_PROBLEM: Preserving a persecuted refugee micro-minority from absorption: Zoroastrians who fled to India as a tiny community surrounded by numerically dominant religions used marital closure of the community edge to prevent dissolution and keep priestly lineages and ritual transmission viable; colonial-era codification (the Parsi Marriage and Divorce Act, 1865 and 1936) then gave the community's institutions legal authority over matrimonial matters.
% FOUNDING_PROBLEM_CORROBORATION: Outside the benefiting parties: Indian court records (Goolrokh Gupta v. Bombay Parsi Panchayat, Supreme Court 2017; the Valsad fire temple litigation) attest the dispute over the rule's present function; census demography documents the community's decade-on-decade contraction that reframes the founding problem; and academic historians of the Parsi settlement and its 19th-century codification corroborate the founding problem as absorption-prevention while noting its transformation. No corroborating source outside the benefiting parties attests that the original absorption threat persists in its original form.
narrative_ontology:disappearance_verdict(family_law_authority__parsi_zoroastrian_reading, world_rearranges).
narrative_ontology:founding_problem_status(family_law_authority__parsi_zoroastrian_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(family_law_authority__parsi_zoroastrian_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(family_law_authority__parsi_zoroastrian_reading, 'none', 1).
narrative_ontology:epsilon_provenance(family_law_authority__parsi_zoroastrian_reading, 0.63, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(family_law_authority__parsi_zoroastrian_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(family_law_authority__parsi_zoroastrian_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(family_law_authority__parsi_zoroastrian_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.63: costs are concentrated and severe on a minority — outmarried women lose status, their children lose enrollment, non-Parsi spouses lose temple and burial access — while the community-wide cost (a contracting marriage pool) is diffuse. The extraction is revocational rather than transferary: what is taken from payers is membership and access, extinguished rather than handed to another seat. Suppression 0.58: enforcement is institutional (trustee rulings, temple and dokhma trust policies, priestly refusal of rites) compounded by the intense social sanction available only inside a micro-community, but exit exists — civil marriage under the Special Marriage Act, diaspora congregations, lapsed affiliation — at real cost. Theater 0.35: the welfare apparatus (housing colonies, funds, hospitals) is genuinely functional, but a rising share of institutional energy goes to edge-policing whose functional yield is increasingly offset by demographic cost. Accessibility_collapse 0.45: alternatives are known and legally available; they persist rather than collapsing. Resistance 0.5: a live litigation line, reform petitions, and women's organizations, against an orthodox majority that holds the institutions. Claim/metric independence: the orthodox seat presents this arrangement as pure coordination — community preservation — and would claim rope; from the analytical seat I claim tangled_rope because the same structure that coordinates identity continuity also operates gender-asymmetric status revocation, and it requires active enforcement to hold. The three measurement series share one seven-point grid (1936-2024); the suppression_requirement series is authored because this story specifically tracks enforcement-capacity change — trustee and trust enforcement hardened as demographic pressure rose — not merely extraction drift. No cyclical oscillation is modeled: the drift is monotonic.
 *
 * PERSPECTIVAL GAP:
 *   The trustee and priesthood seats should compute as beneficiaries running a structure they experience as the community's survival mechanism; the intermarried-women and children seats should compute as targets bearing status revocation they did not choose and cannot exit by choice, since their birth identity persists after recognition is withdrawn. Within one nominal community, endogamous lineages and outmarried families experience the same rules as opposite structures. The judiciary seat sees a constitutional-equality question where the institutional seats see a survival question. The engine computes this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (trustees, priesthood, endogamous lineages) sit near the beneficiary end: the community edge subsidizes their authority, ritual economy, and identity continuity, and their exit options are identity-locked rather than attractive. Victims sit near the target end: intermarried women and their children bear revocation under identity-locked or trapped exit; non-Parsi spouses are governed without standing. Marriage-eligible youth are the genuinely dual-positioned seat — they bear the pool constraint, a real cost of the endogamy requirement in a community of roughly fifty thousand with a skewed sex ratio and heavy emigration, while receiving the identity and welfare goods the edge maintains. The beneficiary/victim declarations place them in the victim set, which the engine reads toward the target end; I have not overridden that, because their cost-bearing is real even where their compliance is voluntary. The extraction's revocational character means no seat receives what is stripped from payers — hence gain_flow 'diffuse', an affirmative finding made after checking each seat: the priesthood's ritual fees and the lineages' welfare access are benefits of in-community marriage generally, not the transferred extracted good. No directionality overrides were needed: the beneficiary/victim declarations plus exit options produce the correct ordering, and the one genuinely ambiguous seat (youth) is ambiguous within its power atom, which a per-atom override cannot repair without misstating the women and spouses who share that atom.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — keeping a refugee micro-minority from being absorbed into numerically dominant surroundings — was real, and the endogamous covenant plausibly did that work for centuries. The problem has since transformed: the community is integrated and prosperous, the acute threat is now demographic contraction, and the same arrangement plausibly worsens the new problem. I author founding_problem_status 'contested' rather than 'dead' because the parties genuinely dispute whether absorption-risk persists in diaspora conditions; the mismatch check (contested status crossed with a world_rearranges verdict) does not trip the dead-mandate capture flag. The tangled_rope classification is what prevents mislabeling in both directions: reading the arrangement as pure extraction would erase the genuine identity-coordination good — a three-thousand-year-old tradition does persist through this edge, and real welfare pooling happens behind it — while reading it as pure coordination, the orthodox rope claim, would immunize the gender-asymmetric revocation machinery from scrutiny. Keeping both faces visible on one structure is the analytical point of this story.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This story instantiates only the parsi_zoroastrian_reading of the family_law_authority kernel — what structural facts would differ under the sibling readings, and where exactly is the disagreement located?',
    'Comparative authoring of the sibling files (hindu_dharmashastra_reading, muslim_shariat_reading, christian_canonical_reading, secular_contractual_reading) against the same kernel. The disagreement is located in what constitutes marital validity: community edge membership here, dharmic sacramental form, contractual form plus religious injunction, ecclesiastical sacrament, or individual consent under state law.',
    'Under the secular_contractual_reading the status-revocation machinery does not exist at all and epsilon approaches zero; under this reading it is the constraint''s core operation. The epsilon gap between sibling files measures the kernel contest itself — it is not measurement ambiguity within one constraint, and no single story should average across it.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer-frame omega: one reading of a contested kernel; sibling readings are separate constraints with their own epsilon values.').

omega_variable(
    endogamy_demographic_direction,
    'Does the endogamy requirement currently preserve the community or accelerate its contraction?',
    'Cohort demography: intermarriage and outmarriage-driven attrition measured against retention gains attributable to edge strictness; counterfactual modeling of liberalized membership on projected community size.',
    'If endogamy accelerates extinction, the coordination function fails at its own stated end and the arrangement drifts toward inertial edge-keeping — extraction persisting without the preservation good it cites — pulling classification away from tangled_rope toward a degraded profile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(endogamy_demographic_direction, empirical, 'Whether the boundary''s preservation logic still holds demographically.').

omega_variable(
    gender_rule_doctrinal_status,
    'Is the asymmetric status rule — a woman''s status revoked on outmarriage while men''s children remain admissible via navjote — a doctrinal necessity of the Zoroastrian tradition or a customary accretion hardened by colonial-era codification and the 1908 Davar Beamon judgment?',
    'Textual-historical analysis: Sasanian and Pahlavi marriage provisions compared against 19th- and 20th-century community codification records and the reasoning of the case law that fixed the asymmetry.',
    'If accretion, the gendered extraction is separable from the coordination function and reform without schism becomes structurally available; if doctrinal, the extraction is load-bearing and liberalization requires the community to revise the kernel itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gender_rule_doctrinal_status, empirical, 'Origin and detachability of the gender-asymmetric status rule.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the suppression that keeps outmarried women bound to the community structural (institutional denial of temple, dokhma, and fund access) or internalized (an identity that persists after recognition is withdrawn)?',
    'Post-exit trajectory: whether outmarried women continue to seek ritual access, self-identify as Zoroastrian, and litigate for re-entry after status revocation. Persistence of the bond after the mechanism operates indicates internalization.',
    'If substantially internalized, effective suppression exceeds the structural measure: revocation does not release the target but excommunicates an identity that remains, raising the arrangement''s effective coercive content above the authored scalar.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Mechanism split of measured suppression under the status-revocation rule.').

omega_variable(
    orthodox_consensus_survivorship,
    'Is the orthodox majority''s support for the community edge an authentic preference or an artifact of the edge itself — those who would dissent have already exited, removing their voices from the community''s opinion pool?',
    'Compare membership-liberalization preferences across current members, outmarried members, and lapsed descendants; test whether expressed consensus survives inclusion of the exited.',
    'If survivorship artifact, the consensus the trustees cite is endogenous to the arrangement — the coordination function''s legitimacy claim weakens and the operation reads as more extractive than its consent base suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(orthodox_consensus_survivorship, empirical, 'Whether community consensus on endogamy is endogenous to exit-driven selection.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(family_law_authority__parsi_zoroastrian_reading, 1936, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fami_tr_t1936, family_law_authority__parsi_zoroastrian_reading, theater_ratio, 1936, 0.15).
narrative_ontology:measurement_basis(fami_tr_t1936, observed).
narrative_ontology:measurement(fami_tr_t1954, family_law_authority__parsi_zoroastrian_reading, theater_ratio, 1954, 0.17).
narrative_ontology:measurement_basis(fami_tr_t1954, observed).
narrative_ontology:measurement(fami_tr_t1972, family_law_authority__parsi_zoroastrian_reading, theater_ratio, 1972, 0.2).
narrative_ontology:measurement_basis(fami_tr_t1972, observed).
narrative_ontology:measurement(fami_tr_t1990, family_law_authority__parsi_zoroastrian_reading, theater_ratio, 1990, 0.24).
narrative_ontology:measurement_basis(fami_tr_t1990, observed).
narrative_ontology:measurement(fami_tr_t2006, family_law_authority__parsi_zoroastrian_reading, theater_ratio, 2006, 0.28).
narrative_ontology:measurement_basis(fami_tr_t2006, observed).
narrative_ontology:measurement(fami_tr_t2017, family_law_authority__parsi_zoroastrian_reading, theater_ratio, 2017, 0.32).
narrative_ontology:measurement_basis(fami_tr_t2017, observed).
narrative_ontology:measurement(fami_tr_t2024, family_law_authority__parsi_zoroastrian_reading, theater_ratio, 2024, 0.35).
narrative_ontology:measurement_basis(fami_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(fami_be_t1936, family_law_authority__parsi_zoroastrian_reading, base_extractiveness, 1936, 0.5).
narrative_ontology:measurement_basis(fami_be_t1936, observed).
narrative_ontology:measurement(fami_be_t1954, family_law_authority__parsi_zoroastrian_reading, base_extractiveness, 1954, 0.53).
narrative_ontology:measurement_basis(fami_be_t1954, observed).
narrative_ontology:measurement(fami_be_t1972, family_law_authority__parsi_zoroastrian_reading, base_extractiveness, 1972, 0.55).
narrative_ontology:measurement_basis(fami_be_t1972, observed).
narrative_ontology:measurement(fami_be_t1990, family_law_authority__parsi_zoroastrian_reading, base_extractiveness, 1990, 0.58).
narrative_ontology:measurement_basis(fami_be_t1990, observed).
narrative_ontology:measurement(fami_be_t2006, family_law_authority__parsi_zoroastrian_reading, base_extractiveness, 2006, 0.6).
narrative_ontology:measurement_basis(fami_be_t2006, observed).
narrative_ontology:measurement(fami_be_t2017, family_law_authority__parsi_zoroastrian_reading, base_extractiveness, 2017, 0.62).
narrative_ontology:measurement_basis(fami_be_t2017, observed).
narrative_ontology:measurement(fami_be_t2024, family_law_authority__parsi_zoroastrian_reading, base_extractiveness, 2024, 0.63).
narrative_ontology:measurement_basis(fami_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(fami_su_t1936, family_law_authority__parsi_zoroastrian_reading, suppression_requirement, 1936, 0.42).
narrative_ontology:measurement_basis(fami_su_t1936, observed).
narrative_ontology:measurement(fami_su_t1954, family_law_authority__parsi_zoroastrian_reading, suppression_requirement, 1954, 0.44).
narrative_ontology:measurement_basis(fami_su_t1954, observed).
narrative_ontology:measurement(fami_su_t1972, family_law_authority__parsi_zoroastrian_reading, suppression_requirement, 1972, 0.47).
narrative_ontology:measurement_basis(fami_su_t1972, observed).
narrative_ontology:measurement(fami_su_t1990, family_law_authority__parsi_zoroastrian_reading, suppression_requirement, 1990, 0.5).
narrative_ontology:measurement_basis(fami_su_t1990, observed).
narrative_ontology:measurement(fami_su_t2006, family_law_authority__parsi_zoroastrian_reading, suppression_requirement, 2006, 0.53).
narrative_ontology:measurement_basis(fami_su_t2006, observed).
narrative_ontology:measurement(fami_su_t2017, family_law_authority__parsi_zoroastrian_reading, suppression_requirement, 2017, 0.56).
narrative_ontology:measurement_basis(fami_su_t2017, observed).
narrative_ontology:measurement(fami_su_t2024, family_law_authority__parsi_zoroastrian_reading, suppression_requirement, 2024, 0.58).
narrative_ontology:measurement_basis(fami_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(family_law_authority__parsi_zoroastrian_reading, identity_coordination).
narrative_ontology:affects_constraint(family_law_authority__parsi_zoroastrian_reading, hindu_dharmashastra_reading).
narrative_ontology:affects_constraint(family_law_authority__parsi_zoroastrian_reading, muslim_shariat_reading).
narrative_ontology:affects_constraint(family_law_authority__parsi_zoroastrian_reading, christian_canonical_reading).
narrative_ontology:affects_constraint(family_law_authority__parsi_zoroastrian_reading, secular_contractual_reading).

% DUAL FORMULATION NOTE:
% family_law_authority is one contested kernel decomposed into five readings, each a separate constraint file with its own epsilon and its own beneficiary/victim structure: this parsi_zoroastrian_reading (validity = community edge membership; epsilon 0.63, gender-asymmetric status revocation), hindu_dharmashastra_reading (sacramental samskara under dharmic texts), muslim_shariat_reading (contractual nikah under Quranic injunction), christian_canonical_reading (ecclesiastical sacrament), and secular_contractual_reading (individual consent under state law; epsilon near zero — no status-revocation machinery exists there at all). The epsilon spread across the readings is the kernel contest itself measured, not observable-dependent ambiguity within one constraint. This reading structurally influences the secular reading — its codified communal exception shapes Indian accommodation jurisprudence in both directions — and coexists with the other communal readings inside the personal-law system.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
