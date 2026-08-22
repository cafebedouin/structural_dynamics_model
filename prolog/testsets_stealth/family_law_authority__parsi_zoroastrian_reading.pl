% ============================================================================
% CONSTRAINT STORY: family_law_authority__parsi_zoroastrian_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
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
 *   human_readable: Parsi Zoroastrian Marriage and Community-Boundary Regime
 *   domain: comparative_law/political_theory/religious_governance
 *
 * SUMMARY:
 *   The Parsi Zoroastrian reading governs marriage as the institution that
 *   reproduces the anjuman: the bounded community descended from Zoroastrians
 *   who fled Persia for Gujarat roughly twelve centuries ago. Under the Parsi
 *   Marriage and Divorce Act, 1936, a marriage is valid only when solemnized
 *   by a Zoroastrian priest with the ashirwad ceremony before two witnesses;
 *   orthodox trusteeships extend that ritual gate into a membership boundary,
 *   and per orthodox administration a member who marries outside forfeits
 *   access, asymmetrically for women and their children. The arrangement's
 *   defenders present it as the only mechanism a community of under 60,000
 *   has for surviving absorption; its challengers, intermarried women, their
 *   children, and reformist trusts, read it as a gendered exclusion enforced
 *   by institutions whose authority depends on the line they draw. This story
 *   is ONE reading of the family_law_authority kernel; the sibling readings
 *   are separate constraint files linked through network.affects_constraints.
 *   The epsilon referent is the standing Parsi arrangement under contest,
 *   assessed by this reading's own lights, which prize preservation and can
 *   still see the gendered cost. KEY AGENTS (by structural relationship):
 *   orthodox_community_trustees (institutional/constrained), primary
 *   agenda-setter, administers the boundary and captures its gains;
 *   zoroastrian_priesthood (organized/identity_locked), agenda-setter and
 *   beneficiary, holds the ritual monopoly; intermarried_parsis_women
 *   (moderate/identity_locked), primary target;
 *   children_of_intermarried_women (moderate/constrained), secondary target
 *   with a partial rival exit; endogamous_community_members
 *   (moderate/identity_locked), dual beneficiary-payer seat;
 *   non_parsi_spouses (moderate/mobile), excluded outsider;
 *   reformist_mobeds_and_trusts (organized/constrained), excluded rival
 *   validity track; diaspora_zoroastrian_congregations (organized/mobile),
 *   beneficiary seat outside enforcement reach; indian_judiciary
 *   (institutional/analytical), observer that alters enforcement without
 *   settling the boundary.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(family_law_authority__parsi_zoroastrian_reading, 0.58).
domain_priors:suppression_score(family_law_authority__parsi_zoroastrian_reading, 0.55).
domain_priors:theater_ratio(family_law_authority__parsi_zoroastrian_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(family_law_authority__parsi_zoroastrian_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(family_law_authority__parsi_zoroastrian_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(family_law_authority__parsi_zoroastrian_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(family_law_authority__parsi_zoroastrian_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(family_law_authority__parsi_zoroastrian_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(family_law_authority__parsi_zoroastrian_reading, tangled_rope).
narrative_ontology:human_readable(family_law_authority__parsi_zoroastrian_reading, "Parsi Zoroastrian Marriage and Community-Boundary Regime").
narrative_ontology:topic_domain(family_law_authority__parsi_zoroastrian_reading, "comparative_law/political_theory/religious_governance").

domain_priors:requires_active_enforcement(family_law_authority__parsi_zoroastrian_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(family_law_authority__parsi_zoroastrian_reading, '79d78fca-a103-4846-bee7-698b29d77c13').
narrative_ontology:cs_kernel_codification('79d78fca-a103-4846-bee7-698b29d77c13', formalized).
narrative_ontology:cs_authority_grounding('79d78fca-a103-4846-bee7-698b29d77c13', lineage).
narrative_ontology:cs_interpretation_layer_present('79d78fca-a103-4846-bee7-698b29d77c13').
narrative_ontology:cs_reading_relation('79d78fca-a103-4846-bee7-698b29d77c13', family_law_authority__hindu_dharmashastra_reading, coexists_with).
narrative_ontology:cs_reading_relation('79d78fca-a103-4846-bee7-698b29d77c13', family_law_authority__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('79d78fca-a103-4846-bee7-698b29d77c13', family_law_authority__christian_canonical_reading, coexists_with).
narrative_ontology:cs_reading_relation('79d78fca-a103-4846-bee7-698b29d77c13', family_law_authority__secular_contractual_reading, influences).
narrative_ontology:cs_axiom('79d78fca-a103-4846-bee7-698b29d77c13', foundational, priestly_ritual_constitutes_marriage_validity).
narrative_ontology:cs_axiom_status(priestly_ritual_constitutes_marriage_validity, holdable).
narrative_ontology:cs_axiom_grounding('79d78fca-a103-4846-bee7-698b29d77c13', priestly_ritual_constitutes_marriage_validity, theological).
narrative_ontology:cs_axiom('79d78fca-a103-4846-bee7-698b29d77c13', foundational, endogamy_preserves_anjuman_continuity).
narrative_ontology:cs_axiom_status(endogamy_preserves_anjuman_continuity, holdable).
narrative_ontology:cs_axiom_grounding('79d78fca-a103-4846-bee7-698b29d77c13', endogamy_preserves_anjuman_continuity, instrumental).
narrative_ontology:cs_axiom('79d78fca-a103-4846-bee7-698b29d77c13', secondary, intermarriage_forfeits_community_status).
narrative_ontology:cs_axiom_status(intermarriage_forfeits_community_status, holdable).
narrative_ontology:cs_axiom_grounding('79d78fca-a103-4846-bee7-698b29d77c13', intermarriage_forfeits_community_status, conventional).
narrative_ontology:cs_reference_frame('79d78fca-a103-4846-bee7-698b29d77c13', endogamous_anjuman_ritual_order).
narrative_ontology:cs_drift_state('79d78fca-a103-4846-bee7-698b29d77c13', post_goolrokh_demographic_crisis, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('79d78fca-a103-4846-bee7-698b29d77c13', '2026-08-10T00:00:00Z').
narrative_ontology:cs_kernel_id(family_law_authority__parsi_zoroastrian_reading, family_law_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(family_law_authority__parsi_zoroastrian_reading, zoroastrian_priesthood).
narrative_ontology:constraint_beneficiary(family_law_authority__parsi_zoroastrian_reading, orthodox_community_trustees).
narrative_ontology:constraint_beneficiary(family_law_authority__parsi_zoroastrian_reading, endogamous_community_members).
narrative_ontology:constraint_victim(family_law_authority__parsi_zoroastrian_reading, intermarried_parsis_women).
narrative_ontology:constraint_victim(family_law_authority__parsi_zoroastrian_reading, children_of_intermarried_women).
narrative_ontology:constraint_victim(family_law_authority__parsi_zoroastrian_reading, non_parsi_spouses).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(family_law_authority__parsi_zoroastrian_reading, diaspora_zoroastrian_congregations).
narrative_ontology:constraint_victim(family_law_authority__parsi_zoroastrian_reading, endogamous_community_members).
narrative_ontology:constraint_vindicates(family_law_authority__parsi_zoroastrian_reading, anjuman_preservation_doctrine).
narrative_ontology:constraint_vindicates(family_law_authority__parsi_zoroastrian_reading, priestly_validity_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Elected and appointed trustees of the community's punchayats and trusts (foremost the Bombay Parsi Punchayat) who administer fire temples, funeral infrastructure, housing colonies, and charitable funds. They decide who counts as a member for access to these institutions, defend the line in court and in the press, and fund litigation to maintain it. Their authority over substantial endowments is constituted by the membership line they police; relaxing it invites general-body revolt, and abandoning trusteeship costs them their standing in the only community they belong to.
narrative_ontology:constraint_stakeholder(family_law_authority__parsi_zoroastrian_reading, orthodox_community_trustees, agenda_setter,
    institutional, generational, constrained, national).

% The hereditary priestly class (mobeds) who alone may solemnize a valid marriage through the ashirwad ceremony and initiate children through navjote. Their livelihood and standing flow from performing rituals only they can perform, which makes their participation constitutive of every marriage the community recognizes. Leaving the priestly role means abandoning a hereditary vocation and the family line that carries it.
narrative_ontology:constraint_stakeholder(family_law_authority__parsi_zoroastrian_reading, zoroastrian_priesthood, agenda_setter,
    organized, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(family_law_authority__parsi_zoroastrian_reading, zoroastrian_priesthood, beneficiary).

% Community members who marry within the line. They receive full ritual access, recognized membership for themselves and their children, and continuity of a roughly thirteen-century-old identity. They also carry the cost side: a shrinking pool of eligible partners, late or forgone marriage, and the social pressure that keeps families inside the line. Their self-understanding is built on the community's continuity; treating membership as optional is not a live option from where they stand.
narrative_ontology:constraint_stakeholder(family_law_authority__parsi_zoroastrian_reading, endogamous_community_members, beneficiary,
    moderate, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(family_law_authority__parsi_zoroastrian_reading, endogamous_community_members, payer).

% Parsi women who married outside the community. They remain Zoroastrian in belief and practice but are treated by orthodox trustees as having left: refused entry to fire temples, refused funeral rites in community institutions (the Goolrokh Gupta litigation), with their children denied initiation. They cannot become non-Parsi, because they still are Parsi, and they cannot be recognized as Parsi by the institutions that govern access. Civil marriage law lets them marry; nothing lets them stay.
narrative_ontology:constraint_stakeholder(family_law_authority__parsi_zoroastrian_reading, intermarried_parsis_women, payer,
    moderate, biographical, identity_locked, national).

% Children of Parsi mothers and non-Parsi fathers. Raised in the community's faith and often its language and customs, they are denied navjote initiation and membership by orthodox trustees, while children of Parsi fathers and non-Parsi mothers have historically been admitted. Some reformist trusts will initiate them, at the cost of orthodox recognition. Most face a choice between a community that will not have them and a reformist affiliation the community does not honor.
narrative_ontology:constraint_stakeholder(family_law_authority__parsi_zoroastrian_reading, children_of_intermarried_women, payer,
    moderate, biographical, constrained, national).

% The husbands and wives outside the line. They are barred from membership, ritual access, and community institutions outright, regardless of their own wishes or practice. Unlike the intermarried Parsis, they lose nothing they grew up with, because the community was never theirs; they remain full participants in the wider society. Their stake is in their spouse's and children's exclusion, not in their own standing.
narrative_ontology:constraint_stakeholder(family_law_authority__parsi_zoroastrian_reading, non_parsi_spouses, excluded,
    moderate, biographical, mobile, national).

% Priests and trusts outside the orthodox apparatus who solemnize intermarriages, initiate children of Parsi mothers, and hold that Zoroastrian law accepts converts and does not penalize women's marriages. Orthodox trustees refuse to recognize their rituals, so their acts of validity do not travel: a navjote they perform does not open a fire temple door. They operate a rival validity track that the orthodox machinery exists, in part, to refuse.
narrative_ontology:constraint_stakeholder(family_law_authority__parsi_zoroastrian_reading, reformist_mobeds_and_trusts, excluded,
    organized, generational, constrained, national).

% North American, European, and other diaspora congregations that inherit the community's identity but sit outside the Indian trusteeships' enforcement reach. Many admit children of Parsi mothers and accept intermarried families, creating a parallel, more permeable membership practice. Members move between the two regimes, and the orthodox apparatus cannot extend its refusals to their fire temples.
narrative_ontology:constraint_stakeholder(family_law_authority__parsi_zoroastrian_reading, diaspora_zoroastrian_congregations, beneficiary,
    organized, generational, mobile, global).

% The courts that adjudicate challenges to trusteeship exclusions and interpret the Parsi Marriage and Divorce Act, 1936. They take testimony from all the other seats, weigh religious freedom against the Constitution's equality guarantees, and their rulings can alter what the enforcement machinery may do, though they have so far declined to settle the membership question on the merits.
narrative_ontology:constraint_stakeholder(family_law_authority__parsi_zoroastrian_reading, indian_judiciary, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(family_law_authority__parsi_zoroastrian_reading, orthodox_community_trustees).
narrative_ontology:fixing_cost_class(family_law_authority__parsi_zoroastrian_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Standardizes marriage ritual validity across a dispersed micro-minority and maintains the membership boundary that defines who may access community institutions (fire temples, funeral infrastructure, charitable funds) and who may transmit the identity to children. For a community of under 60,000, the boundary solves a real collective problem: without it, membership, ritual validity, and institutional access are undefined.
% TRANSFER_FUNCTION: Moves membership status, ritual access, and identity recognition away from intermarried members and their descendants toward the orthodox institutional core; moves ritual fees and deference toward the priesthood; and preserves the community's collective continuity at the cost of the excluded.
% ABSENT_VOICES: Intermarried Parsi women and their children had no seat in the trusteeships and councils that set the membership rules; non-Parsi spouses have no standing at all; reformist priests are unrecognized by the orthodox apparatus. Their objections reach the arrangement only through litigation, the press, and rival congregations, never through the bodies that administer it. The unanimity of orthodox positions partly reflects that these seats were never in the room.
% DISAPPEARANCE_RATIONALE: If the endogamy-and-priestly-validity arrangement vanished overnight, intermarried families would reintegrate, the reformist and orthodox validity tracks would collapse into one, trusteeship authority over membership would dissolve into ordinary association rules, and the community's demographic trajectory would likely change as the excluded married back in. The community would persist as a religious denomination; the boundary-defined anjuman would not survive in its current form.
% FOUNDING_PROBLEM: Preserving a tiny refugee minority's religious identity and ritual continuity across twelve centuries of dispersion in India: preventing absorption into the surrounding majorities while keeping fire temples, funeral rites, and priestly transmission viable for a community too small to survive diffuse membership.
% FOUNDING_PROBLEM_CORROBORATION: Indian census series and independent demographers, all outside the beneficiary set, attest the founding problem is live: the community fell from roughly 114,000 in 1941 to under 60,000, with a median age far above the national average. Parsi reformist litigants and secular family-law scholars corroborate the problem's liveness while disputing that endogamous enforcement is the remedy. Corroboration of the problem is external; the contest is over the means.
narrative_ontology:disappearance_verdict(family_law_authority__parsi_zoroastrian_reading, world_rearranges).
narrative_ontology:founding_problem_status(family_law_authority__parsi_zoroastrian_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(family_law_authority__parsi_zoroastrian_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(family_law_authority__parsi_zoroastrian_reading, 'none', 1).
narrative_ontology:epsilon_provenance(family_law_authority__parsi_zoroastrian_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

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
 *   Extraction (0.58) is substantial but bounded: the arrangement takes membership status, ritual access, and identity recognition from identifiable people, intermarried women and their children above all, while most members comply voluntarily and receive real goods in return. Suppression (0.55) is authored as a raw structural property and is not scaled here; only extractiveness is scaled by the engine, by directionality and scope. Exit routes exist, civil marriage under the Special Marriage Act, reformist initiation, diaspora congregations, but each carries heavy social and religious penalty, so alternatives are penalized rather than absent. Theater (0.30) is low-to-moderate: the ritual and administrative functions are real and performed daily; the theatrical share is the growing public defense of the boundary, trustee statements, litigation, ceremonial reaffirmation, that defends the line rather than operating the institutions. Accessibility_collapse (0.45) is moderate: alternatives persist and are understood, but none delivers what the orthodox track delivers, recognized membership, so they substitute only partially. Resistance (0.55) is real and organized: the Goolrokh Gupta litigation, reformist trusts, and challenges from within the community's own women. The measurement series run on one shared grid (points 0/15/30/45/60/75 of a 1950-2025 interval) so no metric is ever sampled against another metric's end-state. The rising suppression_requirement series tracks a genuine enforcement-capacity story: mid-century exclusion was largely customary and self-enforcing; as intermarriage rose and decline accelerated, trustees moved to active refusal, litigation funding, and public defense, hardening enforcement machinery over the interval.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seats and the payer seats compute differently by construction. From the trusteeship, the boundary is the community: administering it is stewardship, and every exclusion is the price of continuity, so that seat should see near-pure coordination with itself as beneficiary. From the intermarried women's seat, the same administration is exile from an identity they still hold: extraction at the full-target end with no exit that preserves selfhood. Four moderate-power seats diverge on exit alone: endogamous members (identity_locked, beneficiary side), intermarried women (identity_locked, target side), their children (constrained, because reformist initiation exists but buys no orthodox recognition), and non-Parsi spouses (mobile, because the community was never theirs and exclusion costs them standing only vicariously). Same nominal power level, four different constraints; the differentiator is identity fusion and the availability of a recognized exit, not global standing. The judiciary sees the whole structure and has so far declined to rule on the merits: an analytical seat that changes enforcement conditions without settling the membership question.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (priesthood, trustees, endogamous members) drive those seats toward the beneficiary end of d; victim declarations (intermarried women, their children, non-Parsi spouses) drive toward the target end. Endogamous community members are the genuinely dual seat, declared beneficiary with a secondary payer role, because they collect identity continuity while paying the demographic cost of a shrinking marriage pool; their derived d should sit nearer the middle than any other beneficiary's. Exit modulation matters: identity_locked victims (the women) sit nearer full-target than their constrained children, whose rival-track exit damps effective extraction; the mobile non-Parsi spouses, though declared victims, sit farthest from full-target among the victims because the arrangement refuses them rather than drawing continuing payment from them. Diaspora congregations are declared beneficiaries whose derived d should land near symmetric: they collect the identity goods the boundary produces while actively eroding the boundary itself from outside enforcement reach. No directionality_overrides are authored: the override key is the coarse power atom, and this story's distinct agents share power levels (four moderate seats with different roles and exits), so any override would misapply across structurally different seats. The role-plus-exit-plus-scope derivation captures the structure.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem, survival of a micro-minority's identity and ritual continuity, is live and independently corroborated by census demography, so this is not a mandatrophy case: the arrangement has not outlived its function, and founding_problem_status=live combined with disappearance_verdict=world_rearranges raises no zombie flag. The tangled_rope claim is what prevents both mislabels. Reading the arrangement as pure extraction erases the genuine coordination function: a community of 60,000 cannot define membership, ritual validity, or institutional access without a boundary, and the boundary has real, voluntarily affirmed value for most members. Reading it as pure coordination launders a gendered exclusion whose enforcement exists to keep a specific class of members and their descendants outside, with the costs falling on those with the least say. The live risk is drift, not obsolescence: if endogamy's efficacy fails demographically (see the endogamy_efficacy_ambiguity omega), the preservation cover decays while the exclusion machinery persists, the classic path from hybrid coordination-extraction toward pure extraction. The theater_ratio series is the watched signal for that decay. Coalition potential among the victims is structurally weak: the target class is small, dispersed, and identity-locked, and the excluded seats lack a shared institutional platform, which is part of why resistance has taken the form of individual litigation rather than collective refusal.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_plurality,
    'This constraint is one reading of the family_law_authority kernel (parsi_zoroastrian_reading). How would the sibling readings restructure this arrangement''s beneficiary and victim sets, and where exactly does the disagreement sit?',
    'Comparative structural analysis across the five declared readings: the secular_contractual_reading dissolves priestly validity and endogamy enforcement entirely, leaving private-association choice; the shariat reading relocates exit mechanics into divorce law while keeping religious adjudication; the canonical and dharmashastra readings relocate the boundary into sacrament and samskara respectively. Each sibling is a separate constraint story with its own epsilon and victim set.',
    'Adopting a sibling reading changes who counts as a victim of the same marriage act: under the secular reading the intermarried women are free contractors with no claim against anyone; under this reading they are members wrongly excluded. Per-seat classification of identical conduct flips across readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_plurality, conceptual, 'Committer structure: one reading of a five-reading kernel; sibling readings are separate constraints, not positions inside this one.').

omega_variable(
    endogamy_efficacy_ambiguity,
    'Does endogamous enforcement actually preserve the anjuman, or does it accelerate collapse by excluding intermarried families and depressing marriage formation?',
    'Demographic counterfactual modeling against admission-policy variants, plus a natural experiment via diaspora congregations that admit children of Parsi mothers: compare retention, marriage, and birth rates across the two regimes.',
    'If exclusion accelerates decline, the coordination function is self-undermining: the preservation cover decays while the exclusion persists, and the arrangement drifts toward pure extraction. If exclusion preserves, part of the measured extraction is the price of the coordination itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(endogamy_efficacy_ambiguity, empirical, 'Whether the small-community preservation logic is empirically sound or self-defeating.').

omega_variable(
    gendered_asymmetry_provenance,
    'Is the asymmetric penalty on women''s intermarriage (women forfeit status, children of Parsi mothers are excluded, children of Parsi fathers were historically admitted) doctrinally essential to Zoroastrian law, or a colonial-era customary codification?',
    'Textual-historical analysis: Avestan and Pahlavi marriage provisions versus 19th-20th century trusteeship practice and the drafting history of the Parsi Marriage and Divorce Act, 1936.',
    'If customary accretion, the reading can shed the asymmetry without kernel revision and the arrangement drifts toward pure coordination. If doctrinal, removing it is kernel revision and the reading''s identity changes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gendered_asymmetry_provenance, empirical, 'Doctrinal versus customary origin of the gender asymmetry in status forfeiture.').

omega_variable(
    identity_lock_depth_exclusion,
    'Is the suppression binding intermarried women and their children structural (institutional refusal of access) or internalized (self-understanding as Parsi persisting despite exclusion), and in what proportion?',
    'Post-exit trajectory study of families who accepted exclusion or affiliated reformist: if identity fusion and deference to orthodox institutions persist after access is regained or refused for good, the internalized share is large.',
    'If substantially internalized, the structural suppression measure understates the arrangement''s hold on its targets: exit does not dissolve it, and classification from structural metrics alone would overstate the freedom actually available.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_depth_exclusion, empirical, 'Structural versus internalized share of the exclusion''s hold on its targets.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(family_law_authority__parsi_zoroastrian_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(parsi_marriage_boundary_tr_t0, family_law_authority__parsi_zoroastrian_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(parsi_marriage_boundary_tr_t0, observed).
narrative_ontology:measurement(parsi_marriage_boundary_tr_t15, family_law_authority__parsi_zoroastrian_reading, theater_ratio, 15, 0.18).
narrative_ontology:measurement_basis(parsi_marriage_boundary_tr_t15, observed).
narrative_ontology:measurement(parsi_marriage_boundary_tr_t30, family_law_authority__parsi_zoroastrian_reading, theater_ratio, 30, 0.21).
narrative_ontology:measurement_basis(parsi_marriage_boundary_tr_t30, observed).
narrative_ontology:measurement(parsi_marriage_boundary_tr_t45, family_law_authority__parsi_zoroastrian_reading, theater_ratio, 45, 0.24).
narrative_ontology:measurement_basis(parsi_marriage_boundary_tr_t45, observed).
narrative_ontology:measurement(parsi_marriage_boundary_tr_t60, family_law_authority__parsi_zoroastrian_reading, theater_ratio, 60, 0.27).
narrative_ontology:measurement_basis(parsi_marriage_boundary_tr_t60, observed).
narrative_ontology:measurement(parsi_marriage_boundary_tr_t75, family_law_authority__parsi_zoroastrian_reading, theater_ratio, 75, 0.3).
narrative_ontology:measurement_basis(parsi_marriage_boundary_tr_t75, observed).

% Extraction over time
narrative_ontology:measurement(parsi_marriage_boundary_be_t0, family_law_authority__parsi_zoroastrian_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement_basis(parsi_marriage_boundary_be_t0, observed).
narrative_ontology:measurement(parsi_marriage_boundary_be_t15, family_law_authority__parsi_zoroastrian_reading, base_extractiveness, 15, 0.44).
narrative_ontology:measurement_basis(parsi_marriage_boundary_be_t15, observed).
narrative_ontology:measurement(parsi_marriage_boundary_be_t30, family_law_authority__parsi_zoroastrian_reading, base_extractiveness, 30, 0.48).
narrative_ontology:measurement_basis(parsi_marriage_boundary_be_t30, observed).
narrative_ontology:measurement(parsi_marriage_boundary_be_t45, family_law_authority__parsi_zoroastrian_reading, base_extractiveness, 45, 0.51).
narrative_ontology:measurement_basis(parsi_marriage_boundary_be_t45, observed).
narrative_ontology:measurement(parsi_marriage_boundary_be_t60, family_law_authority__parsi_zoroastrian_reading, base_extractiveness, 60, 0.55).
narrative_ontology:measurement_basis(parsi_marriage_boundary_be_t60, observed).
narrative_ontology:measurement(parsi_marriage_boundary_be_t75, family_law_authority__parsi_zoroastrian_reading, base_extractiveness, 75, 0.58).
narrative_ontology:measurement_basis(parsi_marriage_boundary_be_t75, observed).

% Suppression requirement over time
narrative_ontology:measurement(parsi_marriage_boundary_su_t0, family_law_authority__parsi_zoroastrian_reading, suppression_requirement, 0, 0.32).
narrative_ontology:measurement_basis(parsi_marriage_boundary_su_t0, observed).
narrative_ontology:measurement(parsi_marriage_boundary_su_t15, family_law_authority__parsi_zoroastrian_reading, suppression_requirement, 15, 0.37).
narrative_ontology:measurement_basis(parsi_marriage_boundary_su_t15, observed).
narrative_ontology:measurement(parsi_marriage_boundary_su_t30, family_law_authority__parsi_zoroastrian_reading, suppression_requirement, 30, 0.42).
narrative_ontology:measurement_basis(parsi_marriage_boundary_su_t30, observed).
narrative_ontology:measurement(parsi_marriage_boundary_su_t45, family_law_authority__parsi_zoroastrian_reading, suppression_requirement, 45, 0.47).
narrative_ontology:measurement_basis(parsi_marriage_boundary_su_t45, observed).
narrative_ontology:measurement(parsi_marriage_boundary_su_t60, family_law_authority__parsi_zoroastrian_reading, suppression_requirement, 60, 0.51).
narrative_ontology:measurement_basis(parsi_marriage_boundary_su_t60, observed).
narrative_ontology:measurement(parsi_marriage_boundary_su_t75, family_law_authority__parsi_zoroastrian_reading, suppression_requirement, 75, 0.55).
narrative_ontology:measurement_basis(parsi_marriage_boundary_su_t75, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(family_law_authority__parsi_zoroastrian_reading, identity_coordination).
narrative_ontology:affects_constraint(family_law_authority__parsi_zoroastrian_reading, family_law_authority__hindu_dharmashastra_reading).
narrative_ontology:affects_constraint(family_law_authority__parsi_zoroastrian_reading, family_law_authority__muslim_shariat_reading).
narrative_ontology:affects_constraint(family_law_authority__parsi_zoroastrian_reading, family_law_authority__christian_canonical_reading).
narrative_ontology:affects_constraint(family_law_authority__parsi_zoroastrian_reading, family_law_authority__secular_contractual_reading).

% DUAL FORMULATION NOTE:
% The family_law_authority kernel decomposes into five reading-stories (this file plus the four sibling readings), each epsilon-invariant on its own referent. This story authors epsilon for the standing Parsi arrangement (endogamy plus priestly ritual validity plus institutional enforcement of the membership line), not for marriage governance generally; each sibling authors its own epsilon for its own standing arrangement. The family is the unit of cross-reading comparison; no single file carries the contest.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
