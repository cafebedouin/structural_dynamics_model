% ============================================================================
% CONSTRAINT STORY: family_law_authority__muslim_shariat_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_family_law_authority__muslim_shariat_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: family_law_authority__muslim_shariat_reading
 *   human_readable: Nikah: Islamic Marriage Governance (Quranic-Hadith Reading)
 *   domain: religious/legal/political
 *
 * SUMMARY:
 *   Islamic marriage (nikah) under the Quranic-hadith reading is a
 *   contractual arrangement (ijab-qabul) with embedded asymmetries: husbands
 *   hold unilateral talaq (divorce) rights; wives are obligated to obedience
 *   (ta'ah) and confined to limited khul (consent-based divorce) grounds;
 *   mahr (dower) is a one-time transfer; nafaqah (maintenance) is the
 *   husband's obligation and his tool for compliance enforcement; child
 *   custody shifts to paternal lineage post-infancy; polygyny is permitted
 *   for husbands (up to four wives simultaneously). This constraint is ONE
 *   READING of a contested kernel (family_law_authority): five distinct
 *   readings from five religious/secular traditions compete to govern
 *   marriage. The nikah reading instantiates marriage as a civil contract
 *   under Islamic law, not a sacrament (Christian), samskara (Hindu), or
 *   secular civil contract. The gap between the claim (tangled_rope, solving
 *   real coordination problems) and the metrics (high extractiveness 0.68,
 *   substantial suppression 0.62) reflects the structural divergence: from
 *   the beneficiary seat (ulama, male household heads), the arrangement
 *   solves coordination; from the payer seat (women, divorced women,
 *   children), it extracts asymmetric exit costs and patriarchal control. The
 *   engine computes this per-seat divergence from the structural data. The
 *   measurement series track a modest rise in extractiveness (0.58 → 0.68
 *   over 40 years) and theater ratio (0.15 → 0.28), reflecting increasing
 *   performative justification of the constraint as women's rights movements
 *   press it, while suppression remains stable and slightly elevated (0.58 →
 *   0.62). The rise in theater suggests enforcement has shifted from
 *   naturalized practice toward explicit legitimacy defense.
 *
 * KEY AGENTS:
 *   - Ulama authority structure (institutional agenda-setter, interprets Quranic/hadith law, certifies marriages, rules on talaq/custody)
 *   - Male household heads (moderate power, beneficiary, hold unilateral talaq, default child custody, permitted polygyny)
 *   - Women marriage principals (powerless, payer, trapped by identity-lock, limited khul grounds, asymmetric exit)
 *   - Divorced women (powerless, payer, face social stigma and subsistence loss post-talaq, most vulnerable seat)
 *   - Children/custody claims (powerless, payer, treated as paternal property, forced maternal separation post-infancy)
 *   - State civil authorities (institutional observer, increasingly intervene with reforms, alternative authority)
 *   - Women's rights movements (organized excluded voices, mobilized against triple talaq and for equal rights)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(family_law_authority__muslim_shariat_reading, 0.68).
domain_priors:suppression_score(family_law_authority__muslim_shariat_reading, 0.62).
domain_priors:theater_ratio(family_law_authority__muslim_shariat_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(family_law_authority__muslim_shariat_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(family_law_authority__muslim_shariat_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(family_law_authority__muslim_shariat_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(family_law_authority__muslim_shariat_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(family_law_authority__muslim_shariat_reading, resistance, 0.59).

% --- Constraint claim ---
narrative_ontology:constraint_claim(family_law_authority__muslim_shariat_reading, tangled_rope).
narrative_ontology:human_readable(family_law_authority__muslim_shariat_reading, "Nikah: Islamic Marriage Governance (Quranic-Hadith Reading)").
narrative_ontology:topic_domain(family_law_authority__muslim_shariat_reading, "religious/legal/political").

domain_priors:requires_active_enforcement(family_law_authority__muslim_shariat_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(family_law_authority__muslim_shariat_reading, '12e6eaaa-9d47-4ed7-9b87-666e73e8f92a').
narrative_ontology:cs_kernel_codification('12e6eaaa-9d47-4ed7-9b87-666e73e8f92a', fixed_text).
narrative_ontology:cs_authority_grounding('12e6eaaa-9d47-4ed7-9b87-666e73e8f92a', lineage).
narrative_ontology:cs_interpretation_layer_present('12e6eaaa-9d47-4ed7-9b87-666e73e8f92a').
narrative_ontology:cs_reading_relation('12e6eaaa-9d47-4ed7-9b87-666e73e8f92a', family_law_authority__christian_canonical_reading, coexists_with).
narrative_ontology:cs_reading_relation('12e6eaaa-9d47-4ed7-9b87-666e73e8f92a', family_law_authority__hindu_dharmashastra_reading, coexists_with).
narrative_ontology:cs_reading_relation('12e6eaaa-9d47-4ed7-9b87-666e73e8f92a', family_law_authority__parsi_zoroastrian_reading, coexists_with).
narrative_ontology:cs_reading_relation('12e6eaaa-9d47-4ed7-9b87-666e73e8f92a', family_law_authority__secular_contractual_reading, coexists_with).
narrative_ontology:cs_axiom('12e6eaaa-9d47-4ed7-9b87-666e73e8f92a', foundational, talaq_unilateral_male_prerogative).
narrative_ontology:cs_axiom_status(talaq_unilateral_male_prerogative, holdable).
narrative_ontology:cs_axiom_grounding('12e6eaaa-9d47-4ed7-9b87-666e73e8f92a', talaq_unilateral_male_prerogative, empirically_contingent).
narrative_ontology:cs_axiom('12e6eaaa-9d47-4ed7-9b87-666e73e8f92a', foundational, quranic_contractual_legitimacy).
narrative_ontology:cs_axiom_status(quranic_contractual_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('12e6eaaa-9d47-4ed7-9b87-666e73e8f92a', quranic_contractual_legitimacy, theological).
narrative_ontology:cs_axiom('12e6eaaa-9d47-4ed7-9b87-666e73e8f92a', secondary, mahr_dower_obligation).
narrative_ontology:cs_axiom_status(mahr_dower_obligation, holdable).
narrative_ontology:cs_axiom_grounding('12e6eaaa-9d47-4ed7-9b87-666e73e8f92a', mahr_dower_obligation, theological).
narrative_ontology:cs_reference_frame('12e6eaaa-9d47-4ed7-9b87-666e73e8f92a', quranic_marriage_contract_authority).
narrative_ontology:cs_drift_state('12e6eaaa-9d47-4ed7-9b87-666e73e8f92a', contemporary_reform_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('12e6eaaa-9d47-4ed7-9b87-666e73e8f92a', '').
narrative_ontology:cs_kernel_id(family_law_authority__muslim_shariat_reading, family_law_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(family_law_authority__muslim_shariat_reading, ulama_authority_structure).
narrative_ontology:constraint_beneficiary(family_law_authority__muslim_shariat_reading, male_household_heads).
narrative_ontology:constraint_beneficiary(family_law_authority__muslim_shariat_reading, family_patriarchs).
narrative_ontology:constraint_victim(family_law_authority__muslim_shariat_reading, women_marriage_principals).
narrative_ontology:constraint_victim(family_law_authority__muslim_shariat_reading, divorced_women).
narrative_ontology:constraint_victim(family_law_authority__muslim_shariat_reading, children_custody_claims).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(family_law_authority__muslim_shariat_reading, male_household_heads).
narrative_ontology:constraint_vindicates(family_law_authority__muslim_shariat_reading, quranic_contractual_authority).
narrative_ontology:constraint_vindicates(family_law_authority__muslim_shariat_reading, prophetic_sunna_grounding).
narrative_ontology:constraint_vindicates(family_law_authority__muslim_shariat_reading, mahr_obligation_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Islamic legal scholars and judge-administrators (qadis, muftis) interpret Quranic text and hadith to adjudicate marriage formation, obligations, dissolution, and custody. They administer the nikah contract framework, certify marriages, rule on talaq validity, and determine mahr amounts. Their authority rests on scriptural reading and jurisprudential tradition (fiqh). They do not themselves marry but set the terms under which others do.
narrative_ontology:constraint_stakeholder(family_law_authority__muslim_shariat_reading, ulama_authority_structure, agenda_setter,
    institutional, civilizational, analytical, global).

% Under this reading, husbands hold unilateral talaq (divorce) rights, retain default custody of children past infancy, may take up to four wives simultaneously, and are obligated to provide nafaqah (maintenance). They benefit from contractual exit without cause and plural marriage access; they pay financial obligations and are bound by marital duties. Their options are constrained by both the ulama framework and by family/community pressure.
narrative_ontology:constraint_stakeholder(family_law_authority__muslim_shariat_reading, male_household_heads, beneficiary,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(family_law_authority__muslim_shariat_reading, male_household_heads, payer).

% Women enter nikah contracts, receive mahr (dower/gift) from the husband at marriage, and are entitled to nafaqah and obedience from the husband. Pre-2019 triple talaq practice, women could not dissolve marriage unilaterally without khul (mutual consent) or judicial intervention on limited grounds (cruelty, non-support). Their exit is locked by identity fusion (marriage defines social standing, property rights, children's legitimacy, and family honor) and by structural barriers (limited grounds for khul, evidentiary burden, community/family pressure against initiation). They are trapped in unsatisfactory marriages unless the husband consents or they satisfy judicial criteria.
narrative_ontology:constraint_stakeholder(family_law_authority__muslim_shariat_reading, women_marriage_principals, payer,
    powerless, biographical, identity_locked, global).

% After talaq, women face social stigma, reduced remarriage prospects, loss of marital home access (iddah period of waiting; no guaranteed housing), limited child custody (typically lost after infancy), and subsistence insecurity if the husband does not maintain them through the iddah period. The mahr is a one-time transfer; alimony (mut'ah) is discretionary. Divorced women are the most trapped: they have exited marriage but retain identity/property losses, social marginalization, and no structured means of economic reconstruction. They bear extraction without ongoing benefit.
narrative_ontology:constraint_stakeholder(family_law_authority__muslim_shariat_reading, divorced_women, payer,
    powerless, biographical, trapped, global).

% Children are treated as property of the father's lineage under classical Islamic law. Custody (hadanah) shifts from mother (infancy) to paternal relatives (post-infancy). Children are trapped in the marital structure; they bear the extraction of maternal separation and paternal control without independent choice or remedial voice.
narrative_ontology:constraint_stakeholder(family_law_authority__muslim_shariat_reading, children_custody_claims, payer,
    powerless, biographical, trapped, global).

% Female relatives (mothers, sisters, aunts) who wish to influence marital outcomes, protect daughters, or exercise guardianship (wilayah) in marriage formation are structurally excluded from direct legal standing. They advise, pressure, and sometimes negotiate, but formal authority rests with male guardians. Their exclusion sustains male agenda-setting power.
narrative_ontology:constraint_stakeholder(family_law_authority__muslim_shariat_reading, female_kin_guardians, excluded,
    moderate, biographical, constrained, global).

% Civil authorities in Muslim-majority and Muslim-diaspora states observe, sometimes regulate, and increasingly intervene in the nikah framework. Some states (Turkey, Tunisia, Morocco pre-2004) secularized marriage law entirely; others (Egypt, Indonesia, most Gulf states) incorporate sharia elements while adding civil protections (spousal consent for talaq, written notification requirements, custody standards). Authorities can mandate legal reforms that alter the constraint's operation, though political Islam resists.
narrative_ontology:constraint_stakeholder(family_law_authority__muslim_shariat_reading, state_civil_authorities, observer,
    institutional, generational, analytical, national).

% Feminist, women's rights, and gender-justice movements challenge the asymmetries in this constraint. Pre-2019, they mobilized against triple talaq; post-2019 (India) and in ongoing campaigns, they seek equal divorce access, automatic custody presumptions for mothers, and equitable property division. They are excluded from the sharia-reading authority structure but increasingly exert pressure through legislation, litigation, and social mobilization.
narrative_ontology:constraint_stakeholder(family_law_authority__muslim_shariat_reading, women_rights_movements, excluded,
    organized, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(family_law_authority__muslim_shariat_reading, ulama_authority_structure).
narrative_ontology:fixing_cost_class(family_law_authority__muslim_shariat_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a legal framework for marriage formation (ijab-qabul), financial obligations (mahr, nafaqah), gender roles (obedience/maintenance), dissolution (talaq), and child custody. Solves the coordination problem of how to recognize marriages as valid, binding contracts with standardized terms across dispersed Muslim communities and generations, without centralized state enforcement of the pre-modern period.
% TRANSFER_FUNCTION: Moves financial resources (mahr) from husband to wife at marriage; obligates husband to provide nafaqah (housing, food, clothing, medical care); restricts women's unilateral exit while permitting husbands' costless exit (pre-2019). The gender-asymmetric divorce access transfers exit-option value to husbands; the identity-lock on women transfers control of marriage stability to the husband and his kin.
% ABSENT_VOICES: Women reformers within Islam who argue for equal talaq rights, female guardianship (wilayah), and equal custody presumptions; secular Muslims who reject sharia governance of marriage; non-Muslim citizens in multi-faith states whose marriage law differs; children who have no voice in custodial disposition; divorced women who bear lasting harm and have no retroactive remedy or compensation.
% DISAPPEARANCE_RATIONALE: If this constraint disappeared — if nikah authority dissolved and sharia governance of marriage ceased — marriage law and custody regimes would reorganize under state civil law, family codes, or alternative religious traditions. The patriarchal transfer structure would be replaced (most civil codes presume equal custody, equal divorce access, equitable property division). Women's exit options would expand, children's interests would become a primary custody criterion rather than paternal property, and mahr/nafaqah would be replaced by spousal support and child support. The social organization of marriage, kinship, and gender would fundamentally shift.
% FOUNDING_PROBLEM: Early Islamic community needed a framework to govern marriage as a binding contract between consenting adults (ijab-qabul), regulate financial obligations (mahr to protect women; nafaqah to ensure family support), and establish clear rules for dissolution and child assignment, in the absence of centralized state authority.
% FOUNDING_PROBLEM_CORROBORATION: Classical Islamic jurists and contemporary traditionalist ulama attest the founding problem (marriage formation, financial security, custody) is solved by the nikah framework. Women's rights advocates, reformist Islamic scholars, and secular commentators attest the founding problem is partly solved (contracts are formed, some financial transfer occurs) but the solution is entangled with extraction: gender-asymmetric exit, identity-fusion traps, and patriarchal resource control. Legislative history in Egypt (2000 khul reform), Morocco (2004 reforms), and India (2019 triple talaq ban) documents the shift: the founding problems of contract formation and basic financial security are now solved by civil family codes; the remaining nikah persistence is increasingly read as extractive patriarchal structure rather than necessary coordination.
narrative_ontology:disappearance_verdict(family_law_authority__muslim_shariat_reading, world_rearranges).
narrative_ontology:founding_problem_status(family_law_authority__muslim_shariat_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(family_law_authority__muslim_shariat_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(family_law_authority__muslim_shariat_reading, 'none', 1).
narrative_ontology:epsilon_provenance(family_law_authority__muslim_shariat_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(family_law_authority__muslim_shariat_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(family_law_authority__muslim_shariat_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(family_law_authority__muslim_shariat_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.68: The constraint transfers unilateral exit rights to husbands (costless talaq), retains women's exit-cost at near-maximal (identity-lock, limited khul grounds, social stigma, subsistence insecurity), and assigns child custody to paternal lineage irrespective of children's welfare interests. The mahr and nafaqah are real but one-time or contingent transfers; they do not offset the exit-cost asymmetry. A pure coordination reading (Rope) would presume these asymmetries are necessary for contract formation and family stability; the measured extractiveness reflects the claim that they exceed this necessity and operate as patriarchal resource control. Suppression 0.62: The constraint's persistence depends on both structural barriers (legal inaccessibility of khul, evidentiary burden for cruelty/non-support, social stigma against divorce-initiating women) and internalized suppression (identity fusion, religious legitimacy internalized as personal duty). The triple talaq pre-2019 was a pure structural suppression (unilateral male exit, no reciprocal female exit); post-2019 Indian ban and ongoing civil reforms (Egypt 2000, Morocco 2004) shift suppression toward internalization (women still perceive khul as shameful, seek 'permission' from community/husband even when technically available). Theater 0.28 (low-moderate): The constraint maintains real functions (marriage certification, financial obligations, custody clarity) but increasingly requires explicit legitimacy work (ulama defenses of gender complementarity, re-readings of talaq as discouraged, mahr-reframing as women's protection). The rise from 0.15 to 0.28 reflects this shift — enforcement has become more theatrical as women's rights challenges mount. Accessibility collapse 0.71: Alternatives (secular family law, equal divorce, maternal custody preference) are not suppressed — they exist in some jurisdictions and in reformist Islamic jurisprudence. But the constraint operates in contexts where alternatives are unavailable or delegitimized by religious authority. Exit appears closed not because other options do not exist globally, but because they are inaccessible or unthinkable within the framework (a woman cannot 'choose' secular law if her community and family deny it legitimacy). Resistance 0.59: Substantial active resistance emerges from women's movements, reformist Islamic scholars, and secular states, but the constraint persists because the ulama authority and patriarchal family structures maintain enforcement. The resistance is real and growing but not yet sufficient to overturn the constraint in most Muslim-majority jurisdictions.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats (women, divorced women, children) experience this as a snare: high extraction, identity-trapping suppression, and asymmetric exit costs that cannot be negotiated within the framework. The beneficiary seats (ulama, male household heads) experience it as a rope: real coordination for marriage formation, financial security, and family stability, with gender-specific roles that are justified as complementary rather than extractive. The observer seat (state authorities) increasingly measures the constraint as tangled_rope or snare: the coordination functions are now available through secular family codes (contracts, financial support, custody rules), while the extraction persists through patriarchal asymmetries. The gap is structural: from within the sharia-reading framework, the asymmetries are 'natural' and 'divinely ordained'; from outside it, they are contingent choices that extract value from the disempowered. The engine computes this divergence as per-seat type divergence: the beneficiary-seat computation yields rope-type confidence; the payer-seat computation yields snare-type or high tangled_rope extraction. This divergence is the measurement the corpus captures.
 *
 * DIRECTIONALITY LOGIC:
 *   Ulama: d ≈ 0.1 (near-beneficiary end). They do not marry; they set and maintain the rules. They benefit from authority legitimacy, from the claim that their interpretation is Quranic fidelity, and from the social power this grants them. They pay almost nothing — they are not subject to the asymmetries of the constraint they administer. Male household heads: d ≈ 0.35 (moderately beneficiary, with real but constrained options). They collect talaq rights, default custody, and nafaqah obligations from wives; they pay maintenance costs and are bound by marital duties. They could exit, and some do (through talaq or through secular law where available). Their options are constrained by family/community pressure and by Islamic law's rules on remarriage and custody. They benefit from the constraint more than they pay, and their options are better than women's, so they sit well into the beneficiary half. Women marriage principals: d ≈ 0.78 (strongly targeted). They pay the asymmetric exit cost (identity-lock, limited khul grounds, social stigma, subsistence insecurity). They collect mahr and nafaqah — real benefits but conditional and one-time. They are trapped by identity fusion (marriage defines social standing, property rights, children's legitimacy). Their exit options are among the most constrained in any constraint system. State civil authorities: d = 0.5 (analytical). They observe; they do not collect from or pay into the constraint. Some states reinforce it (Saudi Arabia); some replace it (Tunisia). The directionality override is not needed here; the structural derivation from roles and exit options captures it.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy (the founding problem has died but the constraint persists) is a live question for this constraint. The founding problem — establishing a legal framework for marriage formation and financial security in the absence of centralized state authority — has been solved by civil family codes in most jurisdictions where Muslims live as citizens. Egypt's 2000 khul reform, Morocco's 2004 reforms, and India's 2019 triple talaq ban document the shift: these jurisdictions recognized that the coordination function (marriage contracts, financial obligations, custody) is achievable through secular family law, often with greater protection for women and children. The remaining nikah persistence in these jurisdictions is increasingly read as patriarchal tradition ('how we've always done it') rather than necessary coordination. However, in jurisdictions where Islamic law is the only legal framework (Saudi Arabia, Afghanistan under Taliban rule, parts of Yemen), mandatrophy is not live — the constraint is still the machinery for marriage governance. For diaspora Muslims in secular states, mandatrophy is complex: the constraint persists as a parallel legal/cultural system, but the founding problems are solved by the state (civil marriage, child support laws, anti-domestic-violence statutes). The persistence is partly theatrical (ulama perform legitimacy) and partly identity-constitutive (marriage within Islamic community norms has social/spiritual meaning beyond legal function). The commentary distinguishes these cases in the omega on mandatrophy status.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    talaq_necessity_vs_patriarchal_control,
    'Is unilateral male talaq (divorce without cause) structurally necessary for Islamic marriage as a contract, or is it a patriarchal norm that can be reformed without dissolving the nikah framework?',
    'Comparative analysis of reformist Islamic jurisprudence (e.g., Morocco 2004, Egypt 2000, scholars who argue for mutual/equal talaq access) against traditionalist ulama positions; empirical outcomes in reformed jurisdictions to assess whether marriage stability and family welfare improve, decline, or remain stable under equal divorce access.',
    'If talaq asymmetry is contingent (can be reformed), extractiveness drops substantially (0.35-0.45 range), suppression drops (0.35-0.45), and the constraint reclassifies toward rope or lower tangled_rope. If talaq asymmetry is seen as essential to the Quranic reading, extractiveness remains high and the constraint stays tangled_rope/snare. This resolves whether the constraint is a foundational Islamic institution or a patriarchal choice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(talaq_necessity_vs_patriarchal_control, empirical, 'Whether unilateral talaq is essential to Islamic marriage or a reform-contingent norm.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression 0.62 primarily structural (legal barriers, social sanctions, economic dependency) or primarily internalized (women''s belief in the religious legitimacy of their subordinate role, identity fusion with the marital status)?',
    'Post-exit trajectory analysis: in cases where women exit marriage (through khul, judicial decree, or emigration to secular jurisdictions), does the suppression persist post-exit (a sign of internalization) or collapse (a sign of structural barriers)? Comparative ethnographic/survey evidence from contexts where legal barriers are reduced but religious/cultural norms persist.',
    'If suppression is primarily structural, removing legal barriers (easier khul, spousal consent requirements for talaq, civil family-law alternatives) would substantially reduce extraction. If suppression is primarily internalized, legal reform is necessary but insufficient; cultural transformation is needed. This affects whether the constraint is classifiable as snare (structural extraction) or tangled_rope (hybrid coordination + internalized suppression as the binding mechanism).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Whether suppression is structural or internalized.').

omega_variable(
    mandatrophy_status_by_jurisdiction,
    'Is the founding problem (establishing valid marriage contracts and financial obligations in the absence of centralized state authority) still live, dead, or contested across jurisdictions where this constraint operates?',
    'Historical-legal analysis: in jurisdictions that maintain Islamic family law (Saudi Arabia, Afghanistan, parts of Yemen, Iran), is the constraint still primary machinery for marriage governance (live founding problem)? In jurisdictions that have reformed (Egypt, Morocco, Tunisia, Indonesia with civil family codes), is the constraint increasingly parallel/cultural rather than legal (dead founding problem, persistence is cultural/identity rather than systemic necessity)? In diaspora contexts (Europe, North America, Australia), what is the role of nikah — does it serve a legal function or a community/spiritual function?',
    'If founding problem is live (most of world''s Muslims live under civil law or reformed Islamic family law, but in some jurisdictions Islamic law still governs marriage), the constraint is transitional/contested — mandatrophy is emerging but not complete. This affects classification: if mandatrophy is true, the constraint should be classified as Piton (persisting through inertia/identity rather than function). If founding problem is still live, Tangled Rope or Rope classification holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mandatrophy_status_by_jurisdiction, empirical, 'Founding problem live/dead status by jurisdiction and community context.').

omega_variable(
    alternative_reading_foreclosure,
    'Does the Quranic-hadith reading logically foreclose the secular contractual reading (or vice versa), or can both be held simultaneously within different parties'' frameworks?',
    'Theological and legal-philosophical analysis: can a Muslim who believes the Quran is divine guidance accept that marriage under secular state law is valid and binding? Does sharia doctrine require all Muslims to marry under Islamic law, or does it permit secular marriage as an alternative? Can the two readings coexist as options for different parties/communities, or does the sharia reading''s claim to universal applicability logically rule out secular family law as legitimate?',
    'If the readings foreclose each other, the boundary is sharp and the kernel splits into genuinely incompatible frameworks (one side wins, one side loses, in any given jurisdiction). If they coexist, both can be live options in multi-faith or Muslim-plural contexts. This determines the reading_relations entry for secular_contractual_reading: ''forecloses'' vs. ''coexists_with''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_reading_foreclosure, conceptual, 'Whether sharia and secular readings logically foreclose or coexist.').

omega_variable(
    mahr_protection_vs_bride_price,
    'Does mahr (dower) function as a protective transfer to wives (as ulama doctrine claims) or as a bride price/marital transaction cost that benefits husbands and patriarchs?',
    'Comparative historical-legal analysis: do wives retain, control, and benefit from mahr post-marriage, or is it typically claimed/controlled by husbands or patriarchs? Do mahr amounts track protection (higher for vulnerable brides) or status/negotiation (higher for wealthy families, scarce for poor families)? Post-divorce, how often does mahr compensate divorced women or cover subsistence costs versus remaining token or disputed?',
    'If mahr is protective (wives control it, claim it post-divorce, amounts sufficient for livelihood), extractiveness drops 0.05-0.10 because it represents genuine financial transfer. If mahr is symbolic or controlled by patriarchs (bride price dynamic), extractiveness remains high. This affects whether the constraint is read as mutualistic (mahr reciprocity for husband''s nafaqah) or patriarchal (symbolic token obscuring asymmetric extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mahr_protection_vs_bride_price, empirical, 'Whether mahr functions as wife protection or bride price.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(family_law_authority__muslim_shariat_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fami_tr_t0, family_law_authority__muslim_shariat_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(fami_tr_t0, observed).
narrative_ontology:measurement(fami_tr_t5, family_law_authority__muslim_shariat_reading, theater_ratio, 5, 0.17).
narrative_ontology:measurement_basis(fami_tr_t5, observed).
narrative_ontology:measurement(fami_tr_t10, family_law_authority__muslim_shariat_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement_basis(fami_tr_t10, observed).
narrative_ontology:measurement(fami_tr_t15, family_law_authority__muslim_shariat_reading, theater_ratio, 15, 0.23).
narrative_ontology:measurement_basis(fami_tr_t15, observed).
narrative_ontology:measurement(fami_tr_t25, family_law_authority__muslim_shariat_reading, theater_ratio, 25, 0.27).
narrative_ontology:measurement_basis(fami_tr_t25, observed).
narrative_ontology:measurement(fami_tr_t40, family_law_authority__muslim_shariat_reading, theater_ratio, 40, 0.28).
narrative_ontology:measurement_basis(fami_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(fami_be_t0, family_law_authority__muslim_shariat_reading, base_extractiveness, 0, 0.58).
narrative_ontology:measurement_basis(fami_be_t0, observed).
narrative_ontology:measurement(fami_be_t5, family_law_authority__muslim_shariat_reading, base_extractiveness, 5, 0.61).
narrative_ontology:measurement_basis(fami_be_t5, observed).
narrative_ontology:measurement(fami_be_t10, family_law_authority__muslim_shariat_reading, base_extractiveness, 10, 0.64).
narrative_ontology:measurement_basis(fami_be_t10, observed).
narrative_ontology:measurement(fami_be_t15, family_law_authority__muslim_shariat_reading, base_extractiveness, 15, 0.67).
narrative_ontology:measurement_basis(fami_be_t15, observed).
narrative_ontology:measurement(fami_be_t25, family_law_authority__muslim_shariat_reading, base_extractiveness, 25, 0.68).
narrative_ontology:measurement_basis(fami_be_t25, observed).
narrative_ontology:measurement(fami_be_t40, family_law_authority__muslim_shariat_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(fami_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(fami_su_t0, family_law_authority__muslim_shariat_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(fami_su_t0, observed).
narrative_ontology:measurement(fami_su_t5, family_law_authority__muslim_shariat_reading, suppression_requirement, 5, 0.59).
narrative_ontology:measurement_basis(fami_su_t5, observed).
narrative_ontology:measurement(fami_su_t10, family_law_authority__muslim_shariat_reading, suppression_requirement, 10, 0.6).
narrative_ontology:measurement_basis(fami_su_t10, observed).
narrative_ontology:measurement(fami_su_t15, family_law_authority__muslim_shariat_reading, suppression_requirement, 15, 0.61).
narrative_ontology:measurement_basis(fami_su_t15, observed).
narrative_ontology:measurement(fami_su_t25, family_law_authority__muslim_shariat_reading, suppression_requirement, 25, 0.62).
narrative_ontology:measurement_basis(fami_su_t25, observed).
narrative_ontology:measurement(fami_su_t40, family_law_authority__muslim_shariat_reading, suppression_requirement, 40, 0.62).
narrative_ontology:measurement_basis(fami_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(family_law_authority__muslim_shariat_reading, attachment_coordination).
narrative_ontology:boltzmann_floor_override(family_law_authority__muslim_shariat_reading, 0.12).
narrative_ontology:affects_constraint(family_law_authority__muslim_shariat_reading, family_law_authority__christian_canonical_reading).
narrative_ontology:affects_constraint(family_law_authority__muslim_shariat_reading, family_law_authority__hindu_dharmashastra_reading).
narrative_ontology:affects_constraint(family_law_authority__muslim_shariat_reading, family_law_authority__parsi_zoroastrian_reading).
narrative_ontology:affects_constraint(family_law_authority__muslim_shariat_reading, family_law_authority__secular_contractual_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of five structurally distinct readings of the family_law_authority kernel. The readings differ in authority grounding (ecclesiastical, dharmic, sharia, Zoroastrian, state), gender regime (sacrament vs. samskara vs. contract with asymmetric exit vs. contract with equal exit), and property/custody rules. Each reading has its own ε, beneficiary/victim structure, and suppression profile. They are linked by affects_constraints to enable contamination analysis: if one reading's enforcement erodes in a jurisdiction, downstream pressure affects the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(family_law_authority__muslim_shariat_reading, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
