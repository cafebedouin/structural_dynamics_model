% ============================================================================
% CONSTRAINT STORY: marriage_authority_kernel__christian_canonical_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_authority_kernel__christian_canonical_reading, []).

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
 *   constraint_id: marriage_authority_kernel__christian_canonical_reading
 *   human_readable: Christian Canonical Marriage Authority (Indian Christian Marriage Act 1872)
 *   domain: religious governance / constitutional pluralism
 *
 * SUMMARY:
 *   The Indian Christian Marriage Act 1872 codifies Christian canonical law
 *   on marriage, divorce, and annulment for Christians in India. Marriage is
 *   treated as a sacrament whose validity is adjudicated by church tribunals
 *   on canonical grounds (impediment, defect of consent); divorce is
 *   restricted to fault-based grounds (adultery, cruelty, desertion), and
 *   dissolution is not recognized — remarriage after divorce carries
 *   sacramental penalty. This reading of the marriage-authority kernel treats
 *   ecclesiastical authority as legitimate and derives law from Christian
 *   doctrine. The constraint embeds sacramental theology in secular Indian
 *   law, creating a two-tier system where church tribunals and civil courts
 *   enforce different authority structures over the same persons. The
 *   measurement series span 1872–2026, tracking extractiveness accumulation
 *   and rising theater ratio as the constraint's founding function (preserve
 *   Christian community governance) decays relative to its extraction
 *   function (enforce sacramental indissolubility).
 *
 * KEY AGENTS:
 *   - Christian church tribunals: agenda-setters, institutional power, adjudicate marriage validity and annulment on canonical grounds
 *   - Christian women seeking divorce: powerless payers, identity-locked exit, bear the cost of restricted divorce access
 *   - Divorced Christians remarrying: moderate-power payers, constrained exit, face sacramental stigma and social exclusion
 *   - Patriarchal doctrine adherents: organized beneficiaries, mobile exit, benefit from legal reinforcement of traditional household structures
 *   - Christian civil courts: institutional observers, analytical seat, police the boundary between ecclesiastical and secular adjudication
 *   - Secular legal reformers: moderate-power excluded voices, advocate for uniform civil code and no-fault divorce
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority_kernel__christian_canonical_reading, 0.58).
domain_priors:suppression_score(marriage_authority_kernel__christian_canonical_reading, 0.42).
domain_priors:theater_ratio(marriage_authority_kernel__christian_canonical_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__christian_canonical_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(marriage_authority_kernel__christian_canonical_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(marriage_authority_kernel__christian_canonical_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__christian_canonical_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(marriage_authority_kernel__christian_canonical_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority_kernel__christian_canonical_reading, tangled_rope).
narrative_ontology:human_readable(marriage_authority_kernel__christian_canonical_reading, "Christian Canonical Marriage Authority (Indian Christian Marriage Act 1872)").
narrative_ontology:topic_domain(marriage_authority_kernel__christian_canonical_reading, "religious governance / constitutional pluralism").

domain_priors:requires_active_enforcement(marriage_authority_kernel__christian_canonical_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority_kernel__christian_canonical_reading, 'de00e34a-d305-4e82-8c67-dad3b4d399fe').
narrative_ontology:cs_kernel_codification('de00e34a-d305-4e82-8c67-dad3b4d399fe', fixed_text).
narrative_ontology:cs_authority_grounding('de00e34a-d305-4e82-8c67-dad3b4d399fe', lineage).
narrative_ontology:cs_interpretation_layer_present('de00e34a-d305-4e82-8c67-dad3b4d399fe').
narrative_ontology:cs_reading_relation('de00e34a-d305-4e82-8c67-dad3b4d399fe', marriage_authority_kernel__hindu_codified_reading, coexists_with).
narrative_ontology:cs_reading_relation('de00e34a-d305-4e82-8c67-dad3b4d399fe', marriage_authority_kernel__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('de00e34a-d305-4e82-8c67-dad3b4d399fe', marriage_authority_kernel__parsi_communal_reading, coexists_with).
narrative_ontology:cs_reading_relation('de00e34a-d305-4e82-8c67-dad3b4d399fe', marriage_authority_kernel__secular_civil_reading, influences).
narrative_ontology:cs_axiom('de00e34a-d305-4e82-8c67-dad3b4d399fe', foundational, marriage_sacramental_indissolubility).
narrative_ontology:cs_axiom_status(marriage_sacramental_indissolubility, holdable).
narrative_ontology:cs_axiom_grounding('de00e34a-d305-4e82-8c67-dad3b4d399fe', marriage_sacramental_indissolubility, theological).
narrative_ontology:cs_axiom('de00e34a-d305-4e82-8c67-dad3b4d399fe', foundational, ecclesiastical_adjudicatory_authority).
narrative_ontology:cs_axiom_status(ecclesiastical_adjudicatory_authority, holdable).
narrative_ontology:cs_axiom_grounding('de00e34a-d305-4e82-8c67-dad3b4d399fe', ecclesiastical_adjudicatory_authority, deontological).
narrative_ontology:cs_reference_frame('de00e34a-d305-4e82-8c67-dad3b4d399fe', canonical_sacramental_authority).
narrative_ontology:cs_drift_state('de00e34a-d305-4e82-8c67-dad3b4d399fe', contemporary_constitutional_pluralism_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('de00e34a-d305-4e82-8c67-dad3b4d399fe', '').
narrative_ontology:cs_kernel_id(marriage_authority_kernel__christian_canonical_reading, marriage_authority_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__christian_canonical_reading, christian_church_tribunals).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__christian_canonical_reading, patriarchal_doctrine_adherents).
narrative_ontology:constraint_victim(marriage_authority_kernel__christian_canonical_reading, christian_women_seeking_divorce).
narrative_ontology:constraint_victim(marriage_authority_kernel__christian_canonical_reading, divorced_christians_remarrying).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__christian_canonical_reading, christian_community_members_content).
narrative_ontology:constraint_vindicates(marriage_authority_kernel__christian_canonical_reading, sacramental_indissolubility_doctrine).
narrative_ontology:constraint_vindicates(marriage_authority_kernel__christian_canonical_reading, canonical_authority_as_legitimate).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Exercise adjudicatory authority over marriage validity, annulment proceedings, and nullification claims within the Christian community. Interpret canonical law and decide cases on grounds of impediment, defect of consent, and sacramental form. Their authority is codified in the Indian Christian Marriage Act 1872 but derives legitimacy from church doctrine of marriage as sacrament.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__christian_canonical_reading, christian_church_tribunals, agenda_setter,
    institutional, generational, constrained, national).

% Cannot dissolve a marriage through civil divorce under the 1872 Act; must petition church tribunals on narrow grounds (adultery, cruelty, desertion) for annulment, which requires proof of canonical impediment or defect of consent at the time of marriage, not merely unhappiness. Exit from the marriage is blocked by identity-fusion (religious identity conflates with marital status); exit from the religious framework itself is psychologically/socially costly. Bear the cost of trapped marriage status.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__christian_canonical_reading, christian_women_seeking_divorce, payer,
    powerless, biographical, identity_locked, national).

% Face social and sacramental stigma for remarriage after divorce, as the church does not recognize dissolution and subsequent marriage is treated as bigamy or illicit union. Civil remarriage is legally possible but sacramentally illegitimate; remarriage outside the church risks excommunication or social exclusion. Constrained by institutional barriers and internalized shame.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__christian_canonical_reading, divorced_christians_remarrying, payer,
    moderate, biographical, constrained, national).

% Benefit from legal architecture that embeds sacramental indissolubility, limiting women's exit options from marriage and sustaining traditional household structures. Doctrinal commitment to male headship and wifely obedience is reinforced by the divorce restriction; the constraint maintains a religious-legal alignment that privileges this worldview.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__christian_canonical_reading, patriarchal_doctrine_adherents, beneficiary,
    organized, generational, mobile, national).

% Have jurisdiction over Christian marriages under the 1872 Act but defer to church authority on validity and annulment. Enforce the Act as written but increasingly field challenges on constitutional grounds (equality, religious freedom, right to dignity). Their role is to police the boundary between secular enforcement and ecclesiastical adjudication.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__christian_canonical_reading, christian_civil_courts, observer,
    institutional, generational, analytical, national).

% Argue for uniform civil code and no-fault divorce for all religious communities, including Christians. Are not seated at the table where Christian personal law is administered; their objections arise in constitutional court, legislative advocacy, and academic discourse but are excluded from canonical adjudication itself. Would reframe marriage authority away from ecclesiastical control.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__christian_canonical_reading, secular_legal_reform_advocates, excluded,
    moderate, biographical, constrained, national).

% Experience the constraint as coordination: the 1872 Act preserves church authority over their religious identity, allows church adjudication of marriage disputes according to Christian tradition, and sustains communal religious governance. For those whose beliefs align with sacramental indissolubility, the constraint vindicates their worldview and provides a familiar legal framework.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__christian_canonical_reading, christian_community_members_content, beneficiary,
    moderate, biographical, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_authority_kernel__christian_canonical_reading, christian_church_tribunals).
narrative_ontology:fixing_cost_class(marriage_authority_kernel__christian_canonical_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes Christian church tribunals as the legitimate adjudicatory body for marriage validity and annulment within the Christian community, enabling marriage disputes to be resolved according to sacramental doctrine rather than uniform civil procedure. Preserves religious community autonomy in personal law governance and embeds canonical authority in secular Indian law.
% TRANSFER_FUNCTION: Transfers authority to define the boundaries of valid marriage from civil courts to church tribunals; transfers exit from marriage (divorce) from civil procedure to ecclesiastical grounds and church adjudication. Transfers cost to those seeking divorce (especially women) in the form of restricted access, burden of proof, and sacramental penalty for remarriage.
% ABSENT_VOICES: Women who have been divorced or seek divorce (especially poor and uneducated women without resources for church litigation); interfaith couples; LGBTQ+ Christians (whose marriages are not recognized at all); Christians who reject sacramental theology. These voices are structurally excluded from canonical adjudication and their objections are heard only in constitutional court, not in the framework that directly governs them.
% DISAPPEARANCE_RATIONALE: If the 1872 Act were repealed and Christian marriages fell under uniform civil law with no-fault divorce and civil courts, church authority would dissolve, married women would gain unilateral divorce rights, remarriage would lose sacramental stigma, and Christian personal law governance would cease. The Christian community would reorganize under secular marriage law; the ecclesiastical control surface would collapse.
% FOUNDING_PROBLEM: Post-colonial India inherited Christian personal law from British colonial codification of Christian doctrine. The founding problem was to preserve Christian community governance of marriage after independence while embedding it in the new secular constitutional order — to allow Christian law to coexist with Hindu law, Muslim law, and eventually secular civil law without constitutional hierarchy.
% FOUNDING_PROBLEM_CORROBORATION: The Indian government codified this problem as legitimate in the 1872 Act (attested by British colonial records and post-independence affirmation). The Christian church attests the problem is live: community autonomy requires preservation. Secular reformers and constitutional scholars (external to the benefiting parties) attest the founding problem is antiquated: the post-colonial state has no obligation to preserve sectarian governance, and individual rights to equality and dignity now override communal authority claims.
narrative_ontology:disappearance_verdict(marriage_authority_kernel__christian_canonical_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority_kernel__christian_canonical_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority_kernel__christian_canonical_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(marriage_authority_kernel__christian_canonical_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_authority_kernel__christian_canonical_reading, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority_kernel__christian_canonical_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_authority_kernel__christian_canonical_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_authority_kernel__christian_canonical_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.58 at 2026) because the constraint imposes asymmetric exit costs: women are identity-locked (Christian identity fuses with marital status), divorced Christians cannot remarry sacramentally, and the burden of proof for annulment is high. Suppression is moderate (0.42) because the restriction is defended by doctrinal legitimacy (sacramental theology) and operates through internalized religious belief as much as through legal barriers; compliance is partly voluntary. Theater ratio is low-moderate (0.28) because the founding function (preserve church governance) remains partially real — church authority does coordinate some dispute resolution for willing participants — but an increasing share of enforcement activity defends doctrinal purity rather than genuine coordination (e.g., preventing remarriage). The measurement series show steady slow accumulation: extraction and theater have drifted upward over 150+ years as the founding problem (post-colonial religious pluralism) has become contested and constitutional challenges have mounted. Suppression stays relatively flat because the constraint's suppressive mechanism — internalized religious obligation and identity-fusion — is stable and does not depend on ratcheting legal enforcement.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter and payer seats compute fundamentally different types. Church tribunals (and their adherents) compute this as rope or tangled_rope with legitimate coordination function. Women (especially those constrained by identity-lock) compute this as snare or tangled_rope with asymmetric extraction. The engine computes per-seat; the divergence is the measurement.
 *
 * DIRECTIONALITY LOGIC:
 *   d for church tribunals: 0.0–0.15 (beneficiary, sets rules, collects authority). d for Christian women seeking divorce: 0.75–0.9 (target, identity-locked, high burden of proof, sacramental penalty). d for patriarchal doctrine adherents: 0.1–0.25 (beneficiary, worldview reinforced, exit mobile). d for divorced Christians remarrying: 0.65–0.8 (payer, constrained by sacramental stigma and legal barriers). d for secular reformers: N/A (excluded, observer in commentary only).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint faces a founding-problem mandatrophy: it was built to solve post-colonial religious pluralism (preserve Christian governance alongside Hindu, Muslim, Parsi, and secular law). This founding problem remains partially live (communal autonomy has value, church authority is functioning), but it is heavily contested. Secular constitutional scholars and human-rights advocates (external to beneficiaries) attest the problem is dead — modern constitutional rights (equality, dignity, freedom of religion) supersede claims for sectarian personal law. The theater ratio rising toward 0.28 reflects increasing administrative performance: much enforcement activity now defends sacramental indissolubility rather than genuine dispute resolution (which could be handled by secular courts). The measurement series show extractiveness accumulating slowly (0.52 → 0.58) as constitutional challenges mount and exit pressure on Christian women intensifies, but no steep ratchet. This is piton-adjacent: the constraint persists by institutional inertia (the 1872 Act remains on the books, church authority is institutionalized), the founding problem is contested, but neither party (church nor reform advocates) is mobilized enough to force resolution. The agenda-setter (church tribunals) has reduced incentive to reform because the constraint benefits them; the payers (women seeking divorce) have high identity-lock costs to organize resistance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    canonical_authority_legitimacy_contest,
    'Is ecclesiastical authority over Christian marriage a legitimate preservation of community governance, or an anachronistic sectarian override of constitutional individual rights?',
    'Constitutional court ruling on the validity of personal law systems under Articles 14 (equality) and 25 (freedom of religion); legislative reform unified civil code; empirical outcome of uniform civil code in jurisdictions that adopted it (Goa has no religion-specific marriage law); community voice from Christian women on whether they experience the constraint as protective or extractive.',
    'If legitimate: the constraint remains as communal autonomy governance, typology is tangled_rope or rope. If illegitimate: the constraint reclassifies as snare (extractive personal-law restriction unjustifiably imposed). If contested: the mandatrophy status is correct and the constraint persists by institutional inertia, not resolving consensus.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(canonical_authority_legitimacy_contest, conceptual, 'Whether ecclesiastical authority over Christian marriage in modern India is justified by community autonomy or is a violation of equal citizenship.').

omega_variable(
    sacramental_indissolubility_doctrine_grip,
    'Is the measured suppression (0.42) structural (legal barriers, institutional enforcement, cost of civil procedure) or internalized (Christian women have genuinely adopted sacramental theology and voluntarily constrain their own exit)?',
    'Trajectory of Christian women''s exit post-reform (if civil divorce becomes available to Christians, do divorce rates and remarriage rates change significantly?); qualitative interviews with Christian women on whether they experience divorce restriction as external coercion or internalized conviction; denominational variation (Catholic vs. Protestant Christians differ on indissolubility; measure exit options by denomination post-reform).',
    'If structural: suppression is a raw feature of the enforcement machinery and the constraint is snare-like. If internalized: suppression persists even after legal exit is opened, indicating identity-fusion; the constraint''s real extractiveness is higher than structural measure suggests. If split: codify the proportion in a revised omega measuring the internalized / structural split.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sacramental_indissolubility_doctrine_grip, empirical, 'Whether suppression of divorce-seeking Christian women is externally enforced or internalized through religious identity-fusion.').

omega_variable(
    identity_locked_exit_mechanisms,
    'What are the specific mechanisms by which Christian identity becomes fused with marital status, making exit from marriage feel like apostasy or community betrayal?',
    'Ethnographic study of Christian communities in India; interview cohort of Christian women who remained trapped vs. those who exited; pastoral literature and homilies on remarriage and divorce; measurement of social cost (family rejection, church exclusion, employment discrimination) against legal cost.',
    'Understanding the fusion mechanism informs whether identity-lock is educable/revisable (framing intervention) or structural (requires legal reform). If fusion is narrative-based (teachings, religious pedagogy), reform of religious teaching can shift exit options upward. If fusion is relational (community bonds), the constraint persists even after legal reform because exit carries social cost that legal access cannot undo.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_locked_exit_mechanisms, empirical, 'Mechanism of identity-fusion between Christian religious identity and marital status that locks exit.').

omega_variable(
    sibling_reading_kernel_contest,
    'Which sibling reading of the marriage-authority kernel will prevail in post-2030 India — ecclesiastical personal law, civil-code unification, or some hybrid accommodation?',
    'Outcome of constitutional court cases on Christian personal law (expected 2027–2030); legislative reform trajectory; comparative data from jurisdictions that unified civil code (Goa); political mobilization by Christian church, women''s rights advocates, and secular reformers.',
    'This reading''s persistence depends on the kernel contest outcome. If secular_civil_reading prevails, this reading reclassifies as extinct/superseded. If coexistence holds (Hindu law + Muslim law + Christian law + secular option), this reading remains a live constraint. If this reading survives as the institutional standard, it may accumulate more extraction (theater_ratio rises further) or maintain current levels with persistent contestation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_kernel_contest, empirical, 'Outcome of constitutional pluralism contest over marriage authority in India will determine the trajectory of this constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority_kernel__christian_canonical_reading, 1872, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t1872, marriage_authority_kernel__christian_canonical_reading, theater_ratio, 1872, 0.15).
narrative_ontology:measurement_basis(marr_tr_t1872, observed).
narrative_ontology:measurement(marr_tr_t1950, marriage_authority_kernel__christian_canonical_reading, theater_ratio, 1950, 0.18).
narrative_ontology:measurement_basis(marr_tr_t1950, observed).
narrative_ontology:measurement(marr_tr_t1980, marriage_authority_kernel__christian_canonical_reading, theater_ratio, 1980, 0.22).
narrative_ontology:measurement_basis(marr_tr_t1980, observed).
narrative_ontology:measurement(marr_tr_t2010, marriage_authority_kernel__christian_canonical_reading, theater_ratio, 2010, 0.26).
narrative_ontology:measurement_basis(marr_tr_t2010, observed).
narrative_ontology:measurement(marr_tr_t2020, marriage_authority_kernel__christian_canonical_reading, theater_ratio, 2020, 0.27).
narrative_ontology:measurement_basis(marr_tr_t2020, observed).
narrative_ontology:measurement(marr_tr_t2026, marriage_authority_kernel__christian_canonical_reading, theater_ratio, 2026, 0.28).
narrative_ontology:measurement_basis(marr_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(marr_be_t1872, marriage_authority_kernel__christian_canonical_reading, base_extractiveness, 1872, 0.52).
narrative_ontology:measurement_basis(marr_be_t1872, observed).
narrative_ontology:measurement(marr_be_t1950, marriage_authority_kernel__christian_canonical_reading, base_extractiveness, 1950, 0.54).
narrative_ontology:measurement_basis(marr_be_t1950, observed).
narrative_ontology:measurement(marr_be_t1980, marriage_authority_kernel__christian_canonical_reading, base_extractiveness, 1980, 0.56).
narrative_ontology:measurement_basis(marr_be_t1980, observed).
narrative_ontology:measurement(marr_be_t2010, marriage_authority_kernel__christian_canonical_reading, base_extractiveness, 2010, 0.57).
narrative_ontology:measurement_basis(marr_be_t2010, observed).
narrative_ontology:measurement(marr_be_t2020, marriage_authority_kernel__christian_canonical_reading, base_extractiveness, 2020, 0.58).
narrative_ontology:measurement_basis(marr_be_t2020, observed).
narrative_ontology:measurement(marr_be_t2026, marriage_authority_kernel__christian_canonical_reading, base_extractiveness, 2026, 0.58).
narrative_ontology:measurement_basis(marr_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t1872, marriage_authority_kernel__christian_canonical_reading, suppression_requirement, 1872, 0.38).
narrative_ontology:measurement_basis(marr_su_t1872, observed).
narrative_ontology:measurement(marr_su_t1950, marriage_authority_kernel__christian_canonical_reading, suppression_requirement, 1950, 0.39).
narrative_ontology:measurement_basis(marr_su_t1950, observed).
narrative_ontology:measurement(marr_su_t1980, marriage_authority_kernel__christian_canonical_reading, suppression_requirement, 1980, 0.4).
narrative_ontology:measurement_basis(marr_su_t1980, observed).
narrative_ontology:measurement(marr_su_t2010, marriage_authority_kernel__christian_canonical_reading, suppression_requirement, 2010, 0.41).
narrative_ontology:measurement_basis(marr_su_t2010, observed).
narrative_ontology:measurement(marr_su_t2020, marriage_authority_kernel__christian_canonical_reading, suppression_requirement, 2020, 0.42).
narrative_ontology:measurement_basis(marr_su_t2020, observed).
narrative_ontology:measurement(marr_su_t2026, marriage_authority_kernel__christian_canonical_reading, suppression_requirement, 2026, 0.42).
narrative_ontology:measurement_basis(marr_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority_kernel__christian_canonical_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(marriage_authority_kernel__christian_canonical_reading, 0.1).
narrative_ontology:affects_constraint(marriage_authority_kernel__christian_canonical_reading, marriage_authority_kernel__hindu_codified_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__christian_canonical_reading, marriage_authority_kernel__muslim_shariat_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__christian_canonical_reading, marriage_authority_kernel__parsi_communal_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__christian_canonical_reading, marriage_authority_kernel__secular_civil_reading).

% DUAL FORMULATION NOTE:
% This constraint is part of the marriage-authority-kernel constraint family (5 stories, one per reading of the contested kernel: Christian canonical, Hindu codified, Muslim Shariat, Parsi communal, secular civil). Each reading instantiates a different constraint with different ε, beneficiary/victim structure, and stakeholder seats. All five are linked via network.affects_constraints; each reading's story documents its own axioms, reference frames, and drift states. The family collectively models constitutional pluralism in Indian family law: multiple authority structures (ecclesiastical, statutory, communal, civil) coexist, compete, and influence each other. Decomposition is warranted by ε-invariance: the Christian canonical reading's referent is sacramental authority and restrictive divorce; the secular civil reading's referent is individual autonomy and no-fault dissolution; measuring both under one ε produces incoherent classification. The five readings are structurally distinct (different authority grounds, different victim sets, different founding problems) and are modeled as a network family, not as a single constraint viewed from multiple angles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(marriage_authority_kernel__christian_canonical_reading, powerless, 0.82).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
