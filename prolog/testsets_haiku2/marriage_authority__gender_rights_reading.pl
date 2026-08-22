% ============================================================================
% CONSTRAINT STORY: marriage_authority__gender_rights_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_authority__gender_rights_reading, []).

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
 *   constraint_id: marriage_authority__gender_rights_reading
 *   human_readable: Patriarchal Personal Law Authority — Gender Rights Constitutional Challenge
 *   domain: legal_pluralism/constitutional_law
 *
 * SUMMARY:
 *   This constraint instantiates the GENDER-RIGHTS READING of the
 *   marriage-authority kernel. The kernel is a contested commitment: who has
 *   authority to define marriage, divorce, property rights, and maintenance
 *   obligations — religious communities, the state legislature, or courts
 *   interpreting constitutional equality? The gender-rights reading asserts
 *   that constitutional equality guarantees override communal authority when
 *   personal law imposes gender-specific harms. Patriarchal family-law
 *   practices (unilateral talaq, denial of maintenance, restricted
 *   inheritance) constitute extractive constraints on women within those
 *   communities; constitutional courts can and should strike them down. This
 *   reading sits in structural opposition to the communal-autonomy reading
 *   (which treats personal law pluralism as a right, not a constraint) and
 *   coexists with the secularist reading (which sees the entire personal-law
 *   regime as temporary) and the judicial-harmonization reading (which seeks
 *   incremental reform rather than wholesale rejection). The claim/metric gap
 *   is INTENTIONAL: the constraint is CLAIMED as snare (women are
 *   systematically harmed; no coordination justifies the gender specificity)
 *   while the authored metrics describe high extraction with elevated theater
 *   — judicial language emphasizes women's protection and communal religious
 *   respect, but enforcement machinery defends a fundamentally patriarchal
 *   authority structure.
 *
 * KEY AGENTS:
 *   - women_within_patriarchal_personal_law (powerless, identity-locked) — the constraint's primary victims; denied remedies for divorce, maintenance, property
 *   - communal_religious_authorities (organized) — the constraint's agenda-setter and secondary beneficiary; preserve interpretive monopoly, resist judicial interference
 *   - state_judiciary (institutional) — secondary agenda-setter; expand constitutional equality into personal law via case-by-case review
 *   - women_rights_advocates (organized, mobile) — external beneficiaries; mobilize constitutional claims, litigate public-interest cases, publicize victories
 *   - legislative_majority (institutional, analytical) — observer seat; could enact UCC but politically avoids the decision
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority__gender_rights_reading, 0.81).
domain_priors:suppression_score(marriage_authority__gender_rights_reading, 0.76).
domain_priors:theater_ratio(marriage_authority__gender_rights_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority__gender_rights_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(marriage_authority__gender_rights_reading, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(marriage_authority__gender_rights_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority__gender_rights_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(marriage_authority__gender_rights_reading, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority__gender_rights_reading, snare).
narrative_ontology:human_readable(marriage_authority__gender_rights_reading, "Patriarchal Personal Law Authority — Gender Rights Constitutional Challenge").
narrative_ontology:topic_domain(marriage_authority__gender_rights_reading, "legal_pluralism/constitutional_law").

domain_priors:requires_active_enforcement(marriage_authority__gender_rights_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority__gender_rights_reading, 'ea01f088-736e-419f-8718-0fc5a41cdbee').
narrative_ontology:cs_kernel_codification('ea01f088-736e-419f-8718-0fc5a41cdbee', formalized).
narrative_ontology:cs_authority_grounding('ea01f088-736e-419f-8718-0fc5a41cdbee', extraction).
narrative_ontology:cs_interpretation_layer_present('ea01f088-736e-419f-8718-0fc5a41cdbee').
narrative_ontology:cs_reading_relation('ea01f088-736e-419f-8718-0fc5a41cdbee', marriage_authority__communal_autonomy_reading, forecloses).
narrative_ontology:cs_reading_relation('ea01f088-736e-419f-8718-0fc5a41cdbee', marriage_authority__secularist_reading, coexists_with).
narrative_ontology:cs_reading_relation('ea01f088-736e-419f-8718-0fc5a41cdbee', marriage_authority__federalist_millet_reading, influences).
narrative_ontology:cs_reading_relation('ea01f088-736e-419f-8718-0fc5a41cdbee', marriage_authority__judicial_harmonization_reading, coexists_with).
narrative_ontology:cs_axiom('ea01f088-736e-419f-8718-0fc5a41cdbee', foundational, constitutional_gender_equality_non_negotiable).
narrative_ontology:cs_axiom_status(constitutional_gender_equality_non_negotiable, holdable).
narrative_ontology:cs_axiom_grounding('ea01f088-736e-419f-8718-0fc5a41cdbee', constitutional_gender_equality_non_negotiable, deontological).
narrative_ontology:cs_axiom('ea01f088-736e-419f-8718-0fc5a41cdbee', foundational, religious_authority_constrained_by_human_rights).
narrative_ontology:cs_axiom_status(religious_authority_constrained_by_human_rights, holdable).
narrative_ontology:cs_axiom_grounding('ea01f088-736e-419f-8718-0fc5a41cdbee', religious_authority_constrained_by_human_rights, deontological).
narrative_ontology:cs_reference_frame('ea01f088-736e-419f-8718-0fc5a41cdbee', constitutional_equality_supremacy).
narrative_ontology:cs_drift_state('ea01f088-736e-419f-8718-0fc5a41cdbee', contemporary_post_abolition_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('ea01f088-736e-419f-8718-0fc5a41cdbee', '').
narrative_ontology:cs_kernel_id(marriage_authority__gender_rights_reading, marriage_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority__gender_rights_reading, women_rights_advocates).
narrative_ontology:constraint_beneficiary(marriage_authority__gender_rights_reading, judicial_reformers).
narrative_ontology:constraint_victim(marriage_authority__gender_rights_reading, women_within_patriarchal_personal_law).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(marriage_authority__gender_rights_reading, communal_religious_authorities).
narrative_ontology:constraint_beneficiary(marriage_authority__gender_rights_reading, religious_community_members_conservative).
narrative_ontology:constraint_victim(marriage_authority__gender_rights_reading, religious_community_members_conservative).
narrative_ontology:constraint_vindicates(marriage_authority__gender_rights_reading, constitutional_equality_guarantee).
narrative_ontology:constraint_vindicates(marriage_authority__gender_rights_reading, intra_community_human_rights).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Subject to personal law codes that permit unilateral divorce (triple talaq), restrict maintenance rights, limit inheritance, and condition marital dissolution on male consent or financial settlement they cannot afford. Exit from the marriage requires either acceptance of abandonment without support or navigation of courts designed to defend the marital regime. Exit from the community that enforces these laws is culturally and economically catastrophic — identity and belonging are constituted through the community's framework.
narrative_ontology:constraint_stakeholder(marriage_authority__gender_rights_reading, women_within_patriarchal_personal_law, payer,
    powerless, biographical, identity_locked, national).

% Adjudicate marriage, divorce, maintenance, and inheritance under traditional religious law. Preserve the authority's interpretive monopoly over family law for their community. Benefit from the state's delegation of family law authority and from the institutional legitimacy that personal-law pluralism accords them. Can resist judicial reform through legislative advocacy and doctrinal retrenchment, but judicial review threatens their authority structure directly.
narrative_ontology:constraint_stakeholder(marriage_authority__gender_rights_reading, communal_religious_authorities, agenda_setter,
    organized, civilizational, mobile, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority__gender_rights_reading, communal_religious_authorities, beneficiary).

% Interprets the constitutional equality guarantee and its application to personal law. Increasingly issues orders striking down specific practices (triple talaq, denial of maintenance) and reading constitutional floors into personal law codes. Occupies the structural position to expand or contract gender-equality enforcement without waiting for legislative action. Can be pressured by both gender-rights advocates (demand more) and religious-authority defenders (demand less).
narrative_ontology:constraint_stakeholder(marriage_authority__gender_rights_reading, state_judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Mobilize constitutional equality claims to challenge specific patriarchal practices in personal law. Litigate cases, file public-interest petitions, and publicize judicial victories as evidence that constitutional authority overrides communal autonomy on gender equality. Benefit from judicial victories that expand women's rights within the personal-law regime without requiring formal legislative change. Do not themselves inhabit the personal-law regime — they are external advocates using the state's judicial machinery.
narrative_ontology:constraint_stakeholder(marriage_authority__gender_rights_reading, women_rights_advocates, beneficiary,
    organized, biographical, mobile, national).

% Could in principle enact a Uniform Civil Code (UCC) that would replace personal law pluralism with secular, gender-equal family law for all citizens. Has not done so, partly from political caution (religious-community coalitions retain blocking power) and partly from deference to personal-law pluralism as a consociational settlement. Watches judicial expansion with ambivalence — it shifts de facto law without legislative legitimacy, but also avoids the political cost of the UCC.
narrative_ontology:constraint_stakeholder(marriage_authority__gender_rights_reading, legislative_majority, observer,
    institutional, generational, analytical, national).

% Benefit from religious-authority autonomy (their community's law is not subject to majoritarian democratic revision) and from the personal-law regime's validation of their religious identity. Also pay when judicial expansion restricts practices their tradition considers legitimate or when women's rights advocates' victories are publicized as cultural critique. Experience the constraint as cultural protection (beneficiary framing) and as increasing state interference in communal self-governance (payer framing simultaneously).
narrative_ontology:constraint_stakeholder(marriage_authority__gender_rights_reading, religious_community_members_conservative, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority__gender_rights_reading, religious_community_members_conservative, beneficiary).

% Document and analyze the constraint's operation as a case of judicial imperialism vs. human-rights constitutionalism, religious pluralism vs. gender equality, or consociational democracy vs. majoritarian rights. Produce the interpretive frames that sustain and contest the constraint. Their scholarship does not directly shift the constraint but shapes how it is understood by judges, advocates, and policymakers.
narrative_ontology:constraint_stakeholder(marriage_authority__gender_rights_reading, comparative_legal_scholars, observer,
    organized, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_authority__gender_rights_reading, communal_religious_authorities).
narrative_ontology:fixing_cost_class(marriage_authority__gender_rights_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Personal law pluralism coordinates religious-community autonomy with secular state authority: rather than require a single civil code, the state delegates family law authority to religious authorities, reducing conflict over cultural and religious identity while maintaining state oversight for contract and public order. Each community governs its own marriage, divorce, and inheritance norms.
% TRANSFER_FUNCTION: Moves decision-making authority (and the rents and legitimacy from that authority) from democratic-legislative channels to religious-communal authorities. Moves gender-equality protections from women within those communities to judicial advocacy seats outside them. In the gender-rights reading: constrains women's bodily autonomy, economic security, and legal remedies to the patriarchal norms that religious authorities adjudicate, while validating communal religious identity as a superior claim against state-level equality guarantees.
% ABSENT_VOICES: The women most constrained by patriarchal personal law (especially divorced, widowed, or estranged women without family protection and without resources to litigate) are largely absent from legal argument. Their voices enter the record only through women's-rights advocates' public-interest litigation — they do not themselves adjudicate, negotiate, or challenge the constraint directly. Communities offering alternative (less patriarchal) interpretations of the same religious tradition are structurally excluded by the personal-law regime's deference to established religious authorities.
% DISAPPEARANCE_RATIONALE: If the personal law regime vanished and women's equality claims systematically prevailed in courts (or a UCC were enacted), marriage and divorce law would shift to gender-equal default rules (mutual consent, equal maintenance rights, joint property). Millions of women within patriarchal personal-law regimes would gain immediate legal remedies for practices now permitted (unilateral talaq, denial of maintenance, property exclusion). Religious communities would lose institutional autonomy over family law, and religious identity would no longer carry the same family-law force. The world rearranges — not as much because marriage would disappear, but because the authority structure that governs it would invert.
% FOUNDING_PROBLEM: Religious-community autonomy over personal law emerged as a decolonization and consociational settlement: to avoid majoritarian Hindu-law imposition on minority religious communities (Muslim, Christian, Sikh, Parsi communities), the state constitutionally protected personal law pluralism. Family law authority was delegated to each community's religious authorities, preventing cultural domination while maintaining state security and public-order oversight.
% FOUNDING_PROBLEM_CORROBORATION: The religious-community establishments attest the founding problem is live and permanent — cultural domination remains an ongoing threat, and personal-law autonomy is the only protection against majoritarian revision. Women's-rights advocates, constitutional scholars, and some judicial opinions attest the founding problem is partially obsolete as applied to gender equality — cultural autonomy does not justify women's subjugation, and the state's commitment to constitutional equality should override communal authority on fundamental rights. No third party outside both camps has issued definitive attestation; the divide is fundamentally perspectival.
narrative_ontology:disappearance_verdict(marriage_authority__gender_rights_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority__gender_rights_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority__gender_rights_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(marriage_authority__gender_rights_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_authority__gender_rights_reading, 0.81, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority__gender_rights_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_authority__gender_rights_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_authority__gender_rights_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness at 0.81 (high) reflects that personal law systematically constrains women's divorce rights, maintenance, property, and bodily autonomy to patriarchal norms. The constraint persists through legal authorization (state-delegated authority) + institutional enforcement (religious authorities adjudicate, secular courts defer except where 'unconstitutional'). Theater at 0.42 (moderate) because judicial reform language emphasizes protection and respect for religious community, but the enforcement machinery actually defends gender hierarchy — expansion of constitutional review is itself performative (judges strike down the worst practices while preserving the regime's fundamental patriarchy). Suppression at 0.76 is high because women within the regime have few exits: identity-lock (religious community identity fused with marital legal status), economic dependency (divorce without maintenance is destitution), and normative isolation (appeals to 'tradition' and 'community values' delegitimize resistance). The temporal series shows extractiveness rising then plateauing (phase 1: institutional entrenchment; phase 2: judicial intervention reaching a steady state of reform-without-revolution), theater rising as judicial review creates appearance of reform, and suppression rising as the constraint adapts to judicial pressure by reformulating itself in constitutional language. The plateaus at T=25+ mark the constraint's drift into a new equilibrium where judicial wins on specific practices coexist with systemic gender inequality remaining unchallenged.
 *
 * PERSPECTIVAL GAP:
 *   The women-within-patriarchal-law seat and the communal-authority seat compute radically differently. From women's position: the constraint is a snare (systematic gender harm, suppressed exit, identity-lock, powerless to change the regime). From communal authorities' position: it is rope (legitimate coordination of religious community autonomy, maintained with consent of the faithful, threatened by judicial imperialism). The engine computes these divergences from the authored power/exit/beneficiary data — the women are genuinely powerless and identity-locked; the authorities genuinely hold power and mobility. Judiciary sits asymmetrically: from the gender-rights reading's perspective, they are expanding constitutional equality; from the communal-authority perspective, they are eroding religious autonomy. The divergence is structural, not perspectival opinion — exit options differ, power differs, beneficiary/victim position differs.
 *
 * DIRECTIONALITY LOGIC:
 *   Women within patriarchal personal law: directionality approaches 1.0 (full target). They bear all the costs (restricted divorce, denied maintenance, property exclusion, identity-lock), collect almost no benefits (some indirect benefit from communal cohesion, but heavily outweighed), and have trapped exit. Religious authorities: directionality near 0.0 (beneficiary). They collect institutional authority, rents from legal monopoly, and validation from state pluralism. Women's-rights advocates: directionality near 0.0 (beneficiary). They collect victories, public attention, institutional legitimacy from the gender-rights movement, and do not themselves inhabit the constraining regime. State judiciary: directionality ~0.5 (symmetric or oscillating). They balance competing constitutional claims (equality vs. religious autonomy) and experience pressure from both sides; their power position gives them exit options that let them shift the balance without fully endorsing either reading.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (personal law pluralism as religious-community protection against majoritarian domination) is increasingly obsolete as applied to gender equality. The founding problem it was built to solve (cultural erasure of minority religious communities) is still live, but the solution — delegating all family law to religious authorities without constraint — is now understood to create a gender-equality problem that the mandate does not address. Judicial expansion fills the gap (imposing constitutional floors) but cannot fully resolve it without collapsing the personal-law regime itself. The constraint persists because (1) the political cost of formal legislative change (a UCC) is high, (2) the religious communities retain blocking power, and (3) the current equilibrium (judicial reform + personal law coexistence) suits neither gender advocates nor communal authorities perfectly but prevents total defeat for either. This is classic mandatrophy drift: the arrangement was designed to solve religious-community autonomy (which persists), not to address gender equality (which is increasingly constraining but not the constraint's mandate), so reform efforts address the mandate gap without dismantling the mandate itself.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    religious_authority_legitimacy_source,
    'Does religious authority over personal law derive legitimacy from religious doctrine itself, or from state delegation + community consent? If the former, can constitutional equality constrain it? If the latter, can the state withdraw it?',
    'Textual analysis of religious doctrinal sources and comparative study of how religious authorities themselves justify their family-law authority in jurisdictions with and without state delegation.',
    'If legitimacy derives from doctrine, courts cannot override it without delegitimizing the authority; if it derives from state delegation, courts can impose constitutional constraints. The distinction determines whether judicial expansion is constitutional review (legitimate) or doctrinal usurpation (illegitimate).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(religious_authority_legitimacy_source, conceptual, 'Source of religious authority legitimacy — intrinsic vs. delegated.').

omega_variable(
    gender_equality_magnitude_in_personal_law,
    'What fraction of family law injustice experienced by women in patriarchal personal-law regimes is attributable to (a) the regime''s gender-specific rules vs. (b) women''s broader economic and social inequality? How much would judicial strikes on gender-specific practices alone improve women''s situation?',
    'Empirical study of women''s outcomes before and after specific judicial victories (triple talaq struck down, maintenance expanded, etc.) vs. outcomes where economic/social inequality persists.',
    'High gender-specificity would support the snare classification and the view that targeted judicial reform can meaningfully help; high background-inequality would suggest the constraint is less about explicit rules than about structural subordination, requiring systemic (not judicial) reform.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(gender_equality_magnitude_in_personal_law, empirical, 'Extent to which gender inequality in personal law is due to explicit rules vs. structural inequality.').

omega_variable(
    identity_lock_mechanism_in_religious_community,
    'Is the exit-lock (''identity_locked'' on the women-within-patriarchal-law seat) structurally imposed (the community punishes exit with exclusion/loss of family) or internalized (women have fused their self-concept with the community''s law and cannot imagine exit)?',
    'Qualitative study of women who exit patriarchal personal-law regimes: do they report external punishment as the primary barrier, or internal incompatibility between exit and self-conception? What changes if the external punishment is removed?',
    'If structural, removing the exit barrier (e.g., anti-excommunication law) would enable exit; if internalized, the constraint persists even after structural barriers fall. High internalization suggests the suppression metric understates the true constraint on autonomy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_in_religious_community, empirical, 'Identity-lock mechanism: structural punishment vs. internalized identity fusion.').

omega_variable(
    judicial_expansion_as_imperialism_or_rights_protection,
    'Is the gender-rights reading''s reliance on judicial constitutional authority itself extractive — imposing external (majoritarian/secular) values on minority communities — or is it the only available mechanism for protecting women''s rights within those communities?',
    'Comparative analysis of women''s rights outcomes in systems where personal law is judicially constrained (this reading''s approach) vs. systems with legislative UCC vs. systems with strong personal-law autonomy and no judicial override.',
    'If judicial constraint produces better outcomes for women than alternatives, it mitigates the imperialism concern; if it produces worse outcomes (e.g., by delegitimizing religious authorities and reducing their responsiveness to women''s grievances), the reading''s framing reverses.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(judicial_expansion_as_imperialism_or_rights_protection, empirical, 'Comparative effectiveness of judicial vs. legislative vs. communal approaches to women''s rights in personal-law systems.').

omega_variable(
    reading_foreclosure_logic,
    'Does the gender-rights reading''s commitment to constitutional equality as a non-negotiable floor logically foreclose the communal-autonomy reading, or do they occupy genuinely different frameworks (one rights-centered, one autonomy-centered) that need not contradict?',
    'Philosophical/jurisprudential analysis: can a legal system simultaneously guarantee constitutional equality on gender AND respect religious community autonomy on family law for all matters? Or is there a hard logical incompatibility at the boundary cases (e.g., divorce)?',
    'If genuinely foreclosed, the readings cannot coexist in a single legal framework; the constraint''s resolution requires choosing one. If they can coexist (e.g., autonomy applies within a constitutional floor), the framework is underdetermined and both readings remain live.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_logic, conceptual, 'Logical relationship between gender-equality and communal-autonomy readings — foreclosure vs. coexistence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority__gender_rights_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t0, marriage_authority__gender_rights_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(marr_tr_t0, observed).
narrative_ontology:measurement(marr_tr_t5, marriage_authority__gender_rights_reading, theater_ratio, 5, 0.32).
narrative_ontology:measurement_basis(marr_tr_t5, observed).
narrative_ontology:measurement(marr_tr_t10, marriage_authority__gender_rights_reading, theater_ratio, 10, 0.36).
narrative_ontology:measurement_basis(marr_tr_t10, observed).
narrative_ontology:measurement(marr_tr_t15, marriage_authority__gender_rights_reading, theater_ratio, 15, 0.38).
narrative_ontology:measurement_basis(marr_tr_t15, observed).
narrative_ontology:measurement(marr_tr_t20, marriage_authority__gender_rights_reading, theater_ratio, 20, 0.41).
narrative_ontology:measurement_basis(marr_tr_t20, observed).
narrative_ontology:measurement(marr_tr_t25, marriage_authority__gender_rights_reading, theater_ratio, 25, 0.42).
narrative_ontology:measurement_basis(marr_tr_t25, observed).
narrative_ontology:measurement(marr_tr_t30, marriage_authority__gender_rights_reading, theater_ratio, 30, 0.42).
narrative_ontology:measurement_basis(marr_tr_t30, observed).
narrative_ontology:measurement(marr_tr_t35, marriage_authority__gender_rights_reading, theater_ratio, 35, 0.42).
narrative_ontology:measurement_basis(marr_tr_t35, observed).

% Extraction over time
narrative_ontology:measurement(marr_be_t0, marriage_authority__gender_rights_reading, base_extractiveness, 0, 0.68).
narrative_ontology:measurement_basis(marr_be_t0, observed).
narrative_ontology:measurement(marr_be_t5, marriage_authority__gender_rights_reading, base_extractiveness, 5, 0.71).
narrative_ontology:measurement_basis(marr_be_t5, observed).
narrative_ontology:measurement(marr_be_t10, marriage_authority__gender_rights_reading, base_extractiveness, 10, 0.74).
narrative_ontology:measurement_basis(marr_be_t10, observed).
narrative_ontology:measurement(marr_be_t15, marriage_authority__gender_rights_reading, base_extractiveness, 15, 0.76).
narrative_ontology:measurement_basis(marr_be_t15, observed).
narrative_ontology:measurement(marr_be_t20, marriage_authority__gender_rights_reading, base_extractiveness, 20, 0.79).
narrative_ontology:measurement_basis(marr_be_t20, observed).
narrative_ontology:measurement(marr_be_t25, marriage_authority__gender_rights_reading, base_extractiveness, 25, 0.81).
narrative_ontology:measurement_basis(marr_be_t25, observed).
narrative_ontology:measurement(marr_be_t30, marriage_authority__gender_rights_reading, base_extractiveness, 30, 0.81).
narrative_ontology:measurement_basis(marr_be_t30, observed).
narrative_ontology:measurement(marr_be_t35, marriage_authority__gender_rights_reading, base_extractiveness, 35, 0.81).
narrative_ontology:measurement_basis(marr_be_t35, observed).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t0, marriage_authority__gender_rights_reading, suppression_requirement, 0, 0.61).
narrative_ontology:measurement_basis(marr_su_t0, observed).
narrative_ontology:measurement(marr_su_t5, marriage_authority__gender_rights_reading, suppression_requirement, 5, 0.64).
narrative_ontology:measurement_basis(marr_su_t5, observed).
narrative_ontology:measurement(marr_su_t10, marriage_authority__gender_rights_reading, suppression_requirement, 10, 0.68).
narrative_ontology:measurement_basis(marr_su_t10, observed).
narrative_ontology:measurement(marr_su_t15, marriage_authority__gender_rights_reading, suppression_requirement, 15, 0.71).
narrative_ontology:measurement_basis(marr_su_t15, observed).
narrative_ontology:measurement(marr_su_t20, marriage_authority__gender_rights_reading, suppression_requirement, 20, 0.74).
narrative_ontology:measurement_basis(marr_su_t20, observed).
narrative_ontology:measurement(marr_su_t25, marriage_authority__gender_rights_reading, suppression_requirement, 25, 0.75).
narrative_ontology:measurement_basis(marr_su_t25, observed).
narrative_ontology:measurement(marr_su_t30, marriage_authority__gender_rights_reading, suppression_requirement, 30, 0.76).
narrative_ontology:measurement_basis(marr_su_t30, observed).
narrative_ontology:measurement(marr_su_t35, marriage_authority__gender_rights_reading, suppression_requirement, 35, 0.76).
narrative_ontology:measurement_basis(marr_su_t35, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority__gender_rights_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(marriage_authority__gender_rights_reading, 0.12).
narrative_ontology:affects_constraint(marriage_authority__gender_rights_reading, marriage_authority__communal_autonomy_reading).
narrative_ontology:affects_constraint(marriage_authority__gender_rights_reading, marriage_authority__secularist_reading).
narrative_ontology:affects_constraint(marriage_authority__gender_rights_reading, marriage_authority__judicial_harmonization_reading).
narrative_ontology:affects_constraint(marriage_authority__gender_rights_reading, marriage_authority__federalist_millet_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of five readings of the marriage_authority kernel. Each reading instantiates a different structural constraint with different beneficiaries, victims, and ε values. The gender-rights reading treats patriarchal personal law as a snare (high extraction from women, suppressed exit, identity-lock). Sibling readings include communal-autonomy (treats personal law as rope, emphasis on religious self-determination), secularist (treats personal law as temporary scaffolding pending UCC), and judicial-harmonization (treats personal law as piton undergoing incremental reform). The five readings are structurally distinct claims about the same kernel; they should not be merged into a single story. Decomposition follows ε-invariance principle: measuring the kernel via 'women's rights impact' gives high ε (snare reading); measuring via 'community autonomy preservation' gives low ε (rope reading). The same kernel, different measurements, different constraints — two constraint stories, not one.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
