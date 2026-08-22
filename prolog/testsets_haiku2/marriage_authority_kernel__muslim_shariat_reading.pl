% ============================================================================
% CONSTRAINT STORY: marriage_authority_kernel__muslim_shariat_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:affects_constraint/2,
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
 *   constraint_id: marriage_authority_kernel__muslim_shariat_reading
 *   human_readable: Shariat-Based Marriage Authority (Muslim Personal Law Reading)
 *   domain: legal/religious/constitutional
 *
 * SUMMARY:
 *   In India's pluralist constitutional framework, Muslims have the right to
 *   be governed by Shariat-based personal law for marriage, divorce,
 *   succession, and related matters, as interpreted by qazis and personal law
 *   boards certified by the religious community. This constraint embodies ONE
 *   READING of the contested kernel 'marriage authority' — the reading that
 *   grounds authority in Islamic jurisprudence transmitted through community
 *   institutions rather than secular civil law or codified Hindu/Christian
 *   law. The reading instantiates a coordination function (preserving
 *   religious autonomy and communal self-determination) and an extraction
 *   function (gendered asymmetry in exit, property, and authority). This
 *   story describes the standing arrangement as the Muslim personal law
 *   reading sees and justifies it; sibling readings (secular_civil_reading,
 *   hindu_codified_reading, christian_canonical_reading,
 *   parsi_communal_reading) offer alternative grounds for the same kernel and
 *   are separate constraint stories. The claim/metric gap is authored
 *   deliberately: the constraint is CLAIMED as tangled_rope (coordination +
 *   asymmetric enforcement + beneficiary/victim structure present), while the
 *   extractiveness and suppression metrics describe substantial operation —
 *   the engine decides whether the coordination justifies the extraction or
 *   whether the extraction dominates.
 *
 * KEY AGENTS:
 *   - Male householders within tradition (moderate power, identity-locked exit) — retain unilateral divorce, inheritance advantage, guardianship authority
 *   - Qazi governance structure (institutional power, arbitrage exit) — interprets Shariat, adjudicates disputes, maintains authority lineage
 *   - Communal religious authority (organized power, constrained exit) — personal law boards and mosque councils preserve interpretation tradition
 *   - Muslim women within personal law jurisdiction (powerless, identity-locked exit) — subject to unilateral talaq, narrower maintenance, half-share inheritance
 *   - State constitutional authority (institutional power, analytical exit) — holds formal ultimate authority but defers to personal law; constitutional tension between gender equality and religious autonomy unresolved
 *   - Reformist Islamic scholars (excluded, moderate power, constrained exit) — propose reinterpretations aligning with gender equality but lack institutional embedding in governance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority_kernel__muslim_shariat_reading, 0.68).
domain_priors:suppression_score(marriage_authority_kernel__muslim_shariat_reading, 0.71).
domain_priors:theater_ratio(marriage_authority_kernel__muslim_shariat_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__muslim_shariat_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(marriage_authority_kernel__muslim_shariat_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(marriage_authority_kernel__muslim_shariat_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__muslim_shariat_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(marriage_authority_kernel__muslim_shariat_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority_kernel__muslim_shariat_reading, tangled_rope).
narrative_ontology:human_readable(marriage_authority_kernel__muslim_shariat_reading, "Shariat-Based Marriage Authority (Muslim Personal Law Reading)").
narrative_ontology:topic_domain(marriage_authority_kernel__muslim_shariat_reading, "legal/religious/constitutional").

domain_priors:requires_active_enforcement(marriage_authority_kernel__muslim_shariat_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority_kernel__muslim_shariat_reading, 'eeeed1bd-7fb3-45c3-89a2-808dbd760857').
narrative_ontology:cs_kernel_codification('eeeed1bd-7fb3-45c3-89a2-808dbd760857', distributed).
narrative_ontology:cs_authority_grounding('eeeed1bd-7fb3-45c3-89a2-808dbd760857', lineage).
narrative_ontology:cs_interpretation_layer_present('eeeed1bd-7fb3-45c3-89a2-808dbd760857').
narrative_ontology:cs_reading_relation('eeeed1bd-7fb3-45c3-89a2-808dbd760857', marriage_authority_kernel__secular_civil_reading, coexists_with).
narrative_ontology:cs_reading_relation('eeeed1bd-7fb3-45c3-89a2-808dbd760857', marriage_authority_kernel__hindu_codified_reading, coexists_with).
narrative_ontology:cs_reading_relation('eeeed1bd-7fb3-45c3-89a2-808dbd760857', marriage_authority_kernel__christian_canonical_reading, coexists_with).
narrative_ontology:cs_reading_relation('eeeed1bd-7fb3-45c3-89a2-808dbd760857', marriage_authority_kernel__parsi_communal_reading, coexists_with).
narrative_ontology:cs_axiom('eeeed1bd-7fb3-45c3-89a2-808dbd760857', foundational, shariat_textual_authority_supreme).
narrative_ontology:cs_axiom_status(shariat_textual_authority_supreme, holdable).
narrative_ontology:cs_axiom_grounding('eeeed1bd-7fb3-45c3-89a2-808dbd760857', shariat_textual_authority_supreme, deontological).
narrative_ontology:cs_axiom('eeeed1bd-7fb3-45c3-89a2-808dbd760857', foundational, communal_religious_interpretation_lineage_legitimate).
narrative_ontology:cs_axiom_status(communal_religious_interpretation_lineage_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('eeeed1bd-7fb3-45c3-89a2-808dbd760857', communal_religious_interpretation_lineage_legitimate, conventional).
narrative_ontology:cs_axiom('eeeed1bd-7fb3-45c3-89a2-808dbd760857', secondary, gendered_asymmetry_islamic_jurisprudence_authentic).
narrative_ontology:cs_axiom_status(gendered_asymmetry_islamic_jurisprudence_authentic, holdable).
narrative_ontology:cs_axiom_grounding('eeeed1bd-7fb3-45c3-89a2-808dbd760857', gendered_asymmetry_islamic_jurisprudence_authentic, deontological).
narrative_ontology:cs_reference_frame('eeeed1bd-7fb3-45c3-89a2-808dbd760857', shariat_as_interpreted_by_authorized_community_lineage).
narrative_ontology:cs_drift_state('eeeed1bd-7fb3-45c3-89a2-808dbd760857', contemporary_constitutional_pluralism_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('eeeed1bd-7fb3-45c3-89a2-808dbd760857', '').
narrative_ontology:cs_kernel_id(marriage_authority_kernel__muslim_shariat_reading, marriage_authority_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__muslim_shariat_reading, male_householders_within_tradition).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__muslim_shariat_reading, qazi_governance_structure).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__muslim_shariat_reading, communal_religious_authority).
narrative_ontology:constraint_victim(marriage_authority_kernel__muslim_shariat_reading, muslim_women_within_personal_law_jurisdiction).
narrative_ontology:constraint_victim(marriage_authority_kernel__muslim_shariat_reading, non_muslim_spouses_of_muslims).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(marriage_authority_kernel__muslim_shariat_reading, muslim_men_seeking_exit_from_tradition).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retain superior marital exit rights (unilateral talaq), inheritance advantages, and authority over household decisions within the framework their community recognizes as legitimate. Talaq remains unilateral (three-word formula in strict schools); remarriage without custody loss; property inheritance preferences. Exit from this framework means exit from community identity and standing.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__muslim_shariat_reading, male_householders_within_tradition, beneficiary,
    moderate, generational, identity_locked, national).

% Interprets Shariat, adjudicates disputes, and maintains the authority structure by deciding which reading of Islamic law governs. Personal law boards certify qazis and control interpretation lineage. Derives legitimacy from textual fidelity and communal recognition; enforcement depends on state recognition of personal law jurisdiction and community deference to verdicts.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__muslim_shariat_reading, qazi_governance_structure, agenda_setter,
    institutional, civilizational, arbitrage, national).

% Personal law boards, mosque councils, and senior ulema maintain the interpretive tradition and adjudicate what counts as valid Shariat in contemporary context. They benefit from control over definition and the prestige it confers; their authority is contested by modernist/reformist interpretations within Islam and by secular constitutional authority.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__muslim_shariat_reading, communal_religious_authority, agenda_setter,
    organized, civilizational, constrained, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority_kernel__muslim_shariat_reading, communal_religious_authority, beneficiary).

% Subject to unilateral talaq (cannot initiate divorce on equal grounds in many schools), maintenance obligations post-divorce that are narrower than secular law, inheritance shares at half of male siblings' shares, restricted mahr enforcement, and limited custody rights post-remarriage. Exit means foregoing community standing, family ties, and the identity framework they were raised in. Formal equality under constitutional law is in tension with personal law application.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__muslim_shariat_reading, muslim_women_within_personal_law_jurisdiction, payer,
    powerless, biographical, identity_locked, national).

% May be subject to Muslim personal law for marriage dissolution depending on interpretation (if the Muslim spouse is male, secular courts may defer to personal law; if Muslim spouse is female, conflict of laws rules vary). No standing in qazi courts by religious definition; caught between civil law and religious law authority with no clear forum.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__muslim_shariat_reading, non_muslim_spouses_of_muslims, payer,
    powerless, biographical, trapped, national).

% If they wish to marry outside the tradition or adopt secular law principles, community and family pressure is severe; reformist interpretations exist but compete for legitimacy with orthodox schools. Formal exit to secular Special Marriage Act is available but carries social cost.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__muslim_shariat_reading, muslim_men_seeking_exit_from_tradition, payer,
    moderate, biographical, constrained, national).

% Constitution (Article 25, 26) protects religious freedom and permits personal law; Article 44 calls for uniform civil code (never enacted). Judiciary has intervened in divorce and inheritance cases; Parliament has amended Muslim personal law (1986 Triple Talaq prohibition, 2019 ban on instant talaq); tension between constitutional gender equality (Article 14, 15) and religious autonomy remains unresolved.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__muslim_shariat_reading, state_constitutional_authority, observer,
    institutional, generational, analytical, national).

% Propose reinterpretations of Shariat that align with gender equality and democratic principles (e.g., joint talaq, equal inheritance, female qazi authority). Excluded from orthodox personal law boards; sometimes heard in state courts; their alternative readings are alive in the corpus of Islamic jurisprudence but lack institutional embedding in governance.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__muslim_shariat_reading, reformist_islamic_scholars, excluded,
    moderate, biographical, constrained, national).

% Argue personal law perpetuates gender discrimination; seek uniform civil code or reformed personal law. Excluded from qazi councils; influence limited to legislative advocacy and constitutional litigation. No seat at the authority table that governs marriage for Muslims choosing personal law.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__muslim_shariat_reading, feminist_movements_and_rights_advocates, excluded,
    organized, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_authority_kernel__muslim_shariat_reading, qazi_governance_structure).
narrative_ontology:fixing_cost_class(marriage_authority_kernel__muslim_shariat_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework recognized as legitimate by Muslim communities for adjudicating marriage, divorce, maintenance, and inheritance questions according to principles they endorse as religiously grounded. Solves the problem of maintaining communal marital order within a pluralist state that respects religious autonomy.
% TRANSFER_FUNCTION: Transfers authority over marriage definition, dissolution, property, and custody from secular state courts to religious qazis and personal law boards certified by the religious community. Transfers legal recognition and enforcement capacity to outcomes that embody gendered role differentiation (unilateral male exit, female inheritance disadvantage, male guardianship norms). In effect, transfers gender-differentiated legal power from the state's gender-equal formal law to community structures that embed traditional asymmetries.
% ABSENT_VOICES: Muslim women advocating for reform within the tradition are partly excluded from orthodox qazi governance but have some legislative voice. Secular feminists are excluded from the authority structure entirely. Non-Muslim spouses are structurally excluded (no standing in religious courts). Modernist Islamic scholars offering alternative interpretations lack institutional embedding in personal law governance, though they participate in state courts.
% DISAPPEARANCE_RATIONALE: If Shariat-based personal law authority disappeared, Muslim marriage would be governed by secular civil law (Special Marriage Act 1954 or state civil codes), gender-equal inheritance would apply automatically, unilateral talaq would be abolished, and the communal religious governance structure would lose its state-recognized authority. Community identity tied to personal law would reorganize under secular law; some communities would resist the change as a loss of religious autonomy.
% FOUNDING_PROBLEM: Islamic communities in a pluralist, postcolonial state seek to preserve marriage law and governance rooted in their religious tradition rather than imposing secular or Hindu-majority law on intimate life. The founding problem is legitimate: how to maintain religious autonomy and communal self-determination in family law within a constitutional framework.
% FOUNDING_PROBLEM_CORROBORATION: Muslim personal law boards, qazi councils, and orthodox Islamic scholars attest the problem is live: religious autonomy remains contested by secular pressures and constitutional gender-equality claims. Independent observers (constitutional scholars, human rights organizations, feminist legal analysts) attest the problem is live but debate whether personal law is the right solution — some advocate reformed personal law, others uniform civil code. The problem is live; the solution is contested.
narrative_ontology:disappearance_verdict(marriage_authority_kernel__muslim_shariat_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority_kernel__muslim_shariat_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority_kernel__muslim_shariat_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(marriage_authority_kernel__muslim_shariat_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_authority_kernel__muslim_shariat_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is 0.68 at interval end, stable through the period except for modest rise from 0.62 to 0.68 in the first 20 years (reflecting gradual legislative pressure and court interventions on triple talaq and maintenance). This moderate-to-high extraction reflects the asymmetric legal power: male unilateral exit, female inheritance disadvantage, male guardianship defaults. Suppression is 0.71, high and stable, because the constraint persists partly through community enforcement of deference to qazis and partly through identity-locking (exit means loss of community standing and family ties). Theater is 0.42, moderate, reflecting genuine religious legitimacy claims alongside performative appeals to 'preserving tradition' in contexts where substantive reform pressures mount. Accessibility collapse is 0.72: once the personal law framework is understood, women's alternatives within the tradition collapse sharply (talaq is unilateral, inheritance is prescribed, maintenance is limited); exit requires leaving the community or invoking secular law, both costly. Resistance is 0.58, moderate: women's rights advocates and reformist scholars mount real opposition; state constitutional courts have intervened on specific practices (triple talaq bans, maintenance rulings); yet orthodox boards maintain institutional control. The measurement series run on one shared time grid (endpoints 0, 40, intermediate points 5–30 at 5-year intervals) so every metric is authored at every time point.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats (muslim_women, non_muslim_spouses) and the agenda-setter seats (qazi_governance_structure, communal_religious_authority) compute fundamentally different types from the same constraint. From the qazi seat, the arrangement is genuine coordination: it preserves religious autonomy, maintains a legitimate jurisprudential tradition, and solves the problem of marital order within a pluralist state. From the payer seats, the same structure is coercive extraction justified by religious authority. The male householder seat is intermediate but beneficiary-tilted: the structure benefits them, so they see coordination; yet they also bear some marriage duties, so they are not purely beneficiary. The engine computes this divergence from the structural data (beneficiary/victim declarations, power atoms, exit_options); the seated divergence is the signal the framework exists to measure.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is highly asymmetric across seats. Male householders sit near beneficiary end (d ≈ 0.2): they retain unilateral exit, inheritance advantage, and authority; while they bear some costs (maintenance obligation, support duty), the structure benefits them more than it costs. Qazi boards sit near beneficiary end (d ≈ 0.15): they collect authority, deference, and the power to define the framework; their enforcement is supported by community legitimacy and state recognition. Muslim women sit near full target end (d ≈ 0.9): they bear asymmetric exit costs, property disadvantage, and guardianship restrictions; while marriage itself is a social good, the personal law framework's distribution of power within marriage extracts from them. The non-Muslim spouse of a Muslim sits similarly high (d ≈ 0.88): caught between systems with no equal standing. Reformist scholars sit at the boundary (d ≈ 0.55): they are partly excluded from governance (costs) but retain some cultural influence (benefits). This asymmetry is the structural foundation for the tangled_rope classification: genuine coordination function (religious autonomy, communal self-determination) paired with asymmetric extraction (gendered power asymmetry embedded in the framework itself).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live (religious autonomy in a pluralist state remains contested), but the core mandate — coordinate marital order while preserving religious autonomy — is increasingly contested by constitutional gender-equality principles and legislative interventions (2019 triple talaq ban, 1986 maintenance amendments). The constraint persists not because the mandate is universally endorsed but because the authority structure that enforces it (qazi boards, community deference, state recognition of personal law jurisdiction) remains in place. This is NOT mandatrophy in the strict sense (the problem solved by the constraint has disappeared): the problem is real and alive. But it is a CONTESTATION: the constraint solves the founding problem for those who accept religious autonomy as a supreme value; it violates the constitutional mandate for gender equality for those who prioritize that. The classification as tangled_rope (not snare) reflects this: genuine coordination function paired with asymmetric extraction, requiring active enforcement, with beneficiaries and victims clearly arrayed. Snare would require the coordination story to be cover; here the coordination is real, even if the extraction is also real.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    shariat_interpretation_boundary,
    'What counts as ''Shariat'' in contemporary personal law governance — is it fixed textual law or living jurisprudential tradition subject to reinterpretation?',
    'Analysis of fatwa-issuing practices, qazi court decisions, and personal law board rulings over time: do they show conservative textual adherence, pragmatic reinterpretation, or both? Case studies of specific practices (talaq, maintenance, inheritance) tracing how interpretation evolves.',
    'If Shariat is treated as fixed text, reformist reinterpretations are delegitimated and extraction metrics remain high; if Shariat is living tradition, reformist interpretations (e.g., joint talaq, equal inheritance) become live alternatives within Islam, potentially lowering extraction for women who could invoke them.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(shariat_interpretation_boundary, conceptual, 'Whether Shariat is locked in historical codification or open to contemporary reinterpretation.').

omega_variable(
    identity_locking_mechanism,
    'Is suppression of women''s alternatives in personal law structurally imposed (external legal barriers) or internalized (women have come to believe the asymmetry is legitimate or inevitable)?',
    'Post-escape suppression trajectory: interview or longitudinal study of women who have left personal law jurisdiction for secular law or exit the community — does suppression persist after the legal structure is removed? If suppression drops sharply, it is structural; if it persists, it is partly internalized.',
    'If suppression is structural, removing personal law would immediately expand women''s options; if internalized, removing personal law alone would not restore full agency — cultural and psychological deconditioning would be necessary. The distinction affects remedies and timelines for change.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_locking_mechanism, empirical, 'Whether the constraint''s suppression of women''s options is externally imposed or internally believed.').

omega_variable(
    qazi_board_capture,
    'To what extent have qazi boards and personal law governance structures been captured by male-dominated orthodox interests, versus remaining genuinely representative of diverse Muslim community opinions on gender and interpretation?',
    'Composition analysis of qazi boards (gender, educational background, urban/rural, sect); participation data on who can bring cases or seek interpretation; comparison with opinion polling of Muslim communities on personal law reform.',
    'If capture is high, the authority structure''s claim to represent Muslim consensus is weakened, and the constraint is more snare-like (authority imposed by a faction); if diverse representation exists, the authority structure better claims to reflect genuinely contested community views, supporting the tangled_rope framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(qazi_board_capture, empirical, 'Whether qazi governance represents diverse Muslim community opinion or is captured by orthodox factions.').

omega_variable(
    constitutional_gender_equality_conflict,
    'Is the tension between Article 25 (religious autonomy) and Articles 14/15 (gender equality) resolvable within the present constitutional framework, or does it require formal amendment or complete personal law abolition?',
    'Constitutional jurisprudence analysis; legislative debates on uniform civil code; comparative study of other pluralist democracies (Canada, UK, Malaysia) and how they reconcile religious autonomy with gender equality.',
    'If resolvable via reform personal law (preserving autonomy while mandating gender equality), the constraint could stay as tangled_rope with lower extraction; if irresolvable, the kernel itself is contested at the constitutional level, and the reading''s legitimacy is fundamentally challenged.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(constitutional_gender_equality_conflict, conceptual, 'Whether constitutional religious autonomy and gender equality can coexist or require fundamental choice.').

omega_variable(
    kernel_contest_scope,
    'Are the five readings of the marriage_authority_kernel genuinely alternative framings of the same kernel, or do they instantiate different kernels altogether (e.g., Hindu codification is about nation-state secularism, Muslim personal law is about religious autonomy)?',
    'Genealogical analysis: do the five readings trace to a common constitutional commitment (pluralism + religious autonomy) or do they represent five separate historical commitments? Does the state treat them as five variants of one kernel or as five independent jurisdictions?',
    'If one kernel with five readings, the constraint family is valid and sibling relationships (coexists_with, influences) are meaningful; if five separate kernels, each reading should be isolated as its own kernel with different sibling sets and genealogies.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_contest_scope, conceptual, 'Whether the five marriage authority readings share a common contested kernel or represent separate commitments.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority_kernel__muslim_shariat_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t0, marriage_authority_kernel__muslim_shariat_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(marr_tr_t5, marriage_authority_kernel__muslim_shariat_reading, theater_ratio, 5, 0.37).
narrative_ontology:measurement(marr_tr_t10, marriage_authority_kernel__muslim_shariat_reading, theater_ratio, 10, 0.39).
narrative_ontology:measurement(marr_tr_t15, marriage_authority_kernel__muslim_shariat_reading, theater_ratio, 15, 0.4).
narrative_ontology:measurement(marr_tr_t20, marriage_authority_kernel__muslim_shariat_reading, theater_ratio, 20, 0.41).
narrative_ontology:measurement(marr_tr_t25, marriage_authority_kernel__muslim_shariat_reading, theater_ratio, 25, 0.42).
narrative_ontology:measurement(marr_tr_t30, marriage_authority_kernel__muslim_shariat_reading, theater_ratio, 30, 0.42).
narrative_ontology:measurement(marr_tr_t40, marriage_authority_kernel__muslim_shariat_reading, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(marr_be_t0, marriage_authority_kernel__muslim_shariat_reading, base_extractiveness, 0, 0.62).
narrative_ontology:measurement(marr_be_t5, marriage_authority_kernel__muslim_shariat_reading, base_extractiveness, 5, 0.64).
narrative_ontology:measurement(marr_be_t10, marriage_authority_kernel__muslim_shariat_reading, base_extractiveness, 10, 0.66).
narrative_ontology:measurement(marr_be_t15, marriage_authority_kernel__muslim_shariat_reading, base_extractiveness, 15, 0.67).
narrative_ontology:measurement(marr_be_t20, marriage_authority_kernel__muslim_shariat_reading, base_extractiveness, 20, 0.68).
narrative_ontology:measurement(marr_be_t25, marriage_authority_kernel__muslim_shariat_reading, base_extractiveness, 25, 0.68).
narrative_ontology:measurement(marr_be_t30, marriage_authority_kernel__muslim_shariat_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(marr_be_t40, marriage_authority_kernel__muslim_shariat_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t0, marriage_authority_kernel__muslim_shariat_reading, suppression_requirement, 0, 0.68).
narrative_ontology:measurement(marr_su_t5, marriage_authority_kernel__muslim_shariat_reading, suppression_requirement, 5, 0.69).
narrative_ontology:measurement(marr_su_t10, marriage_authority_kernel__muslim_shariat_reading, suppression_requirement, 10, 0.7).
narrative_ontology:measurement(marr_su_t15, marriage_authority_kernel__muslim_shariat_reading, suppression_requirement, 15, 0.71).
narrative_ontology:measurement(marr_su_t20, marriage_authority_kernel__muslim_shariat_reading, suppression_requirement, 20, 0.71).
narrative_ontology:measurement(marr_su_t25, marriage_authority_kernel__muslim_shariat_reading, suppression_requirement, 25, 0.71).
narrative_ontology:measurement(marr_su_t30, marriage_authority_kernel__muslim_shariat_reading, suppression_requirement, 30, 0.71).
narrative_ontology:measurement(marr_su_t40, marriage_authority_kernel__muslim_shariat_reading, suppression_requirement, 40, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority_kernel__muslim_shariat_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(marriage_authority_kernel__muslim_shariat_reading, 0.12).
narrative_ontology:affects_constraint(marriage_authority_kernel__muslim_shariat_reading, marriage_authority_kernel__hindu_codified_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__muslim_shariat_reading, marriage_authority_kernel__secular_civil_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__muslim_shariat_reading, marriage_authority_kernel__christian_canonical_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__muslim_shariat_reading, marriage_authority_kernel__parsi_communal_reading).

% DUAL FORMULATION NOTE:
% This constraint is ONE READING of the contested kernel marriage_authority_kernel. The kernel instantiates five structurally distinct constraints, one per reading, linked by network.affects_constraints. Each reading has a different ε (extractiveness), different beneficiary/victim structure, and different type. The Muslim Shariat reading is a tangled_rope with moderate-to-high extraction (0.68) due to gendered asymmetry in exit, property, and authority. The secular civil reading would show lower extraction (gender-equal by design) but higher universality tensions. The Hindu codified reading falls between. This decomposition avoids false averaging across readings and allows the engine to detect which reading produces which type from which seat — the measurement the kernel family apparatus exists to enable.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(marriage_authority_kernel__muslim_shariat_reading, powerless, 0.88).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
