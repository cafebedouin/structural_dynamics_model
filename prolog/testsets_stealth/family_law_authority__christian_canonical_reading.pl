% ============================================================================
% CONSTRAINT STORY: family_law_authority__christian_canonical_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_family_law_authority__christian_canonical_reading, []).

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
 *   constraint_id: family_law_authority__christian_canonical_reading
 *   human_readable: Christian Canonical Marriage Authority (Sacramental/Denominational Reading)
 *   domain: religious governance/comparative family law/political theory
 *
 * SUMMARY:
 *   This story instantiates the christian_canonical_reading of the
 *   family_law_authority kernel: marriage constituted as sacrament (Catholic)
 *   or covenant under denominational governance (Protestant), with validity,
 *   permanence, and dissolution adjudicated by ecclesiastical rather than
 *   civil authority. The arrangement genuinely coordinates — it supplies
 *   ceremony, communal recognition, validity determination, and household
 *   norms that believers value and that no other institution provides them in
 *   the same idiom — while simultaneously extracting obedience, fees,
 *   conformity, and control over intimate life decisions, with identifiable
 *   parties who bear severe costs (spouses who cannot exit, communicants
 *   barred from the central rite). The claim and metrics are authored
 *   independently: the claimed type reflects the authoring seat's structural
 *   judgment that both coordination and asymmetric extraction are present and
 *   actively enforced; the metrics describe the arrangement's observed
 *   operation, including a decades-long pattern of declining external
 *   enforcement alongside creeping extraction and growing tribunal formalism.
 *
 * KEY AGENTS:
 *   - - ecclesiastical_hierarchies: Agenda-setter (institutional/identity_locked) — sets doctrine, collects obedience and continuity; the authority claim is fused with institutional identity
 *   - - canon_law_tribunals: Agenda-setter/beneficiary (institutional/constrained) — adjudicate validity, collect fees and caseload significance
 *   - - ordained_clergy: Beneficiary (organized/constrained) — local operators collecting status and livelihood
 *   - - devout_married_laity: Beneficiary/payer (moderate/identity_locked) — receive sacramental meaning, pay obedience and conformity
 *   - - catholic_spouses_seeking_dissolution: Primary target (powerless/trapped) — bear the no-dissolution rule's full cost
 *   - - protestant_spouses_seeking_divorce: Target with partial relief (moderate/constrained) — costs vary by denomination
 *   - - divorced_remarried_communicants: Payer/excluded (moderate/identity_locked) — inside the community, outside the rite and the conversation
 *   - - civil_family_courts: Inter-institutional observer (institutional/analytical) — run the parallel civil track
 *   - - dissenting_theologians: Excluded voice (moderate/mobile) — internal reform arguments with no decision seat
 *   - - comparative_law_scholars: Analytical observer (analytical/analytical) — see the full multi-tradition structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(family_law_authority__christian_canonical_reading, 0.62).
domain_priors:suppression_score(family_law_authority__christian_canonical_reading, 0.58).
domain_priors:theater_ratio(family_law_authority__christian_canonical_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(family_law_authority__christian_canonical_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(family_law_authority__christian_canonical_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(family_law_authority__christian_canonical_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(family_law_authority__christian_canonical_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(family_law_authority__christian_canonical_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(family_law_authority__christian_canonical_reading, tangled_rope).
narrative_ontology:human_readable(family_law_authority__christian_canonical_reading, "Christian Canonical Marriage Authority (Sacramental/Denominational Reading)").
narrative_ontology:topic_domain(family_law_authority__christian_canonical_reading, "religious governance/comparative family law/political theory").

domain_priors:requires_active_enforcement(family_law_authority__christian_canonical_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(family_law_authority__christian_canonical_reading, '09e184f7-484f-453a-b194-88380a034809').
narrative_ontology:cs_kernel_codification('09e184f7-484f-453a-b194-88380a034809', fixed_text).
narrative_ontology:cs_authority_grounding('09e184f7-484f-453a-b194-88380a034809', lineage).
narrative_ontology:cs_interpretation_layer_present('09e184f7-484f-453a-b194-88380a034809').
narrative_ontology:cs_reading_relation('09e184f7-484f-453a-b194-88380a034809', family_law_authority__hindu_dharmashastra_reading, coexists_with).
narrative_ontology:cs_reading_relation('09e184f7-484f-453a-b194-88380a034809', family_law_authority__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('09e184f7-484f-453a-b194-88380a034809', family_law_authority__parsi_zoroastrian_reading, coexists_with).
narrative_ontology:cs_reading_relation('09e184f7-484f-453a-b194-88380a034809', family_law_authority__secular_contractual_reading, influences).
narrative_ontology:cs_axiom('09e184f7-484f-453a-b194-88380a034809', foundational, marriage_is_indissoluble_sacrament).
narrative_ontology:cs_axiom_status(marriage_is_indissoluble_sacrament, holdable).
narrative_ontology:cs_axiom_grounding('09e184f7-484f-453a-b194-88380a034809', marriage_is_indissoluble_sacrament, theological).
narrative_ontology:cs_axiom('09e184f7-484f-453a-b194-88380a034809', foundational, ecclesiastical_jurisdiction_over_marital_validity).
narrative_ontology:cs_axiom_status(ecclesiastical_jurisdiction_over_marital_validity, holdable).
narrative_ontology:cs_axiom_grounding('09e184f7-484f-453a-b194-88380a034809', ecclesiastical_jurisdiction_over_marital_validity, conventional).
narrative_ontology:cs_reference_frame('09e184f7-484f-453a-b194-88380a034809', indissoluble_sacramental_covenant_under_apostolic_authority).
narrative_ontology:cs_drift_state('09e184f7-484f-453a-b194-88380a034809', contemporary_secular_family_law_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('09e184f7-484f-453a-b194-88380a034809', '').
narrative_ontology:cs_kernel_id(family_law_authority__christian_canonical_reading, family_law_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(family_law_authority__christian_canonical_reading, ecclesiastical_hierarchies).
narrative_ontology:constraint_beneficiary(family_law_authority__christian_canonical_reading, ordained_clergy).
narrative_ontology:constraint_beneficiary(family_law_authority__christian_canonical_reading, devout_married_laity).
narrative_ontology:constraint_victim(family_law_authority__christian_canonical_reading, catholic_spouses_seeking_dissolution).
narrative_ontology:constraint_victim(family_law_authority__christian_canonical_reading, protestant_spouses_seeking_divorce).
narrative_ontology:constraint_victim(family_law_authority__christian_canonical_reading, divorced_remarried_communicants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(family_law_authority__christian_canonical_reading, canon_law_tribunals).
narrative_ontology:constraint_victim(family_law_authority__christian_canonical_reading, devout_married_laity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets and administers the doctrine and discipline of marriage: defines what makes a union valid, who may marry, on what terms separation or remarriage may occur, and what penalties follow nonconformity. Collects obedience, conformity, and institutional continuity from the arrangement; the authority claim over marriage is load-bearing for the institution's broader jurisdictional identity, so abandoning it would unravel far more than marriage policy.
narrative_ontology:constraint_stakeholder(family_law_authority__christian_canonical_reading, ecclesiastical_hierarchies, agenda_setter,
    institutional, generational, identity_locked, global).

% Adjudicates marital validity: receives petitions, takes testimony, issues declarations of nullity or findings of binding bond. Collects procedural fees, caseload significance, and professional purpose from the arrangement. Their docket composition and evidentiary standards are set by the hierarchy above them, limiting independent movement.
narrative_ontology:constraint_stakeholder(family_law_authority__christian_canonical_reading, canon_law_tribunals, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(family_law_authority__christian_canonical_reading, canon_law_tribunals, beneficiary).

% Officiate weddings, counsel couples, enforce communion discipline at the parish level, and refer contested cases to tribunals. They collect role status, livelihood, and pastoral authority from being the arrangement's local operators. Leaving ministry forfeits vocation, community standing, and often housing and income, so their position inside the arrangement is costly to exit.
narrative_ontology:constraint_stakeholder(family_law_authority__christian_canonical_reading, ordained_clergy, beneficiary,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(family_law_authority__christian_canonical_reading, ordained_clergy, agenda_setter).

% Receive sacramental meaning, communal recognition of their unions, ceremonial life-cycle support, and a shared normative frame for household life. They pay in obedience, financial support of the institution, conformity costs, and exposure to the same discipline machinery should their own marriages fail. Their faith identity is fused with the arrangement, so exit would mean leaving the community that constitutes their social and spiritual world.
narrative_ontology:constraint_stakeholder(family_law_authority__christian_canonical_reading, devout_married_laity, beneficiary,
    moderate, biographical, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(family_law_authority__christian_canonical_reading, devout_married_laity, payer).

% Want their marriages ended or declared void and find no recognized path to dissolution: the bond is taught as permanent, civil divorce leaves them barred from communion if they remarry, and the annulment route is uncertain, slow, expensive, and outcome-dependent on tribunal discretion. Individually they have little leverage; collectively they have historically lacked an organized voice inside synodal deliberation.
narrative_ontology:constraint_stakeholder(family_law_authority__christian_canonical_reading, catholic_spouses_seeking_dissolution, payer,
    powerless, biographical, trapped, global).

% Belong to denominations that permit divorce under defined grounds (adultery, abandonment, and in liberal denominations irretrievable breakdown), but face congregational judgment, mandatory counseling gates, pastor discretion, and in conservative bodies formal discipline or loss of membership office. Denominational variance means their costs depend heavily on which body they sit in; moving to a laxer congregation is possible but carries community rupture costs.
narrative_ontology:constraint_stakeholder(family_law_authority__christian_canonical_reading, protestant_spouses_seeking_divorce, payer,
    moderate, biographical, constrained, national).

% Live inside the community while barred from its central rite: under prevailing Catholic discipline they may not receive communion after remarriage without a nullity finding, and analogous exclusions operate in conservative Protestant bodies. Most remain members despite the bar because leaving would cost them the entire religious world they inhabit. They advocate for reception reforms but hold no formal seat in doctrinal decision-making.
narrative_ontology:constraint_stakeholder(family_law_authority__christian_canonical_reading, divorced_remarried_communicants, payer,
    moderate, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(family_law_authority__christian_canonical_reading, divorced_remarried_communicants, excluded).

% Handle marriage, divorce, custody, and support as matters of state law, independently of religious validity. In most jurisdictions a civil divorce dissolves the civil marriage regardless of ecclesiastical position, producing a dual-track system whose friction (who counts as married, who may remarry in which forum) they observe and occasionally adjudicate without resolving.
narrative_ontology:constraint_stakeholder(family_law_authority__christian_canonical_reading, civil_family_courts, observer,
    institutional, generational, analytical, national).

% Scholars and pastoral theologians who argue from inside the tradition's own sources that indissolubility discipline misreads scripture and early practice, or that epikeia and reception history justify communion for the remarried. They publish, petition, and sign reform letters but are outside the magisterial decision loop; unlike trapped members they can relocate to academies or denominations that receive their arguments.
narrative_ontology:constraint_stakeholder(family_law_authority__christian_canonical_reading, dissenting_theologians, excluded,
    moderate, biographical, mobile, continental).

% Study how religious and civil systems partition jurisdiction over marriage across traditions and centuries. They map the structure, compare the Christian canonical arrangement with dharmic, shariat, Zoroastrian, and civil-contractual systems, and take no side in the contest over which authority ought to govern.
narrative_ontology:constraint_stakeholder(family_law_authority__christian_canonical_reading, comparative_law_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(family_law_authority__christian_canonical_reading, ecclesiastical_hierarchies).
narrative_ontology:fixing_cost_class(family_law_authority__christian_canonical_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single authoritative framework within the community for forming, recognizing, and (where doctrine permits) dissolving marriages: ceremony and witness, determination of validity, pastoral dispute resolution, and shared norms for household life, solved once centrally instead of congregation by congregation.
% TRANSFER_FUNCTION: Moves obedience, conformity, fees, annulment costs, and financial support from married members and petitioners to ecclesiastical authorities and tribunals, in exchange for sacramental legitimacy, communal recognition, and access to the community's rites.
% ABSENT_VOICES: Divorced-and-remarried members, spouses unable to obtain nullity findings, women living under subordinationist readings of household doctrine, and secular partners in mixed marriages have historically had no formal seat in synods, curial reform commissions, or denominational court rulemaking; their objections arrive as advocacy from outside the decision rooms.
% DISAPPEARANCE_RATIONALE: If ecclesiastical marriage authority vanished overnight, validity and dissolution questions would shift entirely to civil courts, congregations would improvise recognition norms and fragment further over divorce practice, clergy would lose a core pastoral function, and millions of members' marital status would be redefined by whichever civil register they sat in — the arrangement the secular contractual reading describes would simply take over.
% FOUNDING_PROBLEM: Early Christian communities needed to mark their households off from surrounding marriage practices, give communal and divine weight to marital vows, regulate remarriage after widowhood and desertion, and protect widows, children, and abandoned spouses when imperial law would not.
% FOUNDING_PROBLEM_CORROBORATION: Historians of late antiquity and canon law corroborate that the original jurisdictional function was real and consequential. Civil family-court systems and secular family-law scholarship attest that household-recognition and protection problems persist but are now managed by states in most jurisdictions — corroboration from outside the benefiting parties that the founding problem survives in transmuted form, while the hierarchy alone attests that ecclesiastical jurisdiction remains necessary to answer it.
narrative_ontology:disappearance_verdict(family_law_authority__christian_canonical_reading, world_rearranges).
narrative_ontology:founding_problem_status(family_law_authority__christian_canonical_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(family_law_authority__christian_canonical_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(family_law_authority__christian_canonical_reading, 'none', 1).
narrative_ontology:epsilon_provenance(family_law_authority__christian_canonical_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(family_law_authority__christian_canonical_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(family_law_authority__christian_canonical_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(family_law_authority__christian_canonical_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate-to-high (0.62) because the arrangement transfers real goods — obedience, fees, annulment costs, control over remarriage — from members to the authority structure, while leaving genuine services in return; it is not pure rent, but the transfer is systematically asymmetric. Suppression (0.58) is substantial but falling: the era when church marriage rules carried state force in confessional jurisdictions is over, and enforcement now runs mainly through communion discipline, congregational sanction, and social pressure. Theater (0.28) is low-to-moderate: ceremonies, tribunals, and pastoral care perform real functions, but a growing share of tribunal activity consists of formalism that produces predetermined nullity outcomes for connected petitioners while denying comparable relief to the unconnected — proxy-process replacing adjudication. Accessibility_collapse (0.48) reflects that alternatives (civil marriage, civil divorce, other denominations, disaffiliation) remain available but carry severe identity and community costs for the committed. Resistance (0.52) is real and organized: reform movements, dissenting theology, divorced-Catholic advocacy, and mass defection pressure. The three temporal series share one grid (points 0-60) so every metric is authored at every examined time point; the trajectories tell one story — external enforcement decays while extraction persists and formalism grows, the classic signature of a constraint migrating from enforced operation toward inertia-borne operation.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the hierarchy's position the arrangement is divinely mandated stewardship of a sacrament — the binding character is the point, not a cost, and the seat reads as a well-functioning coordination it administers. From the trapped Catholic spouse's position the same structure operates as a sealed exit with discretionary, fee-bearing relief — the harshest seat in the story. Devout laity sit near symmetric: they receive real goods and pay real costs, and their identity fusion makes the trade feel constitutive rather than imposed. Divorced-and-remarried communicants experience exclusion administered by an institution they cannot leave. The engine derives these divergent classifications from the power, exit, and role data; nothing in the authored claim adjudicates between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality for the hierarchy, tribunals, clergy, and devout laity — the arrangement subsidizes their authority, income, status, and meaning-making respectively. Victim declarations drive high directionality for spouses seeking dissolution and divorced-and-remarried communicants; the trapped exit option of Catholic petitioners pushes them toward the full-target end, while Protestant spouses' denomination-shopping option moderates theirs. Civil courts and comparative scholars are observers with analytical exits and negligible stakes in either direction. No directionality overrides were authored: the beneficiary/victim declarations plus exit options already differentiate the seats correctly, and the override mechanism keys on power atoms that are shared by seats with genuinely different positions (several moderate-power seats diverge sharply in directionality), so a power-level override would misfire across seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — regulating households, protecting the vulnerable, giving vows communal weight — is contested rather than dead: believers still live it, while civil law has absorbed most of its jurisdictional content. That contested status is exactly why the classification matters. Reading the arrangement as pure extraction (snare) would erase the genuine coordination that devout laity demonstrably value and would mispredict the behavior of members who freely bind themselves; reading it as pure coordination (rope) would erase the trapped petitioner, the communion bar, and the annulment fee economy, whose victims are identifiable and whose relief depends on tribunal discretion. The tangled_rope classification holds both facts. The drift data flag the forward risk: if suppression continues decaying while theater keeps rising, the arrangement trends toward maintenance-by-inertia — tribunals processing formalism, discipline enforced mainly by habit — which is the piton trajectory; the rising theater series is the early indicator to watch.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint is one reading of the family_law_authority kernel; what would the sibling readings (hindu_dharmashastra, muslim_shariat, parsi_zoroastrian, secular_contractual) change structurally if instantiated instead?',
    'Comparative generation of the sibling stories: each sibling authors its own epsilon, beneficiary/victim sets, and exit topology over the same underlying population of marriages, and the corpus compares the computed classifications across readings.',
    'The victim set, the agenda-setting seat, and the disappearance verdict all rotate with the reading: under the secular contractual reading the ecclesiastical hierarchy moves from agenda-setter to excluded party, and the trapped-petitioner seat largely dissolves. Cross-reading comparison, not resolution within this file, is the deliverable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer-frame position: this file instantiates one reading of a five-reading kernel; sibling structure lives in sibling files.').

omega_variable(
    catholic_protestant_decomposition,
    'Does bundling Catholic no-dissolution with Protestant divorce-permitting governance in one reading violate epsilon-invariance — are these two constraints wearing one label?',
    'Author the Catholic indissolubility regime and the mainline Protestant disciplinary regime as separate stories and compare computed epsilon and per-seat classifications; if they diverge materially, split the family permanently.',
    'If split, the Catholic variant carries higher epsilon and a trapped-victim seat while the Protestant variant carries lower epsilon with congregation-level variance; the merged story''s intermediate metrics would then be averaging two distinct structures, and the linked stories would replace it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(catholic_protestant_decomposition, conceptual, 'Whether the reading''s internal denominational variance conceals two structurally distinct constraints.').

omega_variable(
    annulment_functional_divorce,
    'Has tribunal nullity practice become functional divorce — delivering dissolution under another name — such that the indissolubility axiom is contradicted by the arrangement''s own operating procedure?',
    'Compare nullity rates, grounds cited (psychological incapacity, simulation of consent), and time-to-decree across tribunals and decades; if decrees track civil divorce petitions closely, nullity is functioning as dissolution.',
    'Confirmation would mean the arrangement internally overrides its foundational axiom for procedurally equipped petitioners, raising theater_ratio and sharpening the asymmetry between connected and unconnected parties; refutation would support the reading''s self-description.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(annulment_functional_divorce, empirical, 'Whether the annulment machinery delivers de facto dissolution, contradicting the no-divorce premise from inside.').

omega_variable(
    enforcement_basis_shift,
    'How much of the arrangement''s current persistence rests on residual social enforcement versus voluntary identity commitment — and does the falling suppression series measure enforcement decay or successful internalization?',
    'Track communion-discipline application rates, congregational sanction cases, and disaffiliation statistics alongside survey data on why compliant members comply; distinguish fear-of-sanction compliance from identity-fused compliance.',
    'If persistence is now mostly identity-internalized, effective suppression for the committed is higher than the structural measure shows and exit costs dominate the classification; if enforcement still bites, the falling series predicts continued loosening and eventual piton drift.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(enforcement_basis_shift, empirical, 'Whether declining enforcement reflects decay or internalization, and what that implies for future drift.').

omega_variable(
    doctrinal_gender_asymmetry,
    'Do subordinationist strands of the household doctrine produce measurably asymmetric burdens on wives in contemporary practice, or has practical equality absorbed the doctrinal text?',
    'Compare outcomes by sex in tribunal petitions, congregational discipline cases, and pastoral-counseling mandates across denominations stratified by how literally the subordination texts are taught.',
    'Measured asymmetry would add a systematically sexed victim dimension the current victim declarations only partially capture and would raise effective extraction for affected seats; absorption would confirm the doctrine''s operative content is now ceremonial.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrinal_gender_asymmetry, empirical, 'Whether the doctrine''s gendered strands impose live asymmetric costs or have gone inert.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(family_law_authority__christian_canonical_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fla_ccr_tr_t0, family_law_authority__christian_canonical_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(fla_ccr_tr_t10, family_law_authority__christian_canonical_reading, theater_ratio, 10, 0.17).
narrative_ontology:measurement(fla_ccr_tr_t20, family_law_authority__christian_canonical_reading, theater_ratio, 20, 0.19).
narrative_ontology:measurement(fla_ccr_tr_t30, family_law_authority__christian_canonical_reading, theater_ratio, 30, 0.22).
narrative_ontology:measurement(fla_ccr_tr_t40, family_law_authority__christian_canonical_reading, theater_ratio, 40, 0.24).
narrative_ontology:measurement(fla_ccr_tr_t50, family_law_authority__christian_canonical_reading, theater_ratio, 50, 0.26).
narrative_ontology:measurement(fla_ccr_tr_t60, family_law_authority__christian_canonical_reading, theater_ratio, 60, 0.28).

% Extraction over time
narrative_ontology:measurement(fla_ccr_be_t0, family_law_authority__christian_canonical_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(fla_ccr_be_t10, family_law_authority__christian_canonical_reading, base_extractiveness, 10, 0.54).
narrative_ontology:measurement(fla_ccr_be_t20, family_law_authority__christian_canonical_reading, base_extractiveness, 20, 0.57).
narrative_ontology:measurement(fla_ccr_be_t30, family_law_authority__christian_canonical_reading, base_extractiveness, 30, 0.59).
narrative_ontology:measurement(fla_ccr_be_t40, family_law_authority__christian_canonical_reading, base_extractiveness, 40, 0.61).
narrative_ontology:measurement(fla_ccr_be_t50, family_law_authority__christian_canonical_reading, base_extractiveness, 50, 0.62).
narrative_ontology:measurement(fla_ccr_be_t60, family_law_authority__christian_canonical_reading, base_extractiveness, 60, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(fla_ccr_su_t0, family_law_authority__christian_canonical_reading, suppression_requirement, 0, 0.72).
narrative_ontology:measurement(fla_ccr_su_t10, family_law_authority__christian_canonical_reading, suppression_requirement, 10, 0.69).
narrative_ontology:measurement(fla_ccr_su_t20, family_law_authority__christian_canonical_reading, suppression_requirement, 20, 0.66).
narrative_ontology:measurement(fla_ccr_su_t30, family_law_authority__christian_canonical_reading, suppression_requirement, 30, 0.63).
narrative_ontology:measurement(fla_ccr_su_t40, family_law_authority__christian_canonical_reading, suppression_requirement, 40, 0.61).
narrative_ontology:measurement(fla_ccr_su_t50, family_law_authority__christian_canonical_reading, suppression_requirement, 50, 0.6).
narrative_ontology:measurement(fla_ccr_su_t60, family_law_authority__christian_canonical_reading, suppression_requirement, 60, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(family_law_authority__christian_canonical_reading, identity_coordination).
narrative_ontology:affects_constraint(family_law_authority__christian_canonical_reading, hindu_dharmashastra_reading).
narrative_ontology:affects_constraint(family_law_authority__christian_canonical_reading, muslim_shariat_reading).
narrative_ontology:affects_constraint(family_law_authority__christian_canonical_reading, parsi_zoroastrian_reading).
narrative_ontology:affects_constraint(family_law_authority__christian_canonical_reading, secular_contractual_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'religious marriage law' covers five structurally distinct arrangements of the family_law_authority kernel, decomposed per the epsilon-invariance principle into five stories sharing one referent population (marriages and their dissolution disputes) but differing in authority locus, victim sets, and exit topology. This story links to all four siblings; the secular_contractual_reading is the downstream successor arrangement in most jurisdictions — civil law's absorption of jurisdiction creates the structural pressure (annulment expansion, denominational liberalization) visible in this reading's drift_state, while this reading's remaining jurisdictional pockets (communion discipline, denominational courts) continue to shape the civil sibling's operating environment in confessional-influenced jurisdictions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
