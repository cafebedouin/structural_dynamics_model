% ============================================================================
% CONSTRAINT STORY: marriage_authority_kernel__hindu_codified_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_authority_kernel__hindu_codified_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: marriage_authority_kernel__hindu_codified_reading
 *   human_readable: Hindu Codified Marriage Authority (Civil Court Interpretation)
 *   domain: constitutional/religious governance
 *
 * SUMMARY:
 *   The Hindu Marriage Act 1955 codified Hindu marriage law and vested
 *   interpretive authority in state civil courts, displacing community-based
 *   religious authority. The Act is presented as a modern, uniform, secular
 *   solution to fragmented pre-1955 custom law. However, structural analysis
 *   reveals a tangled arrangement: the Act coordinates across Hindu
 *   communities (genuine coordination function), but simultaneously extracts
 *   authority from religious bodies and enforces patriarchal structures
 *   through state machinery (asymmetric extraction). Women gain legal
 *   recourse to courts but lose community negotiation leverage. LGBTQ
 *   individuals and religious minorities within Hindu law are excluded. The
 *   constraint has measurably extractive properties despite being framed as
 *   secular modernization. This story instantiates ONE READING of the
 *   marriage-authority-kernel, in which Hindu identity, codified law, and
 *   civil courts form the legitimacy triangle. Other readings (Muslim
 *   Shariat, Christian canonical, Parsi communal, secular civil) are sibling
 *   constraints, not alternative interpretations of this one.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority_kernel__hindu_codified_reading, 0.58).
domain_priors:suppression_score(marriage_authority_kernel__hindu_codified_reading, 0.52).
domain_priors:theater_ratio(marriage_authority_kernel__hindu_codified_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__hindu_codified_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(marriage_authority_kernel__hindu_codified_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(marriage_authority_kernel__hindu_codified_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__hindu_codified_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(marriage_authority_kernel__hindu_codified_reading, resistance, 0.54).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority_kernel__hindu_codified_reading, tangled_rope).
narrative_ontology:human_readable(marriage_authority_kernel__hindu_codified_reading, "Hindu Codified Marriage Authority (Civil Court Interpretation)").
narrative_ontology:topic_domain(marriage_authority_kernel__hindu_codified_reading, "constitutional/religious governance").

domain_priors:requires_active_enforcement(marriage_authority_kernel__hindu_codified_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority_kernel__hindu_codified_reading, '330a05da-fd4c-4740-b9e7-619f25bf1a79').
narrative_ontology:cs_kernel_codification('330a05da-fd4c-4740-b9e7-619f25bf1a79', formalized).
narrative_ontology:cs_authority_grounding('330a05da-fd4c-4740-b9e7-619f25bf1a79', lineage).
narrative_ontology:cs_interpretation_layer_present('330a05da-fd4c-4740-b9e7-619f25bf1a79').
narrative_ontology:cs_reading_relation('330a05da-fd4c-4740-b9e7-619f25bf1a79', marriage_authority_kernel__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('330a05da-fd4c-4740-b9e7-619f25bf1a79', marriage_authority_kernel__christian_canonical_reading, coexists_with).
narrative_ontology:cs_reading_relation('330a05da-fd4c-4740-b9e7-619f25bf1a79', marriage_authority_kernel__parsi_communal_reading, coexists_with).
narrative_ontology:cs_reading_relation('330a05da-fd4c-4740-b9e7-619f25bf1a79', marriage_authority_kernel__secular_civil_reading, influences).
narrative_ontology:cs_axiom('330a05da-fd4c-4740-b9e7-619f25bf1a79', foundational, hindu_law_state_codeifiable).
narrative_ontology:cs_axiom_status(hindu_law_state_codeifiable, holdable).
narrative_ontology:cs_axiom_grounding('330a05da-fd4c-4740-b9e7-619f25bf1a79', hindu_law_state_codeifiable, empirically_contingent).
narrative_ontology:cs_axiom('330a05da-fd4c-4740-b9e7-619f25bf1a79', foundational, civil_court_secular_authority_superior).
narrative_ontology:cs_axiom_status(civil_court_secular_authority_superior, holdable).
narrative_ontology:cs_axiom_grounding('330a05da-fd4c-4740-b9e7-619f25bf1a79', civil_court_secular_authority_superior, deontological).
narrative_ontology:cs_reference_frame('330a05da-fd4c-4740-b9e7-619f25bf1a79', hindu_law_uniform_civil_authority).
narrative_ontology:cs_drift_state('330a05da-fd4c-4740-b9e7-619f25bf1a79', contemporary_2025, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('330a05da-fd4c-4740-b9e7-619f25bf1a79', '').
narrative_ontology:cs_kernel_id(marriage_authority_kernel__hindu_codified_reading, marriage_authority_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__hindu_codified_reading, hindu_nationalist_political_movements).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__hindu_codified_reading, brahminical_patriarchy_defenders).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__hindu_codified_reading, civil_courts_institutional_authority).
narrative_ontology:constraint_victim(marriage_authority_kernel__hindu_codified_reading, hindu_women).
narrative_ontology:constraint_victim(marriage_authority_kernel__hindu_codified_reading, religious_minorities_within_hindu_law).
narrative_ontology:constraint_victim(marriage_authority_kernel__hindu_codified_reading, lgbtq_individuals).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__hindu_codified_reading, hindu_women).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__hindu_codified_reading, hindu_community_organizations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Subject to Hindu Marriage Act rules on marriage validity, divorce grounds, property, and succession. Rules provide legal protections (widow remarriage permitted, monogamy enforced) that improved on pre-1955 custom in some regions, but statutory implementation enforces gender asymmetry: husband's right to restitution of conjugal rights (until 2023), unequal succession claims, default guardianship of children to fathers. Exit from Hindu marriage law without exit from Hindu community identity requires religious conversion (socially catastrophic in family/employment/property contexts) or permanent separation without divorce status (legal limbo). Courts are the authoritative interpreters; women's recourse is to courts, which operate within patriarchal statute.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__hindu_codified_reading, hindu_women, payer,
    powerless, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority_kernel__hindu_codified_reading, hindu_women, beneficiary).

% Interpret, apply, and enforce the Hindu Marriage Act 1955 as the binding authoritative source for Hindu marriage law. Courts frame their role as secular, neutral adjudication of a codified statutory text. Courts set binding precedent through appellate decisions, enforce compliance through contempt powers, and determine scope of Act through interpretation (e.g., which communities count as Hindu, how to handle interfaith marriages, whether restitution of conjugal rights is valid). Institutional benefit consists of jurisdictional expansion (family courts created nationwide), interpretive monopoly over religious law, and legitimacy from positioned neutrality. Courts collectively gain authority over an intimate domain (marriage/family) previously governed by religion/community.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__hindu_codified_reading, civil_courts_institutional_authority, agenda_setter,
    institutional, generational, arbitrage, national).

% Use the Hindu Marriage Act as evidence of Hindu law's modernizability and state-codeability, supporting the project of a Uniform Civil Code that would extend secular law to all citizens (replacing Muslim personal law, Christian law, and Parsi law with uniform rules). The Act vindicates the constitutional mandate (Article 44) for uniform civil law and positions Hindu nationalism as aligned with modernization and state authority. Movement benefits from association with courts and legislatures as the rational interpreters of Hindu tradition. Movement can mobilize the Act as proof that religious law can be codified, secularized, and rationally administered, de-legitimizing resistance from Muslim and Christian communities.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__hindu_codified_reading, hindu_nationalist_political_movements, beneficiary,
    powerful, generational, mobile, national).

% The Hindu Marriage Act codifies Brahminical norms (monogamy, marriage rules, inheritance patterns, male guardianship) as secular law, giving them state enforcement power. Patriarchal structures appear modernized and rational when codified as statute, rather than explicitly religious/traditional. Gender asymmetries in inheritance, guardianship, and marital dissolution remain enforceable through courts. Benefit consists of state-backed enforcement of patriarchal norms without explicit religious framing (courts present gender asymmetry as necessary legal structure, not religious requirement). Community organizations benefit from having patriarchal social norms encoded in law that courts will enforce.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__hindu_codified_reading, brahminical_patriarchy_defenders, beneficiary,
    organized, generational, constrained, national).

% Retain influence over marriage norms through community councils (panchayats), temples, and informal enforcement mechanisms despite formal subordination to civil courts. Courts de facto defer to community custom on minor matters (ceremony requirements, community recognition of marriage); courts recognize community consensus in interpretation of Act (e.g., caste-appropriate marriages, dowry norms as community practice). Benefit from state-enforced uniformity that prevents individual innovators from creating variant marriage norms within Hindu community. Community organizations can invoke civil court authority to enforce community norms against individual dissenters (e.g., inter-caste marriages, age-gap marriages).
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__hindu_codified_reading, hindu_community_organizations, beneficiary,
    organized, generational, mobile, national).

% Are structurally excluded from marriage under the Hindu Marriage Act, which defines marriage as 'the union of one man and one woman.' No legal remedies exist within Hindu law; no court interpretation can expand the Act to include same-sex marriage without overruling the definition itself. Religious conversion offers no escape (all personal law codes except the Special Marriage Act exclude same-sex marriage). No legal recognition of same-sex partnerships, no succession rights, no joint property claims. Exit requires religious conversion AND shifting to the Special Marriage Act (requires both legal and religious transition). Community rejection makes conversion catastrophic. Trapped by law and community identity fusion.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__hindu_codified_reading, lgbtq_individuals, payer,
    powerless, biographical, trapped, national).

% Dalit and other traditionally excluded groups are formally included under Hindu Marriage Act but subject to caste-based discrimination in practice. Courts occasionally intervene (striking down caste restrictions on marriage, enforcing anti-caste principles) but do not uniformly or consistently enforce anti-discrimination. Community panchayats and caste councils override or ignore court orders on inter-caste marriage; inheritance rules de facto enforce caste-based property exclusion (courts recognize community practice). Exit requires religious conversion (Dalit conversion movements have used this as escape route) but community and family enforcement makes it socially catastrophic. The uniform code is nominally uniform; caste-based suppression persists through community enforcement that courts do not reliably check.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__hindu_codified_reading, religious_minorities_within_hindu_law, payer,
    powerless, biographical, identity_locked, national).

% Maintain separate authority over Muslim marriage law (Shariat-based personal law) without the codification and civil-court uniformity applied to Hindu law. Structurally excluded from Hindu law authority; the Hindu codified reading constrains Muslim law by creating precedent and pressure for 'uniform' secular law (Uniform Civil Code). Muslim community leaders resist further codification of Muslim personal law, fearing it would accelerate integration into secular code. Hindu codification creates institutional pressure on Muslim law to either codify (and align with Hindu model) or be marginalized as uncodified/irrational. Trapped between resistance to codification (to preserve autonomy) and risk of being seen as backward if not codified.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__hindu_codified_reading, muslim_personal_law_boards, excluded,
    organized, generational, constrained, national).

% Maintain separate marriage law (Christian Marriage Act 1872, Parsi Marriage and Divorce Act 1936) without the institutional pressure applied to Hindu law codification. Their communities are smaller and less politically salient; less visible in debates about Uniform Civil Code. Structurally excluded from Hindu law authority but also exempted from uniform-code urgency that applies to Muslims (seen as larger population, hence threat). Their law remains pre-1955 style (non-codified, community-based) without the modernization pressure Hindu law faced. Constrained by political salience: if Uniform Civil Code becomes law, they will be included whether they codify or not.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__hindu_codified_reading, christian_and_parsi_personal_law_bodies, excluded,
    moderate, generational, constrained, national).

% Advocate for replacing all personal law codes (Hindu, Muslim, Christian, Parsi) with the Special Marriage Act or a Uniform Civil Code, grounded in constitutional secularism and individual rights (Article 44 mandate). They read the Hindu Marriage Act as a halfway measure that retained patriarchal structures while appearing modern, and as an obstacle to full gender equality and LGBTQ inclusion. They do not administer the constraint; they critique and oppose it. Institutional position is analytical/adversarial: they provide evidence, testimony, and legal arguments that reform is necessary and secular law is feasible.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__hindu_codified_reading, secular_civil_code_advocates, observer,
    organized, generational, analytical, national).

% The constitutional mandate for uniform civil code (Article 44, though non-justiciable) is invoked by courts and legislatures as the vindicated proposition that grounds the Hindu Act's legitimacy. This is a doctrine, not an actor, but its invocation shapes how the constraint is justified. Courts cite the constitutional mandate when defending codification; reform advocates cite it when pushing for further uniform law. The framers' intent (uniform civil code as the constitutional goal) is used to legitimize the Hindu codification as a step toward that goal, even though the goal remains unrealized 70 years after independence.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__hindu_codified_reading, constitutional_framers_lineage, observer,
    analytical, civilizational, analytical, national).
narrative_ontology:stakeholder_non_agent(marriage_authority_kernel__hindu_codified_reading, constitutional_framers_lineage).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_authority_kernel__hindu_codified_reading, civil_courts_institutional_authority).
narrative_ontology:fixing_cost_class(marriage_authority_kernel__hindu_codified_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides uniform, codified rules for Hindu marriage, divorce, property, and succession that apply across all Hindu communities (replacing pre-1955 custom variations). Eliminates local custom variation in marriage validity, bride-price rules, widow status, and inheritance — solves coordination across regions and communities within the Hindu population through one authoritative statutory text interpreted by civil courts.
% TRANSFER_FUNCTION: Transfers authority over marriage definition, validity, dissolution, and property from community-based custom councils and religious authorities to state civil courts. Transfers interpretive power to judges; transfers compliance to judicial enforcement machinery. Transfers gender power dynamics from negotiated community custom to codified statute (sometimes improving women's position, sometimes ossifying patriarchal structures as law). Transfers religious authority over marriage from religious bodies to secular institutions, framed as modernization.
% ABSENT_VOICES: LGBTQ individuals seeking same-sex marriage recognition are structurally excluded and have no seat in the conversation (no amendment path exists within this framework; Special Marriage Act offers an alternative but requires secular framing of their relationship). Dalit and other marginalized Hindu subgroups have formal inclusion but no meaningful power to reshape inheritance or caste-based exclusions enforced through community resistance to court orders. Muslim, Christian, and Parsi communities have parallel law authority but no voice in shaping Hindu law rules. Non-Hindu spouses (interfaith marriages) are routed to the Special Marriage Act, excluding them from this legal structure.
% DISAPPEARANCE_RATIONALE: If the Hindu Marriage Act and its court-administered authority disappeared, marriage arrangements would revert to community custom councils and religious authority in Hindu communities. Divorce would revert to community procedures (often impossible for women); property would follow caste and local custom (widows losing inheritance); regional variation would resurface. The state's uniformity and court access would vanish. This would constitute a major reshaping of Hindu women's legal position and state authority over religious law.
% FOUNDING_PROBLEM: Pre-1955 Hindu marriage law was fragmented by region, caste, sect, and community custom. No uniform rules for widow remarriage, divorce grounds, or inheritance across the Hindu population. This created legal uncertainty for inter-regional marriages, made property disputes unsettled, and left widow status and marital dissolution to community enforcement (often disadvantaging women and preventing remarriage). The founding problem was legal fragmentation and the inability of Hindu women to know their legal status across boundaries.
% FOUNDING_PROBLEM_CORROBORATION: Courts and legislatures cite the fragmentation problem as justifying codification. Academic historians and constitutional scholars outside the benefiting parties (civil courts, Hindu nationalist movements) confirm the pre-1955 fragmentation was real and created hardship. However, they contest whether codification was the only solution — the Special Marriage Act (1954) offered an alternative secular path rejected by Hindu nationalists. Feminist scholars attest that while codification addressed fragmentation, it simultaneously ossified patriarchal structures into law and created new forms of gender extraction (restitution of conjugal rights, unequal inheritance), so the founding problem is partly solved and partly reframed as statutory gender inequity.
narrative_ontology:disappearance_verdict(marriage_authority_kernel__hindu_codified_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority_kernel__hindu_codified_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority_kernel__hindu_codified_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(marriage_authority_kernel__hindu_codified_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_authority_kernel__hindu_codified_reading, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority_kernel__hindu_codified_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_authority_kernel__hindu_codified_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_authority_kernel__hindu_codified_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.38 (1955, near-genuine coordination) to 0.58 (2025, measurable asymmetry) as civil courts accumulate interpretive power and resist reform (dowry criminalization resisted through judicial loop-holes, restitution of conjugal rights upheld despite feminist challenge until 2023). Theater rises from 0.12 to 0.28 as the courts' rhetoric of secular neutrality increasingly masks patriarchal enforcement and caste-based practice (courts celebrate gender-progressive judgments while upholding inheritance rules that disadvantage women; increasing emphasis on 'Hindu values' in judicial language despite secular framing). Suppression rises from 0.35 to 0.52 as identity-locked exit (Hindu women cannot leave without religious conversion or permanent separation) becomes the mechanism for enforcing the constraint despite formal rule-of-law appearance. The time grid is shared across all three metrics at the six time points; the interval spans 1955 (the Act's enactment) to 2025 (contemporary state).
 *
 * PERSPECTIVAL GAP:
 *   The civil courts and Hindu nationalist movements experience this constraint as genuine coordination (legal certainty, modern uniformity, rational authority) with minimal extraction. Hindu women experience it as enforced identity-lock with gendered extraction (patriarchal rules backed by legal authority, no exit path except conversion). LGBTQ individuals experience it as structural exclusion. The engine computes per-seat types from these divergent structural positions: courts compute near-rope (low extraction for them), women compute snare (high extraction, identity-locked, no real alternatives). This gap is not a measurement error — it is the defining feature of the constraint: the same rule appears as different types from different seats because the seats have fundamentally different exit options and structural power.
 *
 * DIRECTIONALITY LOGIC:
 *   Hindu women (payer role, power=powerless, exit=identity_locked): directionality d = 0.95 (nearly full target). They bear costs through patriarchal rules, statutory gender asymmetries, and community enforcement. Exit means religious conversion (community rejection) or permanent separation (legal limbo). Civil courts (agenda_setter role, power=institutional, exit=arbitrage): directionality d = 0.05 (nearly full beneficiary). They gain interpretive authority, institutional expansion, and legitimacy. Exit is not meaningful; they are the authority structure. Hindu nationalist movements (beneficiary role, power=powerful, exit=mobile): directionality d = 0.15 (weak target). They benefit from state codification of Hindu law as model for Uniform Civil Code; they have strong exit to secular nationalism if Hindu law reform accelerates. LGBTQ individuals (payer role, power=powerless, exit=trapped): directionality d = 0.98 (full target). Structural exclusion with no legal remedy; exit means religious conversion (all codes except Special Marriage Act exclude same-sex marriage). No override needed; derivation chain produces correct d from beneficiary/victim + exit data.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (legal fragmentation of pre-1955 Hindu marriage law) is officially 'live' but functionally 'dead': courts and legislatures still cite fragmentation as justifying the Act, but the actual operation reveals that codification solved fragmentation while introducing new forms of extraction (gender asymmetry, patriarchal structures ossified as law, caste-based discrimination enforced through statute). The constraint persists not because fragmentation is active but because courts benefit from the authority it grants, and reform is blocked by Hindu nationalist resistance to secular alternatives (Special Marriage Act). This is a classic mandatrophy marker: the founding problem has been displaced by a new coordination function (gender extraction through state machinery), but the constraint is justified with reference to the original problem. The 2023 Suresh Gupa v. Govt. of India judgment abolishing restitution of conjugal rights partially addressed this by removing one explicit patriarchal mechanism, but it did not alter the underlying structure of gender extraction through civil court authority. Mandatrophy is not fully resolved; the founding problem remains cited despite being functionally obsolete.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_authority,
    'Is the Hindu Marriage Act a discovery of pre-existing Hindu law principles (natural law framing by Hindu nationalists) or a constructed codification that created uniformity where variation existed (secular framing by critics)?',
    'Textual and historical analysis: compare the Act''s rules to pre-1955 local custom codes and temple records. High correspondence would support discovery framing; low correspondence would support construction framing. Academic historical scholarship outside the benefiting parties.',
    'If constructed, the constraint is exposed as beneficiary-serving institutionalization rather than natural law application; if discovered, Hindu nationalism gains legitimacy for the codification model. This affects how the constraint is perceived (mountain-like vs. clearly institutional) and how reform challenges are framed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_authority, empirical, 'Whether the Act codifies pre-existing law or constructs new uniformity.').

omega_variable(
    separation_of_coordination_and_extraction,
    'Could uniform Hindu marriage law be achieved through a secular civil code (Special Marriage Act model) without the patriarchal structures and community-enforcement mechanisms the Act preserves?',
    'Counterfactual comparison with the Special Marriage Act 1954 (already in force) and jurisdictions where secular civil marriage replaced personal law codes. If gender equity and LGBTQ inclusion are higher under secular codification with equivalent coordination benefits, the extraction is separable.',
    'If separable, the patriarchal extraction and gender asymmetry are choices of the Hindu-codified reading, not inherent to coordination. This would classify the constraint as pure snare from women''s and LGBTQ seats (extraction without coordination benefit). If inseparable, the gender asymmetry is the price of community-respect coordination, making it tangled_rope even from extractive seats.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(separation_of_coordination_and_extraction, empirical, 'Whether gender extraction is inherent to Hindu codified law or separable through secular alternatives.').

omega_variable(
    identity_lock_mechanism_internalized,
    'Is the measured suppression (0.52) structural (lack of legal exit from Hindu marriage without conversion) or internalized (Hindu women accept patriarchal rules as part of Hindu identity even when legal exit is available)?',
    'Post-exit trajectory: if women who convert to other religions or adopt the Special Marriage Act report reduction in suppression and patriarchal enforcement, the suppression is partially structural. If suppression persists (women report internalized shame, community rejection, family pressure even post-exit), it is partially internalized. Qualitative research on women who have navigated out of Hindu marriage law.',
    'If structural: the constraint''s suppression is high even after removing the legal mechanism. If internalized: the constraint carries suppression with it in the form of identity-fused enforcement; women''s exit does not liberate them from the constraint''s extracted cost. Either way, suppression is higher than the codified-law picture suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_internalized, empirical, 'Whether suppression is enforced externally (legal, community) or internalized (identity-fused).').

omega_variable(
    sibling_reading_containment,
    'Does the Hindu codified reading logically foreclose the secular civil reading, or do both remain live options that coexist as competing frameworks?',
    'Jurisprudential analysis: Hindu law courts have not declared the Special Marriage Act unconstitutional or illegitimate; both frameworks operate simultaneously in Indian law. However, Hindu nationalist movements and some judges position Hindu codification as the Hindu community''s ''natural'' path, with secular civil law as an alternative for minorities or apostates. If courts formally rule the Special Marriage Act invalid for Hindus, or if constitutional amendment eliminates it, foreclosure is achieved; absent that, the readings coexist.',
    'If coexist: both Hindu codified and secular civil marriage are live options; citizens choose between them. This supports coexists_with relation to the secular reading. If Hindu codification forecloses secular civil marriage for Hindus (through court ruling or legislative action), foreclosure is achieved and the relation type changes to forecloses. The distinction is structural: one framework rules out the other''s core premise (Hindu marriage authority) vs. both remain available.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_containment, conceptual, 'Whether Hindu codified reading logically excludes the secular civil reading in the same legal system.').

omega_variable(
    caste_enforcement_opacity,
    'How much of the measured suppression and gender extraction operates through explicit statutory mechanisms (restitution of conjugal rights, inheritance rules) vs. implicit caste-based enforcement (community resistance to court orders, caste councils overriding civil courts)?',
    'Case law analysis: examine family court judgments involving caste-based marriage objections, inheritance disputes across caste lines, and community-council enforcement of caste rules. High incidence of courts deferring to community consensus or failing to enforce anti-caste protections indicates caste-based enforcement; low incidence indicates extraction operates primarily through statutory text.',
    'If caste enforcement is high, the constraint operates through two enforcement mechanisms: statutory and community-based. The statutory mechanism is codified and can be reformed through legislation; the community mechanism is diffuse and persists through identity-lock and social enforcement independent of law. This affects remedies (statutory reform is necessary but insufficient) and classification (high suppression justified by dual mechanisms).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(caste_enforcement_opacity, empirical, 'Proportion of extraction enforced through statute vs. implicit caste-based community mechanisms.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority_kernel__hindu_codified_reading, 1955, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t1955, marriage_authority_kernel__hindu_codified_reading, theater_ratio, 1955, 0.12).
narrative_ontology:measurement(marr_tr_t1975, marriage_authority_kernel__hindu_codified_reading, theater_ratio, 1975, 0.16).
narrative_ontology:measurement(marr_tr_t1995, marriage_authority_kernel__hindu_codified_reading, theater_ratio, 1995, 0.22).
narrative_ontology:measurement(marr_tr_t2005, marriage_authority_kernel__hindu_codified_reading, theater_ratio, 2005, 0.25).
narrative_ontology:measurement(marr_tr_t2015, marriage_authority_kernel__hindu_codified_reading, theater_ratio, 2015, 0.27).
narrative_ontology:measurement(marr_tr_t2025, marriage_authority_kernel__hindu_codified_reading, theater_ratio, 2025, 0.28).

% Extraction over time
narrative_ontology:measurement(marr_be_t1955, marriage_authority_kernel__hindu_codified_reading, base_extractiveness, 1955, 0.38).
narrative_ontology:measurement(marr_be_t1975, marriage_authority_kernel__hindu_codified_reading, base_extractiveness, 1975, 0.45).
narrative_ontology:measurement(marr_be_t1995, marriage_authority_kernel__hindu_codified_reading, base_extractiveness, 1995, 0.52).
narrative_ontology:measurement(marr_be_t2005, marriage_authority_kernel__hindu_codified_reading, base_extractiveness, 2005, 0.55).
narrative_ontology:measurement(marr_be_t2015, marriage_authority_kernel__hindu_codified_reading, base_extractiveness, 2015, 0.57).
narrative_ontology:measurement(marr_be_t2025, marriage_authority_kernel__hindu_codified_reading, base_extractiveness, 2025, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t1955, marriage_authority_kernel__hindu_codified_reading, suppression_requirement, 1955, 0.35).
narrative_ontology:measurement(marr_su_t1975, marriage_authority_kernel__hindu_codified_reading, suppression_requirement, 1975, 0.4).
narrative_ontology:measurement(marr_su_t1995, marriage_authority_kernel__hindu_codified_reading, suppression_requirement, 1995, 0.47).
narrative_ontology:measurement(marr_su_t2005, marriage_authority_kernel__hindu_codified_reading, suppression_requirement, 2005, 0.5).
narrative_ontology:measurement(marr_su_t2015, marriage_authority_kernel__hindu_codified_reading, suppression_requirement, 2015, 0.51).
narrative_ontology:measurement(marr_su_t2025, marriage_authority_kernel__hindu_codified_reading, suppression_requirement, 2025, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority_kernel__hindu_codified_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(marriage_authority_kernel__hindu_codified_reading, 0.12).
narrative_ontology:affects_constraint(marriage_authority_kernel__hindu_codified_reading, marriage_authority_kernel__muslim_shariat_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__hindu_codified_reading, marriage_authority_kernel__secular_civil_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__hindu_codified_reading, marriage_authority_kernel__christian_canonical_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__hindu_codified_reading, marriage_authority_kernel__parsi_communal_reading).

% DUAL FORMULATION NOTE:
% The marriage-authority-kernel decomposes into five structurally distinct constraints, one per live reading: Hindu codified (THIS story), Muslim Shariat, Christian canonical, Parsi communal, and secular civil. Each reading instantiates different beneficiaries, victims, and extraction profiles despite covering the same domain (marriage law in India). The Hindu codified reading differs from the secular civil reading primarily in authority grounding (civil-court interpretation of codified religious law vs. constitutional individual rights) and in gender-equity/LGBTQ-inclusion mechanisms (patriarchal structures vs. gender-neutral, same-sex recognition). The Hindu codified reading influences the Muslim Shariat reading by creating institutional pressure to maintain separate Muslim personal law (keeping Muslim authority parallel to Hindu rather than integrating into secular code). All five stories are linked via network.affects_constraints; no single story subsumes or falsifies another — they are simultaneous live options in Indian constitutional pluralism.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
