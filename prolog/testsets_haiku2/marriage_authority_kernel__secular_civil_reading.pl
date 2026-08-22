% ============================================================================
% CONSTRAINT STORY: marriage_authority_kernel__secular_civil_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_authority_kernel__secular_civil_reading, []).

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
 *   constraint_id: marriage_authority_kernel__secular_civil_reading
 *   human_readable: Secular Civil Marriage Authority (Special Marriage Act 1954)
 *   domain: legal/constitutional/religious
 *
 * SUMMARY:
 *   The Special Marriage Act 1954 grounds marriage law in secular civil
 *   authority and constitutional individual rights, providing an alternative
 *   to religion-specific personal law systems (Hindu Marriage Act 1955,
 *   Muslim personal law, Christian Marriage Act 1872, Parsi Marriage and
 *   Divorce Act 1936). This is ONE READING of the contested kernel 'marriage
 *   authority kernel': the secular civil reading claims that legitimate
 *   marriage authority derives from state sovereignty, constitutional equal
 *   protection, and individual choice—not from religious doctrine or
 *   community custom. Inter-religious couples, women seeking divorce remedies
 *   unavailable in their religion's personal law, and the secular state
 *   itself are structural beneficiaries. Religious community authorities and
 *   traditional gatekeepers bear the cost of lost institutional control. This
 *   story instantiates only the secular civil reading; sibling readings
 *   (hindu_codified_reading, muslim_shariat_reading,
 *   christian_canonical_reading, parsi_communal_reading) are separate
 *   constraint stories with different ε values and beneficiary/victim
 *   structures. The constraint's extractiveness is modest (0.31) because the
 *   civil law provides genuine coordination for inter-religious marriage
 *   without pure rent-taking; suppression is low (0.18) because the state
 *   does not require conversion or enforce participation—exit to civil law is
 *   structurally available (though socially costly for identity-locked
 *   minorities). Theater is moderate (0.22): the civil law performs its
 *   stated gender-equity and individual-rights function; theatrical
 *   components emerge where the state asserts secular supremacy in domains
 *   where religious law claims prior authority.
 *
 * KEY AGENTS:
 *   - Secular state authority (institutional, agenda-setter): administers civil courts, sets uniform marriage/divorce rules, collects governance authority; legitimacy depends on asserting secular supremacy over personal law domains
 *   - Inter-religious couples (moderate power, beneficiary): can marry across religious boundaries without community gatekeeping; gain access to secular exit options and uniform property/succession rights
 *   - Women seeking exit (moderate power, beneficiary): can initiate divorce through civil courts with statutory grounds (cruelty, adultery, desertion); gain enforceable maintenance and property division independent of religious authority approval
 *   - Religious community authority (institutional, payer + excluded): loses jurisdiction and institutional control when individuals choose the secular civil path; must rely on social enforcement (family pressure, community ostracism) rather than legal authority
 *   - Traditional gatekeepers (powerful, payer): patriarchal and patrilineal authority structures embedded in religious law lose enforceability when individuals exit; women and lower-caste members especially use the secular path to escape prescribed roles
 *   - Muslim and Parsi minorities (moderate power, payer with identity_locked exit): remain subject to personal law; choosing civil law is read as religious/cultural apostasy despite formal legal choice; bear the social cost of identity fusion
 *   - Hindu majority (powerful, beneficiary + payer): have access to both Hindu Marriage Act and secular civil route; benefit from choice and statutory reform but bear the cost of legal pluralism and internal-community conflict
 *   - Constitutional courts (institutional, observer): interpret the boundary between secular civil law and religious personal law; uphold the Act as constitutionally required but also protect personal law traditions as expressions of cultural rights
 *   - Feminist reform movements (organized, beneficiary): use the civil law as a platform for gender equality argument and pressure for reform of religious personal law; benefit from the existence of the secular route as proof that uniform rights are possible
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority_kernel__secular_civil_reading, 0.31).
domain_priors:suppression_score(marriage_authority_kernel__secular_civil_reading, 0.18).
domain_priors:theater_ratio(marriage_authority_kernel__secular_civil_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__secular_civil_reading, extractiveness, 0.31).
narrative_ontology:constraint_metric(marriage_authority_kernel__secular_civil_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(marriage_authority_kernel__secular_civil_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__secular_civil_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(marriage_authority_kernel__secular_civil_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority_kernel__secular_civil_reading, rope).
narrative_ontology:human_readable(marriage_authority_kernel__secular_civil_reading, "Secular Civil Marriage Authority (Special Marriage Act 1954)").
narrative_ontology:topic_domain(marriage_authority_kernel__secular_civil_reading, "legal/constitutional/religious").

domain_priors:requires_active_enforcement(marriage_authority_kernel__secular_civil_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority_kernel__secular_civil_reading, '0e760c32-ea7d-480b-aa7c-e07c1a620388').
narrative_ontology:cs_kernel_codification('0e760c32-ea7d-480b-aa7c-e07c1a620388', formalized).
narrative_ontology:cs_authority_grounding('0e760c32-ea7d-480b-aa7c-e07c1a620388', extraction).
narrative_ontology:cs_interpretation_layer_present('0e760c32-ea7d-480b-aa7c-e07c1a620388').
narrative_ontology:cs_reading_relation('0e760c32-ea7d-480b-aa7c-e07c1a620388', marriage_authority_kernel__hindu_codified_reading, coexists_with).
narrative_ontology:cs_reading_relation('0e760c32-ea7d-480b-aa7c-e07c1a620388', marriage_authority_kernel__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('0e760c32-ea7d-480b-aa7c-e07c1a620388', marriage_authority_kernel__christian_canonical_reading, coexists_with).
narrative_ontology:cs_reading_relation('0e760c32-ea7d-480b-aa7c-e07c1a620388', marriage_authority_kernel__parsi_communal_reading, coexists_with).
narrative_ontology:cs_axiom('0e760c32-ea7d-480b-aa7c-e07c1a620388', foundational, marriage_authority_derives_from_state_constitution).
narrative_ontology:cs_axiom_status(marriage_authority_derives_from_state_constitution, holdable).
narrative_ontology:cs_axiom_grounding('0e760c32-ea7d-480b-aa7c-e07c1a620388', marriage_authority_derives_from_state_constitution, deontological).
narrative_ontology:cs_axiom('0e760c32-ea7d-480b-aa7c-e07c1a620388', foundational, individual_choice_supersedes_religious_authority).
narrative_ontology:cs_axiom_status(individual_choice_supersedes_religious_authority, holdable).
narrative_ontology:cs_axiom_grounding('0e760c32-ea7d-480b-aa7c-e07c1a620388', individual_choice_supersedes_religious_authority, deontological).
narrative_ontology:cs_axiom('0e760c32-ea7d-480b-aa7c-e07c1a620388', secondary, gender_equality_mandatory_in_marriage_law).
narrative_ontology:cs_axiom_status(gender_equality_mandatory_in_marriage_law, holdable).
narrative_ontology:cs_axiom_grounding('0e760c32-ea7d-480b-aa7c-e07c1a620388', gender_equality_mandatory_in_marriage_law, deontological).
narrative_ontology:cs_reference_frame('0e760c32-ea7d-480b-aa7c-e07c1a620388', constitutional_secular_authority).
narrative_ontology:cs_drift_state('0e760c32-ea7d-480b-aa7c-e07c1a620388', contemporary_religious_nationalism_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('0e760c32-ea7d-480b-aa7c-e07c1a620388', '').
narrative_ontology:cs_kernel_id(marriage_authority_kernel__secular_civil_reading, marriage_authority_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__secular_civil_reading, inter_religious_couples).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__secular_civil_reading, women_seeking_exit).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__secular_civil_reading, secular_state_authority).
narrative_ontology:constraint_victim(marriage_authority_kernel__secular_civil_reading, religious_community_authority).
narrative_ontology:constraint_victim(marriage_authority_kernel__secular_civil_reading, traditional_gatekeepers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__secular_civil_reading, hindu_majority_communities).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__secular_civil_reading, christian_minority_communities).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__secular_civil_reading, feminist_reform_movements).
narrative_ontology:constraint_victim(marriage_authority_kernel__secular_civil_reading, hindu_majority_communities).
narrative_ontology:constraint_victim(marriage_authority_kernel__secular_civil_reading, muslim_minority_communities).
narrative_ontology:constraint_victim(marriage_authority_kernel__secular_civil_reading, christian_minority_communities).
narrative_ontology:constraint_victim(marriage_authority_kernel__secular_civil_reading, parsi_minority_communities).
narrative_ontology:constraint_vindicates(marriage_authority_kernel__secular_civil_reading, constitutional_individual_rights).
narrative_ontology:constraint_vindicates(marriage_authority_kernel__secular_civil_reading, secular_state_supremacy_doctrine).
narrative_ontology:constraint_vindicates(marriage_authority_kernel__secular_civil_reading, gender_equality_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Can marry across religious boundaries under the Special Marriage Act without community approval or religious authority consent. The civil law gives them legal standing independent of any religious institution. They gain access to secular uniform succession, divorce, and property rights—exit from religious law is available and legally protected.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__secular_civil_reading, inter_religious_couples, beneficiary,
    moderate, biographical, mobile, national).

% Can initiate divorce through civil courts with uniform grounds (cruelty, adultery, desertion) regardless of religious affiliation. The secular law provides legal remedy independent of religious authority approval. They gain enforceable property division, maintenance, and custody rights codified in statute rather than left to community interpretation.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__secular_civil_reading, women_seeking_exit, beneficiary,
    moderate, biographical, mobile, national).

% Administers the Special Marriage Act through civil courts and marriage registrars. Sets uniform rules across religious boundaries in the name of constitutional rights and secular governance. Collects registration fees and administrative control; the state's legitimacy depends on asserting secular legal supremacy over personal law domains that previously excluded it.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__secular_civil_reading, secular_state_authority, agenda_setter,
    institutional, generational, analytical, national).

% Loses jurisdiction when individuals choose the secular civil path. Religious leaders, councils, and traditional gatekeepers who previously adjudicated marriage validity, dissolution, and property within religious law find their authority bypassed. They bear the cost of reduced institutional control and social influence; their resistance to the civil authority is structurally present but governance power is constrained.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__secular_civil_reading, religious_community_authority, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority_kernel__secular_civil_reading, religious_community_authority, excluded).

% Patriarchal and patrilineal authority structures embedded in religious law (priestly celibacy rules, widow remarriage restrictions, male-only inheritance) lose enforceability when individuals exit to civil law. Women and lower-caste members especially use the secular path to escape prescribed roles. Gatekeepers bear the loss of traditional authority without being able to prevent exit—enforcement must be social (family pressure, community ostracism) rather than legal.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__secular_civil_reading, traditional_gatekeepers, payer,
    powerful, generational, constrained, national).

% Have access to both the Hindu Marriage Act (religion-specific) and the Special Marriage Act (secular alternative). They benefit from choice and from statutory reform of property/divorce within Hindu law itself; they also bear the cost of legal pluralism and potential internal-community conflict when members choose the secular path, creating parallel authorities and contested legitimacy.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__secular_civil_reading, hindu_majority_communities, beneficiary,
    powerful, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority_kernel__secular_civil_reading, hindu_majority_communities, payer).

% Remain subject to personal law (shariat-derived Muslim law) in marriage and succession; the secular civil route is available but choosing it is widely read as religious apostasy or cultural betrayal by community elders. Their formal legal choice is free; their social cost of exit is high due to identity fusion with religious law. They bear the cost of legal pluralism without the structural reform that Hindu codification achieved.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__secular_civil_reading, muslim_minority_communities, payer,
    moderate, generational, identity_locked, national).

% Are governed by the Christian Marriage Act 1872 (which allows divorce on limited grounds) or can opt into the secular civil act. They have some choice but the inherited colonial-era religious law treats marriage as less dissoluble; exit to civil law gains them ground but signals loss of religious identity.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__secular_civil_reading, christian_minority_communities, payer,
    moderate, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority_kernel__secular_civil_reading, christian_minority_communities, beneficiary).

% Are governed by the Parsi Marriage and Divorce Act 1936 (which has historically granted broad divorce rights to women within community law). The secular civil act is available but choosing it has historically been read as ethnic and religious exit; community authority over membership and legitimacy is reinforced through legal endogamy. Their legal choice is formally free; their social cost is identity-fusion with Parsi law.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__secular_civil_reading, parsi_minority_communities, payer,
    moderate, generational, identity_locked, national).

% Interpret and police the boundary between secular civil law and religious personal law. The courts uphold the Special Marriage Act as constitutionally required; they also protect personal law traditions as expressions of cultural rights. Their role is to adjudicate whether a particular constraint within religious law violates constitutional rights—a power that makes them both guarantors of the secular civil reading and potential check on its overreach.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__secular_civil_reading, constitutional_courts, observer,
    institutional, generational, analytical, national).

% Use the civil law as a platform for gender equality argument: secular law provides statutory protections unavailable in religious law and enables coalitional pressure for reform. They benefit from the existence of the secular route as proof that uniform rights are possible; they also use the gap between civil and personal law to highlight inequities in the latter.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__secular_civil_reading, feminist_reform_movements, beneficiary,
    organized, biographical, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_authority_kernel__secular_civil_reading, secular_state_authority).
narrative_ontology:fixing_cost_class(marriage_authority_kernel__secular_civil_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a unified legal framework for inter-religious marriage and divorce, enforceable through civil courts independent of religious authority consent. Solves the coordination problem of individuals wishing to marry across religious boundaries and to exit marriage through secular remedies without community gatekeeping.
% TRANSFER_FUNCTION: Transfers authority from religious institutions and traditional gatekeepers to the secular state and civil courts. The transfer is not revenue-extractive (no direct commission) but authority-extractive: the state collects the power to adjudicate marriage validity, dissolution, and property rights that religious law previously held exclusively. Identity-locked community members experience this as loss of cultural authority and enforcement capacity.
% ABSENT_VOICES: Religious authorities and traditional gatekeepers who held prior institutional power are structurally excluded from the secular civil framework—they are consulted only in the special cases where individuals opt into religious law, not in the civil statute. They would argue that secular law is an imperial imposition that erodes religious autonomy, but their argument is routed outside the civil courts that administer the secular reading.
% DISAPPEARANCE_RATIONALE: If the Special Marriage Act and its secular civil route were repealed, individuals would be forced back into religion-specific personal law systems: inter-religious couples would have no legal marriage option, women seeking divorce would face religious gatekeeping, and the state's claim to secular supremacy would collapse. The availability of the secular route has already altered behavior—removing it would reorganize family law pluralism back toward religious authority monopoly.
% FOUNDING_PROBLEM: Constitutional commitment to secular governance and individual rights created a structural conflict with inherited religious personal law systems that treated marriage as a status determined by religious authority, not individual choice. The founding problem was: how to ground marriage law in constitutional rights when prior law granted religious institutions exclusive authority?
% FOUNDING_PROBLEM_CORROBORATION: Constitutional courts and progressive reform movements attest the founding problem is live: secular governance requires a civil alternative to religious personal law. Religious authorities and traditional gatekeepers attest the founding problem is misframed—they argue religious law does not violate rights but preserves cultural autonomy. Legislative history of the 1954 Act and subsequent amendments show the conflict remained structurally unresolved; neither side recognizes complete reconciliation.
narrative_ontology:disappearance_verdict(marriage_authority_kernel__secular_civil_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority_kernel__secular_civil_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority_kernel__secular_civil_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(marriage_authority_kernel__secular_civil_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_authority_kernel__secular_civil_reading, 0.31, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority_kernel__secular_civil_reading_tests).
:- end_tests(marriage_authority_kernel__secular_civil_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness metric (0.31 at interval end, rising from 0.18 at t0) tracks the state's gradual consolidation of authority over marriage domains that religious law previously controlled. The rise is gradual rather than steep because the secular civil route works through consent rather than coercion—individuals opt in because it serves their interests (inter-religious marriage, easier divorce), not because the state demands participation. Suppression (0.18 at interval end) is low because the state does not suppress religious personal law itself; individuals remain legally free to choose their religion's law if they prefer. The suppression that does exist is social (community ostracism for identity-locked minorities who exit to civil law) rather than state-administered. Theater (0.22) is moderate because the civil law genuinely provides coordination (unified rules for inter-religious couples, statutory protections for women) but increasingly performs symbolic functions (the state's assertion of secular supremacy, the vindication of constitutional individual-rights doctrine) alongside its practical functions. Accessibility collapse rises from 0.28 to 0.42 at the structural level because the civil law has become the de facto default for inter-religious marriage—alternatives (seeking religious authority permission for cross-boundary marriage) have become progressively less available. At the individual level (0.48 to 0.58), the rise is steeper because individuals are increasingly socialized into the civil law framework as the normal path; only identity-locked minorities experience collapse differently. Stakes inflation rises at organizational and individual levels (religious authorities and traditional gatekeepers face institutional loss; individuals face identity-fusion costs) more than at the structural level (the plural legal system persists) because the constraint operates through cumulative attrition of the prior system rather than through direct structural replacement. Resistance is high and stable (0.52–0.68 across levels) because religious authorities mount sustained opposition to secular supremacy; that opposition does not prevent the civil law's operation but does prevent its becoming the only legitimate frame. The measurement series uses one shared time grid so every metric is authored at every examined time point.
 *
 * PERSPECTIVAL GAP:
 *   From the secular state's seat, the constraint is genuine coordination: the civil law solves a real coordination problem (how to enable inter-religious marriage and protect individual choice against religious gatekeeping) at a reasonable cost (civil courts, statutory procedures, registration fees). From the religious community authority's seat, the constraint is authority extraction: the state extracts legitimacy and institutional control that religious law previously held, justified under the cover of individual rights and secularism—a cover story that prevents recognition of the extraction. From the beneficiary seats (inter-religious couples, women seeking exit), the constraint is genuine coordination for them specifically, though they implicitly recognize it as extraction from religious authorities (whose authority they are fleeing). From the identity-locked minorities' seats, the constraint is a false choice: formal legal freedom to use civil law paired with strong social suppression (family pressure, community ostracism, identity loss) that makes the choice conditional on accepting cultural apostasy. The engine computes per-seat classifications from the structural data (power, exit_options, beneficiary/victim designation, directionality); this perspectival gap emerges from the different directionality values across seats, not from different metrics or different claimed types.
 *
 * DIRECTIONALITY LOGIC:
 *   The secular state authority sits at the beneficiary end of the directionality spectrum (d near 0.0): it sets the rules, collects the authority, benefits from expanded jurisdiction, and faces no exit. Inter-religious couples and women seeking exit sit near the beneficiary end as well (low d, around 0.1–0.2): they benefit from the constraint's existence, face no suppression by the state, and can choose to use it or exit to religious law if they prefer—their exit cost is social, not legal. Religious community authorities and traditional gatekeepers sit at the target end (d near 1.0, around 0.8–0.9): they bear the loss of institutional control, their authority is progressively undermined, and their exit is constrained—they cannot restore religious law's prior monopoly through legal or institutional means. Identity-locked minorities (Muslim and Parsi communities) sit closer to the target end (d around 0.65–0.75) than do structural minorities: they have formal legal choice but the social cost of exit is high due to identity fusion, which amplifies their effective extraction even as the state imposes no direct suppression. This divergence—secular beneficiaries with low d, identity-locked payers with high d despite formal legal freedom—is the critical asymmetry the engine should detect.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (how to ground marriage law in constitutional rights when prior law granted religious institutions exclusive authority) was live at 1954 and remains contested, not dead. The Special Marriage Act has not resolved the conflict but rather institutionalized it as legal pluralism: multiple authorities coexist, each claiming legitimacy from different sources (secular state from constitution, Hindu law from codification of tradition, Muslim law from shariat, Christian law from colonial statute, Parsi law from communal statute). Mandatrophy is partially present: the state's rhetoric emphasizes individual choice and secular supremacy, but the actual operation maintains parallel systems where individuals choose between them. This prevents pure mandatrophy (the founding problem is not dead; it is unresolved by design) but creates structural tension: the state asserts the founding problem justifies its supremacy, while religious authorities assert the founding problem is a secular misunderstanding of religious autonomy. The constraint avoids classification as mandatrophy-dead because subsequent constitutional court judgments have upheld both the Special Marriage Act AND the protection of personal law traditions—a framework that keeps the founding problem alive through institutional contradiction. The theater_ratio rise (0.12 to 0.22) reflects increasing performative work: the state must continually assert secular supremacy without actually abolishing religious law; religious authorities must continually defend cultural autonomy while losing institutional power over individuals who exit. Neither side can claim victory; both must perform fidelity to their original mandate despite the unresolved contradiction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    secular_supremacy_legitimacy,
    'Is the secular state''s claim to authority over marriage law grounded in genuine supremacy of constitutional rights, or is it a post-colonial assertion of state power over domains previously governed by religious/communal autonomy?',
    'Genealogical analysis of the constitutional debates at Independence (1947–1950); analysis of whether ''secularism'' was a genuine departure from British colonial governance or a continuation with new authority structure; comparison with non-colonized secular states'' treatment of personal law.',
    'If the claim rests on genuine constitutional principle, the civil law is coordination grounded in rights. If it is post-colonial state consolidation, the civil law is a reading that vindicates state authority over religious domains previously outside state reach—which would elevate the extractiveness assessment and reframe the relationship to religious law as one-sided dominance rather than pluralism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(secular_supremacy_legitimacy, conceptual, 'Whether the secular reading''s authority is grounded in constitutional principle or post-colonial state consolidation.').

omega_variable(
    identity_locked_exit_cost,
    'For identity-locked minorities (Muslims, Parsis), is the social suppression of civil law exit (community ostracism, identity loss) a structural consequence of identity fusion or an active enforcement mechanism wielded by religious authorities to preserve jurisdiction?',
    'Ethnographic study of community responses to civil law marriages; analysis of whether religious authorities actively excommunicate/ostracize civil law users or whether families/communities self-enforce; comparison of identity-lock dynamics across different religious communities.',
    'If suppression is self-enforced by identity-fused individuals and families, the social cost is a genuine but decentralized barrier. If religious authorities actively enforce ostracism, the extraction from identity-locked minorities is higher than the measured suppression reflects—the measured suppression (0.18) captures state-level suppression only, not communal enforcement. This would suggest that the constraint''s effective extraction is asymmetric: low for those with mobile identity, high for those with fused identity despite formal legal choice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_locked_exit_cost, empirical, 'Whether identity-lock suppression is self-enforced or actively enforced by religious authorities.').

omega_variable(
    plural_legal_system_stability,
    'Is the coexistence of multiple marriage law systems (secular civil + four religion-specific personal laws) a stable equilibrium or an unstable transition toward either secular dominance or religious reassertion?',
    'Longitudinal analysis of civil law marriage rates by religion over 70 years (baseline t0 to t70); analysis of constitutional court trajectory (are later judgments constraining religious law or protecting it more strongly?); qualitative data on religious authority adaption (are religious leaders reforming personal law to compete with civil law, or hardening against civil law?)',
    'If the trajectory is toward secular dominance, the civil law is the upstream reading and will increasingly determine the constraint''s effective type. If the trajectory is toward religious reassertion (e.g., post-2000s Hindu nationalism, Islamic law revival), the plural system may destabilize into competing claims of legitimacy—which would elevate resistance and theater metrics and potentially reclassify the constraint toward snare (state-imposed secular reading against religious reassertion) rather than rope (genuine coordination). Current measurements assume the plural system is stable; instability would require remeasurement.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(plural_legal_system_stability, empirical, 'Whether plural legal marriage system is stable or transitioning toward secular dominance or religious reassertion.').

omega_variable(
    kernel_reading_forecloses,
    'Does the secular civil reading''s core premise (marriage authority derives from state/constitution/individual rights) logically foreclose the sibling readings'' premises (marriage authority derives from religious doctrine/community custom), or can both coexist within a single pluralist framework?',
    'Formal analysis of the logical structure of each reading''s grounding claim; examination of whether the Indian constitutional framework (Article 25—freedom of religion + secular state commitment + individual rights) can coherently hold multiple readings simultaneously, or whether endorsing one reading logically entails rejecting others.',
    'If the readings are logically foreclosed (secular supremacy logically entails religious non-authority), then they should be classified as forecloses relations (not coexists_with). If they can coexist within a pluralist constitutional framework, the current coexists_with classification is correct. The distinction affects how the engine models the kernel: if foreclosure is present, the sibling readings are engaged in a zero-sum competition for authority; if coexistence is possible, they are operating in different institutional spaces (civil courts vs. religious bodies) within the same meta-framework (plural constitutionalism).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_forecloses, conceptual, 'Whether the secular reading forecloses sibling readings or permits pluralist coexistence.').

omega_variable(
    gender_equality_extraction_asymmetry,
    'Does the civil law''s provision of gender equality in marriage/divorce rights (equal grounds for divorce, marital property division, custody) constitute genuine coordination or disguised extraction from patriarchal authority structures that beneficiaries (women, egalitarians) would win through cultural pressure regardless?',
    'Comparative analysis: did civil law gender protections accelerate or merely codify gains women had already won through social movements? Analysis of countries with different legal pluralism structures (e.g., Canada''s treatment of religious family law) and whether gender outcomes differ when legal alternatives exist.',
    'If gender equality is genuinely enabled only by civil law alternative, then women_seeking_exit are genuine beneficiaries of the constraint and the ε remains moderate. If gender equality was culturally inevitable and the civil law merely accelerates it, then the constraint''s extractiveness (from the egalitarian perspective) is lower—the civil law is riding an existing cultural trend rather than enabling a fundamentally new coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gender_equality_extraction_asymmetry, empirical, 'Whether gender equality rights in civil law constitute genuine coordination or codification of inevitable cultural trend.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority_kernel__secular_civil_reading, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t0, marriage_authority_kernel__secular_civil_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(marr_tr_t0, projected).
narrative_ontology:measurement(marr_tr_t10, marriage_authority_kernel__secular_civil_reading, theater_ratio, 10, 0.14).
narrative_ontology:measurement_basis(marr_tr_t10, observed).
narrative_ontology:measurement(marr_tr_t20, marriage_authority_kernel__secular_civil_reading, theater_ratio, 20, 0.17).
narrative_ontology:measurement_basis(marr_tr_t20, observed).
narrative_ontology:measurement(marr_tr_t35, marriage_authority_kernel__secular_civil_reading, theater_ratio, 35, 0.21).
narrative_ontology:measurement_basis(marr_tr_t35, observed).
narrative_ontology:measurement(marr_tr_t50, marriage_authority_kernel__secular_civil_reading, theater_ratio, 50, 0.22).
narrative_ontology:measurement_basis(marr_tr_t50, observed).
narrative_ontology:measurement(marr_tr_t70, marriage_authority_kernel__secular_civil_reading, theater_ratio, 70, 0.22).
narrative_ontology:measurement_basis(marr_tr_t70, observed).

% Extraction over time
narrative_ontology:measurement(marr_be_t0, marriage_authority_kernel__secular_civil_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement_basis(marr_be_t0, projected).
narrative_ontology:measurement(marr_be_t10, marriage_authority_kernel__secular_civil_reading, base_extractiveness, 10, 0.22).
narrative_ontology:measurement_basis(marr_be_t10, observed).
narrative_ontology:measurement(marr_be_t20, marriage_authority_kernel__secular_civil_reading, base_extractiveness, 20, 0.26).
narrative_ontology:measurement_basis(marr_be_t20, observed).
narrative_ontology:measurement(marr_be_t35, marriage_authority_kernel__secular_civil_reading, base_extractiveness, 35, 0.29).
narrative_ontology:measurement_basis(marr_be_t35, observed).
narrative_ontology:measurement(marr_be_t50, marriage_authority_kernel__secular_civil_reading, base_extractiveness, 50, 0.31).
narrative_ontology:measurement_basis(marr_be_t50, observed).
narrative_ontology:measurement(marr_be_t70, marriage_authority_kernel__secular_civil_reading, base_extractiveness, 70, 0.31).
narrative_ontology:measurement_basis(marr_be_t70, observed).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t0, marriage_authority_kernel__secular_civil_reading, suppression_requirement, 0, 0.08).
narrative_ontology:measurement_basis(marr_su_t0, projected).
narrative_ontology:measurement(marr_su_t10, marriage_authority_kernel__secular_civil_reading, suppression_requirement, 10, 0.1).
narrative_ontology:measurement_basis(marr_su_t10, observed).
narrative_ontology:measurement(marr_su_t20, marriage_authority_kernel__secular_civil_reading, suppression_requirement, 20, 0.13).
narrative_ontology:measurement_basis(marr_su_t20, observed).
narrative_ontology:measurement(marr_su_t35, marriage_authority_kernel__secular_civil_reading, suppression_requirement, 35, 0.16).
narrative_ontology:measurement_basis(marr_su_t35, observed).
narrative_ontology:measurement(marr_su_t50, marriage_authority_kernel__secular_civil_reading, suppression_requirement, 50, 0.18).
narrative_ontology:measurement_basis(marr_su_t50, observed).
narrative_ontology:measurement(marr_su_t70, marriage_authority_kernel__secular_civil_reading, suppression_requirement, 70, 0.18).
narrative_ontology:measurement_basis(marr_su_t70, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=70
narrative_ontology:measurement(marr_grid_01, marriage_authority_kernel__secular_civil_reading, accessibility_collapse(class), 0, 0.45).
narrative_ontology:measurement(marr_grid_02, marriage_authority_kernel__secular_civil_reading, accessibility_collapse(class), 70, 0.52).
narrative_ontology:measurement(marr_grid_03, marriage_authority_kernel__secular_civil_reading, accessibility_collapse(individual), 0, 0.48).
narrative_ontology:measurement(marr_grid_04, marriage_authority_kernel__secular_civil_reading, accessibility_collapse(individual), 70, 0.58).
narrative_ontology:measurement(marr_grid_05, marriage_authority_kernel__secular_civil_reading, accessibility_collapse(organizational), 0, 0.35).
narrative_ontology:measurement(marr_grid_06, marriage_authority_kernel__secular_civil_reading, accessibility_collapse(organizational), 70, 0.48).
narrative_ontology:measurement(marr_grid_07, marriage_authority_kernel__secular_civil_reading, accessibility_collapse(structural), 0, 0.28).
narrative_ontology:measurement(marr_grid_08, marriage_authority_kernel__secular_civil_reading, accessibility_collapse(structural), 70, 0.42).
narrative_ontology:measurement(marr_grid_09, marriage_authority_kernel__secular_civil_reading, resistance(class), 0, 0.58).
narrative_ontology:measurement(marr_grid_10, marriage_authority_kernel__secular_civil_reading, resistance(class), 70, 0.62).
narrative_ontology:measurement(marr_grid_11, marriage_authority_kernel__secular_civil_reading, resistance(individual), 0, 0.68).
narrative_ontology:measurement(marr_grid_12, marriage_authority_kernel__secular_civil_reading, resistance(individual), 70, 0.65).
narrative_ontology:measurement(marr_grid_13, marriage_authority_kernel__secular_civil_reading, resistance(organizational), 0, 0.62).
narrative_ontology:measurement(marr_grid_14, marriage_authority_kernel__secular_civil_reading, resistance(organizational), 70, 0.55).
narrative_ontology:measurement(marr_grid_15, marriage_authority_kernel__secular_civil_reading, resistance(structural), 0, 0.52).
narrative_ontology:measurement(marr_grid_16, marriage_authority_kernel__secular_civil_reading, resistance(structural), 70, 0.48).
narrative_ontology:measurement(marr_grid_17, marriage_authority_kernel__secular_civil_reading, stakes_inflation(class), 0, 0.38).
narrative_ontology:measurement(marr_grid_18, marriage_authority_kernel__secular_civil_reading, stakes_inflation(class), 70, 0.45).
narrative_ontology:measurement(marr_grid_19, marriage_authority_kernel__secular_civil_reading, stakes_inflation(individual), 0, 0.42).
narrative_ontology:measurement(marr_grid_20, marriage_authority_kernel__secular_civil_reading, stakes_inflation(individual), 70, 0.48).
narrative_ontology:measurement(marr_grid_21, marriage_authority_kernel__secular_civil_reading, stakes_inflation(organizational), 0, 0.35).
narrative_ontology:measurement(marr_grid_22, marriage_authority_kernel__secular_civil_reading, stakes_inflation(organizational), 70, 0.42).
narrative_ontology:measurement(marr_grid_23, marriage_authority_kernel__secular_civil_reading, stakes_inflation(structural), 0, 0.22).
narrative_ontology:measurement(marr_grid_24, marriage_authority_kernel__secular_civil_reading, stakes_inflation(structural), 70, 0.28).
narrative_ontology:measurement(marr_grid_25, marriage_authority_kernel__secular_civil_reading, suppression(class), 0, 0.12).
narrative_ontology:measurement(marr_grid_26, marriage_authority_kernel__secular_civil_reading, suppression(class), 70, 0.22).
narrative_ontology:measurement(marr_grid_27, marriage_authority_kernel__secular_civil_reading, suppression(individual), 0, 0.15).
narrative_ontology:measurement(marr_grid_28, marriage_authority_kernel__secular_civil_reading, suppression(individual), 70, 0.25).
narrative_ontology:measurement(marr_grid_29, marriage_authority_kernel__secular_civil_reading, suppression(organizational), 0, 0.08).
narrative_ontology:measurement(marr_grid_30, marriage_authority_kernel__secular_civil_reading, suppression(organizational), 70, 0.16).
narrative_ontology:measurement(marr_grid_31, marriage_authority_kernel__secular_civil_reading, suppression(structural), 0, 0.05).
narrative_ontology:measurement(marr_grid_32, marriage_authority_kernel__secular_civil_reading, suppression(structural), 70, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority_kernel__secular_civil_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(marriage_authority_kernel__secular_civil_reading, 0.12).
narrative_ontology:affects_constraint(marriage_authority_kernel__secular_civil_reading, marriage_authority_kernel__hindu_codified_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__secular_civil_reading, marriage_authority_kernel__muslim_shariat_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__secular_civil_reading, marriage_authority_kernel__christian_canonical_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__secular_civil_reading, marriage_authority_kernel__parsi_communal_reading).

% DUAL FORMULATION NOTE:
% The 'marriage authority kernel' decomposes into five structurally distinct constraint stories, one per reading. Each reading instantiates a different marriage law authority system with different ε values, beneficiary/victim structures, and classifications. The secular_civil_reading claims modest extractiveness (0.31) and rope classification; sibling readings will claim different types. The kernel is the spanning commitment (marriage law authority exists and must be grounded); the readings are different grounds (secular state/constitution, codified Hindu law, Shariat, Christian canon, Parsi custom). All five stories are linked via network.affects_constraints—changes in one reading's institutional stability or legal scope affect the others' operational environment (e.g., if civil law adoption accelerates, religious law demand decreases; if Hindu nationalism rises, Hindu law pressures to compete with civil law).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(marriage_authority_kernel__secular_civil_reading, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
