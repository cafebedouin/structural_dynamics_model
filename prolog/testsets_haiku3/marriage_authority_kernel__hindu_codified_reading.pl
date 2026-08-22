% ============================================================================
% CONSTRAINT STORY: marriage_authority_kernel__hindu_codified_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
 *   constraint_id: marriage_authority_kernel__hindu_codified_reading
 *   human_readable: Hindu Codified Marriage Authority (Civil Court Interpretation)
 *   domain: religious/legal/constitutional
 *
 * SUMMARY:
 *   The Hindu Marriage Act 1955 codified Hindu marriage law under the Indian
 *   Constitution, replacing a fragmented system of caste councils and
 *   regional custom with uniform statutory rules applied by state courts. The
 *   reading instantiated here is the brahminical-institutional
 *   interpretation: authority derives from codified dharmaśāstra law
 *   interpreted through state courts, preserving Hindu religious identity
 *   while applying secular procedural rationality. This reading competes with
 *   four sibling readings: the Muslim shariat reading (which treats marriage
 *   as governed by Islamic personal law and qazi authority), the Christian
 *   canonical reading (which grounds authority in the Christian Marriage Act
 *   1872 and ecclesiastical tradition), the Parsi communal reading (which
 *   preserves Parsi-specific custom), and the secular civil reading (which
 *   treats marriage as resting on individual consent under the Special
 *   Marriage Act 1954). The Hindu codified reading positions itself as a
 *   middle path: more uniform and gender-protective than caste custom, but
 *   more tradition-preserving than the secular civil alternative. The
 *   constraint operates at moderate extractiveness (0.58): it coordinates
 *   marriage rules uniformly across a religious community while
 *   simultaneously maintaining gender and caste hierarchies that tradition
 *   legitimates. Suppression is moderate (0.51): women and lower castes face
 *   relational and identity costs if they exit to secular law or cross-faith
 *   marriage, but the Act provides legal recourse (courts, appeals) that
 *   caste councils alone would not. Theater has risen from 0.12 to 0.28 over
 *   the interval, reflecting the gap between the Act's formal gender-equality
 *   language and its reproduction of patriarchal outcomes—a symptom of
 *   increasing litigation and reinterpretive effort without functional change
 *   in the underlying power asymmetry.
 *
 * KEY AGENTS:
 *   - Hindu patriarchal authority: brahminical institutions and caste councils that interpret dharmaśāstra and retain enforcement power over marriage rules within the community
 *   - State courts: apply the Hindu Marriage Act 1955 and claim constitutional supremacy over interpretation
 *   - Hindu women: subject to narrower divorce rights, property restrictions, and maintenance tied to marital status; moderate power but identity-locked exit
 *   - Lower-caste Hindus: powerless in the brahminical interpretive hierarchy; identity-locked; caste councils retain authority outside state courts
 *   - Interfaith couples: push toward secular law; moderate power and mobile exit options
 *   - Brahminical institutions: benefit from the Act's preservation of religious authority and dharmaśāstra interpretation within the state framework
 *   - Secular civil advocates: excluded from the personal law system; would replace it with constitutional individual rights
 *   - Constitutional courts: observe and can reinterpret the Act's compliance with constitutional guarantees
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority_kernel__hindu_codified_reading, 0.58).
domain_priors:suppression_score(marriage_authority_kernel__hindu_codified_reading, 0.51).
domain_priors:theater_ratio(marriage_authority_kernel__hindu_codified_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__hindu_codified_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(marriage_authority_kernel__hindu_codified_reading, suppression_requirement, 0.51).
narrative_ontology:constraint_metric(marriage_authority_kernel__hindu_codified_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__hindu_codified_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(marriage_authority_kernel__hindu_codified_reading, resistance, 0.47).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority_kernel__hindu_codified_reading, tangled_rope).
narrative_ontology:human_readable(marriage_authority_kernel__hindu_codified_reading, "Hindu Codified Marriage Authority (Civil Court Interpretation)").
narrative_ontology:topic_domain(marriage_authority_kernel__hindu_codified_reading, "religious/legal/constitutional").

domain_priors:requires_active_enforcement(marriage_authority_kernel__hindu_codified_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority_kernel__hindu_codified_reading, '03235bb1-0c69-444d-ba74-085b59c341b5').
narrative_ontology:cs_kernel_codification('03235bb1-0c69-444d-ba74-085b59c341b5', fixed_text).
narrative_ontology:cs_authority_grounding('03235bb1-0c69-444d-ba74-085b59c341b5', extraction).
narrative_ontology:cs_interpretation_layer_present('03235bb1-0c69-444d-ba74-085b59c341b5').
narrative_ontology:cs_reading_relation('03235bb1-0c69-444d-ba74-085b59c341b5', marriage_authority_kernel__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('03235bb1-0c69-444d-ba74-085b59c341b5', marriage_authority_kernel__christian_canonical_reading, coexists_with).
narrative_ontology:cs_reading_relation('03235bb1-0c69-444d-ba74-085b59c341b5', marriage_authority_kernel__parsi_communal_reading, coexists_with).
narrative_ontology:cs_reading_relation('03235bb1-0c69-444d-ba74-085b59c341b5', marriage_authority_kernel__secular_civil_reading, influences).
narrative_ontology:cs_axiom('03235bb1-0c69-444d-ba74-085b59c341b5', foundational, marriage_sacred_bond_doctrine).
narrative_ontology:cs_axiom_status(marriage_sacred_bond_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('03235bb1-0c69-444d-ba74-085b59c341b5', marriage_sacred_bond_doctrine, theological).
narrative_ontology:cs_axiom('03235bb1-0c69-444d-ba74-085b59c341b5', foundational, brahminical_interpretive_authority).
narrative_ontology:cs_axiom_status(brahminical_interpretive_authority, holdable).
narrative_ontology:cs_axiom_grounding('03235bb1-0c69-444d-ba74-085b59c341b5', brahminical_interpretive_authority, conventional).
narrative_ontology:cs_axiom('03235bb1-0c69-444d-ba74-085b59c341b5', secondary, patriarchal_hierarchy_natural_order).
narrative_ontology:cs_axiom_status(patriarchal_hierarchy_natural_order, overridden).
narrative_ontology:cs_axiom_grounding('03235bb1-0c69-444d-ba74-085b59c341b5', patriarchal_hierarchy_natural_order, empirically_contingent).
narrative_ontology:cs_reference_frame('03235bb1-0c69-444d-ba74-085b59c341b5', dharmasastra_codified_state_law).
narrative_ontology:cs_drift_state('03235bb1-0c69-444d-ba74-085b59c341b5', contemporary_constitutional_equality_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('03235bb1-0c69-444d-ba74-085b59c341b5', '').
narrative_ontology:cs_kernel_id(marriage_authority_kernel__hindu_codified_reading, marriage_authority_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__hindu_codified_reading, hindu_patriarchal_authority).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__hindu_codified_reading, state_courts).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__hindu_codified_reading, brahminical_institutions).
narrative_ontology:constraint_victim(marriage_authority_kernel__hindu_codified_reading, hindu_women).
narrative_ontology:constraint_victim(marriage_authority_kernel__hindu_codified_reading, lower_caste_hindus).
narrative_ontology:constraint_victim(marriage_authority_kernel__hindu_codified_reading, interfaith_couples).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__hindu_codified_reading, hindu_women).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hindu personal law institutions (dharmaśāstra councils, temple authorities, caste panchayats) interpret and enforce the Hindu Marriage Act 1955 within their jurisdictions. They define marriage as a sacred bond (samskara) and control property, divorce, maintenance, and custody outcomes through both formal opinion and informal community sanction. Identity fusion is total: the authority claims legitimacy from transmission of Vedic law, not from statutory codification.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__hindu_codified_reading, hindu_patriarchal_authority, agenda_setter,
    institutional, civilizational, identity_locked, national).

% Adjudicate disputes under the Hindu Marriage Act 1955, which they apply as uniform civil law to all those registered as Hindu. They claim authority from constitutional supremacy and statutory interpretation. They benefit from the legal jurisdiction the Act vests in them; they also pay the cost of navigating conflicts between the Act's secular language and Hindu doctrinal sources.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__hindu_codified_reading, state_courts, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority_kernel__hindu_codified_reading, state_courts, beneficiary).

% Subject to marriage rules that grant them limited divorce rights (grounds are narrower than those for husbands), restrict property control, and tie maintenance to marital status. The Act provides clearer property and maintenance rules than some traditional authorities would, but leaves interpretive discretion to courts that reproduce patriarchal dharmaśāstra logic. Exit through conversion (to Islam, Christianity, or secular civil code) is theoretically possible but socially costly; their identity as Hindu is often fused with family continuity.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__hindu_codified_reading, hindu_women, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority_kernel__hindu_codified_reading, hindu_women, beneficiary).

% Subject to the same Hindu Marriage Act framework but lack access to brahminical interpreters and courts; caste authorities retain enforcement power outside the state system. The formal uniformity of the Act masks caste-specific application. Exit through caste abandonment or interfaith conversion carries severe relational costs.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__hindu_codified_reading, lower_caste_hindus, payer,
    powerless, biographical, identity_locked, national).

% Hindu-Muslim or Hindu-Christian couples are pushed toward the Special Marriage Act 1954 (secular civil code) to avoid applying Hindu law to one party. The Hindu Marriage Act's implicit assumption of religious homogeneity forces exit from the Hindu framework for cross-faith unions. Exit is more feasible than for Hindu women or lower castes but carries community cost.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__hindu_codified_reading, interfaith_couples, payer,
    moderate, biographical, mobile, national).

% Brahminical temples, gurukuls, and dharmaśāstra councils retain interpretive authority over what counts as legitimate Hindu marriage, even within the state court system. The Act's language—referring to Hindu custom, sapinda relations, and sacred bond—channels legitimacy back to brahminical sources. They benefit from the constraint that reserves marriage authority within Hindu personal law rather than opening it to secular or minority-community rules.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__hindu_codified_reading, brahminical_institutions, beneficiary,
    powerful, civilizational, identity_locked, national).

% Constitutional reformers, gender-rights advocates, and secular legal theorists argue marriage law should rest on individual consent and equal rights, not community or religious tradition. They would replace all personal law codes (Hindu, Muslim, Christian, Parsi) with a single Uniform Civil Code. Their exclusion is structural: they lack the institutional base (religious authority, community affiliation) that the personal law system privileges.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__hindu_codified_reading, secular_civil_advocates, excluded,
    institutional, generational, constrained, national).

% The Supreme Court of India periodically reviews whether personal law codes comply with constitutional equality and liberty guarantees. They observe the constraint's operation and can issue reinterpretive rulings that modify its enforcement (e.g., recognizing Hindu marriage equality, expanding divorce rights). Analytical rather than structurally embedded in the constraint.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__hindu_codified_reading, constitutional_courts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_authority_kernel__hindu_codified_reading, brahminical_institutions).
narrative_ontology:fixing_cost_class(marriage_authority_kernel__hindu_codified_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes uniform rules for Hindu marriage within the Hindu community: marriage formation (offer, acceptance, presence of witnesses, parental consent where applicable), property rights, divorce grounds (adultery, cruelty, desertion, insanity), maintenance obligations, and custody of children. Replaces varied caste and regional custom with a single statutory framework that state courts uniformly apply.
% TRANSFER_FUNCTION: Transfers interpretive authority over Hindu family life from caste panchayats and regional custom to state courts applying the Hindu Marriage Act. Simultaneously transfers legitimacy from individual consent and equal rights to community continuity and dharmaśāstra tradition. Ties maintenance, property, and custody outcomes to marital status, distributing economic benefit to the spouse with the narrower exit options (typically the husband).
% ABSENT_VOICES: Secular civil-code reformers (who argue marriage should rest on individual consent, not community tradition) are structurally excluded because the system privileges those with religious institutional bases. Non-Hindu religious minorities and lower-caste Hindus outside brahminical authority have limited voice in interpretation, though they are subject to the constraint.
% DISAPPEARANCE_RATIONALE: If the Hindu Marriage Act and its enforcement apparatus vanished, Hindu marriage law would revert to caste-specific and regional custom (if brahminical authorities retained authority) or would be absorbed into a secular civil code applied uniformly across all religious communities (if the constitutional path prevailed). The state's unified adjudicatory structure would fragment or merge with other personal law codes.
% FOUNDING_PROBLEM: Hindu marriage had no uniform law at independence (1947): caste councils, regional custom, and Sanskrit dharmaśāstra texts provided overlapping and conflicting authorities. The Hindu Marriage Act 1955 was enacted to bring uniform, secular-facing rules to Hindu marriage while retaining religious authority within those rules (a constitutional compromise: personal law for religious communities, but codified and uniform within each).
% FOUNDING_PROBLEM_CORROBORATION: The constitutional framers and the Act's legislative sponsors attest the founding problem was real: India had inherited a fragmented personal law system that disadvantaged women and lower castes. Constitutional courts confirm the problem was pressing. Secular civil-code advocates and gender-rights groups attest the founding problem was not best solved by preserving religious authority within uniform codification — they argue it was solved by constitutional equality principles. Brahminical authorities and conservative Hindu jurists attest the problem was solved by restoring dharmaśāstra unity through the Act's codification.
narrative_ontology:disappearance_verdict(marriage_authority_kernel__hindu_codified_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority_kernel__hindu_codified_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority_kernel__hindu_codified_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
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
 *   Extractiveness at 0.58 reflects the constraint's dual nature: it provides uniform rules (coordination benefit) while maintaining gender and caste hierarchies (extraction cost). The constraint extracts from women by tying maintenance and custody to marital status and narrowing divorce grounds; it extracts from lower castes by preserving brahminical interpretation authority. The Act benefits brahminical institutions (who gain state-enforced interpretive authority) and state courts (who gain jurisdiction). Suppression at 0.51 is moderate because women and lower castes face identity costs and relational pressure if they exit (conversion, secular marriage), but state courts provide legal recourse and appeals that caste councils alone would not. Theater has risen from 0.12 to 0.28 as constitutional pressure for gender equality has grown: the Act's formal language now includes gender-neutral grounds for divorce and maintenance, but courts continue to reproduce patriarchal outcomes in practice. The rising theater ratio indicates the constraint's coordination story (uniform, rational rules) increasingly covers its extractive function (hierarchy preservation), not that the extractive function has declined. Measurements are shared on one grid so every metric appears at every time point.
 *
 * PERSPECTIVAL GAP:
 *   The patriarch and the state court perceive the constraint as genuine coordination and rational law-making; the woman and the lower-caste Hindu perceive it as enforced hierarchy dressed in gender-neutral language. From the beneficiary seats, the Act solved a real problem (fragmented custom, unpredictable rules); from the payer seats, it unified oppression at the state level. The state court's seat is internally divided: it claims constitutional supremacy over interpretation, but dharmaśāstra doctrine in its own case law reproduces patriarchal precedent. The engine computes these seat divergences from the structural data (power, exit options, beneficiary/victim position) independently of the narrative framing.
 *
 * DIRECTIONALITY LOGIC:
 *   The Hindu patriarchal authority and brahminical institutions are full beneficiaries (d near 0): they gain interpretive authority, institutional standing, and the legal preservation of their own doctrinal preferences. State courts are partially beneficiary (d ~0.25-0.35): they gain jurisdiction and constitutional authority, but pay the cost of navigating conflicts between the Act's secular language and Hindu doctrinal sources. Hindu women are targets (d near 0.75): they bear narrowed divorce grounds, property restrictions, and maintenance asymmetry, with constrained exit (identity-locked). Lower-caste Hindus are full targets (d near 0.90): powerless in brahminical hierarchy and identity-locked to caste membership. Interfaith couples are targets (d near 0.65) but with more mobile exit (Special Marriage Act is available). Secular civil advocates are excluded from the directionality calculation: they lack structural position in the constraint and would be repositioned entirely under a different framework. Constitutional courts are analytical observers (d = 0.5, external), measuring compliance and issuing reinterpretive rulings.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (fragmented marriage law without uniform rules) was genuinely live at the Act's enactment (1955). By 2010–2020, constitutional courts and gender-rights advocates broadly attest the founding problem is substantially dead or solved: uniform codified rules now exist, property and maintenance rights are clearly stated, and appeal mechanisms are in place. Yet the constraint persists and suppression has risen slightly (0.38 to 0.51) as enforcement machinery has hardened to defend hierarchy against court-ordered equality reforms. The theater ratio rising from 0.12 to 0.28 indicates the Act increasingly functions as narrative cover (gender-neutral language in opinion, formal grounds for relief) while outcomes remain patriarchal. This pattern—founding problem dead, extraction steady, theater rising, enforcement hardening—marks mandatrophy: the Act's coordinating function (fragmentation solved) has outlived, but its extractive function (hierarchy preservation) persists and requires increasing performance of legitimacy to maintain. The constraint should be classified as tangled_rope precisely because it coordinates (provides uniform rules) and extracts (maintains hierarchy) simultaneously, not because one function subsumes the other.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    dharmasastra_legitimacy_vs_statutory_positivism,
    'Is the Hindu Marriage Act''s authority grounded in dharmaśāstra doctrine transmitted through brahminical institutions, or in the Constitution and statutory law as interpreted by state courts?',
    'Examine court opinions over the interval (0-70): do courts justify decisions by citing Vedic sources and brahminical precedent, or by citing the Act''s statutory text and constitutional principles? Has the ratio shifted over time? Courts claiming constitutional supremacy while grounding reasoning in dharmaśāstra indicate unresolved tension.',
    'If authority is genuinely dharmaśāstra-derived, the constraint derives legitimacy from religious tradition and the brahminical hierarchy is structural (not merely cultural). If authority is statutory-derived, the constraint derives legitimacy from constitutionalism and gender-neutral interpretation should reshape outcomes. Mixed authority makes the constraint a fusion mechanism where religious legitimacy is laundered through state institutions.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(dharmasastra_legitimacy_vs_statutory_positivism, empirical, 'Whether the Hindu Marriage Act''s authority grounds in transmission of dharmaśāstra or in constitutional statutory law.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the measured suppression (0.51) primarily structural (legal barriers, caste enforcement, relational costs) or internalized (beliefs about shame, duty, identity inviolability)?',
    'Post-exit trajectory: if women and lower-caste Hindus who exit to secular law or conversion continue to experience pressure from family and community to return, suppression is partly internalized. If pressure drops sharply after exit, suppression is primarily structural. Track longitudinal data on post-exit outcomes for women (maintenance compliance, custody disputes, remarriage acceptance) and lower castes (community re-entry, status restoration).',
    'If primarily internalized, the constraint''s effective suppression is higher than the 0.51 scalar suggests—targets carry the suppression with them after exit. If primarily structural, fixing the constraint requires legal reform. If mixed, remediation requires both legal change and identity re-formation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether the Hindu marriage constraint''s suppression is structural or internalized.').

omega_variable(
    uniform_code_benefit_to_whom,
    'Did the uniform Hindu Marriage Act''s codification of rules primarily benefit women and lower castes (by replacing unpredictable caste councils) or primarily benefit brahminical institutions (by state-enforcing their interpretation)?',
    'Compare outcomes pre- and post-Act for women (divorce rates, property retention, maintenance awarded) and lower castes (caste-council authority vs. state-court appeals, caste-based property rules vs. Act-based rules). If the Act''s uniformity lifted women and lower-caste outcomes relative to caste-council baseline, it solved a coordination problem. If outcomes worsened or remained stagnant despite legal clarity, the Act primarily served brahminical institutional capture.',
    'If the Act primarily benefited its nominal beneficiaries, it is a genuine rope with coordination function. If it primarily served brahminical capture, it is a snare that used the veneer of uniformity to centralize extractive authority. The measurement series suggests the constraint''s balance plateaued post-1980 (extraction flat from 0.50-0.58, theater rising modestly), indicating the coordination benefit was exhausted and the extractive function stabilized.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(uniform_code_benefit_to_whom, empirical, 'Whether the Hindu Marriage Act''s uniformity primarily benefited or extracted from its nominal beneficiaries.').

omega_variable(
    kernel_reading_alternative_secular_civil_frame,
    'Could the same marriage disputes (property, custody, divorce grounds, maintenance) be governed by the secular Special Marriage Act 1954 without loss of coordination benefit?',
    'Compare outcomes for Hindu couples who elect Special Marriage Act governance (available since 1954) with outcomes under the Hindu Marriage Act. If Special Act outcomes show equal clarity, lower dispute rates, and better gender equity without religious authority, the coordination function is separable from religious legitimacy. If Special Act courts report that Hindu-tradition considerations are still invoked by parties, the readings are not fully separable.',
    'If the readings are separable (secular codification provides equivalent coordination without hierarchy), the Hindu codified reading is a cultural choice defended post-hoc, not a structural necessity. If inseparable (Hindu-tradition reasoning persists even under Special Act), the readings genuinely differ in their conception of what marriage IS (sacrament vs. contract), and the constraint''s extraction component is intrinsic to the religious framing. This omega addresses the kernel-level contest: is the disagreement about how to govern marriage (the Act''s substance) or about what authority grounds marriage law (the constitutional question)?',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_alternative_secular_civil_frame, conceptual, 'Whether the Hindu codified reading and secular civil reading offer substantively different marriage governance or just different authority frames for equivalent rules.').

omega_variable(
    brahminical_identity_lock_persistence,
    'For brahminical institutions and high-caste authorities, is the identity fusion to the Hindu Marriage Act (as a dharmaśāstra codification) a genuine structural commitment, or a strategic claim that would dissolve if the Act were revised toward gender equality?',
    'Observe brahminical institutional response to Supreme Court rulings that expand women''s divorce rights, recognize Hindu marriage equality, or move the Act''s language toward gender neutrality. If brahminical voices oppose the changes on grounds that they violate dharmaśāstra, identity-locking is genuine. If they accept the changes as a reinterpretation of dharmaśāstra (rather than abandoning the Act), the identity-lock is fluid (not inviolable).',
    'If identity-locking is genuine and inviolable, brahminical opposition to gender-equality reforms is a structural constraint that cannot be overcome by litigation alone—it requires either reform that preserves religious authority or a constitutional shift that eliminates personal law codes entirely. If identity-locking is fluid (dharmaśāstra can be reinterpreted), brahminical opposition is a political position that can shift with institutional pressure. The distinction matters for evaluating whether the Hindu codified reading will survive or foreclosed by constitutional gender-equality doctrines.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(brahminical_identity_lock_persistence, conceptual, 'Whether brahminical identity-lock to the Hindu Marriage Act is structurally rigid or strategically fluid.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority_kernel__hindu_codified_reading, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t0, marriage_authority_kernel__hindu_codified_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(marr_tr_t10, marriage_authority_kernel__hindu_codified_reading, theater_ratio, 10, 0.15).
narrative_ontology:measurement(marr_tr_t20, marriage_authority_kernel__hindu_codified_reading, theater_ratio, 20, 0.19).
narrative_ontology:measurement(marr_tr_t30, marriage_authority_kernel__hindu_codified_reading, theater_ratio, 30, 0.24).
narrative_ontology:measurement(marr_tr_t40, marriage_authority_kernel__hindu_codified_reading, theater_ratio, 40, 0.27).
narrative_ontology:measurement(marr_tr_t50, marriage_authority_kernel__hindu_codified_reading, theater_ratio, 50, 0.28).
narrative_ontology:measurement(marr_tr_t60, marriage_authority_kernel__hindu_codified_reading, theater_ratio, 60, 0.28).
narrative_ontology:measurement(marr_tr_t70, marriage_authority_kernel__hindu_codified_reading, theater_ratio, 70, 0.28).

% Extraction over time
narrative_ontology:measurement(marr_be_t0, marriage_authority_kernel__hindu_codified_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(marr_be_t10, marriage_authority_kernel__hindu_codified_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(marr_be_t20, marriage_authority_kernel__hindu_codified_reading, base_extractiveness, 20, 0.55).
narrative_ontology:measurement(marr_be_t30, marriage_authority_kernel__hindu_codified_reading, base_extractiveness, 30, 0.57).
narrative_ontology:measurement(marr_be_t40, marriage_authority_kernel__hindu_codified_reading, base_extractiveness, 40, 0.58).
narrative_ontology:measurement(marr_be_t50, marriage_authority_kernel__hindu_codified_reading, base_extractiveness, 50, 0.58).
narrative_ontology:measurement(marr_be_t60, marriage_authority_kernel__hindu_codified_reading, base_extractiveness, 60, 0.58).
narrative_ontology:measurement(marr_be_t70, marriage_authority_kernel__hindu_codified_reading, base_extractiveness, 70, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t0, marriage_authority_kernel__hindu_codified_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(marr_su_t10, marriage_authority_kernel__hindu_codified_reading, suppression_requirement, 10, 0.4).
narrative_ontology:measurement(marr_su_t20, marriage_authority_kernel__hindu_codified_reading, suppression_requirement, 20, 0.43).
narrative_ontology:measurement(marr_su_t30, marriage_authority_kernel__hindu_codified_reading, suppression_requirement, 30, 0.46).
narrative_ontology:measurement(marr_su_t40, marriage_authority_kernel__hindu_codified_reading, suppression_requirement, 40, 0.49).
narrative_ontology:measurement(marr_su_t50, marriage_authority_kernel__hindu_codified_reading, suppression_requirement, 50, 0.5).
narrative_ontology:measurement(marr_su_t60, marriage_authority_kernel__hindu_codified_reading, suppression_requirement, 60, 0.51).
narrative_ontology:measurement(marr_su_t70, marriage_authority_kernel__hindu_codified_reading, suppression_requirement, 70, 0.51).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority_kernel__hindu_codified_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(marriage_authority_kernel__hindu_codified_reading, 0.12).
narrative_ontology:affects_constraint(marriage_authority_kernel__hindu_codified_reading, marriage_authority_kernel__muslim_shariat_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__hindu_codified_reading, marriage_authority_kernel__christian_canonical_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__hindu_codified_reading, marriage_authority_kernel__parsi_communal_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__hindu_codified_reading, marriage_authority_kernel__secular_civil_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of five readings of the contested kernel 'what authority grounds Hindu marriage law in India?' The Hindu codified reading (this story) treats authority as derived from the Hindu Marriage Act 1955 codified as dharmaśāstra law interpreted by state courts. The sibling readings instantiate different authority framings: Muslim shariat reading (Islamic personal law and qazi authority), Christian canonical reading (Christian Marriage Act 1872 and canonical tradition), Parsi communal reading (Parsi-specific 1936 Act), secular civil reading (individual consent under Special Marriage Act 1954). Each reading has a distinct ε (extractiveness from its own logic): the Hindu codified reading at 0.58 (moderate extraction through hierarchy preservation), the secular civil reading at 0.15-0.20 (minimal extraction through individual equality), the Muslim shariat reading at 0.62-0.68 (higher extraction in patriarchal interpretation). The readings do not resolve to a single constraint; they decompose as a family of five stories linked by the shared kernel and different authority framings. The Hindu codified reading influences the secular civil reading by setting a precedent that personal law codes can be codified and uniform, creating constitutional pressure for the secular alternative to expand. It coexists (not forecloses) with the Muslim and Christian readings, as they operate in different religious communities.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(marriage_authority_kernel__hindu_codified_reading, powerful, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
