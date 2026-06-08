% ============================================================================
% CONSTRAINT STORY: christian_canonical_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_christian_canonical_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: christian_canonical_reading
 *   human_readable: Christian Canonical Authority in Indian Marriage Law (1872 Reading)
 *   domain: comparative_law/religious_governance/family_law
 *
 * SUMMARY:
 *   The Christian Marriage Act of 1872 codified Christian canonical law
 *   principles into Indian civil law, establishing fault-based divorce,
 *   ecclesiastical jurisdiction over annulment, and sacramental
 *   indissolubility as the legal framework for Christian marriage in India.
 *   This constraint instantiates one reading of a contested kernel — the
 *   authority structure that governs Christian marriage and family
 *   relationships in India. The reading grounds its legitimacy in canonical
 *   theology (the doctrine that marriage is a sacrament indissoluble except
 *   by death or papal dispensation) as transmitted through British colonial
 *   legal codification. The constraint exhibits asymmetric extraction: the
 *   church hierarchy and male heads of household benefit from legal
 *   enforcement of indissolubility and fault-based divorce bars, while women
 *   seeking divorce and interfaith couples bear costs through trapped exit
 *   options and ecclesiastical oversight. The structural data — identifiable
 *   beneficiaries, victims, and enforcement requirements — reveal this as not
 *   a timeless natural law but a constructed institutional arrangement whose
 *   claim to inevitability naturalizes contingent religious/legal choices.
 *   The theater ratio has risen from 0.42 (1872) to 0.75 (2022), reflecting
 *   growing disconnection between the stated doctrine (eternal Christian
 *   principle) and actual practice (many Christian denominations now permit
 *   no-fault divorce; Indian Christians increasingly use secular civil code
 *   for pragmatic exit). The constraint is one of six readings of the
 *   marriage_authority_kernel; others include Hindu codified marriage law
 *   (1955 reading), Muslim Shariat-based reading, Parsi communal reading, and
 *   secular civil reading. Each reading instantiates different beneficiaries,
 *   different victim sets, and different legitimacy claims.
 *
 * KEY AGENTS:
 *   - Christian Church Authority (institutional/arbitrage): Primary beneficiary. Derives institutional power from canonical law codification, ecclesiastical tribunals for annulment, recognition of church doctrine in civil law.
 *   - Male Heads of Household (powerful/constrained): Secondary beneficiary. Default presumption in inheritance, guardianship, and custody; legal bars to wife-initiated divorce.
 *   - Women Seeking Divorce (powerless/trapped): Primary victim. Cannot exit failed marriages without proving fault (cruelty, adultery, desertion); carry social stigma; often economically dependent.
 *   - Interfaith Couples (moderate/constrained): Secondary victim. Face barriers to marriage recognition if partner is not baptized Christian; lose autonomy in divorce and religious practice.
 *   - Non-Conformist Christian Minorities (moderate/constrained): Tertiary victim. Evangelical, Pentecostal, and other non-Catholic/non-Orthodox communities whose theology permits no-fault divorce but are bound by canonical framework.
 *   - Post-Colonial Indian State (institutional/constrained): Maintains framework through institutional inertia and minority-protection commitments; lacks political will to amend.
 *   - Women's Rights Coalition (organized/mobile): Organized agents advocating constitutional reform; see scaffold sunset via Articles 14, 15, 21.
 *   - Analytical Observer (analytical/analytical): Sees constraint from civilizational view; risks naturalizing constructed doctrine as timeless principle.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(christian_canonical_reading, 0.35).
domain_priors:suppression_score(christian_canonical_reading, 0.48).
domain_priors:theater_ratio(christian_canonical_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(christian_canonical_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(christian_canonical_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(christian_canonical_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(christian_canonical_reading, tangled_rope).
narrative_ontology:human_readable(christian_canonical_reading, "Christian Canonical Authority in Indian Marriage Law (1872 Reading)").
narrative_ontology:topic_domain(christian_canonical_reading, "comparative_law/religious_governance/family_law").

domain_priors:requires_active_enforcement(christian_canonical_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(christian_canonical_reading, '1f6dffae-661c-4f8c-9a7b-42f7d9669387').
narrative_ontology:cs_kernel_codification('1f6dffae-661c-4f8c-9a7b-42f7d9669387', fixed_text).
narrative_ontology:cs_authority_grounding('1f6dffae-661c-4f8c-9a7b-42f7d9669387', extraction).
narrative_ontology:cs_interpretation_layer_present('1f6dffae-661c-4f8c-9a7b-42f7d9669387').
narrative_ontology:cs_reading_relation('1f6dffae-661c-4f8c-9a7b-42f7d9669387', christian_canonical_reading__hindu_codified_reading, influences).
narrative_ontology:cs_reading_relation('1f6dffae-661c-4f8c-9a7b-42f7d9669387', christian_canonical_reading__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('1f6dffae-661c-4f8c-9a7b-42f7d9669387', christian_canonical_reading__parsi_communal_reading, coexists_with).
narrative_ontology:cs_reading_relation('1f6dffae-661c-4f8c-9a7b-42f7d9669387', christian_canonical_reading__secular_civil_reading, coexists_with).
narrative_ontology:cs_axiom('1f6dffae-661c-4f8c-9a7b-42f7d9669387', foundational, marriage_is_sacrament_indissoluble).
narrative_ontology:cs_axiom_status(marriage_is_sacrament_indissoluble, overridden).
narrative_ontology:cs_axiom_grounding('1f6dffae-661c-4f8c-9a7b-42f7d9669387', marriage_is_sacrament_indissoluble, theological).
narrative_ontology:cs_axiom('1f6dffae-661c-4f8c-9a7b-42f7d9669387', foundational, church_authority_over_matrimonial_dissolution).
narrative_ontology:cs_axiom_status(church_authority_over_matrimonial_dissolution, holdable).
narrative_ontology:cs_axiom_grounding('1f6dffae-661c-4f8c-9a7b-42f7d9669387', church_authority_over_matrimonial_dissolution, conventional).
narrative_ontology:cs_reference_frame('1f6dffae-661c-4f8c-9a7b-42f7d9669387', sacramental_marriage_doctrine).
narrative_ontology:cs_drift_state('1f6dffae-661c-4f8c-9a7b-42f7d9669387', post_vatican_two_contemporary, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('1f6dffae-661c-4f8c-9a7b-42f7d9669387', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(christian_canonical_reading, marriage_authority_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(christian_canonical_reading, christian_church_authority).
narrative_ontology:constraint_beneficiary(christian_canonical_reading, male_heads_of_household).
narrative_ontology:constraint_beneficiary(christian_canonical_reading, canonically_orthodox_divorce_avoiders).
narrative_ontology:constraint_victim(christian_canonical_reading, women_seeking_divorce).
narrative_ontology:constraint_victim(christian_canonical_reading, interfaith_couples).
narrative_ontology:constraint_victim(christian_canonical_reading, non_conformist_christian_minorities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(christian_canonical_reading, non_conformist_christian_communities).
narrative_ontology:constraint_vindicates(christian_canonical_reading, sacramental_indissolubility_of_marriage).
narrative_ontology:constraint_vindicates(christian_canonical_reading, church_jurisdiction_over_matrimony).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The church (Catholic, Orthodox, some Protestant denominations) administers canonical law through ecclesiastical tribunals for annulment and marriage dissolution cases. The 1872 Act gives legal force to church doctrine and reservs certain matters (validity of marriage, annulment grounds) to ecclesiastical judgment. The church benefits from state recognition of its authority and can exit through legislative advocacy or by ignoring state law (as many denominations now do).
narrative_ontology:constraint_stakeholder(christian_canonical_reading, christian_church_hierarchy, agenda_setter,
    institutional, generational, arbitrage, national).

% Women married under the Christian Marriage Act cannot initiate divorce without proving grounds (cruelty, adultery, desertion). They bear the cost of failed marriages through legal bars to exit, carry social stigma, and often lack economic independence. Exit from the marriage itself requires proving fault — a high barrier designed to deter divorce.
narrative_ontology:constraint_stakeholder(christian_canonical_reading, women_seeking_divorce, payer,
    powerless, biographical, trapped, national).

% Men under the Act have default presumptions in custody, guardianship, and inheritance. They can exit the marriage through legal separation or by seeking grounds for divorce, but the fault requirement means they cannot be divorced without their misconduct. They co-administer the system through male-dominated church hierarchies and through informal social enforcement of marriage contracts.
narrative_ontology:constraint_stakeholder(christian_canonical_reading, male_heads_of_household, beneficiary,
    powerful, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(christian_canonical_reading, male_heads_of_household, agenda_setter).

% Couples in which one party is Christian and the other is not face barriers to marriage recognition (one party must be baptized for full Christian sacramental marriage). They also face divorce constraints: if they marry under the Christian Marriage Act, they are bound by its fault-based framework even if the non-Christian partner's religious tradition permits no-fault divorce. They can exit to the Special Marriage Act 1954 (secular civil marriage) but at the cost of religious recognition.
narrative_ontology:constraint_stakeholder(christian_canonical_reading, interfaith_couples, payer,
    moderate, biographical, constrained, national).

% Evangelical, Pentecostal, and independent Christian churches whose theology permits no-fault divorce or remarriage are constrained by the canonical framework if their members marry under the Christian Marriage Act. They bear costs through doctrinal conflict (their theology is not honored in law) and through members' exodus to secular marriage alternatives. They have limited ability to exit because the legal framework applies irrespective of individual denomination.
narrative_ontology:constraint_stakeholder(christian_canonical_reading, non_conformist_christian_communities, payer,
    moderate, generational, constrained, national).

% The state maintains the Christian Marriage Act framework through lack of legislative reform. The state has constitutional commitments to secular governance (Articles 14, 15, 44) but also to protection of minority religious rights (Article 25). The state performs both secularism and religious pluralism while maintaining religious law enclaves. Constraints to exit include political gridlock (religious minorities fear losing legal autonomy; secularists fear violating religious freedom), demand for community autonomy, and administrative complexity of unified civil code reform.
narrative_ontology:constraint_stakeholder(christian_canonical_reading, post_colonial_indian_state, agenda_setter,
    institutional, generational, constrained, national).

% Women's groups, secular-liberal constitutionalists, and civil rights organizations advocate for reform of the Christian Marriage Act (and all religious marriage law) toward a secular civil code with no-fault divorce available equally to both parties. They mobilize legal strategy (constitutional litigation under Articles 14, 15, 21), legislative advocacy (demand for civil code bills), and community support. They are not bound by the Christian Marriage Act's constraints and have exit pathways through secular marriage and divorce alternatives.
narrative_ontology:constraint_stakeholder(christian_canonical_reading, womens_rights_coalition, observer,
    organized, generational, mobile, national).

% The doctrine that marriage is a sacrament indissoluble except by death or papal dispensation is the ideological kernel of the Christian canonical reading. This doctrine is not a stakeholder but is vindicated by the legal structure. It is included here to note that what the constraint vindicates (the doctrine) is analytically distinct from who benefits from it (the church, male household heads). The doctrine is listed under base_properties.vindicated_propositions, not beneficiaries.
narrative_ontology:constraint_stakeholder(christian_canonical_reading, sacramental_indissolubility_doctrine, observer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(christian_canonical_reading, sacramental_indissolubility_doctrine).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Recognition and legal validity of Christian marriage; ecclesiastical authority to adjudicate marriage dissolution; standardized procedures for marriage registration and divorce grounds.
% TRANSFER_FUNCTION: The constraint transfers legal authority from individual consent to sacramental doctrine: women transfer autonomy over divorce exit to the church and male authority; interfaith couples transfer religious autonomy to canonical requirements; non-conformist denominations transfer doctrinal authority to canonical law. The transfer is asymmetric: church authority gains power; trapped parties lose autonomy.
% ABSENT_VOICES: Christian women who have migrated to secular divorce; progressive Christian theologians who have adopted post-Vatican II positions on divorce; younger generations of Indian Christians who practice secular marriage outside the 1872 Act. These voices would object to the fault-based regime and ecclesiastical oversight but have largely withdrawn from within the Christian Marriage Act framework by using secular alternatives. Their absence from the legal framework means the remaining (more conservative) Christian voice dominates codification.
% DISAPPEARANCE_RATIONALE: The conservative Christian reading claims world_rearranges: if the Christian Marriage Act disappeared, Christian marriage would lack legal recognition and church authority would be undermined. The secular reading claims world_unchanged: if the Act disappeared, Christians would marry under the secular Special Marriage Act 1954, nothing substantive changes (as many already do). The women's rights reading claims world_rearranges (in desirable direction): if the Act disappeared, women would gain equal divorce rights. The factual test: have the Act's core functions (recognizing Christian marriage, enforcing indissolubility) become substitutable by other mechanisms (civil marriage recognition, the reality of Christian communities permitting no-fault divorce outside law)? Evidence suggests substantial substitutability — many Christian functions have been absorbed by secular alternatives, suggesting disappearance would be more world_unchanged than the conservative reading admits.
% FOUNDING_PROBLEM: The founding problem (1872 objective) was to codify Christian canonical marriage law within India's civil legal framework during British colonial rule, imposing fault-based divorce bars and ecclesiastical jurisdiction over marriage dissolution, thereby enforcing the sacramental indissolubility doctrine and recognizing church authority over matrimonial matters.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem was resolved (partially: doctrine still affirmed by some denominations; substantially: supplanted by secular alternatives). Corroboration: (1) External — women's rights advocates, secular jurists, constitutional scholars; (2) Internal — practicing Christians who have exited the framework; (3) Institutional — Indian courts increasingly read the constraint as violating constitutional equality. Corroboration is NOT from the beneficiary set (church hierarchy, conservative theologians) because their testimony cannot be disentangled from institutional interest in maintaining the regime.
narrative_ontology:disappearance_verdict(christian_canonical_reading, contested).
narrative_ontology:founding_problem_status(christian_canonical_reading, dead).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: WOMAN TRAPPED IN FAULT-BASED REGIME — Cannot exit an abusive or failed marriage without proving fault (cruelty, adultery, desertion). Trapped by legal requirement for grounds, social stigma, and economic dependency. No arbitrage option available. Maximum experienced extraction.
constraint_indexing:constraint_classification(christian_canonical_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: INTERFAITH COUPLE (TANGLED ROPE) — Constrained by the Christian Marriage Act's requirement that both parties share canonical Christian commitment for the marriage to be recognized. Benefits from legal recognition of marriage itself; pays through loss of autonomy in divorce and religious practice. Coordination function exists (marriage recognition); extraction exists (canonical constraints). Asymmetric but not total.
constraint_indexing:constraint_classification(christian_canonical_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CHURCH AUTHORITY (ROPE) — Derives institutional authority from canonical law codification. Benefits from legal recognition of its marriage doctrine and disciplinary power over annulment and divorce. Sees the constraint as coordination: the law stabilizes the sacramental understanding of marriage and legitimizes ecclesiastical tribunals. Net beneficiary. Arbitrage option: church can petition government for legislative amendment or can use parallel ecclesiastical processes.
constraint_indexing:constraint_classification(christian_canonical_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: MALE HEAD OF HOUSEHOLD (TANGLED ROPE) — Experiences coordination benefit (legal recognition of marriage, default custody and inheritance presumptions). Experiences extraction cost (requirement to prove fault for divorce, ecclesiastical oversight of marriage dissolution). Asymmetric but moderate — many exit paths available through informal separation, remarriage outside Christian doctrine, or relocation.
constraint_indexing:constraint_classification(christian_canonical_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: POST-COLONIAL STATE (PITON) — Maintains the 1872 canonical framework not because it functions well but through institutional inertia and minority-protection commitments. The state inherited the colonial codification and lacks political will to amend. Theater ratio high: the state performs constitutional secularism while maintaining religious law enclaves. Sunset pressure exists (demand for civil divorce equality) but the constraint persists through institutional degradation rather than active function.
constraint_indexing:constraint_classification(christian_canonical_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: WOMEN'S RIGHTS COALITION (SCAFFOLD) — Organized actors (women's groups, secular-liberal constitutionalists) see the canonical regime as a temporary constraint whose sunset is both desirable and structurally arriving through constitutional pressure (Articles 14, 15, 21 of the Constitution). See legislative pathways to uniform civil code or opt-in secular marriage registration. Mobile exit options: advocate for statutory reform, support migration to secular frameworks, litigate in constitutional courts. Sunset rationale: courts increasingly read fault-based divorce provisions as violating constitutional equality; newer Christian communities adopt no-fault divorce practices.
constraint_indexing:constraint_classification(christian_canonical_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER — FROM NATURAL LAW VIEW (MOUNTAIN) — From a civilizational view, sacramental indissolubility of marriage might appear as a timeless natural-law feature of Christian theology — a doctrine that persists regardless of legal codification because it reflects immutable principles. However, the structural data contradicts this: identifiable beneficiaries (church authority, male household heads), victims (women, interfaith couples), and enforcement requirements reveal this as a false summit — the 'natural' appearance naturalizes what is a contingent religious/institutional choice.
constraint_indexing:constraint_classification(christian_canonical_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(christian_canonical_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(christian_canonical_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(christian_canonical_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(christian_canonical_reading, TR),
    TR >= 0.70.

:- end_tests(christian_canonical_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35): Moderate. The Christian reading extracts through divorce barriers and ecclesiastical oversight, but the extraction is not severe because (1) many Christian communities now ignore the canonical bar and seek secular divorce outside the act's scope; (2) interfaith couples have alternative marriage pathways; (3) property rights and inheritance have been substantially reformed outside the 1872 framework. The value reflects that the constraint's extraction mechanism is real but increasingly bypassed rather than internalized. Suppression (0.48): Moderate-high. Barriers exist (fault requirement, ecclesiastical tribunals, social stigma, religious framing that presents doctrine as unchangeable) but are not total — women can petition courts, secular alternatives are available, and state courts increasingly apply constitutional equality protections over canonical bars. Suppression is declining (0.52 in 1872 to 0.42 in 2022) as constitutional pressure mounts. Theater ratio (0.58, rising to 0.75): High and rising. The constraint increasingly appears as performative: church doctrine persists in law while actual Christian practice has shifted (Vatican II, Protestant denomination reforms, Indian Christian community practice of secular divorce). The gap between stated doctrine (sacramental indissolubility) and lived practice (Christian families using no-fault civil code) indicates the constraint is maintained through institutional inertia and legal text rather than genuine community commitment.
 *
 * PERSPECTIVAL GAP:
 *   The Christian canonical reading produces perspectival divergence across all contexts. The church authority sees coordination (rope) — legal recognition of doctrine, ecclesiastical power. The woman trapped in a failed marriage sees extraction (snare) — no exit without proving abuse. The interfaith couple sees mixed coordination and extraction (tangled rope) — marriage recognized but autonomy constrained. The post-colonial state sees degraded ritual (piton) — maintains framework through inertia, not function. The women's rights coalition sees temporary constraint with a sunset (scaffold) — constitutional pressure and legislative pathways toward reform. The analytical observer risks seeing timeless doctrine (mountain) but the structural data reveals false summit — identifiable beneficiaries and victims show this is contingent institutional arrangement, not natural law.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each agent derives from their structural relationship to the constraint. Church authority: beneficiary, arbitrage exit → d ≈ 0.1 (beneficiary end). Male household heads: beneficiary (default custody/inheritance), constrained exit → d ≈ 0.25. Women seeking divorce: victim, trapped exit → d ≈ 0.95 (target end). Interfaith couples: victim (autonomy constrained), constrained exit → d ≈ 0.75. State: beneficiary (legitimizes religious law), constrained exit → d ≈ 0.35. Coalition: neither (observer seat), mobile exit → d ≈ 0.5. The engine's chi computation amplifies extraction for trapped victims and damps it for beneficiaries with arbitrage. The analytical observer's mountain classification is computed as false summit: despite being classified as mountain by the analytical seat, the beneficiary presence triggers the engine's false_summit_mountain signature, which reclassifies to tangled_rope (the structural computed type).
 *
 * MANDATROPHY ANALYSIS:
 *   This reading's mandate was the codification of Christian canonical marriage principles into colonial Indian civil law (1872 objective: impose canonical divorce bars and ecclesiastical jurisdiction). The mandate has substantially outlived its function: (1) Vatican II (1962–1965) shifted Catholic theology toward permitting divorced remarriage in some cases; (2) major Protestant denominations now permit no-fault divorce; (3) Indian Christian communities increasingly use secular civil marriage and no-fault divorce under the Special Marriage Act 1954 and Divorce Act 1969; (4) constitutional courts have increasingly read the 1872 framework as violating Articles 14 (equality), 15 (non-discrimination), and 21 (life and liberty). The constraint persists not because the founding mandate is still operative but because institutional inertia, minority-protection framing, and legislative gridlock maintain it. The mandatrophy is already substantial: base_properties.mandatrophy_resolved should be true. The theater ratio (0.75) reflects that the constraint's operation is increasingly theatrical — the state performs enforcement while communities practice exit; the church performs canonical doctrine while denominations depart from it. This is the classic signature of Piton degradation overlaid on Tangled Rope structure: the coordination function (recognizing Christian marriage) persists; the extraction mechanism (divorce bars) persists; but the legitimacy claim (sacramental doctrine) has eroded, leaving mainly theatrical maintenance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_doctrine,
    'Is sacramental indissolubility a natural law of Christian theology or a constructed institutional doctrine that benefits identifiable actors?',
    'Comparative theology analysis: trace doctrine across Christian traditions (some denominations recognize divorce for cause). Historical genealogy of the doctrine in 19th-century colonial codification decisions. Analysis of whose interests the doctrine serves.',
    'If natural law: mountain classification holds. If constructed: false summit — reclassifies as tangled_rope or snare. This is the primary falsifiable distinction between readings 1 and 5.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_doctrine, conceptual, 'Whether sacramental indissolubility is timeless doctrine or institutional construction').

omega_variable(
    fault_requirement_necessity,
    'Is the fault-based divorce requirement structurally necessary to the Christian reading, or is it a historically contingent amplification?',
    'Examination of canonical sources: does the doctrine of indissolubility logically entail a fault requirement, or does that requirement come from 19th-century colonial judicial interpretation? Comparison with other Christian-majority jurisdictions that maintain the doctrine but permit no-fault divorce.',
    'If necessary: the reading is strictly fault-based. If contingent: the Christian reading could coexist with no-fault divorce while preserving the sacramental doctrine. Changes the vector of coexistence vs foreclosure relations with the secular_civil_reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fault_requirement_necessity, empirical, 'Whether fault requirement logically follows from doctrine or is historically contingent').

omega_variable(
    canon_law_vs_common_law_inheritance,
    'How much of the 1872 Christian Marriage Act derives from Christian canonical tradition versus British common-law patrimonial assumptions and colonial institutional convenience?',
    'Genealogical analysis of the 1872 Act''s provisions: which clauses derive from Decretals or Church councils, which from British Married Women''s Property Act models, which from colonial administrative convenience. Expert testimony from canon law historians and legal historians.',
    'If heavily common-law contaminated: the reading''s claim to canonical purity is undermined, and coexistence relations with secular_civil_reading become different (secular reading is not opposed to canonical principles but to colonial hybrid). If purely canonical: the reading stands as an authority in its own right.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(canon_law_vs_common_law_inheritance, empirical, 'Proportion of canonical vs common-law sources in 1872 Act').

omega_variable(
    minority_protection_mandate,
    'To what extent does Article 25 of the Indian Constitution''s guarantee of freedom of religion mandate preservation of Christian canonical marriage law, and to what extent does it permit its reform?',
    'Constitutional court jurisprudence: Indian Supreme Court rulings on religious freedom vs equality rights (Articles 25 vs 14, 15, 21). Comparison with how minority status is invoked for other religious law regimes. Test: does the constitutional mandate protect the doctrine of indissolubility, or only the right of Christian communities to regulate their own affairs?',
    'If mandate is strict: the constraint has constitutional armor. If permissive: reform pathways are constitutionally open, and the scaffold perspective''s sunset is legally available. Changes the temporal trajectory.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(minority_protection_mandate, empirical, 'Constitutional mandate for preservation vs reform of Christian marriage law').

omega_variable(
    reading_council_shift,
    'Did the Second Vatican Council (1962–1965) shift Christian theology on divorce and remarriage in ways that undermine the 1872 reading''s doctrinal foundation?',
    'Comparative theology: analysis of Vatican II''s teaching on marriage (Gaudium et Spes) vs pre-Council doctrine. Examination of how major Christian denominations responded post-Vatican II. Does the 1872 reading represent pre-Council doctrine that subsequent church bodies have revised?',
    'If yes: the reading is internally overridden — modern church authority does not hold the position. The axiom status becomes ''overridden'' rather than ''holdable''. Changes coexistence relations with sibling readings that appeal to contemporary Christian thought.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_council_shift, empirical, 'Whether Vatican II and post-conciliar theology undermine the 1872 reading').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(christian_canonical_reading, 0, 150).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ccr_theater_1872, christian_canonical_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(ccr_theater_1897, christian_canonical_reading, theater_ratio, 25, 0.5).
narrative_ontology:measurement(ccr_theater_1947, christian_canonical_reading, theater_ratio, 75, 0.58).
narrative_ontology:measurement(ccr_theater_1992, christian_canonical_reading, theater_ratio, 120, 0.68).
narrative_ontology:measurement(ccr_theater_2022, christian_canonical_reading, theater_ratio, 150, 0.75).

% Extraction over time
narrative_ontology:measurement(ccr_extrac_1872, christian_canonical_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(ccr_extrac_1897, christian_canonical_reading, base_extractiveness, 25, 0.3).
narrative_ontology:measurement(ccr_extrac_1947, christian_canonical_reading, base_extractiveness, 75, 0.35).
narrative_ontology:measurement(ccr_extrac_1992, christian_canonical_reading, base_extractiveness, 120, 0.38).
narrative_ontology:measurement(ccr_extrac_2022, christian_canonical_reading, base_extractiveness, 150, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(ccr_suppress_1872, christian_canonical_reading, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(ccr_suppress_1947, christian_canonical_reading, suppression_requirement, 75, 0.48).
narrative_ontology:measurement(ccr_suppress_2022, christian_canonical_reading, suppression_requirement, 150, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(christian_canonical_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(christian_canonical_reading, hindu_codified_reading).
narrative_ontology:affects_constraint(christian_canonical_reading, secular_civil_code_reform).
narrative_ontology:affects_constraint(christian_canonical_reading, interfaith_marriage_validity).

% DUAL FORMULATION NOTE:
% The Christian canonical reading is part of the marriage_authority_kernel constraint family. The kernel's extractiveness and suppression vary substantially across readings (canonical reading: ε ≈ 0.35; secular reading: ε ≈ 0.08; Muslim Shariat reading: ε ≈ 0.52). Each reading has distinct beneficiaries and victims. The family is linked through the shared contested kernel (what counts as legitimate authority to govern marriage) and through causal influence (the 1872 precedent shaped later codifications; the secular reading responds to all religious codifications as a unified critique). Network edges model institutional coupling: reform of one reading creates pressure on others (constitutional court rulings on Christian Marriage Act reverberate through Hindu Marriage Act jurisprudence, which influences Shariat debate).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
