% ============================================================================
% CONSTRAINT STORY: bill_of_rights_1689__anti_catholic_settlement_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bill_of_rights_1689__anti_catholic_settlement_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: bill_of_rights_1689__anti_catholic_settlement_reading
 *   human_readable: Bill of Rights 1689: Anti-Catholic Settlement Reading
 *   domain: legal/doctrinal/political_theology
 *
 * SUMMARY:
 *   The Bill of Rights 1689, read through the anti-Catholic settlement lens,
 *   is a structurally extractive constraint grounded in religious
 *   confessionalism. The document is framed in the language of universal
 *   'rights' — prohibitions on excessive bail, protection of jury trial,
 *   parliamentary privilege — but these rights apply only to Protestant
 *   subjects. Concurrently, the Bill entrenches confessional bars on crown
 *   succession, parliamentary office, military commission, and judicial
 *   authority. Catholic subjects and heirs bear the suppressive machinery
 *   (religious test oaths, statutory exclusions, deprivation of succession
 *   rights) while being excluded from the 'rights' the Bill purports to
 *   secure. This reading reconstructs the Bill's historical function: it was
 *   not primarily a rights charter but a settlement instrument consolidating
 *   Protestant parliamentary power after the Glorious Revolution and
 *   permanently excluding Catholic succession claims to the throne. The
 *   confessional machinery persisted in law for 324 years (1689–2013) despite
 *   centuries of erosion in institutional practice. The constraint exhibits
 *   classic snare signatures: high extractiveness (0.68), very high
 *   suppression (0.82), moderate theater (0.55 initially, rising to 0.88 by
 *   2013), and identifiable beneficiaries (Protestant establishment, crown's
 *   succession line) and victims (Catholic subjects and heirs). The temporal
 *   trajectory shows extractiveness declining as legal enforcement decayed
 *   post-1829, but theater rising as the Bill was increasingly invoked as a
 *   rights-protection symbol while its confessional content remained
 *   unrepealed. The constraint is read as a kernel — one of three
 *   structurally distinct interpretations (anti-Catholic settlement,
 *   parliamentary privilege foundation, proto-rights charter). This reading
 *   coexists with the parliamentary privilege reading (different
 *   constituencies use the Bill for different legitimacy claims) and
 *   influences (but does not foreclose) the proto-rights reading (modern
 *   rights charters quote the Bill's positive-rights language while
 *   deliberately omitting its confessional machinery).
 *
 * KEY AGENTS:
 *   - Protestant Establishment (Crown, Church of England, Parliamentary Majority): Institutional beneficiary — consolidates religious and political power via confessional bars; experiences the Bill as coordination (stabilizing succession, legitimizing authority). Exit option: arbitrage (can change terms at will).
 *   - Catholic Subjects and Heirs: Primary victims — trapped by oath requirements, statutory exclusions from office, deprivation of succession rights. Cannot exit without converting (infinite identity cost). Trapped exit option.
 *   - Catholic Gentry and Merchants: Secondary victims — constrained by legal disabilities (cannot sit Parliament, practice law, inherit without conforming). Can exit via conversion but conversion requires abandoning faith identity. Identity-locked exit option.
 *   - Dissident Protestant Minorities (Nonconformists, Dissenters): Mixed victims and beneficiaries — benefit from rights protections (jury trial, bail limits) that apply to all Protestants, but face conformity requirements and are excluded from office. Constrained exit option.
 *   - Institutional Remembrancers (Courts, Parliament, Archives): Maintain the Bill's legal force long after practical enforcement erodes; cite it as rights authority while leaving confessional text unrepealed. Piton-level inertia.
 *   - Analytical Observer: Sees potential false summit — the Bill invokes universal rights language while entrenching particular religious exclusions; risks misclassifying the confessional bars as natural law or necessary coordination cost.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bill_of_rights_1689__anti_catholic_settlement_reading, 0.68).
domain_priors:suppression_score(bill_of_rights_1689__anti_catholic_settlement_reading, 0.82).
domain_priors:theater_ratio(bill_of_rights_1689__anti_catholic_settlement_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bill_of_rights_1689__anti_catholic_settlement_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(bill_of_rights_1689__anti_catholic_settlement_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(bill_of_rights_1689__anti_catholic_settlement_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bill_of_rights_1689__anti_catholic_settlement_reading, snare).
narrative_ontology:human_readable(bill_of_rights_1689__anti_catholic_settlement_reading, "Bill of Rights 1689: Anti-Catholic Settlement Reading").
narrative_ontology:topic_domain(bill_of_rights_1689__anti_catholic_settlement_reading, "legal/doctrinal/political_theology").

domain_priors:requires_active_enforcement(bill_of_rights_1689__anti_catholic_settlement_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bill_of_rights_1689__anti_catholic_settlement_reading, '0fc320b7-6a5d-4ed5-bce0-5de80650229a').
narrative_ontology:cs_kernel_codification('0fc320b7-6a5d-4ed5-bce0-5de80650229a', formalized).
narrative_ontology:cs_authority_grounding('0fc320b7-6a5d-4ed5-bce0-5de80650229a', extraction).
narrative_ontology:cs_interpretation_layer_present('0fc320b7-6a5d-4ed5-bce0-5de80650229a').
narrative_ontology:cs_reading_relation('0fc320b7-6a5d-4ed5-bce0-5de80650229a', bill_of_rights_1689__parliamentary_privilege_reading, coexists_with).
narrative_ontology:cs_reading_relation('0fc320b7-6a5d-4ed5-bce0-5de80650229a', bill_of_rights_1689__proto_rights_charter_reading, influences).
narrative_ontology:cs_axiom('0fc320b7-6a5d-4ed5-bce0-5de80650229a', foundational, confessional_exclusion_constitutive).
narrative_ontology:cs_axiom_status(confessional_exclusion_constitutive, holdable).
narrative_ontology:cs_axiom_grounding('0fc320b7-6a5d-4ed5-bce0-5de80650229a', confessional_exclusion_constitutive, empirically_contingent).
narrative_ontology:cs_axiom('0fc320b7-6a5d-4ed5-bce0-5de80650229a', secondary, extractive_legitimacy_via_religious_authority).
narrative_ontology:cs_axiom_status(extractive_legitimacy_via_religious_authority, holdable).
narrative_ontology:cs_axiom_grounding('0fc320b7-6a5d-4ed5-bce0-5de80650229a', extractive_legitimacy_via_religious_authority, theological).
narrative_ontology:cs_reference_frame('0fc320b7-6a5d-4ed5-bce0-5de80650229a', confessional_settlement_authority).
narrative_ontology:cs_drift_state('0fc320b7-6a5d-4ed5-bce0-5de80650229a', contemporary_post_2013_repeal, gap(authority_erosion, severe, true)).
narrative_ontology:cs_created_at('0fc320b7-6a5d-4ed5-bce0-5de80650229a', '').
narrative_ontology:cs_kernel_id(bill_of_rights_1689__anti_catholic_settlement_reading, bill_of_rights_1689).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bill_of_rights_1689__anti_catholic_settlement_reading, protestant_establishment).
narrative_ontology:constraint_beneficiary(bill_of_rights_1689__anti_catholic_settlement_reading, crown_succession_male_protestants).
narrative_ontology:constraint_victim(bill_of_rights_1689__anti_catholic_settlement_reading, catholic_subjects).
narrative_ontology:constraint_victim(bill_of_rights_1689__anti_catholic_settlement_reading, catholic_heirs_to_crown).
narrative_ontology:constraint_victim(bill_of_rights_1689__anti_catholic_settlement_reading, dissident_religious_minorities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CATHOLIC SUBJECTS AND EXCLUDED HEIRS (SNARE) — Trapped by confessional succession bars and oath requirements. Cannot exit the kingdom or the constraint. Barred from crown office, military commission, judicial office by explicit statutory language. Excluded heirs experience cascading deprivation: the crown passes to Protestant lines even when Catholic heirs exist by consanguinity. The 'rights' secured by the Bill apply only to Protestant subjects; Catholic subjects bear the suppressive machinery with no voice in its construction.
constraint_indexing:constraint_classification(bill_of_rights_1689__anti_catholic_settlement_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CATHOLIC GENTRY AND MERCHANTS (SNARE) — Constrained by oath requirements and legal disabilities: cannot sit in Parliament, practice law, inherit estates without conforming. Can exit via conversion (to Protestantism) but conversion has infinite cost — it requires abandoning faith identity and community. The exit is formally available but structurally unavailable for identity-locked agents. Career, property, political participation all gated by religious test. Suppression is high; extraction flows toward Protestant rivals who face no equivalent barriers.
constraint_indexing:constraint_classification(bill_of_rights_1689__anti_catholic_settlement_reading, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PROTESTANT ESTABLISHMENT (ROPE) — Institutional beneficiaries (Church of England, crown's Protestant line) experience the Bill of Rights as genuine coordination: it stabilizes the succession against Catholic counter-claims, secures Protestant church property against Catholic reversion, legitimizes Protestant parliamentary authority through 'rights' language. The constraint is extractive from below (toward Catholics) but coordinative from within (among Protestants). The establishment has total exit option (arbitrage) — they can reshape the terms any time, but they choose not to because the current terms benefit them. Effective extraction toward this agent is negative (they are subsidized).
constraint_indexing:constraint_classification(bill_of_rights_1689__anti_catholic_settlement_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: DISSIDENT PROTESTANT MINORITIES (TANGLED ROPE) — Constrained but not trapped. The Bill of Rights does provide substantive coordination (jury trial, no excessive bail, prohibition on cruel punishment) that benefits them. But it also enforces a Protestant monopoly on office and succession. They see both a coordination benefit (rights protections) and asymmetric extraction (religious conformity required for full participation). Not snare-level extraction because the rights benefits are real and significant; but not pure rope because the confessional gate is non-negotiable. Active enforcement via oath requirements and test acts.
constraint_indexing:constraint_classification(bill_of_rights_1689__anti_catholic_settlement_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: INSTITUTIONAL THEATER (PITON) — From a distance-temporal perspective, the Bill functions largely as ritual validation of settled power after the Glorious Revolution. The 'rights' were already in force de facto; the Bill codified them as de jure legitimacy theater. The document's primary function was to foreclose reversionary claims and settle succession questions, not to establish novel protections. Modern institutional reading emphasizes the positive rights content while treating the confessional machinery as historical baggage. This degraded-function perspective captures that the Bill's real structural work was settlement of power, not rights-granting; modern institutions maintain it as a symbol of rights tradition while the extractive confessional core has atrophied (but not legally revoked until 2013, nearly 325 years later).
constraint_indexing:constraint_classification(bill_of_rights_1689__anti_catholic_settlement_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — A civilizational natural-law perspective might argue that the confessional character of the Bill reflects an immutable theological order: Protestantism and Catholicism are irreconcilable worldviews, the Bill simply codified natural law about religious incompatibility with state authority. From this view, the confessional suppression is not extractive but protective — preventing catastrophic religious conflict. However, this perspective is a false summit: the engine will detect that the Bill has identifiable beneficiaries (the Protestant establishment) and victims (Catholics), signaling that the 'immutable theological order' is actually a contingent institutional arrangement that benefits particular agents.
constraint_indexing:constraint_classification(bill_of_rights_1689__anti_catholic_settlement_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bill_of_rights_1689__anti_catholic_settlement_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(bill_of_rights_1689__anti_catholic_settlement_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(bill_of_rights_1689__anti_catholic_settlement_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(bill_of_rights_1689__anti_catholic_settlement_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(bill_of_rights_1689__anti_catholic_settlement_reading, TR),
    TR >= 0.70.

:- end_tests(bill_of_rights_1689__anti_catholic_settlement_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The constraint extracts substantial benefits for the Protestant establishment: monopoly on crown succession, exclusive access to military and judicial office, unilateral power to set religious terms for political participation. Catholic subjects bear the cost (deprivation of office, participation, inheritance rights) with no voice. The value of 0.68 reflects that the extraction is not maximal (snares with ε > 0.80 involve total deprivation or enslavement) but is severe and systemic. Suppression (0.82): Very high. Multiple overlapping mechanisms prevent exit: religious test oaths (formal bars), statutory exclusions from office (structural barriers), deprivation of inheritance and succession rights (economic costs), and identity-locking (conversion requires abandoning faith community). The 1829 Catholic Emancipation Act formally removed many bars but left the Bill's statutory confessional language unrepealed until 2013 — a 184-year gap indicating institutional resistance to formal acknowledgment of the constraint's extractive nature. Theater ratio (0.55 → 0.88 over 324 years): Moderate initially, rising sharply. In 1689, the Bill's function was practical: it consolidated power and formalized succession exclusions. By 1829, emancipation debates invoked the Bill as a rights-protection document while leaving its confessional bars intact, creating performative contradiction. By 2013, the Bill was cited almost exclusively for its positive-rights content (jury trial, bail limits); the confessional machinery had been formally repealed but persisted in institutional memory as historical curiosity. This rising theater trajectory models how an extractive constraint can become performative after its coercive force decays: the document is maintained for symbolic value while its actual suppressive function erodes.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximal perspectival divergence. The Protestant establishment sees genuine coordination (Rope): the Bill stabilizes succession against reversionary claims and legitimizes parliamentary authority. Catholics see pure extraction (Snare): they are excluded from the rights secured and trapped by the suppression. Dissident Protestants see mixed dynamics (Tangled Rope): rights benefits plus conformity extraction. The piton perspective observes that the Bill's functional role has atrophied — it is maintained as a symbol of rights tradition while its confessional content has been formally superseded. The mountain perspective risks false summit: claiming that confessional settlement is an immutable feature of religious statecraft, naturalizing what is actually a contingent institutional choice. This gap is the reading's diagnostic signature: the Bill cannot be accurately classified without specifying which constituency's perspective anchors the analysis.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality is computed from beneficiary/victim status and exit options. Protestant establishment beneficiaries with arbitrage exit (can reshape terms at will) have d ≈ 0.05, producing negative χ — the constraint subsidizes them. Catholic subjects trapped by religious test oaths and statutory exclusions have d ≈ 0.95, producing maximum χ — the constraint extracts from them at maximum rate. Catholic gentry constrained by legal disabilities but with identity_locked exit have d ≈ 0.82, producing high χ — structurally mobile (could convert) but perceptually immobile (identity fused with faith community). Dissident Protestant minorities benefit from rights protections but face conformity requirements; they have d ≈ 0.55, producing moderate χ. The snare classification emerges from the high suppression (0.82) and high base extractiveness (0.68) combined with victim presence and beneficiary presence and no genuine coordination function shared across the beneficiary-victim boundary. The constraint coordinates among Protestants (settling succession, stabilizing parliamentary authority) but extracts from Catholics (via exclusion). This is the defining signature of a tangled rope or snare: genuine coordination function plus asymmetric extraction. The snare gate (high extraction, high suppression, beneficiary + victim + no genuine shared coordination) fires because the coordination benefits do not extend to the victim set — Catholics are excluded from the 'rights' the constraint protects.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resolves the mandatrophy by reconstructing the Bill's structural function: it performs BOTH coordination (among Protestants) AND extraction (from Catholics), which defines the snare-adjacent boundary. The snare classification is confirmed by the tangled_rope perspective (dissident Protestants) and the rope perspective (establishment beneficiaries) — the constraint genuinely coordinates among some agents while extracting from others. This is snare rather than tangled_rope because suppression is very high (0.82) and the coordination function is monopolistic (benefits only Protestants). A tangled_rope would show more symmetric distribution of both benefits and costs; this constraint's benefits are highly asymmetric (Protestants only) and its costs are borne entirely by the victim set (Catholics). The mandatrophy does not dissolve — it is precisely the structure of the constraint. The analytical observer's mountain reading is a false summit, detected by the presence of identifiable beneficiaries and victims. The Bill's confessional character is not an immutable law but an institutional choice, and the choice benefits particular agents.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    confessional_necessity_vs_contingency,
    'Is the confessional character of the Bill structurally necessary to accomplish its stated coordination goals (parliamentary privilege, succession stability, rights protections), or is it a contingent choice by the Protestant-dominated Parliament?',
    'Counterfactual historical analysis: could a religiously neutral succession rule have accomplished the same stability? Did continental Protestant states (Prussia, Sweden) use confessional bars as heavily? What percentage of the Bill''s text is devoted to confessional exclusion vs. rights articulation?',
    'If necessary: confessional suppression is a coordination cost (lowers snare severity toward tangled_rope). If contingent: the suppression is pure extraction layered on coordination (confirms snare classification). This omega determines whether the constraint is a false summit or a genuine snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(confessional_necessity_vs_contingency, empirical, 'Whether confessional exclusions are structurally necessary or contingent institutional choices').

omega_variable(
    reading_foreclosure_via_empirical_challenge,
    'If historical evidence shows that non-confessional succession arrangements (e.g., Catholic-inclusive oath language) were viable and deliberated but rejected for political expediency, does this foreclose the natural-law reading and confirm the snare reading?',
    'Parliamentary records, private correspondence of drafters, comparative analysis of contemporary European settlement models. Explicit rejection of non-confessional language would establish deliberate choice, not necessity.',
    'If evidence of deliberate rejection: forecloses the mountain reading entirely within any single historical framework. The Bill cannot simultaneously be both a natural law and a deliberately chosen confessional settlement. This would move the analytical observer from mountain toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_via_empirical_challenge, empirical, 'Whether confessional exclusions were deliberate choices or structural necessities').

omega_variable(
    catholic_emancipation_ratchet_irreversibility,
    'The Catholic Emancipation Acts (1829, 1926, etc.) slowly removed confessional bars. But the Bill''s statutory text was never formally repealed until 2013. Why the 184-year gap? Was the Bill''s legal force maintained symbolically even as its practical force eroded?',
    'Analysis of legal reasoning in emancipation debates: Did abolition acts override the Bill, or merely ignore it? Did courts apply the Bill''s confessional language after 1829? When was the Bill formally struck from the statute books and why so late?',
    'If maintained symbolically during emancipation: demonstrates institutional inertia and false summit dynamics — the ''rights'' document was invoked to protect minority rights while its own confessional machinery was left intact, creating a performative contradiction. If the Bill was legally superseded earlier: suggests the snare''s extractive force was actively contested and partially dismantled, complicating the snare classification toward a degraded constraint (piton-adjacent).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(catholic_emancipation_ratchet_irreversibility, empirical, 'Role of the Bill in Catholic emancipation and trajectory of its legal force').

omega_variable(
    reading_contest_kernel_identity,
    'Which reading of the Bill''s kernel reflects the most accurate characterization of the document''s historical function: anti-Catholic settlement, parliamentary privilege foundation, or proto-rights charter?',
    'This is a committer-frame omega about the kernel contest itself. The three readings coexist as live interpretive positions held by different constituencies (Protestant establishment, parliamentary reformers, rights advocates). None forecloses the others within a single coherent historical narrative — they are genuinely incommensurable framings. Resolution requires specifying the authority grounding that adjudicates the kernel: Church of England theology, parliamentary precedent, or natural rights philosophy. Each authority privileges a different reading.',
    'This omega routes to the cs_structure.reading_relations field: the anti_catholic_settlement_reading coexists_with the parliamentary_privilege_reading and proto_rights_charter_reading because different constituencies use the Bill to justify different claims. The readings do not logically foreclose each other; they compete in institutional practice.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_contest_kernel_identity, conceptual, 'The kernel contest: which reading most accurately captures the Bill''s historical function').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bill_of_rights_1689__anti_catholic_settlement_reading, 0, 324).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bill_tr_t0, bill_of_rights_1689__anti_catholic_settlement_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(bill_tr_t140, bill_of_rights_1689__anti_catholic_settlement_reading, theater_ratio, 140, 0.6).
narrative_ontology:measurement(bill_tr_t185, bill_of_rights_1689__anti_catholic_settlement_reading, theater_ratio, 185, 0.75).
narrative_ontology:measurement(bill_tr_t324, bill_of_rights_1689__anti_catholic_settlement_reading, theater_ratio, 324, 0.88).

% Extraction over time
narrative_ontology:measurement(bill_be_t0, bill_of_rights_1689__anti_catholic_settlement_reading, base_extractiveness, 0, 0.78).
narrative_ontology:measurement(bill_be_t50, bill_of_rights_1689__anti_catholic_settlement_reading, base_extractiveness, 50, 0.75).
narrative_ontology:measurement(bill_be_t140, bill_of_rights_1689__anti_catholic_settlement_reading, base_extractiveness, 140, 0.68).
narrative_ontology:measurement(bill_be_t185, bill_of_rights_1689__anti_catholic_settlement_reading, base_extractiveness, 185, 0.52).
narrative_ontology:measurement(bill_be_t324, bill_of_rights_1689__anti_catholic_settlement_reading, base_extractiveness, 324, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(bill_su_t0, bill_of_rights_1689__anti_catholic_settlement_reading, suppression_requirement, 0, 0.92).
narrative_ontology:measurement(bill_su_t140, bill_of_rights_1689__anti_catholic_settlement_reading, suppression_requirement, 140, 0.7).
narrative_ontology:measurement(bill_su_t324, bill_of_rights_1689__anti_catholic_settlement_reading, suppression_requirement, 324, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bill_of_rights_1689__anti_catholic_settlement_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(bill_of_rights_1689__anti_catholic_settlement_reading, bill_of_rights_1689__parliamentary_privilege_reading).
narrative_ontology:affects_constraint(bill_of_rights_1689__anti_catholic_settlement_reading, bill_of_rights_1689__proto_rights_charter_reading).

% DUAL FORMULATION NOTE:
% The Bill of Rights 1689 is a contested kernel with three structurally distinct constraint interpretations. This file models the anti_catholic_settlement_reading; the sibling readings are separate constraint stories with different epsilon values and different beneficiary/victim structures. All three are linked via network.affects_constraints to indicate kernel kinship. Each reading instantiates different aspects of the same historical text but has different base_extractiveness values reflecting different structural framings: the anti_catholic_settlement reading emphasizes confessional exclusion (ε=0.68, snare); the parliamentary_privilege reading emphasizes legislative immunity (ε < 0.35, rope-level coordination); the proto_rights_charter reading emphasizes positive rights protections (ε=0.15, rope). These readings do not compete within a single empirical framework — they are committer-level contest over the kernel's meaning.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(bill_of_rights_1689__anti_catholic_settlement_reading, institutional, 0.04).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
