% ============================================================================
% CONSTRAINT STORY: ulysses_chp04
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ulysses_chp04, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: ulysses_chp04
 *   human_readable: The Domestic Calypso (7 Eccels Street)
 *   domain: social/economic/religious
 *
 * SUMMARY:
 *   Leopold Bloom navigates the domestic constraint of marriage at 7 Eccles
 *   Street in Dublin on June 16, 1904. The marriage to Molly exhibits the
 *   structural characteristics of Tangled Rope: it provides genuine
 *   coordination functions (household management, social legitimacy, economic
 *   stability through shared resources, companionship) while simultaneously
 *   extracting from Bloom asymmetrically through his legal and financial
 *   obligation, sexual jealousy management, and loss of autonomy. The
 *   constraint is enforced actively by Molly, by Dublin bourgeois
 *   respectability norms, by Catholic sacramental marriage doctrine, and by
 *   law. Yet it also exhibits degradation markers: the sacramental claim
 *   (sacred indissoluble bond) is increasingly theatrical while actual
 *   emotional connection has atrophied; Molly's planned infidelity (with
 *   Boylan) mirrors Bloom's earlier infidelities, suggesting the enforcement
 *   mechanism is weakening. This makes it simultaneously a Snare (from
 *   Bloom's trapped perspective), a Rope (from Molly's beneficiary
 *   perspective), a Piton (from the religious institution's degraded
 *   authority perspective), and a Scaffold (from the emerging feminist and
 *   legal reform movements' perspective). The constraint embodies the
 *   transition from Victorian patriarchal marriage as unquestioned natural
 *   law toward its modern recognition as a contingent institutional form.
 *
 * KEY AGENTS:
 *   - Leopold Bloom: Primary victim (powerless/trapped) — bears emotional, financial, and legal costs of marriage without exit options; trapped by social/economic/legal disability
 *   - Molly Bloom: Primary beneficiary (institutional/arbitrage) — controls household space, sexual access, social reputation; has arbitrage options (departure, divorce, social alternatives) not available to Bloom
 *   - Dublin Bourgeois Respectability: Institutional beneficiary (institutional/arbitrage) — enforces constraint through gossip, scandal risk, social ostracism; benefits from Bloom's compliance
 *   - Catholic Church: Institutional enforcer (institutional/arbitrage) — maintains sacramental authority; benefits from legitimacy of marriage constraint; performs theatrical enforcement
 *   - Women's Suffrage and Legal Reform Movements: Organized reformers (organized/mobile) — see the constraint as degraded institution with sunset potential; building alternative legal/social pathways
 *   - Analytical Observer: Civilizational context (analytical/analytical) — risks naturalizing patriarchal marriage as universal human law rather than contingent institutional form
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ulysses_chp04, 0.52).
domain_priors:suppression_score(ulysses_chp04, 0.68).
domain_priors:theater_ratio(ulysses_chp04, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ulysses_chp04, extractiveness, 0.52).
narrative_ontology:constraint_metric(ulysses_chp04, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(ulysses_chp04, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ulysses_chp04, tangled_rope).
narrative_ontology:human_readable(ulysses_chp04, "The Domestic Calypso (7 Eccels Street)").
narrative_ontology:topic_domain(ulysses_chp04, "social/economic/religious").

domain_priors:requires_active_enforcement(ulysses_chp04).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ulysses_chp04, molly_bloom).
narrative_ontology:constraint_beneficiary(ulysses_chp04, cultural_patriarchy).
narrative_ontology:constraint_victim(ulysses_chp04, leopold_bloom).
narrative_ontology:constraint_victim(ulysses_chp04, domestic_autonomy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LEOPOLD BLOOM (SNARE) — Trapped within domestic obligation, financial dependency, and social expectations. Cannot exit marriage without catastrophic social/financial consequence in 1904 Dublin. Bears full cost of household management, emotional labor, and marital infidelity without formal recourse. Experiences the constraint as coercive extraction with minimal coordination benefit.
constraint_indexing:constraint_classification(ulysses_chp04, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: MOLLY BLOOM (ROPE) — Benefits from household coordination without matching financial or legal obligation. Controls domestic space, sexual access, and social reputation. Has arbitrage options (departure, divorce settlements, social alternatives) that Bloom lacks. Experiences constraint as coordination mechanism that systematically favors her position.
constraint_indexing:constraint_classification(ulysses_chp04, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 3: DUBLIN BOURGEOIS RESPECTABILITY (TANGLED ROPE) — The social institution that enforces marital obligation while simultaneously exploiting the marriage for its own legitimacy. Requires Bloom's compliance (extraction via reputation risk) while offering coordination benefits (social standing, legal recognition). Active enforcement through gossip, scandal risk, and social ostracism. Constrained exit — breaking norms requires emigration or social suicide.
constraint_indexing:constraint_classification(ulysses_chp04, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: CATHOLIC SACRAMENTAL MARRIAGE (PITON) — The religious institution that consecrates the domestic constraint is increasingly performative by 1904. Catholic marriage theology treats the sacrament as binding regardless of consent renewal or actual conjugal function. The institution maintains the constraint through theatrical authority (priest, sacrament, indissolubility doctrine) despite functional degradation — Irish Catholic marriage already shows signs of strain, infidelity, and emotional abandonment. Theater ratio high because the sacramental claims (sacred bond, moral indissolubility) exceed the actual relational function.
constraint_indexing:constraint_classification(ulysses_chp04, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 5: FEMINIST AND LEGAL REFORM MOVEMENTS (SCAFFOLD) — Organized agents (women's suffrage movements, divorce law reformers, Irish independence movements questioning British-imposed legal frameworks) see the domestic constraint as a temporary coordination failure with sunset potential. New divorce laws, married women's property acts, and suffrage expansion are creating alternative pathways. Mobile exit options for these agents because they can organize, petition, and build counter-institutions. Theater ratio declining as legal and social reforms chip away at enforcement mechanisms.
constraint_indexing:constraint_classification(ulysses_chp04, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (MOUNTAIN) — Risks naturalizing the domestic constraint as an immutable law of human sexuality and gender: men inevitably stray, women inevitably manage household, infidelity is the natural order. This perspective treats the constraint as emerging from unchangeable human nature rather than contingent institutional arrangements. However, the structural data contradicts mountain classification — the enforcement mechanisms (social gossip, legal disability, religious authority) are all human-constructed and historically contingent. The false summit reveals how naturalization discourse legitimizes institutional extraction.
constraint_indexing:constraint_classification(ulysses_chp04, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ulysses_chp04_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ulysses_chp04, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ulysses_chp04, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ulysses_chp04, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(ulysses_chp04, TR),
    TR >= 0.70.

:- end_tests(ulysses_chp04_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint extracts from Bloom through multiple channels: legal marriage obligation (property control, alimony risk, divorce stigma), household labor coordination (Bloom must maintain income and status), emotional labor (jealousy management, infidelity tolerance), and sexual obligation (conjugal rights doctrine). However, extraction is not maximal because the marriage also provides genuine coordination benefits (household stability, social standing, economic efficiency through shared resources). The value reflects a hybrid mechanism where extraction is embedded within coordination function. Suppression (0.68): High. Bloom's suppression is severe — he cannot exit marriage without legal penalty, social ostracism, and economic catastrophe in 1904 Dublin. Catholic doctrine teaches marriage indissolubility; Irish law provides no divorce; social convention treats marital separation as scandalous abandonment. Yet suppression is not total because emigration, informal separation, or ecclesiastical annulment remain theoretical options (extremely costly but structurally possible). Theater ratio (0.65): Moderate-high. The Catholic sacramental marriage increasingly relies on theatrical authority (priest, ritual, indissolubility doctrine) rather than actual relational function — the marriage is emotionally cold, sexually strained, and punctuated by mutual infidelity. The enforcement mechanisms are substantially performative: the appearance of respectability, the social ritual of married status, the ecclesiastical claim of sacred bond that exceeds the actual emotional/sexual reality. Theater has increased from 0.55 to 0.65 over the interval as emotional disconnection has grown while institutional claims have remained constant.
 *
 * PERSPECTIVAL GAP:
 *   The gap between Bloom's experienced Snare and Molly's experienced Rope is maximal within this constraint system. This gap generates the paradox of the constraint: it appears as natural law to the analytical observer (Mountain — 'men stray, women manage') precisely because the beneficiary's experience of coordination obscures the victim's experience of extraction. The sacramental institution's increasingly theatrical nature (Piton) mirrors this paradox: the doctrine of sacred indissolubility performs authority over an actual relationship in dissolution. The scaffold perspective (legal reform movements) correctly identifies the constraint as solvable precisely because it is not natural law — the gap between Piton performance and Snare reality reveals contingency, not necessity.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality computation flows from structural position and exit options. Bloom: powerless power atom + trapped exit → high d (≈0.95) → high f(d) ≈ 1.42 → high experienced extractiveness. Molly: institutional power atom + arbitrage exit → low d (≈0.15) → low f(d) ≈ -0.01 → negative/minimal experienced extractiveness. Dublin society: institutional + arbitrage → low d → low f(d) → experiences constraint as lightweight coordination (gossip as social enforcement requires minimal overhead). Catholic church: institutional + arbitrage → low d → enforcement appears as doctrine/ritual without personal cost. Feminist reformers: organized + mobile → moderate d (≈0.40-0.55) → moderate f(d) ≈ 0.40-0.75 → they experience the constraint as a solvable coordination failure requiring mobilization. The scope modifier σ(S) = 0.8 (local) dampens effective extractiveness slightly relative to χ computation, reflecting that the constraint operates primarily within Dublin household and society rather than at regional/national scale (though the institutional mechanisms are continental/Irish-wide).
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: The constraint avoids mislabeling by distinguishing between coordination function and extraction mechanism through the tangled_rope classification and beneficiary/victim declarations. The marriage DOES provide coordination (household management, social legitimacy, economic efficiency) AND extraction (asymmetric obligation, legal disability, suppression of exit). The mandatrophy resolution requires affirming both: (1) the coordination function is real — household stability requires coordinated labor and resource management; (2) the extraction is real — this coordination is enforced asymmetrically, benefiting Molly and patriarchal institutions while extracting from Bloom. The false summit (Mountain) would claim marriage is natural law; the false coordination (Rope) would claim it is purely cooperative; the false extraction (Snare with no coordination) would ignore genuine household stability function. The Tangled Rope classification with active enforcement, beneficiaries, and victims captures the actual hybrid structure. The measurement trajectory (theater_ratio 0.55→0.65) and the multi-perspectival gap demonstrate that the coordination function is degrading (increasing performance gap) while extraction mechanism persists (sustained suppression) — the classic pathway to degradation from Rope toward Piton, visible in the religious institution's increasingly theatrical authority.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    infidelity_as_extraction_or_symptom,
    'Is Bloom''s infidelity (his search for sexual alternatives outside marriage) a form of resistance to the domestic extraction, or does it constitute a secondary extraction mechanism that deepens the constraint?',
    'Analysis of Bloom''s internal monologue regarding his infidelity motivations; comparison of his emotional state with household departure vs. return; examination of whether infidelity increases or decreases his functional trapped status',
    'If infidelity is genuine resistance: the constraint is less total than snare classification suggests; chi experienced by Bloom is lower. If infidelity is symptom of total entrapment: the snare classification holds; Bloom''s psychological escape attempts do not reduce structural extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(infidelity_as_extraction_or_symptom, conceptual, 'Whether infidelity represents resistance or deepened entrapment').

omega_variable(
    molly_agency_or_complicity,
    'Does Molly Bloom actively enforce the domestic extraction as a beneficiary, or is she herself trapped within the patriarchal constraint despite her apparent control of household space?',
    'Detailed analysis of Molly''s perspective and choices; examination of her exit options relative to Bloom''s; assessment of her actual control vs. her perceived control of household and marital dynamics',
    'If Molly is agent of extraction: rope/tangled_rope classification from her perspective holds. If Molly is secondary victim: she becomes a victim group alongside Bloom; the constraint''s suppression is higher; the beneficiary group shrinks or shifts to patriarchal institutions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(molly_agency_or_complicity, conceptual, 'Whether Molly is beneficiary or secondary victim of the constraint').

omega_variable(
    dublin_vs_universal_institutional_scope,
    'Is this constraint structurally specific to Dublin Catholic bourgeois society in 1904, or does it represent a universal institutional form (patriarchal marriage) that manifests similarly across time and geography?',
    'Comparative analysis of marriage constraints in other cultures/periods; examination of whether the extractiveness structure (male entrapment, female benefit, religious enforcement) replicates or varies fundamentally',
    'If Dublin-specific: constraint_id should be local scope; network decomposition would create parallel constraints for other societies. If universal: mountain-like invariance across contexts; scope should be global/universal; multiple instances represent manifestations of the same underlying constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(dublin_vs_universal_institutional_scope, empirical, 'Whether constraint is Dublin-specific or universally manifested').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ulysses_chp04, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dom_cal_tr_t0, ulysses_chp04, theater_ratio, 0, 0.55).
narrative_ontology:measurement(dom_cal_tr_t5, ulysses_chp04, theater_ratio, 5, 0.62).
narrative_ontology:measurement(dom_cal_tr_t10, ulysses_chp04, theater_ratio, 10, 0.65).

% Extraction over time
narrative_ontology:measurement(dom_cal_be_t0, ulysses_chp04, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(dom_cal_be_t5, ulysses_chp04, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(dom_cal_be_t10, ulysses_chp04, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ulysses_chp04, resource_allocation).
narrative_ontology:boltzmann_floor_override(ulysses_chp04, 0.35).
narrative_ontology:affects_constraint(ulysses_chp04, irish_womens_legal_disability_1904).
narrative_ontology:affects_constraint(ulysses_chp04, victorian_sexual_morality).
narrative_ontology:affects_constraint(ulysses_chp04, catholic_marriage_doctrine_ireland).

% DUAL FORMULATION NOTE:
% The domestic constraint at 7 Eccles Street is a manifestation of the broader patriarchal marriage institution operative in Ireland in 1904. This story focuses on the individual household-level constraint structure; parallel stories address the institutional/doctrinal constraints (Catholic marriage theology, Irish legal framework, bourgeois respectability norms) that generate the conditions enabling household extraction. The household constraint is downstream of the institutional constraints but has its own extractiveness structure derived from Bloom and Molly's specific positions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ulysses_chp04, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
