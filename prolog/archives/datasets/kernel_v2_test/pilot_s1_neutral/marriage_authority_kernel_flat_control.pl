% ============================================================================
% CONSTRAINT STORY: marriage_authority_kernel_flat_control
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_authority_kernel_flat_control, []).

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
    narrative_ontology:flat_control_of/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: marriage_authority_kernel_flat_control
 *   human_readable: Marriage Authority Adjudication Across Legal Pluralism
 *   domain: comparative_law/legal_pluralism/constitutional_theory
 *
 * SUMMARY:
 *   The shared commitment that some legitimate authority adjudicates
 *   marriage, divorce, inheritance, and custody operates across societies
 *   with radically different grounding (state law, religious law, customary
 *   law) and radically different substantive rules. This is a constraint
 *   story about the MECHANISM of authority adjudication itself — not about
 *   marriage as a social institution, but about who gets to decide what
 *   marriage means, when it ends, what spouses owe each other, and how
 *   children and property transfer. The constraint coordinates a genuine
 *   collective-action problem (without authority adjudication, conflicts over
 *   property division and child custody lack resolution). It also extracts
 *   from parties without power to choose their applicable law or negotiate
 *   its terms. The substantive rules differ so radically across systems
 *   (patrilineal vs. matrilineal inheritance, fault-based vs. no-fault
 *   divorce, religious vs. secular custody standards) that the same
 *   structural position (exit-constrained spouse) experiences dramatically
 *   different extractiveness depending on which authority's rules apply. This
 *   variance is not a solution to extraction — it is the distribution
 *   mechanism. The constraint's natural law face (every society must
 *   adjudicate marriage) conceals that authority monopoly benefits authority
 *   administrators at the cost of exit-constrained parties.
 *
 * KEY AGENTS:
 *   - Exit-Constrained Spouse (powerless/trapped): Bears maximum extraction through authority adjudication of divorce terms, inheritance, custody. Trapped by economics, social stigma, legal barriers, and lack of forum-shopping options.
 *   - Authority Administrators (institutional/arbitrage): State courts, religious judges, customary elders. Experience the constraint as coordination (solving genuine adjudicative problems). Benefit from authority monopoly and jurisdictional power. Have arbitrage options (can change rules, can appeal to legitimacy grounding, can enforce compliance).
 *   - Economically Advantaged Spouse (powerful/mobile): Benefits from authority adjudication when rules favor concentration of resources. Also constrained but less severely. Can sometimes forum-shop in plural legal systems.
 *   - Disinherited Children (powerless/trapped): Inherit based on authority-set rules. Cannot challenge rule-setting process. In patrilineal systems, female children face systematic extraction through inheritance rules set by male authority holders.
 *   - Informal Marriage Parties (powerless/identity_locked): Recognizable as married by community but unrecognized by state. No formal adjudication available, but also no formal extraction mechanism (outside state reach). Identity-locked through community recognition without legal standing.
 *   - Reform Coalition (organized/constrained): Civil rights, feminist, LGBTQ+ organizations pushing substantive rule changes and authority-grounding shifts. Face constrained exit (must work through political system, meet resistance from traditional authorities).
 *   - Cosmopolitan Elite (powerful/arbitrage): Wealthy, mobile individuals with forum-shopping options across plural legal systems. Can exit unfavorable authority systems through relocation, prenuptial contracts, private arbitration.
 *   - Analytical Observer (analytical/analytical): Sees the constraint as natural necessity (authority adjudication inherent to all societies) but risks naturalizing what is contingent institutional arrangement.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority_kernel_flat_control, 0.52).
domain_priors:suppression_score(marriage_authority_kernel_flat_control, 0.68).
domain_priors:theater_ratio(marriage_authority_kernel_flat_control, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority_kernel_flat_control, extractiveness, 0.52).
narrative_ontology:constraint_metric(marriage_authority_kernel_flat_control, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(marriage_authority_kernel_flat_control, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority_kernel_flat_control, tangled_rope).
narrative_ontology:human_readable(marriage_authority_kernel_flat_control, "Marriage Authority Adjudication Across Legal Pluralism").
narrative_ontology:topic_domain(marriage_authority_kernel_flat_control, "comparative_law/legal_pluralism/constitutional_theory").

domain_priors:requires_active_enforcement(marriage_authority_kernel_flat_control).

% --- Construction-pair linkage (forced-flat control of a kernel) ---
narrative_ontology:flat_control_of(marriage_authority_kernel_flat_control, marriage_authority_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority_kernel_flat_control, authority_administrators).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel_flat_control, economically_advantaged_spouse).
narrative_ontology:constraint_victim(marriage_authority_kernel_flat_control, exit_constrained_spouse).
narrative_ontology:constraint_victim(marriage_authority_kernel_flat_control, disinherited_children).
narrative_ontology:constraint_victim(marriage_authority_kernel_flat_control, informal_marriage_parties).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXIT-CONSTRAINED SPOUSE (SNARE) — Trapped within a jurisdiction where the applicable authority (customary law, religious law, statutory law) enforces marriage terms disadvantageous to this agent. Exit-constrained by economics, social stigma, custody consequences, and lack of alternative adjudicating authority. Bears maximum extraction: the authority adjudicates divorce terms, inheritance claims, and child access in ways that concentrate resources away from this agent and offer no appeal to a more favorable legal framework. Zero degrees of freedom.
constraint_indexing:constraint_classification(marriage_authority_kernel_flat_control, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: AUTHORITY ADMINISTRATOR (ROPE) — State or religious institutions that adjudicate marriage matters experience the constraint as coordination: they are solving a genuine collective-action problem (who decides marriage validity, divorce terms, custody, inheritance?) and they have arbitrage options (can enforce their chosen legal framework, can modify the framework if norms shift, can appeal to legitimacy grounded in their authority source — state, scripture, custom). Net beneficiary. The extraction visible to perspective 1 appears to perspective 2 as legitimate adjudicative authority: clarity, finality, and institutional stability.
constraint_indexing:constraint_classification(marriage_authority_kernel_flat_control, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: ECONOMICALLY ADVANTAGED SPOUSE (TANGLED ROPE) — Benefits from the authority's adjudication when its substantive rules favor resource concentration (e.g., patrilineal inheritance, wife's property transfer to husband on marriage, restrictive divorce terms for the economically dependent spouse). Also constrained by the same authority system: cannot unilaterally exit the marriage without its permission, faces social stigma, may lose custody under its rules. Mixed position: the constraint coordinates family property relations AND extracts from the less advantaged spouse. High theater when the authority performs legitimacy through ritual, ceremony, or doctrinal pronouncements that obscure the asymmetric extraction.
constraint_indexing:constraint_classification(marriage_authority_kernel_flat_control, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER / NATURAL AUTHORITY VIEW (MOUNTAIN) — From a civilizational perspective that takes state or religious authority as foundational and immutable, the constraint appears as a natural law: every society must have SOME authority that adjudicates marriage, divorce, inheritance, and custody. The specific rules differ, but the FACT that some adjudicating authority exists is necessary. Authority grounds itself in this naturality: 'we must have someone decide these questions.' However, this perspective conceals the contingency of WHO gets to decide and WHOSE substantive rules prevail — contestation that perspectival gaps reveal.
constraint_indexing:constraint_classification(marriage_authority_kernel_flat_control, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 5: COSMOPOLITAN ELITE WITH FORUM SHOPPING (PITON) — Wealthy, mobile individuals in continental jurisdictions (EU, North America) with multiple legal frameworks available (statutory law, contractual marriage terms, divorce forum choice, custody arbitration, international enforcement) experience the authority system as degraded theater: the adjudication mechanism persists through institutional inertia, but it is increasingly bypassed by wealth-enabled alternatives (private arbitration, prenuptial contracts, choice of jurisdiction). For this perspective, the authority system is maintained as cultural performance while real power concentrates in alternative arrangements. The constraint's functional extraction decays as exit options proliferate, leaving pure theater.
constraint_indexing:constraint_classification(marriage_authority_kernel_flat_control, piton,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 6: REFORM COALITION (TANGLED ROPE / SCAFFOLD variant) — Civil rights organizations, feminist movements, LGBTQ+ advocates, and religious modernists are organized agents pushing for substantive rule changes (gender-equal inheritance, no-fault divorce, same-sex marriage recognition) and authority-grounding shifts (constitutional supremacy over religious law, statutory codification of customary law, contractual marriage terms). They experience the constraint as both coordinating legitimate questions AND extracting from marginalized groups until rules reform. They have constrained exit (must work within the political system, face resistance from traditional authority holders) but clear sunset logic: as legal reform advances, the extraction mechanism weakens. High theater as competing authorities stage legitimacy battles (constitution vs. scripture, statutory law vs. custom).
constraint_indexing:constraint_classification(marriage_authority_kernel_flat_control, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority_kernel_flat_control_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(marriage_authority_kernel_flat_control, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(marriage_authority_kernel_flat_control, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_authority_kernel_flat_control, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(marriage_authority_kernel_flat_control, TR),
    TR >= 0.70.

:- end_tests(marriage_authority_kernel_flat_control_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   EXTRACTIVENESS (0.52, rising over interval from 0.38 to 0.52): Moderate, rising trajectory reflects two opposing forces. Codification and formalization (t=0 to t=5) increased clarity in adjudication but also increased the authority's enforcement capacity — parties previously outside formal reach became subject to state authority (extractiveness rose 0.38 → 0.45). Contemporary period (t=5 to t=10) shows rising extractiveness to 0.52 as reform movements have partly succeeded in equalizing rules (reducing patrilineal bias, enabling no-fault divorce, recognizing same-sex marriage) but simultaneously expanded the authority's scope — more categories of actors are now subject to adjudication (LGBTQ+ parties, blended families, inheritance outside traditional kinship). The rising extraction is not from increased unfairness per marriage but from increased coverage of what marriage means (estate planning, asset division, reproductive rights, inheritance across gender/sexual identity). SUPPRESSION (0.68, stable over interval): High and stable. Barriers to exit marriage are material (economics, custody, social stigma), legal (divorce grounds, filing requirements, property loss), and internalized (identity, commitment, religious belief). Suppression is not decreasing despite reform because alternatives (informal marriage, contractual marriage, legal separation vs. divorce) remain secondary options available only to privileged actors. For trapped agents, suppression stays at 0.68 because the underlying barriers (economic dependency, identity fusion, social isolation, legal inability to unilaterally exit) persist even as substantive rules improve. THEATER_RATIO (0.58, declining over interval from 0.72 to 0.58): Declining trajectory reflects decline in ritual and ceremony relative to functional adjudication. Traditional authority (t=0) was heavily ceremonial (religious ritual, elder council ritual, community witness) — high theater (0.72). Statutory codification (t=5) made theater less essential but maintained formal procedure (court ceremony, legal documentation) — moderate theater (0.65). Contemporary period (t=10) shows movement toward contractual and arbitration alternatives that bypass formal ceremony entirely — lower theater (0.58). However, theater has not approached zero: formal marriage remains ceremonially marked, divorce proceedings retain ritual language, and inheritance adjudication maintains formal procedures even when substantive rules have become contractual.
 *
 * PERSPECTIVAL GAP:
 *   The gap between the snare-classified exit-constrained spouse and the rope-classified authority administrator is the core diagnostic gap. The same constraint (authority adjudication of marriage terms) appears to the trapped spouse as pure extraction — the authority sets rules that concentrate resources away from them with no exit option. To the authority administrator, the identical constraint appears as coordination — they are solving the genuine problem of determining inheritance, divorce, and custody when multiple parties have conflicting claims. The gap reveals that extraction and coordination are not opposite phenomena but overlapping ones: the authority genuinely coordinates property division (what would happen without adjudication? contested claims, social disorder?) while simultaneously extracting from specific categories (economically dependent spouses, disinherited children). The tangled-rope classification reflects this overlap: the constraint does coordinate AND extract. The piton perspective (cosmopolitan elite with forum shopping) reveals that the constraint's functional force erodes with wealth and mobility — the adjudication mechanism persists as theater (cultural performance of authority) long after its extraction capacity has been circumvented by alternatives. The mountain perspective risks naturalizing contingency: authority adjudication appears to be a natural law of all societies, but this naturality conceals that it is a contingent institutional arrangement that benefits authority administrators and disadvantages exit-constrained parties.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from structural position: power level, exit options, and relationship to extraction flow. Exit-constrained spouses (powerless, trapped) have d→1.0 (full target): they face material barriers to exit and the authority sets rules to their disadvantage. Authority administrators (institutional, arbitrage) have d→0.0 (full beneficiary): they control the adjudication process and have arbitrage options (can change rules, can appeal to authority sources, can enforce compliance). Economically advantaged spouses (powerful, mobile) have d→0.3 (mild target when their rules dominate, higher d when rules shift against them). Disinherited children (powerless, trapped) have d→1.0 (full target): no exit from inheritance rules set by others. Informal marriage parties (powerless, identity_locked) have d→0.6 (moderate-high target): structurally outside formal extraction but identity-bound to community recognition that lacks legal standing. Reform coalitions (organized, constrained) have d→0.5 (symmetric): they both benefit from eventual rule changes and bear costs of working within political constraints. Cosmopolitan elite (powerful, arbitrage) have d→0.0 (full beneficiary): they can exit unfavorable rules through forum shopping and relocation. The engine applies f(d) to compute effective extraction χ, modulated by spatial scope: smaller scopes make extraction harder to verify and easier to maintain; larger scopes (continental, global) make multiple legal systems visible, increasing pressure for harmonization and reducing individual extraction capacity.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding mandate of marriage adjudication was resolving property distribution, kinship determination, and inheritance succession in contexts where multiple parties (spouses, children, extended kin) had overlapping claims. This mandate remains partially live — modern divorce and inheritance law explicitly solve these coordination problems. However, LGBTQ+ marriage recognition and contemporary estate-planning reforms have partially dissolved the mandate by removing assumptions it rested on (heterosexual dyads, patrilineal inheritance, religious grounding). The constraint now serves additional mandates (recognizing diverse family structures, enabling contractual family terms, protecting children across non-biological kinship) that did not drive its original formation. Mandatrophy is PARTIAL: the constraint's original function (property coordination in traditional kinship systems) has become a subset of its contemporary function (adjudicating diverse family arrangements, managing state enforcement of contractual family terms). The persistence despite mandate drift is explained by institutional inertia: authority systems created to solve the original problem now perpetuate themselves by expanding scope and incorporating new mandates. This partial mandatrophy explains the rising theater_ratio early (codification phase) followed by declining theater (contractual alternatives): the original ceremonial and communal adjudication systems gave way to formal state authority, which is now giving way to market-mediated alternatives (prenuptial contracts, private arbitration, forum shopping). The constraint is not fully piton (atrophied function maintained as performance) but shows piton-adjacent features for high-wealth actors who can bypass formal authority entirely.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    authority_grounding_multiplicity,
    'When multiple legal systems (statutory, customary, religious, contractual) have plausible claims to legitimate authority over marriage, whose adjudication takes precedence, and on what grounds?',
    'Case-by-case analysis of forum-shopping outcomes, conflict-of-laws doctrine evolution, constitutional court decisions on legal pluralism. Empirical test: which authority''s rules are actually enforced when different systems issue conflicting adjudications?',
    'If state statutory law prevails: constraint resolves as state monopoly (reduces extraction visibility for religious/customary authorities). If multiple authorities coexist: constraint fragments into separate stories per legal system. If resolution depends on individual bargaining power: extractiveness increases (privileged agents choose favorable forum).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(authority_grounding_multiplicity, empirical, 'Which authority framework prevails when legal systems conflict').

omega_variable(
    exit_mechanism_for_informal_marriages,
    'For parties in informal or customary marriages (unions recognized by community but not state), what exit options actually exist when no formal authority adjudicates their dissolution or property division?',
    'Empirical study of informal marriage dissolution (mediation outcomes, community enforcement, property loss rates). Survey of informal marriage parties'' exit costs and alternative forums. Historical analysis of transition from customary to statutory marriage systems.',
    'If exit options are substantively higher for informal parties: the formal authority system is extractive (formally married parties are trapped). If informal parties face equal or worse exit constraints: the constraint operates below the formal authority system entirely (stories decompose).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exit_mechanism_for_informal_marriages, empirical, 'Exit options for parties in informal or customary marriages').

omega_variable(
    substantive_rule_variance_extraction_mechanism,
    'Does the radical variance in substantive rules across legal systems (inheritance laws, divorce grounds, custody standards) make the constraint inherently extractive to whoever inhabits a disfavored jurisdiction, or does equal variance mean no authority has systematic extraction capacity?',
    'Cross-jurisdictional analysis: compare extraction intensity within each jurisdiction to variance between jurisdictions. If extraction concentrates systemically on the same categories (women, economically dependent spouses, children) across all rule systems despite different content, variance does not prevent extraction. If extraction correlates with rule content (patrilineal systems extract from spouses, matrilineal extract from children), then variance is distribution mechanism, not extraction prevention.',
    'If systemic extraction across all rule sets: constraint is snare from the perspective of structurally vulnerable agents regardless of jurisdiction (strengthens snare classification). If extraction varies with rules: constraint decomposes into separate per-jurisdiction stories. If variance prevents coalition (each jurisdiction''s victims are scattered across different victim categories): suppression effectiveness increases (organized resistance harder).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(substantive_rule_variance_extraction_mechanism, empirical, 'Whether substantive rule variance masks systemic extraction mechanisms').

omega_variable(
    theater_ratio_across_authority_types,
    'Does the theater_ratio (0.58) represent proportional theater across all authority types (statutory, religious, customary), or do certain authority groundings require higher theater to maintain legitimacy?',
    'Comparative analysis of ritual, ceremonial, and doctrinal content required for legitimacy across authority types. Measure: what fraction of authority activity is adjudication vs. legitimacy-maintenance ceremony? For statutory law: court procedure, written opinions, precedent citation. For religious law: doctrinal interpretation, scriptural citation, clerical authority performance. For customary law: elder council ritual, community witness, tradition narration.',
    'If religious/customary authorities require substantially higher theater than statutory: extraction mechanism relies on performative legitimacy. If theater is uniform: legitimacy performance is independent of authority grounding. If theater is decreasing (statutory > customary > informal): formal authority systems are theatrically intensive (possible piton signature).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theater_ratio_across_authority_types, empirical, 'Theater requirements vary across authority-grounding types').

omega_variable(
    false_summit_natural_authority,
    'Is the mountain classification (natural authority principle) a genuine natural law — every society must adjudicate marriage — or a constructed constraint that benefits authority administrators by naturalizing their monopoly?',
    'Comparative anthropology: societies with minimal or no centralized marriage adjudication (gift economies with mutual bride-price adjustment, kinship systems with distributed decision-making, egalitarian communities with no formal divorce/inheritance law). Do they face coordination failures that force authority development, or do alternatives emerge? Historical analysis: did authority monopolies develop through evolutionary necessity or through coercive imposition?',
    'If true natural law: mountain classification is correct, and beneficiary presence is incidental. If constructed: false summit, reclassify to tangled_rope or snare, beneficiaries are vindicated-proposition operators (authority administrators).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_natural_authority, conceptual, 'Whether authority adjudication is natural necessity or constructed extraction').

omega_variable(
    mandate_obsolescence_lgbtq_recognition,
    'The founding mandate of marriage adjudication is managing property, kinship, and inheritance in heterosexual dyads. Does LGBTQ+ marriage recognition represent mandate expansion (new problem solved) or mandate dissolution (fundamental assumption overturned)?',
    'Constitutional court decisions, legislative debates, and empirical outcomes comparing LGBTQ+ and heterosexual marriage adjudication. If identical rules apply: mandate expanded (constraint persists). If fundamentally new rules required (no spousal property transfer, alternative inheritance claims, non-binary custody frameworks): mandate partially dissolved and redesigned.',
    'If mandate dissolution: mandatrophy confirmed (original founding problem no longer explains the constraint''s persistence). If expansion: mandate is more plastic than its original grounding suggests (reduces false summit plausibility).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandate_obsolescence_lgbtq_recognition, empirical, 'LGBTQ+ recognition as mandate expansion or dissolution').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority_kernel_flat_control, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(theater_t0_traditional_ceremony, marriage_authority_kernel_flat_control, theater_ratio, 0, 0.72).
narrative_ontology:measurement(theater_t5_legalization, marriage_authority_kernel_flat_control, theater_ratio, 5, 0.65).
narrative_ontology:measurement(theater_t10_decoupling, marriage_authority_kernel_flat_control, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(extractiveness_t0_traditional_authority, marriage_authority_kernel_flat_control, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(extractiveness_t5_codification_phase, marriage_authority_kernel_flat_control, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(extractiveness_t10_contemporary, marriage_authority_kernel_flat_control, base_extractiveness, 10, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(suppression_t0_traditional, marriage_authority_kernel_flat_control, suppression_requirement, 0, 0.62).
narrative_ontology:measurement(suppression_t5_codification, marriage_authority_kernel_flat_control, suppression_requirement, 5, 0.68).
narrative_ontology:measurement(suppression_t10_contemporary, marriage_authority_kernel_flat_control, suppression_requirement, 10, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority_kernel_flat_control, resource_allocation).
narrative_ontology:affects_constraint(marriage_authority_kernel_flat_control, spousal_property_transfer_asymmetry).
narrative_ontology:affects_constraint(marriage_authority_kernel_flat_control, inheritance_rule_patrilineality).
narrative_ontology:affects_constraint(marriage_authority_kernel_flat_control, custody_determination_gender_bias).
narrative_ontology:affects_constraint(marriage_authority_kernel_flat_control, divorce_forum_shopping_plural_law).

% DUAL FORMULATION NOTE:
% The marriage authority constraint operates at the meta-level (who gets to decide?) and influences four substantive constraints (what do they decide?). The meta-level constraint is a single story showing extractiveness across all authority types. Decomposition into per-authority-type stories (statutory-law marriage adjudication, religious-law marriage adjudication, customary-law marriage adjudication) would show different extractiveness values and different beneficiary/victim distributions — each would be a distinct constraint. This FLAT construction keeps the meta-level constraint unified to show how radical rule variance operates under the shared commitment to authority adjudication.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(marriage_authority_kernel_flat_control, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
