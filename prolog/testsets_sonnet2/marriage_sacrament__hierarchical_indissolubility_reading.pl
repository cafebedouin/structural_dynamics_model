% ============================================================================
% CONSTRAINT STORY: marriage_sacrament__hierarchical_indissolubility_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_sacrament__hierarchical_indissolubility_reading, []).

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
 *   constraint_id: marriage_sacrament__hierarchical_indissolubility_reading
 *   human_readable: Sacramental Marriage as Ontological Bond Under Hierarchical Adjudication (Indissolubility Reading)
 *   domain: religious_doctrine/canon_law/political_sociology
 *
 * SUMMARY:
 *   This story instantiates the hierarchical_indissolubility_reading of the
 *   marriage_sacrament kernel: marriage is treated as an ontological reality
 *   established at the moment of valid consent, which no human authority —
 *   including the spouses themselves — can dissolve. Because the bond is
 *   constitutive rather than aspirational, any apparent ending of the
 *   marriage (civil divorce, remarriage) does not touch its sacramental
 *   reality; only a tribunal finding that the marriage was never validly
 *   contracted (nullity) can free a person to remarry within the Church. This
 *   reading generates a formal adjudicative bureaucracy — the tribunal system
 *   — whose operation excludes non-adjudicated divorced-and-remarried
 *   Catholics from the Eucharist. The sibling civic_pastoral_reading, which
 *   treats indissolubility as an ideal admitting pastoral discernment, is a
 *   DIFFERENT constraint with a different ε and a different victim set (or
 *   none) — it is not blended into this story per the ε-invariance principle.
 *
 * KEY AGENTS:
 *   - magisterial_hierarchy: institutional agenda-setter, defines and enforces the doctrine
 *   - canon_law_tribunal_system: institutional adjudicator, administers annulment process and bears no cost from its own operation
 *   - divorced_catholics_seeking_remarriage: powerless payers, must undergo tribunal process for any path to remarriage
 *   - civilly_remarried_catholics: powerless and trapped, excluded from sacraments absent annulment
 *   - civic_pastoral_reading_advocates: organized but excluded, hold the sibling reading without institutional control
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_sacrament__hierarchical_indissolubility_reading, 0.68).
domain_priors:suppression_score(marriage_sacrament__hierarchical_indissolubility_reading, 0.62).
domain_priors:theater_ratio(marriage_sacrament__hierarchical_indissolubility_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_sacrament__hierarchical_indissolubility_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(marriage_sacrament__hierarchical_indissolubility_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(marriage_sacrament__hierarchical_indissolubility_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_sacrament__hierarchical_indissolubility_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(marriage_sacrament__hierarchical_indissolubility_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_sacrament__hierarchical_indissolubility_reading, tangled_rope).
narrative_ontology:human_readable(marriage_sacrament__hierarchical_indissolubility_reading, "Sacramental Marriage as Ontological Bond Under Hierarchical Adjudication (Indissolubility Reading)").
narrative_ontology:topic_domain(marriage_sacrament__hierarchical_indissolubility_reading, "religious_doctrine/canon_law/political_sociology").

domain_priors:requires_active_enforcement(marriage_sacrament__hierarchical_indissolubility_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_sacrament__hierarchical_indissolubility_reading, '5cb28831-a997-40af-8f3b-8d5d2afe1866').
narrative_ontology:cs_kernel_codification('5cb28831-a997-40af-8f3b-8d5d2afe1866', formalized).
narrative_ontology:cs_authority_grounding('5cb28831-a997-40af-8f3b-8d5d2afe1866', lineage).
narrative_ontology:cs_interpretation_layer_present('5cb28831-a997-40af-8f3b-8d5d2afe1866').
narrative_ontology:cs_reading_relation('5cb28831-a997-40af-8f3b-8d5d2afe1866', marriage_sacrament__civic_pastoral_reading, coexists_with).
narrative_ontology:cs_axiom('5cb28831-a997-40af-8f3b-8d5d2afe1866', foundational, marital_bond_is_ontologically_constitutive_not_aspirational).
narrative_ontology:cs_axiom_status(marital_bond_is_ontologically_constitutive_not_aspirational, holdable).
narrative_ontology:cs_axiom_grounding('5cb28831-a997-40af-8f3b-8d5d2afe1866', marital_bond_is_ontologically_constitutive_not_aspirational, theological).
narrative_ontology:cs_axiom('5cb28831-a997-40af-8f3b-8d5d2afe1866', foundational, only_hierarchical_tribunal_finding_can_establish_nullity).
narrative_ontology:cs_axiom_status(only_hierarchical_tribunal_finding_can_establish_nullity, holdable).
narrative_ontology:cs_axiom_grounding('5cb28831-a997-40af-8f3b-8d5d2afe1866', only_hierarchical_tribunal_finding_can_establish_nullity, conventional).
narrative_ontology:cs_reference_frame('5cb28831-a997-40af-8f3b-8d5d2afe1866', tridentine_sacramental_ontology).
narrative_ontology:cs_drift_state('5cb28831-a997-40af-8f3b-8d5d2afe1866', post_vatican_ii_pastoral_turn, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('5cb28831-a997-40af-8f3b-8d5d2afe1866', '').
narrative_ontology:cs_kernel_id(marriage_sacrament__hierarchical_indissolubility_reading, marriage_sacrament).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_sacrament__hierarchical_indissolubility_reading, magisterial_hierarchy).
narrative_ontology:constraint_beneficiary(marriage_sacrament__hierarchical_indissolubility_reading, canon_law_tribunal_system).
narrative_ontology:constraint_beneficiary(marriage_sacrament__hierarchical_indissolubility_reading, intact_first_marriages_in_good_standing).
narrative_ontology:constraint_victim(marriage_sacrament__hierarchical_indissolubility_reading, divorced_catholics_seeking_remarriage).
narrative_ontology:constraint_victim(marriage_sacrament__hierarchical_indissolubility_reading, civilly_remarried_catholics).
narrative_ontology:constraint_victim(marriage_sacrament__hierarchical_indissolubility_reading, annulment_petitioners).
narrative_ontology:constraint_vindicates(marriage_sacrament__hierarchical_indissolubility_reading, marital_bond_ontological_permanence_doctrine).
narrative_ontology:constraint_vindicates(marriage_sacrament__hierarchical_indissolubility_reading, sacramental_validity_requires_hierarchical_determination).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Defines and administers the doctrine that a valid sacramental marriage cannot be dissolved by any human authority, only by death or a tribunal finding of invalidity from the outset. Sets canon law, staffs and funds the tribunal system, and controls access to sacraments as the enforcement lever. Bears no personal cost from the doctrine's operation; its institutional authority and coherence depend on maintaining the bond's ontological status.
narrative_ontology:constraint_stakeholder(marriage_sacrament__hierarchical_indissolubility_reading, magisterial_hierarchy, agenda_setter,
    institutional, civilizational, analytical, global).

% Adjudicates annulment petitions, employing canon lawyers, judges, and administrative staff. Charges fees (sliding scale in principle, but real costs including counsel, travel, and time), and takes months to years to resolve cases. Its continued institutional existence and staffing depend on the ongoing operation of the indissolubility doctrine requiring formal adjudication rather than simple dissolution.
narrative_ontology:constraint_stakeholder(marriage_sacrament__hierarchical_indissolubility_reading, canon_law_tribunal_system, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(marriage_sacrament__hierarchical_indissolubility_reading, canon_law_tribunal_system, beneficiary).

% Receive full sacramental participation and social standing within the Church without needing to interact with the tribunal system. Benefit from a doctrine that stabilizes marital expectations and reinforces the seriousness of their own vows, without bearing any of its adjudicative costs.
narrative_ontology:constraint_stakeholder(marriage_sacrament__hierarchical_indissolubility_reading, intact_first_marriages_in_good_standing, beneficiary,
    moderate, biographical, mobile, national).

% Civilly divorced and wishing to remarry within the Church, they must petition a tribunal to have their first marriage declared null — not dissolved, but retroactively deemed to have never validly existed. This requires reconstructing intimate personal history for canon lawyers, incurring cost and delay, and accepting a legal fiction (nullity rather than dissolution) as the only sanctioned path forward. Exit means leaving the sacramental community or living outside its full participation.
narrative_ontology:constraint_stakeholder(marriage_sacrament__hierarchical_indissolubility_reading, divorced_catholics_seeking_remarriage, payer,
    powerless, biographical, constrained, national).

% Having remarried civilly without an annulment, they are canonically considered to remain bound to their first spouse and are excluded from receiving the Eucharist and other sacraments under the hierarchy's enforcement of indissolubility. Many remain practicing, socially embedded Catholics with no realistic institutional path back to full participation short of separating from their current spouse or securing a retroactive nullity finding.
narrative_ontology:constraint_stakeholder(marriage_sacrament__hierarchical_indissolubility_reading, civilly_remarried_catholics, payer,
    powerless, biographical, trapped, national).

% Distinct from those who have already remarried, these are petitioners mid-process: bearing tribunal fees, providing witness testimony about the failed marriage, and waiting — sometimes years — for a determination that will decide whether they may licitly remarry in the Church. The process itself is a cost independent of outcome.
narrative_ontology:constraint_stakeholder(marriage_sacrament__hierarchical_indissolubility_reading, annulment_petitioners, payer,
    powerless, biographical, constrained, national).

% Clergy, theologians, and lay Catholics who hold that indissolubility is a pastoral ideal admitting compassionate discernment (e.g., the Kasper proposal, situational admission to communion) rather than a metaphysical fact requiring tribunal adjudication. Their reading exists within the same Church but does not control the hierarchy's binding enforcement apparatus; they can advocate and practice pastoral accompaniment locally but cannot alter the canonical requirement.
narrative_ontology:constraint_stakeholder(marriage_sacrament__hierarchical_indissolubility_reading, civic_pastoral_reading_advocates, excluded,
    organized, generational, constrained, national).

% Study the historical development of the indissolubility doctrine, the Pauline and Petrine privilege exceptions, and comparative canon law across Christian traditions. Can describe the tribunal system's actual operation and outcomes without being subject to its enforcement themselves.
narrative_ontology:constraint_stakeholder(marriage_sacrament__hierarchical_indissolubility_reading, canon_law_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_sacrament__hierarchical_indissolubility_reading, canon_law_tribunal_system).
narrative_ontology:fixing_cost_class(marriage_sacrament__hierarchical_indissolubility_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, publicly legible standard for what counts as a valid, binding marriage within the Church, protecting spouses (especially historically, wives) from unilateral abandonment and giving the community a shared, non-negotiable definition of the marital bond that cannot be eroded case-by-case.
% TRANSFER_FUNCTION: Moves time, money, emotional disclosure, and sacramental access from divorced and remarried Catholics to the tribunal system and, indirectly, to the doctrinal authority of the hierarchy — petitioners pay in labor and fees to have a legal fiction of nullity established, or else are excluded from communion indefinitely.
% ABSENT_VOICES: Divorced Catholics who cannot afford or navigate the tribunal process, and those in abusive first marriages who find the burden of proving canonical grounds retraumatizing, are structurally underrepresented in the doctrine's formation; civic_pastoral_reading advocates within the Church argue for discernment-based admission but do not control the adjudicating authority.
% DISAPPEARANCE_RATIONALE: If hierarchical adjudication of indissolubility vanished overnight, the tribunal system would lose its function, divorced and remarried Catholics would gain immediate access to sacraments without petitioning, and the Church's doctrinal claim to control valid marriage status would collapse into something closer to the civic_pastoral_reading's discernment model — a substantial reorganization of both institutional structure and lived practice for millions of Catholics.
% FOUNDING_PROBLEM: To establish, against arbitrary unilateral repudiation (historically disadvantaging wives) and against competing civil or informal standards, a stable and theologically grounded definition of marriage as a permanent bond that neither spouse nor secular authority could dissolve at will.
% FOUNDING_PROBLEM_CORROBORATION: The hierarchy attests the ontological-permanence problem remains fully live and doctrinally necessary. Canon law historians and sociologists of religion, along with civic_pastoral_reading advocates within the Church itself, attest that the original protective function (guarding against abandonment) has been substantially supplanted in practice by an adjudicative bureaucracy whose primary observable effect is exclusion and delay rather than protection — a reading corroborated by rising annulment rates in some dioceses interpreted by outside sociologists as evidence of doctrine straining against lived reality rather than describing it.
narrative_ontology:disappearance_verdict(marriage_sacrament__hierarchical_indissolubility_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_sacrament__hierarchical_indissolubility_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_sacrament__hierarchical_indissolubility_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(marriage_sacrament__hierarchical_indissolubility_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_sacrament__hierarchical_indissolubility_reading, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_sacrament__hierarchical_indissolubility_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_sacrament__hierarchical_indissolubility_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_sacrament__hierarchical_indissolubility_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68) because the doctrine's practical effect is prolonged, costly exclusion from core sacramental goods (Eucharist, licit remarriage) for a substantial population, with the annulment process functioning as a gatekeeping mechanism rather than a genuine pastoral accompaniment. Suppression (0.62) reflects that the only sanctioned exit from exclusion is institutional (petitioning the tribunal) — there is no self-help remedy within the framework this reading endorses. Theater ratio (0.40) captures that a portion of tribunal activity (extensive interviews, procedural formality) functions more to legitimate the nullity finding than to serve any protective function the original doctrine was built for. Accessibility collapse (0.58) and resistance (0.55) reflect that, once inside this reading's framework, alternatives are formally closed off, but real resistance persists — both from within (pastoral dissent, civic_pastoral_reading advocates) and from petitioners who experience the process as unjust.
 *
 * PERSPECTIVAL GAP:
 *   From the hierarchy's seat, the doctrine is coordination: a stable, theologically necessary standard protecting the integrity of marriage as an institution. From the seat of a civilly remarried Catholic barred from the Eucharist, the same structure operates as enforced exclusion with no clear terminus. The engine's per-seat computation should register both — the divergence is the point, not an error to reconcile.
 *
 * DIRECTIONALITY LOGIC:
 *   The magisterial_hierarchy and canon_law_tribunal_system sit at the beneficiary end: they set the terms, administer the process, and their institutional coherence and legitimacy depend on the doctrine's continued operation, while they bear none of its costs personally. Divorced Catholics seeking remarriage, civilly remarried Catholics, and annulment petitioners sit at the target end: they pay in money, time, emotional disclosure, and sacramental exclusion, with constrained or trapped exit options (leaving the Church, or living permanently outside full participation). Intact first marriages benefit incidentally from the doctrine's stabilizing function without engaging its adjudicative machinery at all.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — protecting spouses from unilateral, arbitrary abandonment and establishing a stable, non-negotiable marital standard — was historically live and protective, particularly for wives in eras with weak civil protections. Whether that problem remains equally live today, given developed civil family law and social supports, is contested: the hierarchy holds it is doctrinally necessary regardless of civil conditions (the bond's reality does not depend on secular circumstances), while outside sociologists and canon law historians observe the tribunal system's dominant contemporary function is adjudicative gatekeeping rather than spousal protection. Classifying this as tangled_rope rather than pure snare preserves the genuine historical coordination function (a shared, non-negotiable definition of marriage) while registering the asymmetric extraction now falling on divorced and remarried Catholics — collapsing it to snare would erase the doctrine's real coordination history; collapsing it to rope would erase the concentrated, institutionally enforced costs its current operation imposes on a specific victim class.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ontological_vs_constructed_bond,
    'Is the sacramental marital bond genuinely ontological (a metaphysical fact independent of human recognition) or is ''ontological'' status itself a doctrinal construction that serves the hierarchy''s adjudicative authority?',
    'This is not empirically resolvable by external evidence; it depends on theological premises internal to the tradition. The strongest available proxy is examining whether the doctrine''s practical administration (tribunal outcomes, exceptions like the Pauline/Petrine privilege) is more consistent with a discovered metaphysical fact or with a negotiated institutional rule that has been adjusted over centuries in response to pastoral and political pressure.',
    'If constructed, the extraction imposed on divorced/remarried Catholics rests on a contingent institutional choice rather than a discovered fact, strengthening the case for reclassification toward snare or for treating the civic_pastoral_reading as the more defensible framing. If genuinely ontological by the tradition''s own internal logic, the tangled_rope classification (real coordination function plus real asymmetric cost) is the more accurate reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ontological_vs_constructed_bond, conceptual, 'Whether the ontological status of the sacramental bond is a discovered fact or an institutionally constructed doctrine.').

omega_variable(
    sibling_reading_structural_delta,
    'This constraint is one reading (hierarchical_indissolubility_reading) of the marriage_sacrament kernel. The sibling reading (civic_pastoral_reading) treats indissolubility as an aspirational ideal admitting discernment-based pastoral accompaniment rather than mandatory tribunal adjudication. Where exactly does the disagreement locate structurally?',
    'The disagreement is located at a single structural element: whether nullity-or-nothing is the only sanctioned path to sacramental reintegration, or whether pastoral discernment (internal forum solutions, graduated accompaniment per Amoris Laetitia footnote 351) constitutes a legitimate alternative path. Resolving which reading a given diocese or bishops'' conference operationalizes would require examining actual sacramental admission practice, not doctrinal text alone.',
    'Under the civic_pastoral_reading, the victim set (divorced/remarried Catholics) shrinks or disappears because pastoral accompaniment provides a sanctioned path to reintegration without tribunal adjudication; ε for that sibling constraint is substantially lower. The two readings cannot be blended into one ε without violating ε-invariance — they are authored as separate constraint files linked via network.affects_constraints.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_structural_delta, conceptual, 'Where the hierarchical and civic-pastoral readings of the marriage_sacrament kernel structurally diverge.').

omega_variable(
    tribunal_cost_variance,
    'How much do actual annulment tribunal costs and delays vary across dioceses, and to what extent does that variance reflect genuine case complexity versus institutional capacity or willingness to expedite?',
    'Comparative diocesan data on annulment case duration, fee structures (including fee waivers), and approval rates would allow separating genuine procedural necessity from institutional friction that functions as de facto suppression.',
    'High unexplained variance unrelated to case complexity would support classifying a larger share of the measured extraction as institutional friction (tangled_rope leaning toward snare) rather than necessary coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tribunal_cost_variance, empirical, 'Whether tribunal cost and delay variance reflects necessary adjudication or avoidable institutional friction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_sacrament__hierarchical_indissolubility_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t0, marriage_sacrament__hierarchical_indissolubility_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(marr_tr_t10, marriage_sacrament__hierarchical_indissolubility_reading, theater_ratio, 10, 0.26).
narrative_ontology:measurement(marr_tr_t20, marriage_sacrament__hierarchical_indissolubility_reading, theater_ratio, 20, 0.29).
narrative_ontology:measurement(marr_tr_t30, marriage_sacrament__hierarchical_indissolubility_reading, theater_ratio, 30, 0.32).
narrative_ontology:measurement(marr_tr_t40, marriage_sacrament__hierarchical_indissolubility_reading, theater_ratio, 40, 0.35).
narrative_ontology:measurement(marr_tr_t50, marriage_sacrament__hierarchical_indissolubility_reading, theater_ratio, 50, 0.38).
narrative_ontology:measurement(marr_tr_t60, marriage_sacrament__hierarchical_indissolubility_reading, theater_ratio, 60, 0.4).

% Extraction over time
narrative_ontology:measurement(marr_be_t0, marriage_sacrament__hierarchical_indissolubility_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(marr_be_t10, marriage_sacrament__hierarchical_indissolubility_reading, base_extractiveness, 10, 0.54).
narrative_ontology:measurement(marr_be_t20, marriage_sacrament__hierarchical_indissolubility_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(marr_be_t30, marriage_sacrament__hierarchical_indissolubility_reading, base_extractiveness, 30, 0.61).
narrative_ontology:measurement(marr_be_t40, marriage_sacrament__hierarchical_indissolubility_reading, base_extractiveness, 40, 0.64).
narrative_ontology:measurement(marr_be_t50, marriage_sacrament__hierarchical_indissolubility_reading, base_extractiveness, 50, 0.66).
narrative_ontology:measurement(marr_be_t60, marriage_sacrament__hierarchical_indissolubility_reading, base_extractiveness, 60, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t0, marriage_sacrament__hierarchical_indissolubility_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(marr_su_t10, marriage_sacrament__hierarchical_indissolubility_reading, suppression_requirement, 10, 0.49).
narrative_ontology:measurement(marr_su_t20, marriage_sacrament__hierarchical_indissolubility_reading, suppression_requirement, 20, 0.53).
narrative_ontology:measurement(marr_su_t30, marriage_sacrament__hierarchical_indissolubility_reading, suppression_requirement, 30, 0.56).
narrative_ontology:measurement(marr_su_t40, marriage_sacrament__hierarchical_indissolubility_reading, suppression_requirement, 40, 0.58).
narrative_ontology:measurement(marr_su_t50, marriage_sacrament__hierarchical_indissolubility_reading, suppression_requirement, 50, 0.6).
narrative_ontology:measurement(marr_su_t60, marriage_sacrament__hierarchical_indissolubility_reading, suppression_requirement, 60, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_sacrament__hierarchical_indissolubility_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(marriage_sacrament__hierarchical_indissolubility_reading, marriage_sacrament__civic_pastoral_reading).

% DUAL FORMULATION NOTE:
% This constraint and marriage_sacrament__civic_pastoral_reading are sibling readings of the same marriage_sacrament kernel, decomposed per the ε-invariance principle because they produce structurally distinct claims with different ε values and different (or absent) victim sets. This file (hierarchical_indissolubility_reading) authors high extractiveness (0.68) grounded in mandatory tribunal adjudication and sacramental exclusion absent nullity. The sibling (civic_pastoral_reading) authors substantially lower extractiveness grounded in discernment-based pastoral accompaniment without mandatory adjudication. Both are linked bidirectionally via affects_constraints; each documents the relationship in its own commentary.narrative_context / kernel_context.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
