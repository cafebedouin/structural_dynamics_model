% ============================================================================
% CONSTRAINT STORY: creed_381_pneumatology__filioque_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_creed_381_pneumatology__filioque_reading, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: creed_381_pneumatology__filioque_reading
 *   human_readable: Filioque Clause and Roman Magisterial Authority to Clarify Trinitarian Doctrine
 *   domain: historical_theology/ecclesiastical_authority
 *
 * SUMMARY:
 *   This story instantiates the filioque_reading of the
 *   creed_381_pneumatology kernel: the claim that the Spirit proceeds from
 *   the Father and the Son, and that Roman papal/conciliar magisterium
 *   possesses the authority to clarify implicit Trinitarian doctrine without
 *   requiring ecumenical consent from the Eastern patriarchates. The clause
 *   originated in sixth-century Spanish anti-Arian liturgy (Toledo III, 589),
 *   was adopted for political-theological reasons by the Carolingian court
 *   (Aachen, 809) to differentiate Western Christendom from Byzantium, and
 *   was eventually incorporated into the Roman creed itself (1014), becoming
 *   a primary doctrinal marker of the 1054 East-West Schism. This reading
 *   treats the addition as a legitimate clarification of implicit doctrine
 *   falling within papal/Western conciliar competence; the sibling
 *   monoprocession_reading treats the same addition as an unauthorized breach
 *   of an inviolable text; the sibling ecumenical_reunion_reading treats both
 *   formulas as compatible regional expressions requiring only mutual
 *   recognition rather than correction. All three are separate constraints
 *   with distinct ε, beneficiary/victim structures, and classifications,
 *   linked via network edges — this file does not average or hedge across
 *   them.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(creed_381_pneumatology__filioque_reading, 0.71).
domain_priors:suppression_score(creed_381_pneumatology__filioque_reading, 0.68).
domain_priors:theater_ratio(creed_381_pneumatology__filioque_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(creed_381_pneumatology__filioque_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(creed_381_pneumatology__filioque_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(creed_381_pneumatology__filioque_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(creed_381_pneumatology__filioque_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(creed_381_pneumatology__filioque_reading, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(creed_381_pneumatology__filioque_reading, tangled_rope).
narrative_ontology:human_readable(creed_381_pneumatology__filioque_reading, "Filioque Clause and Roman Magisterial Authority to Clarify Trinitarian Doctrine").
narrative_ontology:topic_domain(creed_381_pneumatology__filioque_reading, "historical_theology/ecclesiastical_authority").

domain_priors:requires_active_enforcement(creed_381_pneumatology__filioque_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(creed_381_pneumatology__filioque_reading, '1482c83c-9b74-48ed-a02a-6d67be2d74dc').
narrative_ontology:cs_kernel_codification('1482c83c-9b74-48ed-a02a-6d67be2d74dc', fixed_text).
narrative_ontology:cs_authority_grounding('1482c83c-9b74-48ed-a02a-6d67be2d74dc', extraction).
narrative_ontology:cs_interpretation_layer_present('1482c83c-9b74-48ed-a02a-6d67be2d74dc').
narrative_ontology:cs_reading_relation('1482c83c-9b74-48ed-a02a-6d67be2d74dc', creed_381_pneumatology__monoprocession_reading, forecloses).
narrative_ontology:cs_reading_relation('1482c83c-9b74-48ed-a02a-6d67be2d74dc', creed_381_pneumatology__ecumenical_reunion_reading, influences).
narrative_ontology:cs_axiom('1482c83c-9b74-48ed-a02a-6d67be2d74dc', foundational, implicit_doctrine_admits_unilateral_clarification).
narrative_ontology:cs_axiom_status(implicit_doctrine_admits_unilateral_clarification, holdable).
narrative_ontology:cs_axiom_grounding('1482c83c-9b74-48ed-a02a-6d67be2d74dc', implicit_doctrine_admits_unilateral_clarification, conventional).
narrative_ontology:cs_axiom('1482c83c-9b74-48ed-a02a-6d67be2d74dc', foundational, son_is_co_principle_of_spirits_procession).
narrative_ontology:cs_axiom_status(son_is_co_principle_of_spirits_procession, holdable).
narrative_ontology:cs_axiom_grounding('1482c83c-9b74-48ed-a02a-6d67be2d74dc', son_is_co_principle_of_spirits_procession, theological).
narrative_ontology:cs_reference_frame('1482c83c-9b74-48ed-a02a-6d67be2d74dc', nicene_constantinopolitan_conciliar_settlement).
narrative_ontology:cs_drift_state('1482c83c-9b74-48ed-a02a-6d67be2d74dc', post_1054_schism_consolidation, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('1482c83c-9b74-48ed-a02a-6d67be2d74dc', '').
narrative_ontology:cs_kernel_id(creed_381_pneumatology__filioque_reading, creed_381_pneumatology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(creed_381_pneumatology__filioque_reading, roman_papal_see).
narrative_ontology:constraint_beneficiary(creed_381_pneumatology__filioque_reading, latin_theological_schools).
narrative_ontology:constraint_beneficiary(creed_381_pneumatology__filioque_reading, carolingian_frankish_monarchy).
narrative_ontology:constraint_victim(creed_381_pneumatology__filioque_reading, eastern_patriarchates).
narrative_ontology:constraint_victim(creed_381_pneumatology__filioque_reading, byzantine_theological_tradition).
narrative_ontology:constraint_victim(creed_381_pneumatology__filioque_reading, conciliar_consent_norm).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(creed_381_pneumatology__filioque_reading, latin_theological_schools).
narrative_ontology:constraint_vindicates(creed_381_pneumatology__filioque_reading, papal_doctrinal_clarification_authority).
narrative_ontology:constraint_vindicates(creed_381_pneumatology__filioque_reading, trinitarian_procession_from_father_and_son).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Progressively adopts and eventually formally endorses the Filioque clause into the Latin liturgical creed, first tolerating regional Frankish usage and later incorporating it into the Roman rite itself (1014). Claims authority to clarify implicit Trinitarian doctrine without requiring convocation of an ecumenical council including the Eastern patriarchates. Gains a doctrinal lever that consolidates Western theological unity under Roman interpretive primacy and provides justification for later claims of universal jurisdiction.
narrative_ontology:constraint_stakeholder(creed_381_pneumatology__filioque_reading, roman_papal_see, agenda_setter,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(creed_381_pneumatology__filioque_reading, roman_papal_see, beneficiary).

% Promotes the Filioque within Frankish liturgy and at the Council of Aachen (809) partly to distinguish Western Christendom theologically from a Byzantine East it is contesting politically and territorially. Benefits from a doctrinal marker of Western distinctiveness that reinforces Carolingian claims to imperial and religious legitimacy independent of Constantinople.
narrative_ontology:constraint_stakeholder(creed_381_pneumatology__filioque_reading, carolingian_frankish_monarchy, beneficiary,
    powerful, generational, mobile, continental).

% Develop and defend the Augustinian double-procession framework as theologically coherent and pastorally necessary against Arian residues. Gain intellectual authority and institutional position from being the interpretive custodians of the clarified doctrine, but are also constrained by having to defend a formula that later becomes a permanent flashpoint rather than a settled matter, requiring perpetual apologetic labor.
narrative_ontology:constraint_stakeholder(creed_381_pneumatology__filioque_reading, latin_theological_schools, beneficiary,
    organized, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(creed_381_pneumatology__filioque_reading, latin_theological_schools, payer).

% Object that the clause was added to the 381 creed without ecumenical consent, violating the explicit prohibition against creedal alteration affirmed at Ephesus (431) and Chalcedon (451). Bear the cost of a unilateral doctrinal change imposed on a text they regard as jointly owned and inviolable; their theological tradition of monoprocession is recast by the Roman reading as a deficient or incomplete articulation rather than a legitimate alternative. Cannot appeal to a mutually recognized authority above Rome's own claim to clarify, since Rome's claim to that authority is precisely what is contested.
narrative_ontology:constraint_stakeholder(creed_381_pneumatology__filioque_reading, eastern_patriarchates, payer,
    powerful, civilizational, constrained, continental).

% Its Cappadocian-derived pneumatology, articulated for centuries as procession from the Father alone (with the Son as the means, not the source), is structurally delegitimized by the filioque reading's framing that the 381 formula was merely implicit and incomplete. Has no exit from the consequences of the schism the clause helps trigger (1054) short of full submission to Roman interpretive primacy, which it regards as the deeper problem, not a proposed solution.
narrative_ontology:constraint_stakeholder(creed_381_pneumatology__filioque_reading, byzantine_theological_tradition, payer,
    organized, civilizational, trapped, continental).

% The norm that ecumenical creeds may only be amended by ecumenical councils (affirmed explicitly at Ephesus and Chalcedon) is directly overridden by unilateral Western insertion and later unilateral Roman ratification. Not an actor itself, but a procedural doctrine whose authority is the thing consumed to make the filioque reading operative.
narrative_ontology:constraint_stakeholder(creed_381_pneumatology__filioque_reading, conciliar_consent_norm, payer,
    institutional, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(creed_381_pneumatology__filioque_reading, conciliar_consent_norm).

% Later Byzantine and Latin delegations at Lyon (1274) and Florence (1439) attempt bilateral or economy-based framings that would let both processional formulas stand as compatible expressions. Their proposals are structurally excluded from this reading's operative logic, which requires the filioque to be the corrected clarification rather than one of two acceptable expressions — reunion agreements reached under this framing are repeatedly repudiated by the Eastern churches once political pressure lifts, because the underlying authority claim was never actually shared.
narrative_ontology:constraint_stakeholder(creed_381_pneumatology__filioque_reading, reunion_council_negotiators, excluded,
    moderate, generational, constrained, continental).

% Trace the clause's origins in anti-Arian Spanish liturgy (Third Council of Toledo, 589), its Frankish political adoption, and its eventual Roman ratification, documenting how a regional liturgical variant became, under this reading, a claimed universal clarification of doctrine binding on churches that were never consulted.
narrative_ontology:constraint_stakeholder(creed_381_pneumatology__filioque_reading, ecclesiastical_historians, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, doctrinally unambiguous formula for the Spirit's procession that Western theologians can use against residual Arian and Germanic heterodoxies, and gives the Latin church one authoritative center capable of resolving live doctrinal disputes without waiting on a general council that increasingly could not be convened with full Eastern participation.
% TRANSFER_FUNCTION: Moves interpretive authority over Trinitarian doctrine from the collective ecumenical council (jointly held by East and West) to the Roman see acting with regional Frankish backing; moves theological legitimacy away from the Cappadocian monoprocession tradition and toward the Augustinian double-procession tradition; moves practical primacy claims toward Rome at the expense of Constantinople's patriarchal standing.
% ABSENT_VOICES: The Eastern patriarchates were not party to the Frankish liturgical adoption (809) nor meaningfully consulted before Rome's own eventual incorporation of the clause (1014); Photius and later Byzantine theologians objected vigorously but from outside the decision-making process that mattered to Western practice. Reunion-council negotiators who sought a both/and framing were repeatedly overridden once political urgency for reunion passed.
% DISAPPEARANCE_RATIONALE: If the filioque reading's authority claim were withdrawn — i.e., if Rome conceded the clause required ecumenical consent it never received — the doctrinal basis for treating the 1054 schism as a matter of Eastern error rather than mutual and disputable difference would collapse, reopening the legitimacy of papal unilateral doctrinal clarification generally and destabilizing centuries of Latin ecclesiology built on the precedent that Rome may clarify implicit doctrine without conciliar concurrence.
% FOUNDING_PROBLEM: The Latin church faced a live anti-Arian pastoral problem in seventh-century Spain (subordinationist readings of the Son threatened by denying his full co-equality including relation to the Spirit) and a later political problem for Frankish rulers wanting theological distinction from Byzantium; the filioque addressed both by making the Son co-source of the Spirit's procession.
% FOUNDING_PROBLEM_CORROBORATION: Contemporary Catholic theologians (including statements from the Pontifical Council for Promoting Christian Unity, 1995) acknowledge the anti-Arian pastoral context that motivated the original Toledo formula no longer describes any live doctrinal threat in either communion, and that the clause's continued liturgical and dogmatic assertion is now sustained primarily by institutional precedent and ecclesiological authority claims rather than the founding pastoral problem — this corroboration comes from within the Roman tradition itself, acknowledging the mismatch; independent historians of the schism (e.g., non-confessional academic treatments) concur that the ninth-century political motive, not ongoing Arian risk, explains the clause's entrenchment.
narrative_ontology:disappearance_verdict(creed_381_pneumatology__filioque_reading, world_rearranges).
narrative_ontology:founding_problem_status(creed_381_pneumatology__filioque_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(creed_381_pneumatology__filioque_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(creed_381_pneumatology__filioque_reading, 'none', 1).
narrative_ontology:epsilon_provenance(creed_381_pneumatology__filioque_reading, 0.71, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(creed_381_pneumatology__filioque_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(creed_381_pneumatology__filioque_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(creed_381_pneumatology__filioque_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.71 at interval end) because the reading transfers real interpretive authority and ecclesiological primacy from a jointly-held conciliar process to a unilaterally-acting Western center, and that transfer persists as the institutional basis for later Roman primacy claims. Suppression is substantial (0.68) because the reading's operation depends on treating Eastern objection as theological error rather than legitimate dissent, and on excluding reunion-council compromise formulas that would have preserved plurality. Theater ratio is moderate (0.42) — genuine anti-Arian pastoral concern was real at the outset, but a growing share of the doctrine's defense over the medieval period serves institutional authority-maintenance (justifying papal primacy generally) rather than the original pastoral problem, which historians on both sides now regard as resolved.
 *
 * PERSPECTIVAL GAP:
 *   From the Roman/Frankish seat, the constraint is legitimate doctrinal clarification exercising properly held authority — genuine coordination against real heterodoxy. From the Eastern seat, the identical structure is unilateral imposition riding on a claimed authority that was never granted, particularly because the procedural norm requiring ecumenical consent (affirmed at Ephesus and Chalcedon) was itself overridden to make the change stick. The engine should compute divergent seat-level classifications from this asymmetry without either seat's framing controlling the other's.
 *
 * DIRECTIONALITY LOGIC:
 *   Roman papal see and the Frankish monarchy are structural beneficiaries: they gain doctrinal grounds for institutional primacy and political-religious distinctiveness respectively, at low cost to themselves (d near the beneficiary end). Eastern patriarchates and the Byzantine theological tradition are targets: their inherited pneumatological tradition is recast as incomplete, and they bear the schism's institutional and pastoral costs with no comparable recourse (d near the full-target end) — their exit options are trapped/constrained because full communion requires accepting the very authority claim in dispute. Latin theological schools are dual-positioned: they benefit from intellectual authority but are also constrained by having to perpetually defend a contested formula.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding pastoral problem (anti-Arian subordinationism) is corroborated as dead even from within the Roman tradition (Pontifical Council, 1995), yet the doctrinal and institutional structure persists and continues to underwrite present-day papal primacy claims — this is the mandatrophy signature: an arrangement whose stated justification no longer exists but whose institutional consequences remain load-bearing. The tangled_rope classification (rather than a bare snare) is warranted because there was a genuine, historically real coordination function at the outset (589 Toledo pastoral need), distinguishing this from pure extraction dressed as doctrine.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    papal_clarification_authority_scope,
    'Does the Roman see''s authority to ''clarify implicit doctrine'' extend to unilateral creedal amendment, or is creedal text categorically distinct from doctrinal elaboration and thus outside even legitimate clarification authority?',
    'Historical-canonical analysis of whether Ephesus (431) and Chalcedon (451)''s prohibitions on creedal alteration were understood by contemporaries as applying to textual insertion specifically, versus doctrinal development generally; comparison with other cases of accepted doctrinal clarification that did not alter conciliar text.',
    'If creedal text is categorically protected, the filioque reading''s core authority claim is structurally indistinguishable from unauthorized breach, collapsing the coordination framing entirely toward snare; if clarification authority genuinely extends to text, the tangled_rope coordination component is stronger than modeled here.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(papal_clarification_authority_scope, conceptual, 'Whether clarification authority can license textual amendment of an ecumenical creed.').

omega_variable(
    kernel_reading_disagreement_location,
    'Where exactly does the filioque_reading diverge from monoprocession_reading and ecumenical_reunion_reading — is it located in the theological content (does the Son co-cause procession), the procedural authority claim (who may clarify), or both?',
    'Textual comparison of the three readings'' foundational premises against the historical record of what each side actually contested at Photius''s 867 encyclical, Lyon 1274, and Florence 1439 — content disputes and authority disputes were sometimes conflated by participants themselves.',
    'If the disagreement is purely procedural (who decides) rather than substantive (what the Trinity is), the ecumenical_reunion_reading''s economy-based solution becomes structurally available in a way the filioque_reading''s framing forecloses; if substantive, no procedural fix resolves it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_location, conceptual, 'Locating whether the kernel dispute is theological, procedural, or both.').

omega_variable(
    carolingian_political_motive_weight,
    'To what extent was the Frankish adoption of the filioque (809) driven by genuine theological conviction versus instrumental political differentiation from Byzantium?',
    'Comparative analysis of Carolingian court correspondence, the Aachen council acts, and contemporaneous Byzantine-Frankish diplomatic tensions (imperial title disputes) to weigh theological versus political motive.',
    'A predominantly political motive strengthens the extraction/authority-consolidation reading of the constraint''s origin; a predominantly theological motive would support a more genuine coordination-function origin, though it would not resolve the later Roman ratification''s separate authority claim.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(carolingian_political_motive_weight, empirical, 'Weighing political versus theological motive in the clause''s Frankish adoption.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(creed_381_pneumatology__filioque_reading, 589, 1439).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cree_tr_t589, creed_381_pneumatology__filioque_reading, theater_ratio, 589, 0.1).
narrative_ontology:measurement(cree_tr_t809, creed_381_pneumatology__filioque_reading, theater_ratio, 809, 0.2).
narrative_ontology:measurement(cree_tr_t1014, creed_381_pneumatology__filioque_reading, theater_ratio, 1014, 0.32).
narrative_ontology:measurement(cree_tr_t1054, creed_381_pneumatology__filioque_reading, theater_ratio, 1054, 0.4).
narrative_ontology:measurement(cree_tr_t1274, creed_381_pneumatology__filioque_reading, theater_ratio, 1274, 0.48).
narrative_ontology:measurement(cree_tr_t1439, creed_381_pneumatology__filioque_reading, theater_ratio, 1439, 0.42).

% Extraction over time
narrative_ontology:measurement(cree_be_t589, creed_381_pneumatology__filioque_reading, base_extractiveness, 589, 0.18).
narrative_ontology:measurement(cree_be_t809, creed_381_pneumatology__filioque_reading, base_extractiveness, 809, 0.35).
narrative_ontology:measurement(cree_be_t1014, creed_381_pneumatology__filioque_reading, base_extractiveness, 1014, 0.6).
narrative_ontology:measurement(cree_be_t1054, creed_381_pneumatology__filioque_reading, base_extractiveness, 1054, 0.78).
narrative_ontology:measurement(cree_be_t1274, creed_381_pneumatology__filioque_reading, base_extractiveness, 1274, 0.7).
narrative_ontology:measurement(cree_be_t1439, creed_381_pneumatology__filioque_reading, base_extractiveness, 1439, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(cree_su_t589, creed_381_pneumatology__filioque_reading, suppression_requirement, 589, 0.15).
narrative_ontology:measurement(cree_su_t809, creed_381_pneumatology__filioque_reading, suppression_requirement, 809, 0.3).
narrative_ontology:measurement(cree_su_t1014, creed_381_pneumatology__filioque_reading, suppression_requirement, 1014, 0.55).
narrative_ontology:measurement(cree_su_t1054, creed_381_pneumatology__filioque_reading, suppression_requirement, 1054, 0.72).
narrative_ontology:measurement(cree_su_t1274, creed_381_pneumatology__filioque_reading, suppression_requirement, 1274, 0.65).
narrative_ontology:measurement(cree_su_t1439, creed_381_pneumatology__filioque_reading, suppression_requirement, 1439, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(creed_381_pneumatology__filioque_reading, identity_coordination).
narrative_ontology:affects_constraint(creed_381_pneumatology__filioque_reading, monoprocession_reading).
narrative_ontology:affects_constraint(creed_381_pneumatology__filioque_reading, ecumenical_reunion_reading).
narrative_ontology:affects_constraint(creed_381_pneumatology__filioque_reading, papal_primacy_jurisdiction_claim).
narrative_ontology:affects_constraint(creed_381_pneumatology__filioque_reading, great_schism_1054_communion_breach).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the creed_381_pneumatology kernel. filioque_reading claims high ε (0.71) grounded in unilateral authority exercise and Eastern exclusion; monoprocession_reading (not this file) would claim the inverse victim/beneficiary structure with Rome as agenda-setter/violator and Eastern churches as the wronged custodians of the original text; ecumenical_reunion_reading (not this file) would claim substantially lower ε, framing both processional formulas as compatible expressions under bilateral recognition rather than unilateral imposition, closer to a rope or scaffold with sunset toward mutual recognition. All three share the same underlying kernel text (the 381 Niceno-Constantinopolitan Creed) but diverge in the epistemic and authority premises applied to it, producing genuinely different constraints, not one constraint viewed three ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
