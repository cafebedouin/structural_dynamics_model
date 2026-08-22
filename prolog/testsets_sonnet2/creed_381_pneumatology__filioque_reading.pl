% ============================================================================
% CONSTRAINT STORY: creed_381_pneumatology__filioque_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: creed_381_pneumatology__filioque_reading
 *   human_readable: Filioque Reading: Papal/Conciliar Authority to Clarify Trinitarian Procession
 *   domain: historical_theology/ecclesiastical_authority
 *
 * SUMMARY:
 *   This story instantiates the Filioque reading of the contested creed-381
 *   pneumatology kernel: the Spirit proceeds from Father and Son, and the
 *   papal/conciliar magisterium possesses standing authority to clarify
 *   implicit Trinitarian doctrine without requiring renewed ecumenical
 *   consent from the Eastern sees. The clause originated regionally (Third
 *   Council of Toledo, 589) as an anti-Arian affirmation of the Son's full
 *   divinity, was absorbed into Frankish/Carolingian liturgical practice, and
 *   was eventually adopted into the Roman rite itself (definitively under
 *   Benedict VIII, c. 1014), over sustained Eastern protest (Photius, 867;
 *   and continuing through 1054). This is ONE of three linked readings of the
 *   same kernel — the monoprocession_reading and ecumenical_reunion_reading
 *   are separate constraint stories with their own ε values and structural
 *   data, not alternative measurements of this one.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(creed_381_pneumatology__filioque_reading, 0.72).
domain_priors:suppression_score(creed_381_pneumatology__filioque_reading, 0.68).
domain_priors:theater_ratio(creed_381_pneumatology__filioque_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(creed_381_pneumatology__filioque_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(creed_381_pneumatology__filioque_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(creed_381_pneumatology__filioque_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(creed_381_pneumatology__filioque_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(creed_381_pneumatology__filioque_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(creed_381_pneumatology__filioque_reading, tangled_rope).
narrative_ontology:human_readable(creed_381_pneumatology__filioque_reading, "Filioque Reading: Papal/Conciliar Authority to Clarify Trinitarian Procession").
narrative_ontology:topic_domain(creed_381_pneumatology__filioque_reading, "historical_theology/ecclesiastical_authority").

domain_priors:requires_active_enforcement(creed_381_pneumatology__filioque_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(creed_381_pneumatology__filioque_reading, '77512abe-bcda-4758-ad14-5baacd5f1e66').
narrative_ontology:cs_kernel_codification('77512abe-bcda-4758-ad14-5baacd5f1e66', fixed_text).
narrative_ontology:cs_authority_grounding('77512abe-bcda-4758-ad14-5baacd5f1e66', extraction).
narrative_ontology:cs_interpretation_layer_present('77512abe-bcda-4758-ad14-5baacd5f1e66').
narrative_ontology:cs_reading_relation('77512abe-bcda-4758-ad14-5baacd5f1e66', creed_381_pneumatology__monoprocession_reading, forecloses).
narrative_ontology:cs_reading_relation('77512abe-bcda-4758-ad14-5baacd5f1e66', creed_381_pneumatology__ecumenical_reunion_reading, influences).
narrative_ontology:cs_axiom('77512abe-bcda-4758-ad14-5baacd5f1e66', foundational, magisterium_may_clarify_implicit_doctrine_unilaterally).
narrative_ontology:cs_axiom_status(magisterium_may_clarify_implicit_doctrine_unilaterally, holdable).
narrative_ontology:cs_axiom_grounding('77512abe-bcda-4758-ad14-5baacd5f1e66', magisterium_may_clarify_implicit_doctrine_unilaterally, conventional).
narrative_ontology:cs_axiom('77512abe-bcda-4758-ad14-5baacd5f1e66', secondary, double_procession_necessary_for_full_trinitarian_coequality).
narrative_ontology:cs_axiom_status(double_procession_necessary_for_full_trinitarian_coequality, holdable).
narrative_ontology:cs_axiom_grounding('77512abe-bcda-4758-ad14-5baacd5f1e66', double_procession_necessary_for_full_trinitarian_coequality, deontological).
narrative_ontology:cs_reference_frame('77512abe-bcda-4758-ad14-5baacd5f1e66', petrine_magisterial_clarification_authority).
narrative_ontology:cs_drift_state('77512abe-bcda-4758-ad14-5baacd5f1e66', great_schism_1054, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('77512abe-bcda-4758-ad14-5baacd5f1e66', '').
narrative_ontology:cs_kernel_id(creed_381_pneumatology__filioque_reading, creed_381_pneumatology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(creed_381_pneumatology__filioque_reading, papal_see).
narrative_ontology:constraint_beneficiary(creed_381_pneumatology__filioque_reading, latin_theological_schools).
narrative_ontology:constraint_victim(creed_381_pneumatology__filioque_reading, eastern_patriarchates).
narrative_ontology:constraint_victim(creed_381_pneumatology__filioque_reading, byzantine_theological_tradition).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(creed_381_pneumatology__filioque_reading, western_laity).
narrative_ontology:constraint_victim(creed_381_pneumatology__filioque_reading, western_laity).
narrative_ontology:constraint_vindicates(creed_381_pneumatology__filioque_reading, papal_doctrinal_clarification_authority).
narrative_ontology:constraint_vindicates(creed_381_pneumatology__filioque_reading, double_procession_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Asserts authority to clarify implicit Trinitarian doctrine through the magisterium, incorporating the Filioque clause into the Latin liturgical creed without convening an ecumenical council with Eastern participation. Collects enhanced doctrinal authority and consolidates Latin ecclesial unity under Roman primacy; administers the enforcement of the amended creed's use in Western liturgy and canon law.
narrative_ontology:constraint_stakeholder(creed_381_pneumatology__filioque_reading, papal_see, agenda_setter,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(creed_381_pneumatology__filioque_reading, papal_see, beneficiary).

% Carolingian and later scholastic theologians developed and championed the double-procession formula as doctrinally necessary and philosophically coherent. They benefit from having their theological framework enshrined as normative Western orthodoxy, gaining intellectual and institutional prestige tied to defending the clause.
narrative_ontology:constraint_stakeholder(creed_381_pneumatology__filioque_reading, latin_theological_schools, beneficiary,
    organized, generational, mobile, continental).

% Constantinople and the other ancient patriarchates hold that the 381 creed is fixed by ecumenical consent and that unilateral Western addition without their agreement is a breach of conciliar order. From this reading's own vantage, they are treated as regional sees whose objection does not bind the magisterium's clarifying authority; their theological autonomy is structurally overridden by the claim that Rome may resolve implicit doctrine unilaterally. Exit means schism, which they eventually take (1054), at the cost of permanent ecclesial rupture.
narrative_ontology:constraint_stakeholder(creed_381_pneumatology__filioque_reading, eastern_patriarchates, payer,
    powerful, civilizational, trapped, continental).

% The Cappadocian-rooted apophatic tradition treats the Father as sole source (monarchia) within the Trinity; the Filioque reading's assertion of magisterial clarification authority renders this entire theological grammar a regional deviation to be corrected rather than a coequal patristic inheritance. Their tradition bears the cost of being reclassified from co-authoritative to subordinate.
narrative_ontology:constraint_stakeholder(creed_381_pneumatology__filioque_reading, byzantine_theological_tradition, payer,
    organized, civilizational, trapped, continental).

% Receive a settled, doctrinally unified liturgical formula and clear catechesis without needing to adjudicate a technical procession dispute themselves. They also inherit the schism's downstream costs — loss of communion with Eastern Christendom, centuries of mutual excommunication — without having chosen the clause's insertion.
narrative_ontology:constraint_stakeholder(creed_381_pneumatology__filioque_reading, western_laity, beneficiary,
    moderate, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(creed_381_pneumatology__filioque_reading, western_laity, payer).

% Genuinely ecumenical councils with full Eastern participation, of the kind the monoprocession reading insists is required to amend a creed of ecumenical status, were never convened to ratify the Filioque before its insertion became fixed Western practice. Their absence from the process is precisely what the sibling reading treats as the breach; this reading's own account does not require their presence.
narrative_ontology:constraint_stakeholder(creed_381_pneumatology__filioque_reading, later_ecumenical_councils, excluded,
    institutional, civilizational, analytical, universal).

% Contemporary theologians and dialogue commissions (e.g., North American Orthodox-Catholic consultations) examine whether the Filioque and monoprocession are compatible theological idioms or genuinely contradictory claims about the Trinity's internal life, without institutional power to bind either communion.
narrative_ontology:constraint_stakeholder(creed_381_pneumatology__filioque_reading, modern_ecumenists, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(creed_381_pneumatology__filioque_reading, papal_see).
narrative_ontology:fixing_cost_class(creed_381_pneumatology__filioque_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides doctrinal closure on an ambiguous patristic question (how precisely the Spirit's procession relates to the Son) so that Western liturgy, catechesis, and canon law can proceed from a single settled formula rather than perpetual theological indeterminacy.
% TRANSFER_FUNCTION: Moves interpretive authority over implicit Trinitarian doctrine from conciliar consensus requiring Eastern participation to the Roman magisterium acting with Frankish/Carolingian theological backing; moves theological legitimacy from the Constantinopolitan and Alexandrian patristic schools toward the Augustinian-Latin school as normative.
% ABSENT_VOICES: The Ecumenical Patriarchate of Constantinople and the other Eastern patriarchates were not signatories to the Filioque's insertion into the Latin creed and repeatedly protested through legates and synodal letters (notably Photius, 867) before and after the insertion became fixed; their objections are treated by this reading as non-binding regional dissent rather than a veto a truly ecumenical amendment would require.
% DISAPPEARANCE_RATIONALE: If the magisterium's claimed authority to unilaterally clarify the creed were withdrawn and the Filioque reading abandoned, the entire doctrinal basis asserted for the 1054 schism's Western position would dissolve, canon law provisions built on papal doctrinal supremacy would require re-grounding, and centuries of Catholic-Orthodox mutual anathema would lose their proximate theological justification (though not necessarily their political and ecclesiological ones).
% FOUNDING_PROBLEM: The Latin church faced Arian and quasi-Arian currents (notably among Visigothic converts) that could be read as subordinating the Son within the Trinity; asserting the Spirit's procession from both Father and Son was originally deployed as an anti-Arian safeguard affirming the Son's full divinity, later generalized into a claim about the magisterium's standing authority to resolve any implicit doctrinal question.
% FOUNDING_PROBLEM_CORROBORATION: The papal see and Latin theological schools (the benefiting parties) attest the clarifying authority remains necessary and doctrinally settled. Outside that circle, Eastern patriarchal sources (Photius's encyclical, subsequent Orthodox synodal statements) and modern historical-critical scholarship (including some Catholic patristic historians) attest that the anti-Arian problem the formula was built to solve was substantially resolved by the Second Council of Toledo's era, and that the magisterium's subsequent generalized claim to unilateral doctrinal clarification outlived the specific crisis and became a freestanding assertion of jurisdictional primacy.
narrative_ontology:disappearance_verdict(creed_381_pneumatology__filioque_reading, world_rearranges).
narrative_ontology:founding_problem_status(creed_381_pneumatology__filioque_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(creed_381_pneumatology__filioque_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(creed_381_pneumatology__filioque_reading, 'none', 1).
narrative_ontology:epsilon_provenance(creed_381_pneumatology__filioque_reading, 0.72, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness rises from 0.25 at the clause's regional anti-Arian origin to 0.72 by 1054 because the coordination rationale (settling an ambiguous doctrinal point to preserve unity) increasingly diverges from what the structure actually does — consolidate Roman jurisdictional primacy at the direct expense of Eastern conciliar standing. Suppression climbs in step (0.20 to 0.68) as the clause moves from optional regional usage to fixed universal Latin liturgical mandate, actively enforced against dissenting sees. Theater ratio grows moderately (0.10 to 0.40): a real anti-Arian theological function persists early, but a growing share of later defense is jurisdictional performance — asserting the authority to clarify matters more than resolving genuine ambiguity.
 *
 * DIRECTIONALITY LOGIC:
 *   The papal see is the clear structural beneficiary: it collects expanded doctrinal authority and consolidated Latin unity, and it administers the enforcement (liturgical mandate, canon law). Latin theological schools benefit secondarily through prestige attached to the Augustinian procession framework. Eastern patriarchates and the Byzantine theological tradition are structural targets: their conciliar veto is overridden, their patristic framework is recategorized as deviant, and their exit option is effectively schism — which is what materializes. Western laity sit ambivalently: real coordination benefit (doctrinal clarity) alongside inherited cost (permanent rupture with Eastern Christendom) they did not choose.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification prevents this reading from being mislabeled as either pure Mountain (natural, uncontested doctrine) or pure Snare (naked power grab with no coordination content). There IS a genuine coordination function — the anti-Arian clarification was a real theological problem in 6th-century Spain — but by the time of universal Roman adoption four centuries later, the founding problem was largely resolved and the arrangement had shifted to justifying centralized magisterial authority as a freestanding claim. That shift from live coordination to inertial jurisdictional assertion, while still requiring active enforcement against a powerful, organized dissenting party (the Eastern patriarchates), is exactly the tangled_rope signature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    anti_arian_necessity_vs_jurisdictional_generalization,
    'Was the Filioque''s later universal, unilaterally-enforced status a necessary extension of its original anti-Arian doctrinal function, or a separable jurisdictional claim (papal clarifying authority) that outlived and exceeded the problem that justified the clause''s introduction?',
    'Textual-historical analysis of whether Latin theologians and popes, from the 7th through 11th centuries, continued to justify the clause primarily on anti-Arian grounds or increasingly on magisterial-authority grounds independent of any live Arian threat.',
    'If jurisdictional generalization dominates from an early point, the coordination function is largely a cover story and the reading tilts toward snare; if the anti-Arian rationale remained substantively operative throughout, the tangled_rope coordination component is more robust.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(anti_arian_necessity_vs_jurisdictional_generalization, empirical, 'Whether the clause''s persistence tracks a live doctrinal threat or a generalized authority claim.').

omega_variable(
    kernel_reading_indeterminacy,
    'Is the 381 creed''s pneumatological clause genuinely silent/ambiguous on the Filioque question (permitting magisterial clarification as this reading holds), or does the creed''s own conciliar history (the addition being explicitly rejected at Ephesus 431 and Chalcedon 451, which forbade any alteration) already settle the question against unilateral amendment?',
    'Examination of the canons of Ephesus and Chalcedon explicitly prohibiting creedal alteration, weighed against the Latin tradition''s claim that clarifying implicit content is not the same as altering the creed''s substance.',
    'If the conciliar prohibition is read as covering substantive clarification, this reading''s core premise (that magisterial clarification is licit without ecumenical consent) is significantly weakened, strengthening the monoprocession_reading''s claim to being the kernel''s default state rather than one contested reading among equals.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_indeterminacy, conceptual, 'Whether the creed''s own conciliar rules already foreclose unilateral clarification, independent of the theological merits of double procession.').

omega_variable(
    papal_authority_beneficiary_or_natural_development,
    'Is centralized papal doctrinal authority a contingent, constructed historical development that this reading''s beneficiaries (the papal see, Latin schools) have an interest in naturalizing, or a legitimate development inherent in the apostolic See''s original commission?',
    'Comparative institutional history of how papal primacy claims expanded specifically around contested doctrinal moments (Filioque, later infallibility) versus how the Petrine commission was understood in the patristic period before these expansions.',
    'If primarily contingent and self-interested, the tangled_rope''s extraction component is stronger than its coordination component; if the primacy claim has independent grounding, the coordination story carries more weight on its own terms.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(papal_authority_beneficiary_or_natural_development, preference, 'Whether papal doctrinal authority is naturalized self-interest or independently grounded development.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(creed_381_pneumatology__filioque_reading, 589, 1054).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cree_tr_t589, creed_381_pneumatology__filioque_reading, theater_ratio, 589, 0.1).
narrative_ontology:measurement_basis(cree_tr_t589, observed).
narrative_ontology:measurement(cree_tr_t700, creed_381_pneumatology__filioque_reading, theater_ratio, 700, 0.15).
narrative_ontology:measurement_basis(cree_tr_t700, observed).
narrative_ontology:measurement(cree_tr_t800, creed_381_pneumatology__filioque_reading, theater_ratio, 800, 0.22).
narrative_ontology:measurement_basis(cree_tr_t800, observed).
narrative_ontology:measurement(cree_tr_t867, creed_381_pneumatology__filioque_reading, theater_ratio, 867, 0.3).
narrative_ontology:measurement_basis(cree_tr_t867, observed).
narrative_ontology:measurement(cree_tr_t950, creed_381_pneumatology__filioque_reading, theater_ratio, 950, 0.35).
narrative_ontology:measurement_basis(cree_tr_t950, observed).
narrative_ontology:measurement(cree_tr_t1014, creed_381_pneumatology__filioque_reading, theater_ratio, 1014, 0.38).
narrative_ontology:measurement_basis(cree_tr_t1014, observed).
narrative_ontology:measurement(cree_tr_t1054, creed_381_pneumatology__filioque_reading, theater_ratio, 1054, 0.4).
narrative_ontology:measurement_basis(cree_tr_t1054, observed).

% Extraction over time
narrative_ontology:measurement(cree_be_t589, creed_381_pneumatology__filioque_reading, base_extractiveness, 589, 0.25).
narrative_ontology:measurement_basis(cree_be_t589, observed).
narrative_ontology:measurement(cree_be_t700, creed_381_pneumatology__filioque_reading, base_extractiveness, 700, 0.35).
narrative_ontology:measurement_basis(cree_be_t700, observed).
narrative_ontology:measurement(cree_be_t800, creed_381_pneumatology__filioque_reading, base_extractiveness, 800, 0.48).
narrative_ontology:measurement_basis(cree_be_t800, observed).
narrative_ontology:measurement(cree_be_t867, creed_381_pneumatology__filioque_reading, base_extractiveness, 867, 0.58).
narrative_ontology:measurement_basis(cree_be_t867, observed).
narrative_ontology:measurement(cree_be_t950, creed_381_pneumatology__filioque_reading, base_extractiveness, 950, 0.65).
narrative_ontology:measurement_basis(cree_be_t950, observed).
narrative_ontology:measurement(cree_be_t1014, creed_381_pneumatology__filioque_reading, base_extractiveness, 1014, 0.7).
narrative_ontology:measurement_basis(cree_be_t1014, observed).
narrative_ontology:measurement(cree_be_t1054, creed_381_pneumatology__filioque_reading, base_extractiveness, 1054, 0.72).
narrative_ontology:measurement_basis(cree_be_t1054, observed).

% Suppression requirement over time
narrative_ontology:measurement(cree_su_t589, creed_381_pneumatology__filioque_reading, suppression_requirement, 589, 0.2).
narrative_ontology:measurement_basis(cree_su_t589, observed).
narrative_ontology:measurement(cree_su_t700, creed_381_pneumatology__filioque_reading, suppression_requirement, 700, 0.3).
narrative_ontology:measurement_basis(cree_su_t700, observed).
narrative_ontology:measurement(cree_su_t800, creed_381_pneumatology__filioque_reading, suppression_requirement, 800, 0.42).
narrative_ontology:measurement_basis(cree_su_t800, observed).
narrative_ontology:measurement(cree_su_t867, creed_381_pneumatology__filioque_reading, suppression_requirement, 867, 0.55).
narrative_ontology:measurement_basis(cree_su_t867, observed).
narrative_ontology:measurement(cree_su_t950, creed_381_pneumatology__filioque_reading, suppression_requirement, 950, 0.6).
narrative_ontology:measurement_basis(cree_su_t950, observed).
narrative_ontology:measurement(cree_su_t1014, creed_381_pneumatology__filioque_reading, suppression_requirement, 1014, 0.65).
narrative_ontology:measurement_basis(cree_su_t1014, observed).
narrative_ontology:measurement(cree_su_t1054, creed_381_pneumatology__filioque_reading, suppression_requirement, 1054, 0.68).
narrative_ontology:measurement_basis(cree_su_t1054, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(creed_381_pneumatology__filioque_reading, identity_coordination).
narrative_ontology:affects_constraint(creed_381_pneumatology__filioque_reading, creed_381_pneumatology__monoprocession_reading).
narrative_ontology:affects_constraint(creed_381_pneumatology__filioque_reading, creed_381_pneumatology__ecumenical_reunion_reading).

% DUAL FORMULATION NOTE:
% This is one of three linked readings of the creed_381_pneumatology kernel. monoprocession_reading holds the 381 creed inviolable absent ecumenical consent and treats the Filioque's insertion as a unilateral breach (high ε for the Latin imposition, victim set centered on Eastern autonomy from the opposite direction). ecumenical_reunion_reading treats both formulas as compatible regional expressions within one communion, with much lower ε since it authors bilateral recognition rather than either side's unilateral imposition as the standing arrangement. Each story authors its own ε, beneficiary/victim structure, and classification per the ε-invariance principle; none averages or hedges across the readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
