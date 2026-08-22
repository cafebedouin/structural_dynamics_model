% ============================================================================
% CONSTRAINT STORY: common_law_precedent_corpus__strict_stare_decisis
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_common_law_precedent_corpus__strict_stare_decisis, []).

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
 *   constraint_id: common_law_precedent_corpus__strict_stare_decisis
 *   human_readable: Strict Stare Decisis: Precedent as Rigid Backward Constraint
 *   domain: legal/jurisprudential
 *
 * SUMMARY:
 *   This story instantiates the strict stare decisis reading of the
 *   common-law precedent kernel: precedent binds as a near-categorical
 *   backward constraint, and departure requires an extraordinary
 *   justification standard that is rarely met. This is deliberately narrower
 *   than the kernel's colloquial label 'precedent doctrine,' which also
 *   covers an evolutionary reading (precedent as adaptive framework
 *   permitting reinterpretation) and a pluralist reading (precedent weight
 *   calibrated case-by-case by domain). Those are separate constraints with
 *   their own ε and stakeholder structures, not measurement variants of this
 *   one. Under the strict reading specifically, the coordination function
 *   (predictability, reliance, equal treatment of like cases) is real, but it
 *   is bundled with an asymmetric extraction: litigants who need doctrinal
 *   correction bear a burden calibrated to be nearly unmeetable, while
 *   institutional and commercial beneficiaries of the status quo pay nothing
 *   to maintain their advantage beyond ordinary litigation.
 *
 * KEY AGENTS:
 *   - appellate_judiciary_institutional_authority: sets and applies the extraordinary-justification standard (institutional/arbitrage)
 *   - settled_commercial_interests: primary beneficiary of doctrinal stability (organized/arbitrage)
 *   - litigants_seeking_novel_relief: bears the extraordinary-justification burden (moderate/constrained)
 *   - historically_marginalized_claimants: bears compounded cost of precedent formed under exclusionary conditions (powerless/trapped)
 *   - legal_academics_and_reform_commissions: analytical observers of doctrinal ossification
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(common_law_precedent_corpus__strict_stare_decisis, 0.52).
domain_priors:suppression_score(common_law_precedent_corpus__strict_stare_decisis, 0.61).
domain_priors:theater_ratio(common_law_precedent_corpus__strict_stare_decisis, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(common_law_precedent_corpus__strict_stare_decisis, extractiveness, 0.52).
narrative_ontology:constraint_metric(common_law_precedent_corpus__strict_stare_decisis, suppression_requirement, 0.61).
narrative_ontology:constraint_metric(common_law_precedent_corpus__strict_stare_decisis, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(common_law_precedent_corpus__strict_stare_decisis, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(common_law_precedent_corpus__strict_stare_decisis, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(common_law_precedent_corpus__strict_stare_decisis, tangled_rope).
narrative_ontology:human_readable(common_law_precedent_corpus__strict_stare_decisis, "Strict Stare Decisis: Precedent as Rigid Backward Constraint").
narrative_ontology:topic_domain(common_law_precedent_corpus__strict_stare_decisis, "legal/jurisprudential").

domain_priors:requires_active_enforcement(common_law_precedent_corpus__strict_stare_decisis).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(common_law_precedent_corpus__strict_stare_decisis, 'e784105b-9769-48fa-9a16-aa8509d47a52').
narrative_ontology:cs_kernel_codification('e784105b-9769-48fa-9a16-aa8509d47a52', distributed).
narrative_ontology:cs_authority_grounding('e784105b-9769-48fa-9a16-aa8509d47a52', practice).
narrative_ontology:cs_interpretation_layer_present('e784105b-9769-48fa-9a16-aa8509d47a52').
narrative_ontology:cs_reading_relation('e784105b-9769-48fa-9a16-aa8509d47a52', common_law_precedent_corpus__evolutionary_framework, coexists_with).
narrative_ontology:cs_reading_relation('e784105b-9769-48fa-9a16-aa8509d47a52', common_law_precedent_corpus__pluralist_balancing, influences).
narrative_ontology:cs_axiom('e784105b-9769-48fa-9a16-aa8509d47a52', foundational, stability_of_settled_law_is_a_paramount_value).
narrative_ontology:cs_axiom_status(stability_of_settled_law_is_a_paramount_value, holdable).
narrative_ontology:cs_axiom_grounding('e784105b-9769-48fa-9a16-aa8509d47a52', stability_of_settled_law_is_a_paramount_value, instrumental).
narrative_ontology:cs_axiom('e784105b-9769-48fa-9a16-aa8509d47a52', foundational, departure_from_precedent_requires_extraordinary_not_ordinary_justification).
narrative_ontology:cs_axiom_status(departure_from_precedent_requires_extraordinary_not_ordinary_justification, holdable).
narrative_ontology:cs_axiom_grounding('e784105b-9769-48fa-9a16-aa8509d47a52', departure_from_precedent_requires_extraordinary_not_ordinary_justification, conventional).
narrative_ontology:cs_reference_frame('e784105b-9769-48fa-9a16-aa8509d47a52', classical_common_law_binding_precedent).
narrative_ontology:cs_drift_state('e784105b-9769-48fa-9a16-aa8509d47a52', contemporary_doctrinal_pluralism_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e784105b-9769-48fa-9a16-aa8509d47a52', '').
narrative_ontology:cs_kernel_id(common_law_precedent_corpus__strict_stare_decisis, common_law_precedent_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__strict_stare_decisis, settled_commercial_interests).
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__strict_stare_decisis, appellate_judiciary_institutional_authority).
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__strict_stare_decisis, incumbent_legal_doctrine_holders).
narrative_ontology:constraint_victim(common_law_precedent_corpus__strict_stare_decisis, litigants_seeking_novel_relief).
narrative_ontology:constraint_victim(common_law_precedent_corpus__strict_stare_decisis, historically_marginalized_claimants).
narrative_ontology:constraint_victim(common_law_precedent_corpus__strict_stare_decisis, lower_court_judges_bound_by_stale_holdings).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the doctrine of when precedent may be overruled, articulates the extraordinary-justification standard, and applies it case by case. Its own institutional legitimacy is partly built on the appearance of doctrinal stability and continuity, which the strict rule protects; it can revise the overruling standard itself when convenient, an option unavailable to litigants.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__strict_stare_decisis, appellate_judiciary_institutional_authority, agenda_setter,
    institutional, generational, arbitrage, national).

% Have structured contracts, transactions, and business models around existing holdings. The strict binding rule protects reliance investments already made; they can lobby to preserve favorable precedent and litigate strategically to entrench it further, and can often route around unfavorable law through contract or jurisdiction choice.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__strict_stare_decisis, settled_commercial_interests, beneficiary,
    organized, generational, arbitrage, national).

% Legal scholars, senior practitioners, and institutions whose professional standing and case law investments (briefs, treatises, litigation strategy built on existing doctrine) are protected by the high bar against overruling. They benefit from precedent's binding force independent of whether the precedent was correctly decided.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__strict_stare_decisis, incumbent_legal_doctrine_holders, beneficiary,
    powerful, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(common_law_precedent_corpus__strict_stare_decisis, incumbent_legal_doctrine_holders, agenda_setter).

% Bring claims that require departing from binding precedent to succeed. Must meet an extraordinarily high threshold to have the precedent revisited at all, often facing dismissal on stare decisis grounds before the merits of changed circumstances are even weighed. Cannot exit the jurisdiction without abandoning the claim entirely.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__strict_stare_decisis, litigants_seeking_novel_relief, payer,
    moderate, biographical, constrained, national).

% Bear the compounded cost when precedent was decided under conditions of exclusion (denial of standing, unequal representation, or outdated social assumptions) and the strict doctrine treats the resulting holding as entitled to the same deference as any other. Have no meaningful exit — the binding precedent applies regardless of the circumstances of its origin, and challenging it requires resources and standing they typically lack.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__strict_stare_decisis, historically_marginalized_claimants, payer,
    powerless, generational, trapped, national).

% Apply precedent they may recognize as poorly reasoned or outdated but lack authority to depart from; can only signal disagreement in dicta or invite appeal, and bear reputational and workload costs when their rulings are read as constrained rather than reasoned. Exit means reversal risk or professional friction, not genuine discretion.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__strict_stare_decisis, lower_court_judges_bound_by_stale_holdings, payer,
    moderate, biographical, constrained, regional).

% Not yet parties to any case, but bound in advance by whatever the corpus of precedent becomes; have no voice in whether precedent from decades or centuries ago remains binding on circumstances no framer of the holding anticipated. Their interests enter only through the advocacy of present parties, if at all.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__strict_stare_decisis, future_litigants_and_publics, excluded,
    powerless, generational, trapped, national).

% Study patterns of precedent adherence and departure, produce critical scholarship on doctrinal ossification, and occasionally influence law reform commissions, but hold no binding authority over the doctrine itself.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__strict_stare_decisis, legal_academics_and_reform_commissions, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(common_law_precedent_corpus__strict_stare_decisis, diffuse).
narrative_ontology:fixing_cost_class(common_law_precedent_corpus__strict_stare_decisis, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides predictability and reliance stability: parties can plan transactions, contracts, and conduct around known legal rules without re-litigating settled questions in every case, and courts can resolve disputes efficiently by applying established rules rather than reasoning each case from first principles.
% TRANSFER_FUNCTION: Moves the burden of legal change from the institution that could revise doctrine cheaply (the judiciary, which merely announces a new rule) onto individual litigants, who must mount extraordinary showings, absorb the cost of failed novel claims, and in the marginalized-claimant case, live under holdings shaped by conditions of exclusion never revisited.
% ABSENT_VOICES: Future litigants and the broader public bound by precedent-in-formation have no seat: a holding issued today binds parties not yet born to the dispute. Historically marginalized claimants whose exclusion shaped the original precedent are structurally underrepresented in the doctrine's own self-correction mechanism, since revisiting requires exactly the standing and resources the precedent's history often denied them.
% DISAPPEARANCE_RATIONALE: If strict stare decisis vanished overnight and courts treated precedent as merely persuasive, settled commercial and institutional reliance would face sudden uncertainty, but litigants with novel claims and marginalized claimants challenging outdated holdings would gain vastly wider pathways to relief; doctrine would become more volatile and case-by-case reasoning would displace categorical deference to prior holdings.
% FOUNDING_PROBLEM: Courts needed a mechanism to prevent arbitrary, ad hoc, or politically driven relitigation of settled legal questions, ensuring like cases are treated alike and that private and public actors can rely on announced legal rules when planning conduct.
% FOUNDING_PROBLEM_CORROBORATION: Judges and commercial interests attest the reliance-and-predictability problem remains fully live. Legal academics studying doctrinal ossification and reform commissions examining wrongly-decided precedent (attesting from outside the beneficiary set) report that in significant domains the binding-force doctrine now serves institutional self-protection and workload management more than genuine reliance interests, particularly where the original holding rested on since-repudiated factual or normative premises.
narrative_ontology:disappearance_verdict(common_law_precedent_corpus__strict_stare_decisis, world_rearranges).
narrative_ontology:founding_problem_status(common_law_precedent_corpus__strict_stare_decisis, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(common_law_precedent_corpus__strict_stare_decisis, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(common_law_precedent_corpus__strict_stare_decisis, 'none', 1).
narrative_ontology:epsilon_provenance(common_law_precedent_corpus__strict_stare_decisis, 0.52, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(common_law_precedent_corpus__strict_stare_decisis_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(common_law_precedent_corpus__strict_stare_decisis, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(common_law_precedent_corpus__strict_stare_decisis_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52) reflects that the strict reading imposes real, asymmetric costs on parties seeking doctrinal correction, but the coordination function is genuinely substantial (predictability has real social value), so ε sits at moderate rather than high. Suppression (0.61) is higher than extraction because the binding force is actively enforced through appellate reversal risk and the extraordinary-justification threshold itself, which is a structural barrier independent of any individual case's merits. Accessibility collapse is high (0.70) — once a precedent is established, the practical paths to displacing it narrow sharply, particularly for resource-poor litigants. Resistance is moderate (0.55): legal academics, reform commissions, and dissenting judges actively contest the doctrine's rigidity, but resistance rarely succeeds against the standard itself. All temporal metrics share one grid (0, 8, 16, 24, 32, 40) — extraction and suppression both drift upward, consistent with the accumulation of case law that raises the practical bar for overruling as more precedent piles onto precedent (a structural ratchet, not merely a policy choice).
 *
 * PERSPECTIVAL GAP:
 *   From the institutional judiciary's seat, the strict rule is disciplined coordination — a defense against arbitrary judicial legislation. From the seat of litigants seeking novel relief, particularly historically marginalized claimants, the same rule operates as an enforced wall that treats the accident of when and by whom a question was first litigated as dispositive for all who come after. The engine's per-seat computation should register this divergence structurally, not because either seat is wrong about its own experience.
 *
 * DIRECTIONALITY LOGIC:
 *   Settled commercial interests and incumbent doctrine holders sit near the beneficiary end: they profit from the binding force being strong precisely because it protects investments already made under existing rules, and they hold arbitrage-grade exit (they can lobby, forum-shop, or contract around adverse doctrine). Litigants seeking novel relief and marginalized claimants sit near the target end: they need the rule not to bind, and the extraordinary-justification standard is calibrated to make that difficult regardless of the merits of their claim. Lower court judges are structurally payers of a different kind — bound by holdings they cannot revise, they absorb reputational and workload cost without the institutional latitude the apex court retains.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing arbitrary relitigation, protecting reliance) remains partially live — the coordination function has not evaporated. But the founding_problem_status is authored as contested rather than dead specifically because reform commissions and academic corroboration (from outside the beneficiary set) indicate the doctrine now also serves institutional self-protection independent of any live reliance interest, especially for precedents formed under conditions the legal system itself has since repudiated. Classifying this as tangled_rope rather than snare or mountain captures that duality: the coordination function is not fictional, but neither is the asymmetric extraction cover story.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    strict_reading_vs_sibling_readings_disagreement_locus,
    'Where exactly does the strict_stare_decisis reading diverge from the evolutionary_framework and pluralist_balancing readings of the same precedent kernel — is it the threshold for overruling, the presumption of stability itself, or the unit of analysis (categorical rule vs. domain-by-domain weighting)?',
    'Compare the three constraint stories'' beneficiary/victim structures and extraction profiles directly: if the evolutionary reading shows substantially lower extractiveness because it treats the overruling threshold as ordinary rather than extraordinary, the disagreement is located in the threshold definition itself, not merely in outcome frequency.',
    'If the disagreement is purely about threshold calibration, the three readings may be closer structurally than they appear and could in principle converge under empirical study of overruling rates; if the disagreement is about the underlying commitment to categorical rules versus contextual balancing, the readings are foreclosing rather than merely differing in degree.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(strict_reading_vs_sibling_readings_disagreement_locus, conceptual, 'Locating the precise structural disagreement between the three kernel readings of precedent doctrine.').

omega_variable(
    reliance_interest_versus_institutional_self_protection,
    'In any given line of precedent, how much of the observed resistance to overruling reflects genuine third-party reliance interests versus institutional self-protection (workload management, appearance of stability, professional investment in existing doctrine)?',
    'Empirical study comparing overruling outcomes in domains with strong demonstrable reliance interests (contract, property) against domains with weak or contested reliance interests (constitutional interpretation, tort standards) to see whether the extraordinary-justification bar is applied uniformly or is calibrated to actual reliance magnitude.',
    'If the bar is uniform regardless of demonstrated reliance, this supports the tangled_rope classification''s premise that a coordination story covers extraction beyond what the coordination function requires; if the bar tracks reliance magnitude closely, the classification would move closer to a genuine rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reliance_interest_versus_institutional_self_protection, empirical, 'Disentangling genuine reliance protection from institutional self-protection within the extraordinary-justification standard.').

omega_variable(
    precedent_formed_under_exclusion_ambiguity,
    'Should precedent formed under conditions of demonstrable exclusion (denial of standing to affected groups, absence of relevant expert testimony now available) receive the same strict deference as precedent formed under full and fair adversarial testing?',
    'Track whether courts applying strict stare decisis create or refuse to create a distinct, lower-deference category for precedent with documented exclusionary origins, versus treating all precedent identically regardless of formation conditions.',
    'If courts refuse to distinguish, the strict reading''s cost to historically marginalized claimants is a structural feature, not an incidental side effect, strengthening the tangled_rope classification over a pure rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(precedent_formed_under_exclusion_ambiguity, preference, 'Whether formation conditions should modulate the strength of binding force under the strict reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(common_law_precedent_corpus__strict_stare_decisis, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t0, common_law_precedent_corpus__strict_stare_decisis, theater_ratio, 0, 0.12).
narrative_ontology:measurement(comm_tr_t8, common_law_precedent_corpus__strict_stare_decisis, theater_ratio, 8, 0.16).
narrative_ontology:measurement(comm_tr_t16, common_law_precedent_corpus__strict_stare_decisis, theater_ratio, 16, 0.19).
narrative_ontology:measurement(comm_tr_t24, common_law_precedent_corpus__strict_stare_decisis, theater_ratio, 24, 0.22).
narrative_ontology:measurement(comm_tr_t32, common_law_precedent_corpus__strict_stare_decisis, theater_ratio, 32, 0.25).
narrative_ontology:measurement(comm_tr_t40, common_law_precedent_corpus__strict_stare_decisis, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(comm_be_t0, common_law_precedent_corpus__strict_stare_decisis, base_extractiveness, 0, 0.34).
narrative_ontology:measurement(comm_be_t8, common_law_precedent_corpus__strict_stare_decisis, base_extractiveness, 8, 0.39).
narrative_ontology:measurement(comm_be_t16, common_law_precedent_corpus__strict_stare_decisis, base_extractiveness, 16, 0.44).
narrative_ontology:measurement(comm_be_t24, common_law_precedent_corpus__strict_stare_decisis, base_extractiveness, 24, 0.47).
narrative_ontology:measurement(comm_be_t32, common_law_precedent_corpus__strict_stare_decisis, base_extractiveness, 32, 0.5).
narrative_ontology:measurement(comm_be_t40, common_law_precedent_corpus__strict_stare_decisis, base_extractiveness, 40, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t0, common_law_precedent_corpus__strict_stare_decisis, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(comm_su_t8, common_law_precedent_corpus__strict_stare_decisis, suppression_requirement, 8, 0.5).
narrative_ontology:measurement(comm_su_t16, common_law_precedent_corpus__strict_stare_decisis, suppression_requirement, 16, 0.53).
narrative_ontology:measurement(comm_su_t24, common_law_precedent_corpus__strict_stare_decisis, suppression_requirement, 24, 0.57).
narrative_ontology:measurement(comm_su_t32, common_law_precedent_corpus__strict_stare_decisis, suppression_requirement, 32, 0.59).
narrative_ontology:measurement(comm_su_t40, common_law_precedent_corpus__strict_stare_decisis, suppression_requirement, 40, 0.61).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(common_law_precedent_corpus__strict_stare_decisis, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(common_law_precedent_corpus__strict_stare_decisis, 0.1).
narrative_ontology:affects_constraint(common_law_precedent_corpus__strict_stare_decisis, common_law_precedent_corpus__evolutionary_framework).
narrative_ontology:affects_constraint(common_law_precedent_corpus__strict_stare_decisis, common_law_precedent_corpus__pluralist_balancing).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the common_law_precedent_corpus kernel, decomposed per the ε-invariance principle: strict_stare_decisis (this story, high rigidity, tangled_rope), evolutionary_framework (adaptive reinterpretation, expected lower extraction), and pluralist_balancing (domain-calibrated weighting, expected intermediate extraction). Each carries its own ε and classification; none is a measurement variant of another.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
