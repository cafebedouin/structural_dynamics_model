% ============================================================================
% CONSTRAINT STORY: separation_of_powers_text__formalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_separation_of_powers_text__formalist_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: separation_of_powers_text__formalist_reading
 *   human_readable: Formalist (Impermeable-Boundary) Reading of the Nondelegation Doctrine
 *   domain: constitutional_law/administrative_law
 *
 * SUMMARY:
 *   This constraint is the formalist reading of the separation-of-powers
 *   kernel: it holds that Article I vests 'all legislative powers'
 *   exclusively in Congress, that this vesting is categorical rather than
 *   functional, and that Congress therefore cannot transfer lawmaking
 *   authority to administrative agencies beyond narrow, tightly-cabined
 *   delegations bearing an 'intelligible principle' construed strictly. Under
 *   this reading, the modern administrative state's broad delegations to
 *   agencies (environmental standard-setting, financial rulemaking, health
 *   and safety regulation) are constitutionally suspect or invalid. The story
 *   authors this reading's own ε: the standing arrangement under contest is
 *   the current broad-delegation administrative state, and this reading
 *   regards that arrangement as extractive of constitutional structure and as
 *   functioning to let Congress evade accountability by outsourcing hard
 *   choices. The referent is NOT the reading's preferred alternative (a
 *   Congress that legislates with exhaustive specificity) — ε describes how
 *   this reading assesses the delegation arrangement as it stands.
 *
 * KEY AGENTS:
 *   - formalist_judiciary_faction: agenda-setter who administers the doctrine case by case
 *   - deregulatory_litigants: beneficiaries who invoke the doctrine to invalidate costly rules
 *   - administrative_agencies: primary payers whose rulemaking capacity is narrowed
 *   - regulatory_beneficiary_public: diffuse payers who lose regulatory protection
 *   - functionalist_judiciary_faction: excluded competing reading within the same kernel
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(separation_of_powers_text__formalist_reading, 0.71).
domain_priors:suppression_score(separation_of_powers_text__formalist_reading, 0.78).
domain_priors:theater_ratio(separation_of_powers_text__formalist_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(separation_of_powers_text__formalist_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(separation_of_powers_text__formalist_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(separation_of_powers_text__formalist_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(separation_of_powers_text__formalist_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(separation_of_powers_text__formalist_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(separation_of_powers_text__formalist_reading, tangled_rope).
narrative_ontology:human_readable(separation_of_powers_text__formalist_reading, "Formalist (Impermeable-Boundary) Reading of the Nondelegation Doctrine").
narrative_ontology:topic_domain(separation_of_powers_text__formalist_reading, "constitutional_law/administrative_law").

domain_priors:requires_active_enforcement(separation_of_powers_text__formalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(separation_of_powers_text__formalist_reading, '7a96323c-516f-40c0-88ae-0314cd2779b5').
narrative_ontology:cs_kernel_codification('7a96323c-516f-40c0-88ae-0314cd2779b5', fixed_text).
narrative_ontology:cs_authority_grounding('7a96323c-516f-40c0-88ae-0314cd2779b5', lineage).
narrative_ontology:cs_interpretation_layer_present('7a96323c-516f-40c0-88ae-0314cd2779b5').
narrative_ontology:cs_reading_relation('7a96323c-516f-40c0-88ae-0314cd2779b5', separation_of_powers_text__functionalist_reading, forecloses).
narrative_ontology:cs_reading_relation('7a96323c-516f-40c0-88ae-0314cd2779b5', separation_of_powers_text__unitary_executive_reading, coexists_with).
narrative_ontology:cs_axiom('7a96323c-516f-40c0-88ae-0314cd2779b5', foundational, legislative_power_categorically_nondelegable).
narrative_ontology:cs_axiom_status(legislative_power_categorically_nondelegable, holdable).
narrative_ontology:cs_axiom_grounding('7a96323c-516f-40c0-88ae-0314cd2779b5', legislative_power_categorically_nondelegable, deontological).
narrative_ontology:cs_axiom('7a96323c-516f-40c0-88ae-0314cd2779b5', secondary, intelligible_principle_test_must_bind_strictly).
narrative_ontology:cs_axiom_status(intelligible_principle_test_must_bind_strictly, holdable).
narrative_ontology:cs_axiom_grounding('7a96323c-516f-40c0-88ae-0314cd2779b5', intelligible_principle_test_must_bind_strictly, conventional).
narrative_ontology:cs_reference_frame('7a96323c-516f-40c0-88ae-0314cd2779b5', original_tripartite_vesting_settlement).
narrative_ontology:cs_drift_state('7a96323c-516f-40c0-88ae-0314cd2779b5', post_new_deal_administrative_state, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('7a96323c-516f-40c0-88ae-0314cd2779b5', '').
narrative_ontology:cs_kernel_id(separation_of_powers_text__formalist_reading, separation_of_powers_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(separation_of_powers_text__formalist_reading, formalist_judiciary_faction).
narrative_ontology:constraint_beneficiary(separation_of_powers_text__formalist_reading, deregulatory_litigants).
narrative_ontology:constraint_beneficiary(separation_of_powers_text__formalist_reading, congress_as_sole_lawmaker).
narrative_ontology:constraint_victim(separation_of_powers_text__formalist_reading, administrative_agencies).
narrative_ontology:constraint_victim(separation_of_powers_text__formalist_reading, regulatory_beneficiary_public).
narrative_ontology:constraint_victim(separation_of_powers_text__formalist_reading, technical_rulemaking_capacity).
narrative_ontology:constraint_vindicates(separation_of_powers_text__formalist_reading, strict_tripartite_separation_doctrine).
narrative_ontology:constraint_vindicates(separation_of_powers_text__formalist_reading, nondelegation_as_binding_rule).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Judges and jurists who read Article I as establishing categorical boundaries between legislative and executive function. They administer the reading by striking or narrowing delegations that lack an 'intelligible principle' construed strictly, and by treating agency rulemaking on major questions as inherently suspect. They set the doctrine's content case by case and could relax it, but their institutional authority and jurisprudential identity are bound up with maintaining the strict-boundary account.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__formalist_reading, formalist_judiciary_faction, agenda_setter,
    institutional, generational, analytical, national).

% Regulated industries and advocacy organizations that invoke the formalist reading to challenge agency rules they find costly. They collect the benefit of invalidated or chilled regulations without administering the doctrine themselves; they fund and litigate test cases and file amicus briefs urging courts toward the strict reading.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__formalist_reading, deregulatory_litigants, beneficiary,
    powerful, biographical, mobile, national).

% As an institution, Congress is nominally restored full lawmaking primacy under this reading, since agencies cannot receive delegated legislative power. In practice individual members often prefer delegation (it lets them avoid politically costly technical line-drawing), so the 'benefit' is institutional and abstract rather than something individual legislators actively want; Congress cannot easily exit the doctrine except by constitutional amendment or by writing statutes with exhaustive specificity it is often unable to produce.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__formalist_reading, congress_as_sole_lawmaker, beneficiary,
    institutional, generational, constrained, national).

% Agencies such as environmental, financial, labor, and health regulators depend on delegated rulemaking authority to translate broad statutory mandates into workable technical rules. Under the formalist reading, their rules are perpetually vulnerable to invalidation for exceeding permissible delegation, and they cannot obtain new authority except through Congress passing highly specific legislation on technical subjects it is often ill-equipped to draft. They cannot exit the constraint; their entire regulatory function is what is being narrowed.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__formalist_reading, administrative_agencies, payer,
    organized, biographical, trapped, national).

% The public that relies on agency rules for clean air and water standards, workplace safety, financial consumer protection, and drug safety bears the cost when rulemaking capacity is struck down or chilled by anticipated formalist challenges. They have no direct standing to defend delegations and no practical exit from the jurisdictions and markets the rules would have governed.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__formalist_reading, regulatory_beneficiary_public, payer,
    powerless, biographical, trapped, national).

% The institutional capacity for technically expert, adaptable rulemaking (as opposed to fixed statutory text) is itself degraded whenever agencies must anticipate formalist challenge and either under-regulate or over-litigate. It is not an actor but a capacity that is diminished as a structural byproduct.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__formalist_reading, technical_rulemaking_capacity, payer,
    powerless, generational, trapped, national).
narrative_ontology:stakeholder_non_agent(separation_of_powers_text__formalist_reading, technical_rulemaking_capacity).

% Judges and scholars committed to the functionalist reading would object that the formalist account misreads the founding design and ignores two centuries of settled delegation practice, but within a formalist-controlled forum their competing reading is not the operative one — it exists in dissents, law review pushback, and in courts where a different judicial coalition holds the majority.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__formalist_reading, functionalist_judiciary_faction, excluded,
    institutional, generational, analytical, national).

% Scholars who study the doctrine's actual operation across cases, documenting when delegations are upheld versus struck, and who trace the correlation between formalist doctrine and deregulatory outcomes without themselves being litigants or agencies.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__formalist_reading, administrative_law_scholars, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(separation_of_powers_text__formalist_reading, diffuse).
narrative_ontology:fixing_cost_class(separation_of_powers_text__formalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The formalist reading solves a genuine accountability problem: it seeks to ensure that binding rules affecting the public are made by electorally accountable legislators rather than by unelected officials, preserving a clear chain of democratic responsibility for lawmaking.
% TRANSFER_FUNCTION: The reading moves rulemaking authority away from technical agencies and back toward Congress (in theory) and toward courts (in practice, since courts adjudicate the boundary), while shifting the practical cost of regulatory gaps and litigation uncertainty onto agencies and the public who rely on regulatory protections.
% ABSENT_VOICES: Agency technical staff and the diffuse public who benefit from environmental, safety, and financial rules rarely appear as parties in the constitutional litigation that decides delegation questions; the doctrine is contested mainly among litigants, industry groups, and competing judicial coalitions, not by those who would lose protections if regulations are struck.
% DISAPPEARANCE_RATIONALE: If the formalist nondelegation reading vanished overnight (i.e., courts uniformly adopted a permissive delegation standard), agencies could issue and defend broad rulemakings without the standing threat of nondelegation challenge, deregulatory litigation strategy would lose a primary vector, and Congress could continue writing broad enabling statutes without redrafting them into exhaustive technical detail.
% FOUNDING_PROBLEM: The founding generation worried that concentrating lawmaking, execution, and adjudication in one body or person recreates the tyranny they had just fought a revolution against; the nondelegation principle was meant to prevent Congress from simply handing its lawmaking job to whichever body it found convenient.
% FOUNDING_PROBLEM_CORROBORATION: Formalist jurists and the litigants who benefit attest the problem remains fully live and is being actively violated by the modern administrative state. Administrative law scholars and functionalist jurists — outside the beneficiary set — attest that the founding-era concern was about consolidation of power in a single body, not about technical rulemaking under congressionally-set standards, and that the modern administrative state with judicial review already supplies the accountability check the doctrine was meant to secure; on their account the founding problem in its literal form is largely resolved by other mechanisms and the formalist reading now functions primarily as a deregulatory lever.
narrative_ontology:disappearance_verdict(separation_of_powers_text__formalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(separation_of_powers_text__formalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(separation_of_powers_text__formalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(separation_of_powers_text__formalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(separation_of_powers_text__formalist_reading, 0.71, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(separation_of_powers_text__formalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(separation_of_powers_text__formalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(separation_of_powers_text__formalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.71 at interval end) reflects that the formalist reading, if consistently applied, removes substantial regulatory capacity from agencies whose statutory mandates were written assuming broad delegation was permissible — the 'cost' is measured in foreclosed protections and rulemaking paralysis, not merely doctrinal disagreement. Suppression (0.78) is high because the doctrine's operation depends on active judicial enforcement against a permissive delegation practice that has been the operative constitutional settlement for nearly a century; enforcing the formalist reading requires overturning or narrowing precedent, which is itself a coercive act against the settled administrative order. Theater ratio is comparatively low (0.28) because the doctrinal machinery (intelligible-principle tests, major-questions doctrine) does real gatekeeping work rather than merely performing scrutiny — though it is rising as major-questions doctrine increasingly does work formerly reserved for formal nondelegation review. accessibility_collapse (0.72) reflects that once a court adopts the strict formalist frame, alternative interpretive moves (deference doctrines, functional balancing) become largely unavailable within that forum. Resistance (0.62) captures active pushback from agencies, functionalist scholars, and affected industries defending existing regulatory authority.
 *
 * PERSPECTIVAL GAP:
 *   From the formalist judiciary faction's seat, this is principled constitutional restoration — a mountain-like return to textual first principles that happens to have been violated for decades. From the administrative agencies' seat, the same doctrine operates as an actively enforced snare on their statutory function, applied selectively and unpredictably. The engine's per-seat computation should reflect this asymmetry: the agenda-setting seat's analytical exit and institutional power produce a very different effective extraction reading than the trapped, organized-power agency seat experiences.
 *
 * DIRECTIONALITY LOGIC:
 *   The formalist judiciary faction sets the doctrine's content and bears no cost from its operation — it is the agenda-setting seat. Deregulatory litigants and, abstractly, Congress-as-institution are declared beneficiaries: litigants concretely collect the benefit of struck rules, while Congress's benefit is largely theoretical since individual members often prefer delegation. Administrative agencies are the clearest victims: their statutory authority is what the doctrine narrows, and they have no exit — they cannot simply route around a constitutional ruling. The regulatory beneficiary public bears diffuse, powerless costs with no standing to intervene directly. Technical rulemaking capacity is marked as a non-agent payer because it is a capacity, not an actor, degraded as a byproduct.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — preventing unaccountable concentration of lawmaking power — remains genuinely live as an abstract constitutional concern, which is why founding_problem_status is authored as contested rather than dead. But the corroboration split matters: administrative law scholars and functionalist jurists, standing outside the beneficiary set, attest that judicial review, notice-and-comment procedure, and congressional oversight already discharge the accountability function the founding-era doctrine worried about, meaning the formalist reading's practical operation increasingly serves as a deregulatory instrument riding on a genealogy it has outgrown. This is exactly the divergence the tangled_rope classification is built to hold: a genuine coordination concern (preventing power concentration) coexists with asymmetric extraction (agencies and the public pay while litigants and formalist jurists benefit), and calling it purely one or the other would either whitewash the extraction or dismiss a real constitutional value.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    formalist_reading_naturalness_ambiguity,
    'Is the formalist nondelegation reading a rediscovery of an original, binding constitutional rule, or a constructed doctrinal revival that serves identifiable deregulatory beneficiaries?',
    'Historical analysis of founding-era delegation practice (e.g., early Congresses'' own broad delegations to executive officers) compared against the formalist reading''s claimed originalist pedigree; tracking whether doctrinal revival correlates with industry litigation funding and outcome patterns.',
    'If the founding-era practice itself included substantial delegation, the formalist reading''s claim to textual inevitability weakens considerably, supporting a constructed-doctrine account; if founding practice was in fact as strict as claimed, the reading''s originalist grounding is stronger even though its modern deployment still has beneficiaries.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(formalist_reading_naturalness_ambiguity, conceptual, 'Whether the formalist reading recovers an original rule or constructs one that benefits specific parties.').

omega_variable(
    kernel_reading_indeterminacy,
    'Does the constitutional text underdetermine the choice between the formalist, functionalist, and unitary-executive readings, such that the choice is driven by extra-textual commitments (judicial philosophy, deregulatory preference) rather than by the text alone?',
    'Comparative doctrinal history showing how the same vesting-clause and Article I text has supported all three readings at different points, and whether shifts between readings track changes in judicial composition versus changes in constitutional consensus.',
    'High indeterminacy would mean the choice of reading is substantially a matter of which faction currently controls the interpreting body, reinforcing this story''s placement of agenda-setting power with the formalist judiciary faction rather than with the text itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_indeterminacy, conceptual, 'Whether the kernel text underdetermines the reading, making reading-selection an exercise of interpretive power.').

omega_variable(
    delegation_practical_alternative,
    'Could Congress realistically legislate at the level of technical specificity the formalist reading would require, or is broad delegation a practical necessity given the complexity of modern regulatory subject matter?',
    'Empirical study of legislative capacity and drafting history for technically complex statutes (environmental, financial, pharmaceutical regulation) attempted without broad delegation, comparing regulatory gaps and delay under stricter delegation regimes historically or in comparative jurisdictions.',
    'If Congress cannot practically match required specificity, the formalist reading functions less as a restoration of accountability and more as a structural block on regulatory capacity regardless of intent — sharpening the tangled_rope classification toward the extractive pole.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(delegation_practical_alternative, empirical, 'Whether exhaustive congressional specificity is a realistic substitute for delegation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(separation_of_powers_text__formalist_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sepa_tr_t0, separation_of_powers_text__formalist_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(sepa_tr_t8, separation_of_powers_text__formalist_reading, theater_ratio, 8, 0.17).
narrative_ontology:measurement(sepa_tr_t16, separation_of_powers_text__formalist_reading, theater_ratio, 16, 0.2).
narrative_ontology:measurement(sepa_tr_t24, separation_of_powers_text__formalist_reading, theater_ratio, 24, 0.23).
narrative_ontology:measurement(sepa_tr_t32, separation_of_powers_text__formalist_reading, theater_ratio, 32, 0.26).
narrative_ontology:measurement(sepa_tr_t40, separation_of_powers_text__formalist_reading, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(sepa_be_t0, separation_of_powers_text__formalist_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(sepa_be_t8, separation_of_powers_text__formalist_reading, base_extractiveness, 8, 0.48).
narrative_ontology:measurement(sepa_be_t16, separation_of_powers_text__formalist_reading, base_extractiveness, 16, 0.55).
narrative_ontology:measurement(sepa_be_t24, separation_of_powers_text__formalist_reading, base_extractiveness, 24, 0.62).
narrative_ontology:measurement(sepa_be_t32, separation_of_powers_text__formalist_reading, base_extractiveness, 32, 0.68).
narrative_ontology:measurement(sepa_be_t40, separation_of_powers_text__formalist_reading, base_extractiveness, 40, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(sepa_su_t0, separation_of_powers_text__formalist_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(sepa_su_t8, separation_of_powers_text__formalist_reading, suppression_requirement, 8, 0.6).
narrative_ontology:measurement(sepa_su_t16, separation_of_powers_text__formalist_reading, suppression_requirement, 16, 0.66).
narrative_ontology:measurement(sepa_su_t24, separation_of_powers_text__formalist_reading, suppression_requirement, 24, 0.71).
narrative_ontology:measurement(sepa_su_t32, separation_of_powers_text__formalist_reading, suppression_requirement, 32, 0.75).
narrative_ontology:measurement(sepa_su_t40, separation_of_powers_text__formalist_reading, suppression_requirement, 40, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(separation_of_powers_text__formalist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(separation_of_powers_text__formalist_reading, separation_of_powers_text__functionalist_reading).
narrative_ontology:affects_constraint(separation_of_powers_text__formalist_reading, separation_of_powers_text__unitary_executive_reading).
narrative_ontology:affects_constraint(separation_of_powers_text__formalist_reading, major_questions_doctrine).
narrative_ontology:affects_constraint(separation_of_powers_text__formalist_reading, chevron_deference_doctrine).

% DUAL FORMULATION NOTE:
% This story is one of three constraints decomposed from the natural-language label 'separation of powers' / 'the nondelegation doctrine,' each reading a shared kernel (the Article I/II vesting-clause text) differently: this file (formalist), separation_of_powers_text__functionalist_reading (permissive/flexible delegation), and separation_of_powers_text__unitary_executive_reading (Article II executive-unity claim, a distinct question about presidential control rather than delegation per se). Each carries its own ε, beneficiary/victim structure, and claimed type — this reading assigns a materially higher ε and victim set (administrative agencies) than the functionalist sibling would assign to the same underlying text, because the formalist reading treats the current delegation-heavy administrative state as the extractive standing arrangement under contest. The unitary_executive_reading is linked because it shares interpretive method (formalist textualism) and often co-occurs in the same litigation strategy, though it addresses a structurally distinct claim (intra-executive-branch unity, not legislative-to-agency delegation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
