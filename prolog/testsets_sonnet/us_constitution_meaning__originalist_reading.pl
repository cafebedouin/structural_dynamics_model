% ============================================================================
% CONSTRAINT STORY: us_constitution_meaning__originalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_meaning__originalist_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: us_constitution_meaning__originalist_reading
 *   human_readable: Originalist Reading of Constitutional Meaning (Fixed at Ratification)
 *   domain: constitutional_law/legal_theory/political_philosophy
 *
 * SUMMARY:
 *   This story instantiates the originalist reading of the contested kernel
 *   'us_constitution_meaning': constitutional meaning is fixed at the moment
 *   of ratification (or amendment) and judges are bound by the historical
 *   public meaning of the text, with contemporary circumstances relevant only
 *   to application, never to the content of meaning itself. This is one of
 *   three sibling readings of the same kernel —
 *   living_constitutionalist_reading and positivist_reading are separate
 *   constraint stories with their own ε values and stakeholder structures,
 *   not alternative measurements of this one. The rise of originalism as
 *   controlling doctrine (particularly from the 1980s legal movement through
 *   recent Supreme Court composition shifts) has been accompanied by
 *   increasing use of history-and-tradition tests to resolve rights claims,
 *   which the temporal measurements track as rising extractiveness and rising
 *   suppression of non-originalist argument.
 *
 * KEY AGENTS:
 *   - originalist_judges: administer the method (institutional/identity_locked) — bear no cost, gain interpretive authority and doctrinal stability
 *   - counter_majoritarian_constraint_advocates: primary beneficiary (organized/mobile) — gains a legitimating framework for blocking disfavored outcomes
 *   - rights_claimants_lacking_18th_century_analogue: primary target (powerless/trapped) — bears the cost of doctrinal foreclosure
 *   - marginalized_groups_excluded_from_founding_era_political_community: secondary target (powerless/trapped) — inherits an exclusionary evidentiary record as binding law
 *   - constitutional_law_scholars: analytical observer — documents selective application and historical contestability
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_meaning__originalist_reading, 0.58).
domain_priors:suppression_score(us_constitution_meaning__originalist_reading, 0.71).
domain_priors:theater_ratio(us_constitution_meaning__originalist_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_meaning__originalist_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(us_constitution_meaning__originalist_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(us_constitution_meaning__originalist_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_meaning__originalist_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(us_constitution_meaning__originalist_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_meaning__originalist_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_meaning__originalist_reading, "Originalist Reading of Constitutional Meaning (Fixed at Ratification)").
narrative_ontology:topic_domain(us_constitution_meaning__originalist_reading, "constitutional_law/legal_theory/political_philosophy").

domain_priors:requires_active_enforcement(us_constitution_meaning__originalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_meaning__originalist_reading, '793f4822-d1df-442b-bfa0-dde6eb46a2a7').
narrative_ontology:cs_kernel_codification('793f4822-d1df-442b-bfa0-dde6eb46a2a7', fixed_text).
narrative_ontology:cs_authority_grounding('793f4822-d1df-442b-bfa0-dde6eb46a2a7', lineage).
narrative_ontology:cs_interpretation_layer_present('793f4822-d1df-442b-bfa0-dde6eb46a2a7').
narrative_ontology:cs_reading_relation('793f4822-d1df-442b-bfa0-dde6eb46a2a7', us_constitution_meaning__living_constitutionalist_reading, forecloses).
narrative_ontology:cs_reading_relation('793f4822-d1df-442b-bfa0-dde6eb46a2a7', us_constitution_meaning__positivist_reading, coexists_with).
narrative_ontology:cs_axiom('793f4822-d1df-442b-bfa0-dde6eb46a2a7', foundational, meaning_fixed_at_ratification).
narrative_ontology:cs_axiom_status(meaning_fixed_at_ratification, holdable).
narrative_ontology:cs_axiom_grounding('793f4822-d1df-442b-bfa0-dde6eb46a2a7', meaning_fixed_at_ratification, conventional).
narrative_ontology:cs_axiom('793f4822-d1df-442b-bfa0-dde6eb46a2a7', foundational, contemporary_values_irrelevant_to_content_of_meaning).
narrative_ontology:cs_axiom_status(contemporary_values_irrelevant_to_content_of_meaning, holdable).
narrative_ontology:cs_axiom_grounding('793f4822-d1df-442b-bfa0-dde6eb46a2a7', contemporary_values_irrelevant_to_content_of_meaning, instrumental).
narrative_ontology:cs_reference_frame('793f4822-d1df-442b-bfa0-dde6eb46a2a7', ratification_era_public_meaning).
narrative_ontology:cs_drift_state('793f4822-d1df-442b-bfa0-dde6eb46a2a7', contemporary_rights_claims_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('793f4822-d1df-442b-bfa0-dde6eb46a2a7', '').
narrative_ontology:cs_kernel_id(us_constitution_meaning__originalist_reading, us_constitution_meaning).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_meaning__originalist_reading, counter_majoritarian_constraint_advocates).
narrative_ontology:constraint_beneficiary(us_constitution_meaning__originalist_reading, settled_property_and_contract_interests).
narrative_ontology:constraint_beneficiary(us_constitution_meaning__originalist_reading, originalist_legal_movement).
narrative_ontology:constraint_victim(us_constitution_meaning__originalist_reading, rights_claimants_lacking_18th_century_analogue).
narrative_ontology:constraint_victim(us_constitution_meaning__originalist_reading, marginalized_groups_excluded_from_founding_era_political_community).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bind themselves and lower courts to historical public meaning evidence — founding-era dictionaries, ratification debates, contemporaneous statutes — treating this as the sole legitimate interpretive method. Their judicial identity and jurisprudential authority are constituted by fidelity to this method; abandoning it would dissolve the distinct professional and philosophical position they occupy. They administer the constraint through opinion-writing and doctrine (e.g., history-and-tradition tests).
narrative_ontology:constraint_stakeholder(us_constitution_meaning__originalist_reading, originalist_judges, agenda_setter,
    institutional, generational, identity_locked, national).

% Political and legal actors who want constitutional meaning insulated from shifting majoritarian preferences and elite judicial policy-making. The originalist reading gives them a stable, citable standard that legitimates blocking outcomes they oppose without having to win the underlying moral or political argument on its merits.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__originalist_reading, counter_majoritarian_constraint_advocates, beneficiary,
    organized, civilizational, mobile, national).

% Holders of long-settled economic arrangements benefit from a reading that treats historical baselines (property rights, contract enforcement, limited federal regulatory reach) as fixed and resistant to reinterpretation in light of contemporary economic conditions or regulatory needs.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__originalist_reading, settled_property_and_contract_interests, beneficiary,
    powerful, generational, arbitrage, national).

% Individuals asserting rights or protections that have no clear analogue in 1788 or amendment-era public understanding — reproductive autonomy, certain privacy claims, novel forms of discrimination, emerging technologies of surveillance and speech. Their claims are structurally disfavored regardless of contemporary moral or empirical consensus; the only path to relief is amendment or waiting for doctrinal reclassification, both effectively foreclosed to them individually.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__originalist_reading, rights_claimants_lacking_18th_century_analogue, payer,
    powerless, biographical, trapped, national).

% Groups who were not part of the political community whose 'public meaning' the ratification-era evidence records (enslaved people, women, non-property-holders) inherit a textual record built without their participation. Originalist method treats that exclusionary record as the fixed reference point, so their present-day claims must be squeezed into a framework that did not contemplate their equal standing.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__originalist_reading, marginalized_groups_excluded_from_founding_era_political_community, payer,
    powerless, generational, trapped, national).

% Compete for the same interpretive authority but are treated by the originalist reading as illegitimate — 'judicial activists' imposing contemporary values rather than discovering fixed meaning. Their arguments are heard in academic and some judicial fora but are structurally disfavored wherever originalist doctrine controls precedent.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__originalist_reading, living_constitutionalist_judges_and_scholars, excluded,
    organized, civilizational, constrained, national).

% Study how originalist doctrine is applied, document its selective use (originalist reasoning invoked more consistently to reach some outcomes than others), and produce empirical work on ratification-era historical records that both supports and undermines specific originalist holdings.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__originalist_reading, constitutional_law_scholars, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(us_constitution_meaning__originalist_reading, diffuse).
narrative_ontology:fixing_cost_class(us_constitution_meaning__originalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a determinate, citable decision procedure that constrains judicial discretion by anchoring interpretation to a fixed historical referent, reducing (in principle) the risk that constitutional meaning simply tracks the political preferences of whichever judges currently sit on the bench.
% TRANSFER_FUNCTION: Moves interpretive authority away from claimants seeking recognition of rights without clear founding-era analogues, and toward historical evidentiary methods that favor incumbents of settled arrangements; moves legitimacy and citation power toward the originalist legal movement and its institutional infrastructure (federalist society pipelines, originalist scholarship networks, favorable judicial appointments).
% ABSENT_VOICES: The historical political community whose 'public meaning' is being recovered did not include enslaved people, women, or non-property-holding men — their absence from the original meaning-making process is treated as a historical fact to be applied, not a defect to be corrected, whenever the method purports to recover 'the' original public understanding.
% DISAPPEARANCE_RATIONALE: If originalism vanished as the controlling interpretive method overnight, decades of settled doctrine grounded in history-and-tradition tests would become vulnerable to relitigation, the originalist legal movement's institutional investment (training pipelines, judicial appointment criteria, citation practice) would lose its central organizing framework, and previously foreclosed rights claims lacking 18th-century analogues would gain a live doctrinal path forward.
% FOUNDING_PROBLEM: Perceived judicial overreach in the mid-to-late 20th century (particularly the Warren and early Burger Courts) in which justices were seen as reading contemporary policy preferences into constitutional text under the guise of interpretation; originalism was built to discipline that discretion by tying meaning to an external, non-manipulable historical fact.
% FOUNDING_PROBLEM_CORROBORATION: Originalist judges and movement scholars attest the discretion problem remains live and originalism remains the only principled check on it. Independent legal historians and comparative constitutional scholars outside the originalist movement note that originalist method itself exhibits significant interpretive discretion in selecting which historical sources count and at what level of generality to state the 'original' principle — suggesting the discipline the method claims to provide is itself contested rather than settled, and that non-originalist judges and legislatures who must live under originalist rulings did not participate in validating the method's premises.
narrative_ontology:disappearance_verdict(us_constitution_meaning__originalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_meaning__originalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_meaning__originalist_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(us_constitution_meaning__originalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_meaning__originalist_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_meaning__originalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_constitution_meaning__originalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(us_constitution_meaning__originalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.58 (moderate-high, rising over the interval) because the method transfers real decision-making leverage away from claimants without founding-era analogues, but the transfer operates through a genuinely coherent judicial methodology rather than naked discretion — hence tangled_rope rather than snare. Suppression is authored higher (0.71) because the doctrine's persistence depends on actively delegitimizing rival interpretive methods (living constitutionalism, purposivism) as illegitimate judicial activism, not merely on being more persuasive. Theater ratio is moderate-low (0.28): the historical research underlying originalist opinions is often genuine scholarly work, but a growing share of the doctrine's felt weight is method-signaling (citing history to license a preferred outcome) rather than history genuinely constraining the outcome — scholars have documented this drift. Accessibility collapse (0.62) reflects that once a court embraces originalism as controlling, non-historical arguments become largely unavailable as a matter of doctrine, though not completely (application-level discretion remains). Resistance (0.68) is high because the method is fiercely contested by a substantial, organized rival legal movement, unlike a genuine mountain which would meet little organized resistance.
 *
 * DIRECTIONALITY LOGIC:
 *   Originalist judges sit near the agenda-setting end: they administer the constraint and their professional identity is constituted by it (identity_locked exit — abandoning originalism would mean abandoning the distinct jurisprudential position that gives their opinions their citational authority). Counter-majoritarian advocates and settled economic interests are beneficiaries with mobile/arbitrage exit — they can shift arguments across doctrinal frames as needed and are not bound to originalism as an identity, only as a useful tool. Rights claimants lacking historical analogues and excluded marginalized groups are the structural targets: trapped exit, since an individual litigant cannot escape the interpretive method controlling the court that hears their case, and their time horizon is often biographical/urgent (a single lifetime, a single case) against the doctrine's generational-to-civilizational horizon.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — perceived unconstrained judicial policy-making — is authored as contested rather than resolved or clearly dead. This prevents the story from being mislabeled as pure extraction: originalism does respond to a real and recurring institutional design problem (how to constrain judicial discretion), and abandoning it entirely would not obviously solve that problem, only relocate the discretion to a different set of interpretive choices (which historical sources, which level of generality). At the same time, the R5 corroboration surfaces that the discipline claimed for the method is itself contested by scholars outside the originalist movement, and that the political community whose original understanding is being 'recovered' did not include the very claimants who now bear the costs of that recovery — this is the structural feature that keeps the classification from settling into a clean rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    originalism_determinacy_vs_discretion,
    'Does the originalist method actually constrain judicial outcomes more than rival interpretive methods, or does it merely relocate discretion into source-selection and level-of-generality choices while claiming greater discipline?',
    'Empirical study comparing outcome predictability and inter-judge agreement rates under originalist versus non-originalist reasoning on matched case sets; analysis of how often originalist opinions selectively cite favorable historical sources while omitting contradictory ones from the same period.',
    'If originalism provides no more real constraint than alternatives, its coordination-function claim weakens substantially and the classification moves toward snare (extraction dressed as discipline); if it provides genuine, measurable constraint, the tangled_rope classification''s coordination component is well-supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(originalism_determinacy_vs_discretion, empirical, 'Whether originalism''s claimed discretion-constraining function is real or a legitimating narrative.').

omega_variable(
    founding_era_exclusion_as_feature_or_defect,
    'Is the exclusion of enslaved people, women, and non-property-holders from the ratification-era ''public meaning'' a fact to be neutrally applied by originalist method, or does it constitute a structural defect that should disqualify that historical record as the binding referent for present-day rights claims?',
    'This is fundamentally a normative/conceptual question rather than an empirical one — it depends on contested premises about whose understanding counts as authoritative and cannot be resolved by additional historical research alone.',
    'If treated as a disqualifying defect, the originalist reading''s legitimacy claim weakens considerably and the victim-designation for excluded groups strengthens further; if treated as a neutral historical fact under a formalist premise, the reading''s internal coherence is preserved even as its distributive effects remain unchanged.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(founding_era_exclusion_as_feature_or_defect, conceptual, 'Whether founding-era political exclusion undermines the legitimacy of treating that era''s understanding as binding.').

omega_variable(
    kernel_reading_selection_pressure,
    'Why has the originalist reading gained controlling doctrinal status relative to its sibling readings over the measured interval — is this because of superior interpretive coherence, or because of successful, well-resourced institutional movement-building (judicial appointments, legal education pipelines, funded scholarship)?',
    'Historical-institutional analysis of the originalist legal movement''s funding, appointment strategy, and academic infrastructure relative to rival interpretive traditions, correlated against the timing of doctrinal ascendance.',
    'If institutional movement-building substantially explains the reading''s ascendance independent of interpretive merit, this supports reading the rising extractiveness/suppression trend as partly a capture dynamic rather than a purely intellectual development, and would strengthen the case for auditing which of the three kernel readings currently controls in which courts and why.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_pressure, empirical, 'Whether originalism''s doctrinal dominance reflects interpretive merit or organized institutional capture of judicial selection.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_meaning__originalist_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t0, us_constitution_meaning__originalist_reading, theater_ratio, 0, 0.14).
narrative_ontology:measurement(us_c_tr_t10, us_constitution_meaning__originalist_reading, theater_ratio, 10, 0.17).
narrative_ontology:measurement(us_c_tr_t20, us_constitution_meaning__originalist_reading, theater_ratio, 20, 0.2).
narrative_ontology:measurement(us_c_tr_t30, us_constitution_meaning__originalist_reading, theater_ratio, 30, 0.23).
narrative_ontology:measurement(us_c_tr_t40, us_constitution_meaning__originalist_reading, theater_ratio, 40, 0.26).
narrative_ontology:measurement(us_c_tr_t50, us_constitution_meaning__originalist_reading, theater_ratio, 50, 0.28).

% Extraction over time
narrative_ontology:measurement(us_c_be_t0, us_constitution_meaning__originalist_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(us_c_be_t10, us_constitution_meaning__originalist_reading, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(us_c_be_t20, us_constitution_meaning__originalist_reading, base_extractiveness, 20, 0.47).
narrative_ontology:measurement(us_c_be_t30, us_constitution_meaning__originalist_reading, base_extractiveness, 30, 0.51).
narrative_ontology:measurement(us_c_be_t40, us_constitution_meaning__originalist_reading, base_extractiveness, 40, 0.55).
narrative_ontology:measurement(us_c_be_t50, us_constitution_meaning__originalist_reading, base_extractiveness, 50, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t0, us_constitution_meaning__originalist_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(us_c_su_t10, us_constitution_meaning__originalist_reading, suppression_requirement, 10, 0.54).
narrative_ontology:measurement(us_c_su_t20, us_constitution_meaning__originalist_reading, suppression_requirement, 20, 0.6).
narrative_ontology:measurement(us_c_su_t30, us_constitution_meaning__originalist_reading, suppression_requirement, 30, 0.65).
narrative_ontology:measurement(us_c_su_t40, us_constitution_meaning__originalist_reading, suppression_requirement, 40, 0.68).
narrative_ontology:measurement(us_c_su_t50, us_constitution_meaning__originalist_reading, suppression_requirement, 50, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_meaning__originalist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(us_constitution_meaning__originalist_reading, us_constitution_meaning__living_constitutionalist_reading).
narrative_ontology:affects_constraint(us_constitution_meaning__originalist_reading, us_constitution_meaning__positivist_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling constraints decomposing the natural-language label 'the meaning of the US Constitution' per the ε-invariance principle: originalist_reading (this story — fixed historical meaning, tangled_rope), living_constitutionalist_reading (enduring principles with evolving application, a separate story with its own ε and beneficiary structure), and positivist_reading (validity from formal enactment procedure, bracketing original-meaning questions entirely, also a separate story). The three do not share an ε value; each reading produces structurally distinct extraction, suppression, and victim profiles and must be evaluated independently. They are linked here via affects_constraints because doctrinal dominance by one reading directly changes the resource availability and judicial-appointment legitimacy conditions available to the others (an upstream/downstream influence relationship, not identity).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
