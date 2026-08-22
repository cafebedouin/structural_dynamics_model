% ============================================================================
% CONSTRAINT STORY: us_constitution_meaning__living_constitutionalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_meaning__living_constitutionalist_reading, []).

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
 *   constraint_id: us_constitution_meaning__living_constitutionalist_reading
 *   human_readable: Living Constitutionalism — Enduring Principles, Evolving Application
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   This story instantiates the living constitutionalist reading of the US
 *   Constitution's meaning: the text encodes enduring principles (liberty,
 *   equality, due process) whose concrete application is properly understood
 *   to evolve as social attitudes, factual circumstances, and moral consensus
 *   change, without requiring formal amendment for every adaptation. This is
 *   one reading among three of a single contested kernel — the originalist
 *   reading (meaning fixed at ratification) and the positivist reading
 *   (validity from enactment procedure alone) are separate constraints with
 *   their own ε and stakeholder structure, not alternate measurements of this
 *   one. Under this reading's own lights, the standing arrangement is a
 *   judiciary empowered to extend constitutional principle to circumstances
 *   the ratifying generation did not contemplate, coordinating the goal of a
 *   durable-but-adaptive constitutional order while imposing real costs on
 *   legislative majorities and litigants who rely on fixed historical
 *   meaning.
 *
 * KEY AGENTS:
 *   - federal_judiciary: agenda-setter administering the interpretive method case by case
 *   - rights_claimants_in_evolving_social_contexts: primary beneficiaries of principle-extension
 *   - legislative_majorities: bear displacement of democratically-enacted policy by judicial reinterpretation
 *   - originalist_litigants: structurally disadvantaged by a method that treats historical meaning as one factor rather than dispositive
 *   - counter_majoritarian_check: the design value put at risk by unconstrained doctrinal movement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_meaning__living_constitutionalist_reading, 0.42).
domain_priors:suppression_score(us_constitution_meaning__living_constitutionalist_reading, 0.38).
domain_priors:theater_ratio(us_constitution_meaning__living_constitutionalist_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_meaning__living_constitutionalist_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(us_constitution_meaning__living_constitutionalist_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(us_constitution_meaning__living_constitutionalist_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_meaning__living_constitutionalist_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(us_constitution_meaning__living_constitutionalist_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_meaning__living_constitutionalist_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_meaning__living_constitutionalist_reading, "Living Constitutionalism — Enduring Principles, Evolving Application").
narrative_ontology:topic_domain(us_constitution_meaning__living_constitutionalist_reading, "constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(us_constitution_meaning__living_constitutionalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_meaning__living_constitutionalist_reading, '55e6a515-640d-4e76-8183-44074bb50a8b').
narrative_ontology:cs_kernel_codification('55e6a515-640d-4e76-8183-44074bb50a8b', fixed_text).
narrative_ontology:cs_authority_grounding('55e6a515-640d-4e76-8183-44074bb50a8b', lineage).
narrative_ontology:cs_interpretation_layer_present('55e6a515-640d-4e76-8183-44074bb50a8b').
narrative_ontology:cs_reading_relation('55e6a515-640d-4e76-8183-44074bb50a8b', us_constitution_meaning__originalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('55e6a515-640d-4e76-8183-44074bb50a8b', us_constitution_meaning__positivist_reading, coexists_with).
narrative_ontology:cs_axiom('55e6a515-640d-4e76-8183-44074bb50a8b', foundational, extra_amendment_principle_extension_legitimate).
narrative_ontology:cs_axiom_status(extra_amendment_principle_extension_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('55e6a515-640d-4e76-8183-44074bb50a8b', extra_amendment_principle_extension_legitimate, conventional).
narrative_ontology:cs_axiom('55e6a515-640d-4e76-8183-44074bb50a8b', secondary, contemporary_moral_consensus_relevant_to_application).
narrative_ontology:cs_axiom_status(contemporary_moral_consensus_relevant_to_application, holdable).
narrative_ontology:cs_axiom_grounding('55e6a515-640d-4e76-8183-44074bb50a8b', contemporary_moral_consensus_relevant_to_application, instrumental).
narrative_ontology:cs_reference_frame('55e6a515-640d-4e76-8183-44074bb50a8b', principle_over_historical_application_framework).
narrative_ontology:cs_drift_state('55e6a515-640d-4e76-8183-44074bb50a8b', post_1960s_rights_expansion_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('55e6a515-640d-4e76-8183-44074bb50a8b', '').
narrative_ontology:cs_kernel_id(us_constitution_meaning__living_constitutionalist_reading, us_constitution_meaning).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_meaning__living_constitutionalist_reading, rights_claimants_in_evolving_social_contexts).
narrative_ontology:constraint_beneficiary(us_constitution_meaning__living_constitutionalist_reading, marginalized_groups_seeking_constitutional_recognition).
narrative_ontology:constraint_victim(us_constitution_meaning__living_constitutionalist_reading, legislative_majorities).
narrative_ontology:constraint_victim(us_constitution_meaning__living_constitutionalist_reading, originalist_litigants).
narrative_ontology:constraint_victim(us_constitution_meaning__living_constitutionalist_reading, settled_expectation_holders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets constitutional text by reference to underlying principles (liberty, equality, due process) as applied to present-day social facts. Decides which contemporary circumstances count as constitutionally relevant and how far settled doctrine can move without a formal amendment. Administers the reading through case law; can expand or contract it opinion by opinion.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__living_constitutionalist_reading, federal_judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Bring claims — same-sex couples seeking marriage recognition, criminal defendants asserting due process protections not enumerated at founding, groups asserting equal protection under evolving social understanding. Cannot obtain relief through text alone; depend on the judiciary reading the text's principles as applicable to their circumstance rather than frozen to 1787/1868 social facts.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__living_constitutionalist_reading, rights_claimants_in_evolving_social_contexts, beneficiary,
    powerless, biographical, trapped, national).

% Groups whose social status was not contemplated as constitutionally protected at ratification. Their claims to protection depend entirely on the interpretive move that principles can extend beyond their historically understood application. Have no exit from the constitutional system and no alternate forum for recognition.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__living_constitutionalist_reading, marginalized_groups_seeking_constitutional_recognition, beneficiary,
    powerless, generational, trapped, national).

% Enact statutes reflecting current democratic preference, which can be invalidated or reshaped by judicial reinterpretation of constitutional principle without going through the amendment process the majority would otherwise need to overcome. Bears the cost of policy outcomes being displaced by unelected judicial updating; recourse is slow (new appointments, constitutional amendment) relative to the speed of doctrinal shift.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__living_constitutionalist_reading, legislative_majorities, payer,
    organized, biographical, constrained, national).

% Litigants and advocacy groups whose case for a particular outcome rests on historical public meaning. Under this reading their originalist arguments are treated as one input among several rather than dispositive, which they experience as the goalposts moving after the fact. Cannot exit the system; can only argue the merits within a framework they consider illegitimate.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__living_constitutionalist_reading, originalist_litigants, payer,
    moderate, biographical, constrained, national).

% Individuals and institutions that structured contracts, property arrangements, or conduct around a prior constitutional interpretation. When doctrine evolves, their settled expectations can be upended retroactively in practical effect even though the text never changed. Bear transition costs with no compensation mechanism.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__living_constitutionalist_reading, settled_expectation_holders, payer,
    moderate, biographical, constrained, national).

% The design principle that constitutional constraints should be resistant to shifting majority will and to the preferences of whichever coalition currently controls the bench. Not a party that can speak for itself; represented, if at all, by dissenting justices, originalist scholars, and legislatures whose enactments are displaced. Its interests are structurally at risk whenever the interpretive method itself is the variable.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__living_constitutionalist_reading, counter_majoritarian_check, excluded,
    institutional, civilizational, analytical, national).
narrative_ontology:stakeholder_non_agent(us_constitution_meaning__living_constitutionalist_reading, counter_majoritarian_check).

% Study and critique the pattern of doctrinal evolution, track which principles have been extended and on what evidentiary basis courts treat 'contemporary consensus' as ascertained, and assess whether the interpretive method is internally disciplined or result-driven.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__living_constitutionalist_reading, constitutional_law_scholars, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(us_constitution_meaning__living_constitutionalist_reading, diffuse).
narrative_ontology:fixing_cost_class(us_constitution_meaning__living_constitutionalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allows a constitutional text drafted for one social configuration to keep functioning as law for a society whose composition, technology, and moral consensus have changed substantially, without requiring supermajority amendment for every adaptation.
% TRANSFER_FUNCTION: Moves the power to fix the practical content of constitutional guarantees from the amendment process (requiring broad supermajority consensus) to the judiciary's assessment of enduring principle applied to present circumstance — shifting authority from legislative/ratifying majorities toward courts, and shifting protection toward claimants whose situations were not anticipated at ratification.
% ABSENT_VOICES: The ratifying generations cannot object to how their text is being extended; legislative majorities whose enactments are displaced by newly-recognized rights have a voice in politics but not in the interpretive methodology itself; originalist scholars are present in the debate but structurally outvoted whenever the reading prevails on the bench.
% DISAPPEARANCE_RATIONALE: If living constitutionalism disappeared as an interpretive practice and courts bound themselves strictly to fixed historical meaning, entire lines of precedent recognizing rights not contemplated at ratification would become vulnerable to reversal, legislative majorities would regain relative power over contested social questions, and claimants without a clear textual or historical hook would lose their primary avenue for constitutional recognition.
% FOUNDING_PROBLEM: A constitution meant to endure across centuries cannot anticipate every future social configuration in its enumerated text; a purely textual/historical-meaning approach risks freezing protections to the moral and factual assumptions of a narrower, less inclusive founding-era society.
% FOUNDING_PROBLEM_CORROBORATION: Courts applying this method attest the problem is live (opinions citing the need to apply principle to modern circumstance). Independent corroboration from outside the beneficiary set is contested: originalist scholars and some political scientists argue the 'problem' is itself a pretext for substituting judicial policy preference for democratic and amendment-based change, and note the interpretive method has no external check on when 'contemporary consensus' is genuinely ascertainable versus judicially assumed.
narrative_ontology:disappearance_verdict(us_constitution_meaning__living_constitutionalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_meaning__living_constitutionalist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_meaning__living_constitutionalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(us_constitution_meaning__living_constitutionalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_meaning__living_constitutionalist_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_meaning__living_constitutionalist_reading_tests).
:- end_tests(us_constitution_meaning__living_constitutionalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42): this reading genuinely solves a durability problem (a centuries-old text serving a changed society) but does so by transferring interpretive authority to judges rather than to the amendment process, which imposes real costs on legislative majorities and settled-expectation holders. Suppression is moderate (0.38) — lower than a purely coercive constraint because dissent is structurally possible (dissenting opinions, subsequent overruling, legislative response, appointment politics) but real because a single generation of judicial appointments can lock in doctrinal shifts that are costly to reverse. Theater ratio is low-moderate (0.22) reflecting that most judicial activity under this method is substantive doctrinal reasoning, though a growing share involves justifying results by reference to 'evolving standards' language that critics argue can mask outcome-driven reasoning. Accessibility collapse is moderate (0.35): the amendment process remains formally available as an alternative but is practically foreclosed by supermajority requirements, making judicial reinterpretation the only realistic path for many claimants. Resistance is substantial (0.6) — originalist jurisprudence, legislative pushback, and academic critique are organized and durable countercurrents.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seats (rights claimants, marginalized groups), the arrangement looks like a rope: principled adaptation solving a real problem of textual rigidity. From the payer seats (legislative majorities, originalist litigants, settled-expectation holders), the same structure looks like extraction of interpretive authority without commensurate accountability — a body with lifetime tenure revising the practical content of law that a supermajority-bound amendment process would otherwise control. The engine computes these as distinct seat-level classifications from the same structural data; the divergence is the analytical payload, not an error to reconcile.
 *
 * DIRECTIONALITY LOGIC:
 *   Rights claimants and marginalized groups are declared beneficiaries: the interpretive method is often their only route to constitutional recognition, so they sit near the beneficiary end of directionality (d low) — the constraint subsidizes their claims by construction. Legislative majorities, originalist litigants, and settled-expectation holders are declared victims/payers: they bear the cost of a method that can displace enacted policy or destabilize reliance interests without requiring the higher-consensus amendment process, so they sit nearer the target end (d high). The federal judiciary is agenda-setter with institutional power and effectively analytical exit (it is not itself regulated by the constraint it administers) — this asymmetry between an administering seat with no exit cost and payer seats with real but slow exit (electoral politics, future appointments, amendment) is the structural core of the tangled_rope classification.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — a text drafted for one social configuration needing to function across centuries — remains live by the judiciary's own account, but the corroboration is genuinely contested: originalist critics argue the 'evolving circumstances' framing has become a vehicle for judicial policy preference decoupled from any external check on when social consensus is truly ascertained, which is exactly the risk the counter-majoritarian check exists to guard against. Classifying this as tangled_rope rather than either pure rope (all coordination) or pure snare (all extraction) prevents both a naive endorsement (ignoring the real cost to legislative majorities and settled expectations) and a naive dismissal (ignoring that the interpretive method does solve a genuine durability problem for rights the ratifying generation could not have anticipated).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    consensus_ascertainment_ambiguity,
    'When courts invoke ''evolving standards of decency'' or ''contemporary consensus,'' is there a principled, externally-verifiable method for ascertaining that consensus, or is the invocation functionally indistinguishable from judicial policy preference dressed in sociological language?',
    'Comparative study of cases where courts claimed a consensus existed against contemporaneous polling, legislative enactment counts, and comparative state law — assessing whether the judicially-asserted consensus tracked measurable social fact or preceded it.',
    'If consensus-ascertainment is unprincipled, effective extraction is higher than the base score suggests (judicial preference substituting for democratic process); if principled and verifiable, the coordination function is more robust and the tangled_rope reading understates the rope component.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consensus_ascertainment_ambiguity, empirical, 'Whether social-consensus invocation is methodologically disciplined or a rhetorical vehicle for outcome-driven interpretation.').

omega_variable(
    counter_majoritarian_risk_magnitude,
    'Does the living constitutionalist method''s risk to counter-majoritarian design (the classic ''judicial overreach'' concern) materialize as an occasional, correctable overreach, or as a structural and cumulative transfer of authority from legislatures to courts?',
    'Longitudinal tracking of how often doctrinal shifts under this method are later narrowed, reversed, or ratified by subsequent amendment/legislation versus how often they persist unchallenged for generations.',
    'If overreach is rare and self-correcting, the victim-side extraction (legislative_majorities, settled_expectation_holders) is overstated relative to actual persistence; if cumulative and rarely reversed, the tangled_rope''s extractive component is understated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(counter_majoritarian_risk_magnitude, conceptual, 'Whether counter-majoritarian risk under this reading is self-limiting or structurally accumulating.').

omega_variable(
    committer_framing_kernel_location,
    'This story treats ''the US Constitution''s meaning'' as a single kernel with three readings differentiated by interpretive method (living, originalist, positivist). An alternative framing would locate the kernel not in the text''s meaning but in the amendment process''s legitimacy — i.e., whether ANY non-amendment mechanism for updating constitutional content is legitimate at all, which would make all three readings species of a prior, more foundational dispute about Article V''s exclusivity.',
    'Compare classification outcomes under both framings: does treating amendment-process-legitimacy as the kernel change which reading forecloses which, or does it simply relocate the same axioms one level up?',
    'If the alternative framing is adopted, the axiom distinguishing this reading (extra_amendment_principle_extension_legitimate) would become the kernel''s own contested axiom rather than a reading-level axiom, and the reading_relations to originalist_reading might shift from coexists_with toward a more direct foreclosure relationship since the two readings would be answering the SAME question rather than parallel questions about interpretive method.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(committer_framing_kernel_location, conceptual, 'Whether the kernel is best located at interpretive-method or at amendment-process-legitimacy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_meaning__living_constitutionalist_reading, 1950, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t1950, us_constitution_meaning__living_constitutionalist_reading, theater_ratio, 1950, 0.15).
narrative_ontology:measurement(us_c_tr_t1965, us_constitution_meaning__living_constitutionalist_reading, theater_ratio, 1965, 0.17).
narrative_ontology:measurement(us_c_tr_t1980, us_constitution_meaning__living_constitutionalist_reading, theater_ratio, 1980, 0.19).
narrative_ontology:measurement(us_c_tr_t1995, us_constitution_meaning__living_constitutionalist_reading, theater_ratio, 1995, 0.2).
narrative_ontology:measurement(us_c_tr_t2010, us_constitution_meaning__living_constitutionalist_reading, theater_ratio, 2010, 0.21).
narrative_ontology:measurement(us_c_tr_t2025, us_constitution_meaning__living_constitutionalist_reading, theater_ratio, 2025, 0.22).

% Extraction over time
narrative_ontology:measurement(us_c_be_t1950, us_constitution_meaning__living_constitutionalist_reading, base_extractiveness, 1950, 0.28).
narrative_ontology:measurement(us_c_be_t1965, us_constitution_meaning__living_constitutionalist_reading, base_extractiveness, 1965, 0.33).
narrative_ontology:measurement(us_c_be_t1980, us_constitution_meaning__living_constitutionalist_reading, base_extractiveness, 1980, 0.36).
narrative_ontology:measurement(us_c_be_t1995, us_constitution_meaning__living_constitutionalist_reading, base_extractiveness, 1995, 0.38).
narrative_ontology:measurement(us_c_be_t2010, us_constitution_meaning__living_constitutionalist_reading, base_extractiveness, 2010, 0.4).
narrative_ontology:measurement(us_c_be_t2025, us_constitution_meaning__living_constitutionalist_reading, base_extractiveness, 2025, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t1950, us_constitution_meaning__living_constitutionalist_reading, suppression_requirement, 1950, 0.3).
narrative_ontology:measurement(us_c_su_t1965, us_constitution_meaning__living_constitutionalist_reading, suppression_requirement, 1965, 0.32).
narrative_ontology:measurement(us_c_su_t1980, us_constitution_meaning__living_constitutionalist_reading, suppression_requirement, 1980, 0.34).
narrative_ontology:measurement(us_c_su_t1995, us_constitution_meaning__living_constitutionalist_reading, suppression_requirement, 1995, 0.35).
narrative_ontology:measurement(us_c_su_t2010, us_constitution_meaning__living_constitutionalist_reading, suppression_requirement, 2010, 0.37).
narrative_ontology:measurement(us_c_su_t2025, us_constitution_meaning__living_constitutionalist_reading, suppression_requirement, 2025, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(us_constitution_meaning__living_constitutionalist_reading, us_constitution_meaning__originalist_reading).
narrative_ontology:affects_constraint(us_constitution_meaning__living_constitutionalist_reading, us_constitution_meaning__positivist_reading).

% DUAL FORMULATION NOTE:
% This story is one of three siblings decomposed from the natural-language label 'the meaning of the US Constitution,' per the ε-invariance principle. Each reading (living_constitutionalist, originalist, positivist) has its own ε, beneficiary/victim structure, and claimed type, because measuring 'constitutional meaning' by interpretive method yields structurally distinct constraints with different stakeholders and different extraction profiles. All three must be linked via affects_constraints as members of the us_constitution_meaning kernel family; none is the 'correct' measurement of a single underlying constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
