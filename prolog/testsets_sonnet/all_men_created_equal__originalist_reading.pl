% ============================================================================
% CONSTRAINT STORY: all_men_created_equal__originalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_all_men_created_equal__originalist_reading, []).

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
 *   constraint_id: all_men_created_equal__originalist_reading
 *   human_readable: Originalist Reading: Equality Bounded by 18th-Century Social Taxonomy
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   This story instantiates the originalist reading of the 'all men are
 *   created equal' kernel: the equality clause's scope is fixed by the social
 *   taxonomy operative among the founders at ratification, and expansion
 *   beyond that taxonomy requires formal amendment rather than
 *   reinterpretation. This is a distinct constraint from the universalist
 *   reading (which treats the principle as always-universal, with founder
 *   practice as a betrayal to be corrected by interpretation) and the
 *   textualist paradox reading (which treats the gap between universal
 *   language and restricted application as an unresolved performative
 *   contradiction rather than a scoped rule). Each reading has its own ε and
 *   its own victim set; they are linked here only through
 *   network.affects_constraints and cs_structure.reading_relations, not
 *   merged.
 *
 * KEY AGENTS:
 *   - founding_era_property_holding_elite: primary historical beneficiary — the referent class the taxonomy was built around
 *   - white_male_landowner_descendants: durable secondary beneficiary — inherit interpretive advantage without organizing
 *   - originalist_judicial_interpreters: agenda-setters — administer the interpretive method that fixes scope
 *   - enslaved_and_formerly_enslaved_black_americans, indigenous_nations, women_excluded_from_founding_polity, non_property_holding_men_at_founding: victims — categorically outside the founders' taxonomy of political personhood
 *   - constitutional_historians: analytical observers — assess fit between claimed original intent and documented historical practice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(all_men_created_equal__originalist_reading, 0.81).
domain_priors:suppression_score(all_men_created_equal__originalist_reading, 0.72).
domain_priors:theater_ratio(all_men_created_equal__originalist_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(all_men_created_equal__originalist_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(all_men_created_equal__originalist_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(all_men_created_equal__originalist_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(all_men_created_equal__originalist_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(all_men_created_equal__originalist_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(all_men_created_equal__originalist_reading, tangled_rope).
narrative_ontology:human_readable(all_men_created_equal__originalist_reading, "Originalist Reading: Equality Bounded by 18th-Century Social Taxonomy").
narrative_ontology:topic_domain(all_men_created_equal__originalist_reading, "constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(all_men_created_equal__originalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(all_men_created_equal__originalist_reading, '2a84f864-7ff9-4052-8180-d9b9043a2954').
narrative_ontology:cs_kernel_codification('2a84f864-7ff9-4052-8180-d9b9043a2954', fixed_text).
narrative_ontology:cs_authority_grounding('2a84f864-7ff9-4052-8180-d9b9043a2954', lineage).
narrative_ontology:cs_interpretation_layer_present('2a84f864-7ff9-4052-8180-d9b9043a2954').
narrative_ontology:cs_reading_relation('2a84f864-7ff9-4052-8180-d9b9043a2954', all_men_created_equal__universalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('2a84f864-7ff9-4052-8180-d9b9043a2954', all_men_created_equal__textualist_paradox_reading, influences).
narrative_ontology:cs_axiom('2a84f864-7ff9-4052-8180-d9b9043a2954', foundational, founders_intent_fixes_original_scope).
narrative_ontology:cs_axiom_status(founders_intent_fixes_original_scope, holdable).
narrative_ontology:cs_axiom_grounding('2a84f864-7ff9-4052-8180-d9b9043a2954', founders_intent_fixes_original_scope, conventional).
narrative_ontology:cs_axiom('2a84f864-7ff9-4052-8180-d9b9043a2954', foundational, scope_expansion_requires_formal_amendment_not_reinterpretation).
narrative_ontology:cs_axiom_status(scope_expansion_requires_formal_amendment_not_reinterpretation, holdable).
narrative_ontology:cs_axiom_grounding('2a84f864-7ff9-4052-8180-d9b9043a2954', scope_expansion_requires_formal_amendment_not_reinterpretation, conventional).
narrative_ontology:cs_reference_frame('2a84f864-7ff9-4052-8180-d9b9043a2954', ratification_era_social_taxonomy).
narrative_ontology:cs_drift_state('2a84f864-7ff9-4052-8180-d9b9043a2954', contemporary_equal_protection_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('2a84f864-7ff9-4052-8180-d9b9043a2954', '').
narrative_ontology:cs_kernel_id(all_men_created_equal__originalist_reading, all_men_created_equal).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(all_men_created_equal__originalist_reading, founding_era_property_holding_elite).
narrative_ontology:constraint_beneficiary(all_men_created_equal__originalist_reading, white_male_landowner_descendants).
narrative_ontology:constraint_beneficiary(all_men_created_equal__originalist_reading, originalist_judicial_interpreters).
narrative_ontology:constraint_victim(all_men_created_equal__originalist_reading, enslaved_and_formerly_enslaved_black_americans).
narrative_ontology:constraint_victim(all_men_created_equal__originalist_reading, indigenous_nations).
narrative_ontology:constraint_victim(all_men_created_equal__originalist_reading, women_excluded_from_founding_polity).
narrative_ontology:constraint_victim(all_men_created_equal__originalist_reading, non_property_holding_men_at_founding).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Drafted and ratified the founding language while holding a social taxonomy that categorically excluded enslaved people, women, and non-property-holders from the polity described as 'men.' Their status as the referent class of the equality clause was never in question at drafting; the taxonomy did the work of narrowing 'equal' to 'equal among people already recognized as full persons under law.'
narrative_ontology:constraint_stakeholder(all_men_created_equal__originalist_reading, founding_era_property_holding_elite, beneficiary,
    institutional, civilizational, arbitrage, national).

% Inherit standing within a constitutional order whose founding equality language was scoped to people structurally like them. When courts read scope by founders' intent, this group's historical inclusion becomes the baseline against which claims for expansion must argue, giving them a durable interpretive advantage without needing to organize or assert anything.
narrative_ontology:constraint_stakeholder(all_men_created_equal__originalist_reading, white_male_landowner_descendants, beneficiary,
    powerful, generational, mobile, national).

% Judges and scholars who hold that constitutional text means what it was understood to mean at ratification. They administer the interpretive method that fixes the equality clause's scope to the founders' social taxonomy, treating subsequent expansions as amendments to a bounded original rather than clarifications of an always-universal principle. Their authority rests on claimed fidelity to fixed original meaning.
narrative_ontology:constraint_stakeholder(all_men_created_equal__originalist_reading, originalist_judicial_interpreters, agenda_setter,
    institutional, generational, analytical, national).

% Categorically outside the founders' taxonomy of persons entitled to the equality claim; the Constitution's three-fifths clause and fugitive slave clause encode this exclusion directly. Under the originalist reading, their eventual inclusion required a war and three constitutional amendments rather than being read into the existing text — the exclusion was the original meaning, not a later betrayal of it.
narrative_ontology:constraint_stakeholder(all_men_created_equal__originalist_reading, enslaved_and_formerly_enslaved_black_americans, payer,
    powerless, generational, trapped, national).

% Treated at founding as external nations or, domestically, as outside the equality polity entirely; the founders' taxonomy placed them beyond the referent class of 'men' created equal in the relevant political sense. The originalist reading offers no textual mechanism for their inclusion absent treaty renegotiation or subsequent amendment.
narrative_ontology:constraint_stakeholder(all_men_created_equal__originalist_reading, indigenous_nations, payer,
    powerless, generational, trapped, national).

% Excluded from the founders' referent class of political persons regardless of property or race; suffrage, contract rights, and civic standing were denied as a matter of the taxonomy itself, not oversight. The originalist reading holds this exclusion was the document's original meaning, requiring the Nineteenth Amendment rather than judicial reinterpretation to remedy.
narrative_ontology:constraint_stakeholder(all_men_created_equal__originalist_reading, women_excluded_from_founding_polity, payer,
    powerless, generational, constrained, national).

% White men without property were widely excluded from suffrage under founding-era state law, consistent with a taxonomy that tied political personhood partly to property. Later expansion of suffrage to this group is, on the originalist reading, evidence of amendment and state-level reform rather than evidence that the founding text always meant to include them.
narrative_ontology:constraint_stakeholder(all_men_created_equal__originalist_reading, non_property_holding_men_at_founding, payer,
    powerless, biographical, constrained, national).

% Study the actual social and legal taxonomy operative at ratification — who counted as a full person, who could hold property, who could vote — and assess how closely originalist judicial claims about 'founders' intent' track the documented historical record versus reconstructing a more flattering intent after the fact.
narrative_ontology:constraint_stakeholder(all_men_created_equal__originalist_reading, constitutional_historians, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(all_men_created_equal__originalist_reading, white_male_landowner_descendants).
narrative_ontology:fixing_cost_class(all_men_created_equal__originalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a fixed, historically anchored referent for constitutional meaning, which coordinates judicial interpretation around a stable text rather than shifting contemporary values — reducing the risk that constitutional meaning drifts with every generation's preferences.
% TRANSFER_FUNCTION: Moves interpretive authority and the benefit of legal legitimacy toward groups who were already inside the founders' social taxonomy at ratification, while placing the burden of formal amendment (rather than reinterpretation) on groups excluded from that taxonomy.
% ABSENT_VOICES: Enslaved people, Indigenous nations, and women had no voice in drafting the taxonomy that the originalist reading now treats as authoritative; their absence from the founding conversation is read, under this method, as absence from the equality clause's original scope rather than as an injustice the clause should be read to correct.
% DISAPPEARANCE_RATIONALE: If originalism ceased to govern constitutional interpretation of the equality clause, courts would be freed to read 'all men are created equal' as a universal principle unconstrained by 18th-century categories, dramatically expanding the textual basis for equal protection claims without requiring formal amendment — reallocating substantial interpretive leverage away from groups whose original inclusion the current method presumes.
% FOUNDING_PROBLEM: The founders needed language that expressed a legitimating universal principle (against monarchy and hereditary privilege) while preserving a social and legal order built on slavery, coverture, and property-based suffrage — the taxonomy resolved this tension by defining 'men' narrowly enough to leave the existing order intact.
% FOUNDING_PROBLEM_CORROBORATION: Originalist scholars and jurists attest the founding problem — fixing meaning against interpretive drift — remains live and is faithfully served by this method. Historians of the founding period, descendants of excluded groups, and Reconstruction-era framers themselves (via the text of the Thirteenth, Fourteenth, and Fifteenth Amendments) attest that the founding taxonomy was itself the injustice, not a neutral boundary to be preserved — this corroboration comes from outside the beneficiary class and from the constitutional record generated to correct the original scope.
narrative_ontology:disappearance_verdict(all_men_created_equal__originalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(all_men_created_equal__originalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(all_men_created_equal__originalist_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(all_men_created_equal__originalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(all_men_created_equal__originalist_reading, 0.81, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(all_men_created_equal__originalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(all_men_created_equal__originalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(all_men_created_equal__originalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.81 by 2025, spiking from a 1787 baseline of 0.92, dipping through the civil rights era, and rising again as originalism became a dominant judicial methodology from the 1980s onward) because the reading's core function is to bound a universal-sounding principle to a narrow historical class, redirecting the interpretive benefit of 'equality' toward those already inside that class. Suppression is high (0.72) because the reading requires active judicial and doctrinal enforcement — courts must affirmatively adopt originalism as method and resist textualist or universalist readings that would otherwise be available from the same words. Theater ratio is moderate (0.45) and rising: originalism increasingly presents itself as neutral historical fidelity even where the 'founders' intent' invoked is reconstructed to match a preferred contemporary outcome, rather than rigorously documented from the historical record.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (originalist interpreters), this method is a principled constraint on judicial overreach — a rope solving a genuine coordination problem (interpretive stability against drift). From the payer seats (historically excluded groups), the same method operates as an enforcement mechanism that locks in the founders' exclusions as authoritative unless overridden by supermajority amendment, which is exactly the asymmetric extraction that makes this a tangled rope rather than a pure rope: there is a real coordination function (textual stability) riding alongside a real extraction (naturalizing historical exclusion as original meaning).
 *
 * DIRECTIONALITY LOGIC:
 *   Founding-era elites and their descendants sit near the beneficiary end: the taxonomy was built around their inclusion, and the originalist method makes that inclusion the fixed reference point against which all later claims are measured. Excluded groups sit near the full-target end: trapped or constrained exit, no textual path to inclusion short of formal amendment, and the burden of historical proof falls on them to show founding-era intent supports their claim — an evidentiary structure stacked against groups who left no drafting-room record because they were excluded from the drafting room.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — anchoring meaning against interpretive drift — may remain partially live as a general judicial-philosophy concern, but the specific taxonomy this reading anchors to (which persons counted as full political persons in 1787) is widely regarded outside the beneficiary class as the injustice itself, not a neutral technical boundary. The Reconstruction Amendments and Nineteenth Amendment are themselves evidence that the political system judged the original taxonomy insufficient — yet the originalist method treats those amendments as bounded additions to an otherwise-fixed original scope rather than as evidence the 'original' reading was defective. This is the seat divergence the classification exists to register: a claimed rope (textual stability) computing, once beneficiary/victim structure is supplied, as extraction riding on a real but narrower coordination function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    originalist_intent_is_historical_fact_or_constructed_narrative,
    'Is ''founders'' intent'' as invoked by originalist jurisprudence a rigorously documented historical fact about ratification-era understanding, or a reconstructed narrative shaped to match preferred contemporary outcomes?',
    'Comparative analysis of originalist judicial opinions against primary ratification-era sources (convention debates, contemporaneous dictionaries, state ratification debates) to assess whether claimed original meanings are consistently derivable from the documented record or vary opportunistically by case.',
    'If originalist intent-claims are frequently underdetermined or contested by the historical record itself, the reading''s theater_ratio should be substantially higher and its coordination claim (neutral historical fidelity) correspondingly weaker — sharpening the tangled_rope classification toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(originalist_intent_is_historical_fact_or_constructed_narrative, empirical, 'Whether originalism''s core evidentiary claim is historically rigorous or retrospectively constructed.').

omega_variable(
    kernel_reading_selection_is_itself_contestable,
    'Is the choice to read the equality kernel through the founders'' taxonomy (rather than the universalist or textualist-paradox framing) itself a neutral interpretive choice, or does the selection of originalism as method already encode a preference for outcomes favorable to historically included groups?',
    'Track which political coalitions have historically advocated for originalist versus universalist interpretive methods, and whether advocacy correlates with material interest in the scope outcome each method produces.',
    'If method-selection correlates strongly with interest in outcome, this supports treating originalism''s beneficiary structure as evidence of motivated reasoning rather than incidental to a neutral interpretive commitment — this is exactly the committer-structure ambiguity the kernel/reading frame is designed to surface rather than resolve within a single constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_is_itself_contestable, conceptual, 'Whether interpretive-method selection is itself value-neutral or outcome-motivated.').

omega_variable(
    amendment_versus_reinterpretation_as_the_only_remedy,
    'Does treating formal constitutional amendment as the sole legitimate mechanism for expanding the equality clause''s scope (rather than judicial reinterpretation) reflect a defensible theory of democratic legitimacy, or does it function to make correction of founding-era exclusions maximally costly for the groups excluded?',
    'Compare the historical cost and success rate of amendment-based versus interpretation-based expansions of equal protection, controlling for the relative political power of the groups seeking each.',
    'If amendment is shown to be systematically more costly and less achievable for less-organized excluded groups, the originalist insistence on amendment-only correction should be read as part of the constraint''s suppression mechanism rather than a neutral procedural preference.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(amendment_versus_reinterpretation_as_the_only_remedy, preference, 'Whether amendment-only correction is neutral procedure or embedded suppression mechanism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(all_men_created_equal__originalist_reading, 1787, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(all__tr_t1787, all_men_created_equal__originalist_reading, theater_ratio, 1787, 0.2).
narrative_ontology:measurement(all__tr_t1830, all_men_created_equal__originalist_reading, theater_ratio, 1830, 0.25).
narrative_ontology:measurement(all__tr_t1865, all_men_created_equal__originalist_reading, theater_ratio, 1865, 0.3).
narrative_ontology:measurement(all__tr_t1920, all_men_created_equal__originalist_reading, theater_ratio, 1920, 0.35).
narrative_ontology:measurement(all__tr_t1965, all_men_created_equal__originalist_reading, theater_ratio, 1965, 0.4).
narrative_ontology:measurement(all__tr_t2000, all_men_created_equal__originalist_reading, theater_ratio, 2000, 0.42).
narrative_ontology:measurement(all__tr_t2025, all_men_created_equal__originalist_reading, theater_ratio, 2025, 0.45).

% Extraction over time
narrative_ontology:measurement(all__be_t1787, all_men_created_equal__originalist_reading, base_extractiveness, 1787, 0.92).
narrative_ontology:measurement(all__be_t1830, all_men_created_equal__originalist_reading, base_extractiveness, 1830, 0.9).
narrative_ontology:measurement(all__be_t1865, all_men_created_equal__originalist_reading, base_extractiveness, 1865, 0.78).
narrative_ontology:measurement(all__be_t1920, all_men_created_equal__originalist_reading, base_extractiveness, 1920, 0.7).
narrative_ontology:measurement(all__be_t1965, all_men_created_equal__originalist_reading, base_extractiveness, 1965, 0.6).
narrative_ontology:measurement(all__be_t2000, all_men_created_equal__originalist_reading, base_extractiveness, 2000, 0.55).
narrative_ontology:measurement(all__be_t2025, all_men_created_equal__originalist_reading, base_extractiveness, 2025, 0.81).

% Suppression requirement over time
narrative_ontology:measurement(all__su_t1787, all_men_created_equal__originalist_reading, suppression_requirement, 1787, 0.85).
narrative_ontology:measurement(all__su_t1830, all_men_created_equal__originalist_reading, suppression_requirement, 1830, 0.85).
narrative_ontology:measurement(all__su_t1865, all_men_created_equal__originalist_reading, suppression_requirement, 1865, 0.9).
narrative_ontology:measurement(all__su_t1920, all_men_created_equal__originalist_reading, suppression_requirement, 1920, 0.75).
narrative_ontology:measurement(all__su_t1965, all_men_created_equal__originalist_reading, suppression_requirement, 1965, 0.65).
narrative_ontology:measurement(all__su_t2000, all_men_created_equal__originalist_reading, suppression_requirement, 2000, 0.6).
narrative_ontology:measurement(all__su_t2025, all_men_created_equal__originalist_reading, suppression_requirement, 2025, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(all_men_created_equal__originalist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(all_men_created_equal__originalist_reading, all_men_created_equal__universalist_reading).
narrative_ontology:affects_constraint(all_men_created_equal__originalist_reading, all_men_created_equal__textualist_paradox_reading).

% DUAL FORMULATION NOTE:
% This story is one of three readings of the all_men_created_equal kernel. The universalist_reading treats the clause as always-universal and reads founder practice as a betrayal correctable by interpretation (lower extraction toward excluded groups, higher legitimacy pressure on originalist courts). The textualist_paradox_reading treats the universal-language/restricted-application gap as an unresolved contradiction rather than a scoped rule or an expansive correction. All three share the same kernel text but instantiate structurally distinct constraints with distinct ε, distinct beneficiary/victim sets, and distinct classifications; they must not be merged or averaged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
