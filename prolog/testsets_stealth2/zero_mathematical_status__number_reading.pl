% ============================================================================
% CONSTRAINT STORY: zero_mathematical_status__number_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_zero_mathematical_status__number_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: zero_mathematical_status__number_reading
 *   human_readable: Zero as a Number with Determinate Arithmetic (Number Reading)
 *   domain: conceptual_history/history_of_mathematics
 *
 * SUMMARY:
 *   This story instantiates the number_reading of the kernel
 *   zero_mathematical_status: the claim, first codified by Brahmagupta (628
 *   CE) and consolidated by the axiomatic tradition, that zero is a full
 *   member of the number system governed by determinate arithmetic (a+0=a,
 *   a-0=a, a*0=0, division by zero indeterminate). Assessed by this reading's
 *   own lights, the constraint is a mountain: the rules are forced by
 *   consistency (given an additive identity and distributivity, a*0=0 is a
 *   theorem, not a choice), they would hold regardless of who defends them,
 *   and no party collects from their operation. The claim/metric gap is
 *   deliberate and small here: claimed_type mountain, metrics describing
 *   near-total absence of extraction, suppression, and theater. Family note:
 *   the colloquial label 'zero' decomposes into three structurally distinct
 *   constraints — this reading (arithmetic membership, epsilon approximately
 *   0.02), placeholder_reading (a notational convention, whose epsilon would
 *   measure the cost of the notation itself), and parmenidean_rejection (an
 *   enforced ontological exclusion, whose epsilon would measure the cost that
 *   exclusion imposed on Greek and scholastic computation). Each is a
 *   separate file; this one links to both via network.affects_constraints.
 *
 * KEY AGENTS:
 *   - - brahmagupta_tradition_astronomers: Codifying beneficiary ([organized]/[constrained]) — first stated the rules as answers to computational difficulties in positional astronomy
 *   - - islamic_golden_age_algebraists: Transmitting beneficiary ([organized]/[constrained]) — extended the rules into equation-solving and carried them westward
 *   - - medieval_european_merchants: Late-adopting beneficiary ([moderate]/[mobile]) — needed the rules for ledgers; retained a real alternative (the counter-abacus) for centuries
 *   - - greek_geometric_tradition: Excluded objector ([institutional]/[identity_locked]) — held that nothing cannot be a quantity; the arrangement's historic opposition
 *   - - modern_mathematicians: Constitutive beneficiary ([institutional]/[constrained]) — inherit the rules as axioms and theorems
 *   - - philosophers_of_mathematics: Analytical observer ([analytical]/[analytical]) — adjudicate the discovered-versus-constructed question
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(zero_mathematical_status__number_reading, 0.02).
domain_priors:suppression_score(zero_mathematical_status__number_reading, 0.02).
domain_priors:theater_ratio(zero_mathematical_status__number_reading, 0.02).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(zero_mathematical_status__number_reading, extractiveness, 0.02).
narrative_ontology:constraint_metric(zero_mathematical_status__number_reading, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(zero_mathematical_status__number_reading, theater_ratio, 0.02).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(zero_mathematical_status__number_reading, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(zero_mathematical_status__number_reading, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(zero_mathematical_status__number_reading, mountain).
narrative_ontology:human_readable(zero_mathematical_status__number_reading, "Zero as a Number with Determinate Arithmetic (Number Reading)").
narrative_ontology:topic_domain(zero_mathematical_status__number_reading, "conceptual_history/history_of_mathematics").

domain_priors:emerges_naturally(zero_mathematical_status__number_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(zero_mathematical_status__number_reading, '977621ec-d6ff-42c9-8221-25360c923699').
narrative_ontology:cs_kernel_codification('977621ec-d6ff-42c9-8221-25360c923699', formalized).
narrative_ontology:cs_authority_grounding('977621ec-d6ff-42c9-8221-25360c923699', expertise).
narrative_ontology:cs_interpretation_layer_present('977621ec-d6ff-42c9-8221-25360c923699').
narrative_ontology:cs_reading_relation('977621ec-d6ff-42c9-8221-25360c923699', zero_mathematical_status__parmenidean_rejection, forecloses).
narrative_ontology:cs_reading_relation('977621ec-d6ff-42c9-8221-25360c923699', zero_mathematical_status__placeholder_reading, forecloses).
narrative_ontology:cs_axiom('977621ec-d6ff-42c9-8221-25360c923699', foundational, zero_is_operable_quantity).
narrative_ontology:cs_axiom_status(zero_is_operable_quantity, holdable).
narrative_ontology:cs_axiom_grounding('977621ec-d6ff-42c9-8221-25360c923699', zero_is_operable_quantity, instrumental).
narrative_ontology:cs_axiom('977621ec-d6ff-42c9-8221-25360c923699', foundational, consistency_forces_zero_laws).
narrative_ontology:cs_axiom_status(consistency_forces_zero_laws, holdable).
narrative_ontology:cs_axiom_grounding('977621ec-d6ff-42c9-8221-25360c923699', consistency_forces_zero_laws, conventional).
narrative_ontology:cs_reference_frame('977621ec-d6ff-42c9-8221-25360c923699', zero_as_constitutive_number).
narrative_ontology:cs_drift_state('977621ec-d6ff-42c9-8221-25360c923699', contemporary_axiomatic_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('977621ec-d6ff-42c9-8221-25360c923699', '').
narrative_ontology:cs_kernel_id(zero_mathematical_status__number_reading, zero_mathematical_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(zero_mathematical_status__number_reading, brahmagupta_tradition_astronomers).
narrative_ontology:constraint_beneficiary(zero_mathematical_status__number_reading, islamic_golden_age_algebraists).
narrative_ontology:constraint_beneficiary(zero_mathematical_status__number_reading, medieval_european_merchants).
narrative_ontology:constraint_beneficiary(zero_mathematical_status__number_reading, modern_mathematicians).
narrative_ontology:constraint_vindicates(zero_mathematical_status__number_reading, distributivity_forces_annihilation).
narrative_ontology:constraint_vindicates(zero_mathematical_status__number_reading, additive_identity_uniqueness).
narrative_ontology:constraint_vindicates(zero_mathematical_status__number_reading, positional_notation_computability).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Seventh-century Indian astronomer-mathematicians working in the Siddhanta tradition. In the Brahmasphutasiddhanta (628 CE) they stated rules for shunya — addition, subtraction, multiplication, and the indeterminate status of division by it — as answers to named difficulties in planetary computation, where differences between positions can be nil. They gained a number system closed under subtraction and a positional notation that computes rather than merely records. Later schools adopted their formulation as canonical; they did not administer it so much as state what the computations already demanded.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__number_reading, brahmagupta_tradition_astronomers, beneficiary,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(zero_mathematical_status__number_reading, brahmagupta_tradition_astronomers, agenda_setter).

% Baghdad-lineage algebraists from al-Khwarizmi onward. They received the Indian rules through translation, extended them into equation-solving and the arithmetic of powers, and transmitted the symbol westward as al-sifr. Their algebra presupposes a quantity that can stand alone as a result; without it, equation-solving stalls at every vanishing term. Leaving the practice would have meant abandoning algebra itself.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__number_reading, islamic_golden_age_algebraists, beneficiary,
    organized, generational, constrained, continental).

% Merchant and banking households from Fibonacci's Liber Abaci (1202) onward. They needed a written arithmetic that cancels debts and aligns columns of unequal length; the counter-abacus they already owned handled neither well. Adoption took centuries because the alternative — reckoning counters — worked adequately and because numeral alteration was a known fraud vector. Those who switched gained auditable ledgers; those who waited paid nothing except efficiency.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__number_reading, medieval_european_merchants, beneficiary,
    moderate, biographical, mobile, continental).

% The Parmenidean-Aristotelian lineage and the scholastics who inherited its ontology. Number meant a multitude of units; 'nothing' could not be counted, and a symbol for absence was a notation, not a quantity. They built geometry and proportion theory that never required zero and treated its admission as a category error. Leaving that position would have dissolved the framework that made their mathematics intelligible, so they did not leave it; their refusal is preserved in the sources and constitutes the oldest recorded objection to this arrangement.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__number_reading, greek_geometric_tradition, excluded,
    institutional, civilizational, identity_locked, continental).

% Practitioners from Peano and Dedekind forward. The axioms open with zero; the field axioms fix its behavior as theorems; analysis, algebra, and topology each presuppose it. They exercise the rules in every proof and computation. There is no rival number system to decamp to that keeps distributivity and consistency while denying the rules, so practice continues inside them by necessity rather than by choice.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__number_reading, modern_mathematicians, beneficiary,
    institutional, generational, constrained, global).

% Philosophers of mathematics and historians of the exact sciences. They examine whether zero's numberhood was discovered or constructed, track how the competing readings of the zero question partition historically, and attest the documentary record of codification and transmission. They collect nothing from the arrangement's operation and bear none of its costs; their seat is analytical.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__number_reading, philosophers_of_mathematics, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(zero_mathematical_status__number_reading, diffuse).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Unifies quantitative practice around a single treatment of nullity: every expression containing 'nothing' — empty positional columns, cancelled debts, vanishing terms — receives a determinate arithmetic value, so that scribes, astronomers, and merchants in different places compute identically from the same written form. It closes the number system under subtraction and makes positional notation computable rather than merely record-keeping.
% TRANSFER_FUNCTION: Nothing transfers. The arrangement moves no money, work, attention, or status between parties; its entire effect is to make certain computations possible and consistent for whoever performs them. Any story in which zero's numberhood moves value between parties would be describing a different arrangement — a currency or ledger convention — not this one.
% ABSENT_VOICES: The Parmenidean lineage — Greek philosophers and the scholastics who inherited their ontology — would object that 'nothing' cannot be a quantity and that the arrangement reifies a notation into a being. Historically they stood outside the computational communities that adopted the rules; today their successors sit in philosophy departments and in the sibling readings of this kernel (parmenidean_rejection, placeholder_reading). Their objection is recorded in the historical record itself: Greek arithmetic's refusal to admit zero is the strongest external attestation that the founding problem was real.
% DISAPPEARANCE_RATIONALE: If zero's numberhood and its rules failed overnight, every arrangement built on arithmetic collapses: positional notation loses computability, algebra loses its additive identity, calculus loses its limiting machinery, ledgers lose debt-cancellation, and digital computation — which encodes everything in binary, zero included — halts. The world does not revert to a pre-zero steady state; it loses the foundation. The modal caveat belongs in commentary: the fact cannot literally fail, only be abandoned, and abandonment is what the Parmenidean reading attempted, at the price of algebraic paralysis.
% FOUNDING_PROBLEM: Positional numeration and commercial astronomy kept producing expressions containing 'nothing' — empty columns, sums that cancel, quantities that are nil — with no determinate way to compute through them. The founding problem was to give absence an arithmetic: to say what rules govern a symbol for nothing so that computation neither stalls nor contradicts itself.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set by the documentary record of transmission: the Brahmasphutasiddhanta (ch. 18) states the rules as answers to named computational difficulties; al-Khwarizmi's algebra and the Latin algorismus tracts transmit them as solutions to problems European computists demonstrably had; and the Greek refusal to admit zero — the strongest interested opposition — attests from outside that the difficulty was live and its resolution contested. Because no party collects rents from the arrangement (extraction near zero), the usual conflict-of-interest worry about beneficiary attestation does not bind; still, the corroboration cited here is textual-historical, not practitioner self-report.
narrative_ontology:disappearance_verdict(zero_mathematical_status__number_reading, world_rearranges).
narrative_ontology:founding_problem_status(zero_mathematical_status__number_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(zero_mathematical_status__number_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(zero_mathematical_status__number_reading, 'none', 1).
narrative_ontology:epsilon_provenance(zero_mathematical_status__number_reading, 0.02, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(zero_mathematical_status__number_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(zero_mathematical_status__number_reading, ExtMetricName, E),
    domain_priors:suppression_score(zero_mathematical_status__number_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(zero_mathematical_status__number_reading),
    narrative_ontology:constraint_metric(zero_mathematical_status__number_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(zero_mathematical_status__number_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(zero_mathematical_status__number_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction 0.02: the rules take nothing from anyone; the residue covers the pedagogical cost of teaching a concept students find paradoxical, which is a cost of the concept, not extraction by it. Suppression 0.02: nothing enforces the rules; they are self-warranting demonstrations, and the historical episodes that look like enforcement (Florence's 1299 ban on Arabic numerals) targeted fraud-via-numeral-alteration, failed within decades, and suppressed a notation's rivals rather than sustaining this arrangement. Theater 0.02: no ritual maintenance exists; every use is functional. Accessibility_collapse 0.95: once zero is admitted alongside the additive identity and distributivity, the zero laws are forced — a practitioner who wants a*0 != 0 must surrender distributivity or consistency, and no working alternative has ever been sustained. Resistance 0.08: the substantial historical resistance (Greek avoidance, medieval suspicion) opposed adoption of zero, not the content of the rules; against the rules themselves, once admitted, resistance is nil. Boltzmann coordination type information_standard: positional notation is an encoding standard, and the zero laws are what complete the encoding so that every written form has a determinate computational reading; the type default floor applies. Measurement series are deliberately flat on one shared seven-point grid: for a mountain, the absence of drift IS the temporal signature, and the flat lines distinguish this constraint from extractive ones whose histories ratchet. Suppression_requirement series are intentionally not authored: the enforcement picture is static (there never was enforcement machinery), so the scalar carries the whole story.
 *
 * PERSPECTIVAL GAP:
 *   Seat divergence is unusually narrow because the arrangement extracts nothing: every seated party computes mountain or near-mountain. The one real divergence is historical and directional: from inside the computational seats (Indian, Islamic, mercantile, modern), the arrangement appears as liberation — problems that previously stalled became solvable. From the excluded Greek seat, the same proposition appears as an ontological error dressed as arithmetic — a category mistake to be refused, not a fact to be learned. The engine computes this from the structural data: identical epsilon read from opposite sides of an adoption boundary. Note also that suppression is authored as a raw structural property and is not scaled by power or scope; the near-zero value is a claim about the arrangement itself, not about any seat's experience of it.
 *
 * DIRECTIONALITY LOGIC:
 *   Four beneficiary groups are declared and no victims: every declaration maps to a real structural relationship. Astronomers gained closure under subtraction (planetary differences that vanish); algebraists gained the identity element that makes equation-solving mechanical; merchants gained debt-cancellation and positional ledgers; modern practitioners inherit the whole edifice. Directionality therefore derives low d for every seated agent, and with epsilon approximately 0.02 effective extraction is negligible from every seat — the engine's arithmetic agrees with the phenomenology: nobody pays. The excluded Greek seat derives neither beneficiary-low nor victim-high d: they bore no cost from the arrangement's operation, only the cost of refusing it, which the beneficiary/victim vocabulary registers as exclusion, not extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — giving absence an arithmetic — was solved within roughly two centuries of codification and is dead as an open problem. The arrangement nonetheless persists at full function: every act of computation exercises it. This is the crucial disambiguation the classification enforces: founding_problem_status=dead records that the problem is closed; mandatrophy tracks whether the arrangement's function has atrophied. Here the function is maximally alive, so mandatrophy_resolved is authored false despite the dead founding problem — persistence-by-necessity, not persistence-by-inertia. A piton reading would require the rules to survive only as performance while nothing depends on them; the opposite is true: everything depends on them and nothing performs them. The mismatch consumer pairing status=dead with verdict=world_rearranges should resolve as foundation, not zombie, on inspection of the flat theater series.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    discovered_or_constructed_zero_laws,
    'Are the zero laws a discovered feature of any consistent quantitative system, or a constructed convention adopted for computational convenience?',
    'Comparative axiomatization survey: if every adequate number system contains a unique element satisfying the zero laws as consequences of its axioms, the discovery side strengthens; if coherent rival systems (free logics, term-formalisms, paraconsistent arithmetics) sustainably dispense with them, the construction side strengthens.',
    'Resolves the false-summit ambiguity this story''s beneficiary declarations trigger: a discovered-law verdict supports the mountain claim against reclassification; a constructed-convention verdict supports reading the arrangement as a coordinated adoption with identifiable beneficiaries — closer to the tangled_rope override target.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(discovered_or_constructed_zero_laws, conceptual, 'Whether zero''s numberhood is natural law or coordinated construction (FSM-mandated omega).').

omega_variable(
    kernel_reading_structural_delta,
    'What structurally changes if a sibling reading of kernel zero_mathematical_status were adopted instead of this one?',
    'Author the sibling files and compare: placeholder_reading shifts the coordination type toward a bare information_standard and shrinks the beneficiary set to users of positional notation; parmenidean_rejection converts the arrangement into an enforced exclusion whose epsilon measures the cost the ban imposed on Greek and scholastic computation.',
    'This story''s epsilon (approximately 0.02) is invariant under re-description of the same practice, but the sibling stories'' epsilon, victims, and types differ sharply; the family comparison, not any single file, carries the kernel''s classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structural_delta, conceptual, 'Committer-frame omega: this constraint is one reading of the zero kernel; siblings instantiate different constraints.').

omega_variable(
    historical_resistance_target,
    'Was the documented historical resistance aimed at this arrangement (zero''s numberhood) or at adjacent practices (Arabic numeral forms, fraud via alteration, abacist craft defense)?',
    'Close reading of the resistance record: Florentine and later European bans cite forgery-via-numeral-alteration and guild protection; Greek sources cite ontology. Separating targets reattributes the resistance metric.',
    'If most resistance targeted adjacent practices, the authored resistance (0.08) is if anything an overestimate for this arrangement; if a substantial strand targeted numberhood itself, the Parmenidean reading''s historical force is greater than its modern marginality suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_resistance_target, empirical, 'Attribution of historical resistance to the arrangement versus its neighbors.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(zero_mathematical_status__number_reading, 628, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(zero_tr_t628, zero_mathematical_status__number_reading, theater_ratio, 628, 0.02).
narrative_ontology:measurement_basis(zero_tr_t628, observed).
narrative_ontology:measurement(zero_tr_t900, zero_mathematical_status__number_reading, theater_ratio, 900, 0.02).
narrative_ontology:measurement_basis(zero_tr_t900, observed).
narrative_ontology:measurement(zero_tr_t1200, zero_mathematical_status__number_reading, theater_ratio, 1200, 0.02).
narrative_ontology:measurement_basis(zero_tr_t1200, observed).
narrative_ontology:measurement(zero_tr_t1450, zero_mathematical_status__number_reading, theater_ratio, 1450, 0.02).
narrative_ontology:measurement_basis(zero_tr_t1450, observed).
narrative_ontology:measurement(zero_tr_t1650, zero_mathematical_status__number_reading, theater_ratio, 1650, 0.02).
narrative_ontology:measurement_basis(zero_tr_t1650, observed).
narrative_ontology:measurement(zero_tr_t1850, zero_mathematical_status__number_reading, theater_ratio, 1850, 0.02).
narrative_ontology:measurement_basis(zero_tr_t1850, observed).
narrative_ontology:measurement(zero_tr_t2026, zero_mathematical_status__number_reading, theater_ratio, 2026, 0.02).
narrative_ontology:measurement_basis(zero_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(zero_be_t628, zero_mathematical_status__number_reading, base_extractiveness, 628, 0.02).
narrative_ontology:measurement_basis(zero_be_t628, observed).
narrative_ontology:measurement(zero_be_t900, zero_mathematical_status__number_reading, base_extractiveness, 900, 0.02).
narrative_ontology:measurement_basis(zero_be_t900, observed).
narrative_ontology:measurement(zero_be_t1200, zero_mathematical_status__number_reading, base_extractiveness, 1200, 0.02).
narrative_ontology:measurement_basis(zero_be_t1200, observed).
narrative_ontology:measurement(zero_be_t1450, zero_mathematical_status__number_reading, base_extractiveness, 1450, 0.02).
narrative_ontology:measurement_basis(zero_be_t1450, observed).
narrative_ontology:measurement(zero_be_t1650, zero_mathematical_status__number_reading, base_extractiveness, 1650, 0.02).
narrative_ontology:measurement_basis(zero_be_t1650, observed).
narrative_ontology:measurement(zero_be_t1850, zero_mathematical_status__number_reading, base_extractiveness, 1850, 0.02).
narrative_ontology:measurement_basis(zero_be_t1850, observed).
narrative_ontology:measurement(zero_be_t2026, zero_mathematical_status__number_reading, base_extractiveness, 2026, 0.02).
narrative_ontology:measurement_basis(zero_be_t2026, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(zero_mathematical_status__number_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(zero_mathematical_status__number_reading, information_standard).
narrative_ontology:affects_constraint(zero_mathematical_status__number_reading, parmenidean_rejection).
narrative_ontology:affects_constraint(zero_mathematical_status__number_reading, placeholder_reading).

% DUAL FORMULATION NOTE:
% Family decomposition of the colloquial label 'zero': number_reading (this file) authors epsilon for zero's integrated arithmetic membership (approximately 0.02, claimed mountain); placeholder_reading authors epsilon for the notational convention itself; parmenidean_rejection authors epsilon for the enforced ontological exclusion. Chronology runs placeholder convention (empty-place marks) before arithmetic codification before axiomatic consolidation, and the Parmenidean exclusion is upstream in the Greek lineage; both siblings are cited as evidence in contests over this reading, which is why edges run from this file to both. Each member carries its own epsilon, stakeholders, and type; no file hedges across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
