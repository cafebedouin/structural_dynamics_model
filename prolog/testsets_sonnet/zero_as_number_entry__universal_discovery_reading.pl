% ============================================================================
% CONSTRAINT STORY: zero_as_number_entry__universal_discovery_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_zero_as_number_entry__universal_discovery_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: zero_as_number_entry__universal_discovery_reading
 *   human_readable: Zero-as-Number Is a Timeless Logical Consequence of Positional Notation (Universal Discovery Reading)
 *   domain: history_of_mathematics/philosophy_of_mathematics
 *
 * SUMMARY:
 *   This constraint instantiates the universal_discovery_reading of the
 *   zero_as_number_entry kernel: zero-as-number is treated as a logically
 *   necessary consequence of positional notation combined with standard
 *   arithmetic operations, a truth that was always mathematically available
 *   in the same sense the Pythagorean theorem was always true before
 *   Pythagoras. Indian mathematicians (documented from Bakhshali-era usage
 *   through Brahmagupta's formal rules for zero in the 7th century CE) are
 *   credited as the first to formalize it; Europeans arrived at the same
 *   operational treatment later, whether via transmission (Al-Khwarizmi,
 *   Fibonacci) or, on this reading's account, via a parallel route they could
 *   equally have taken indigenously given sufficient engagement with
 *   positional arithmetic. Historical priority of discovery is treated as
 *   fully decoupled from the ontological status of the discovered fact — the
 *   same decoupling used for any mathematical theorem discovered
 *   independently by multiple cultures or individuals (e.g., calculus,
 *   Newton/Leibniz). This is the low-ε, high-accessibility-collapse reading
 *   of the kernel: it authors zero-as-number as a mountain because the
 *   metaphysical stance it takes is that no cultural or cognitive scaffolding
 *   was strictly necessary for it to be found — only sufficient mathematical
 *   engagement with the right structure (positional notation) was required,
 *   and that structure exerts the same logical pull on any sufficiently
 *   developed number system. The sibling readings
 *   (contingent_thinkability_reading, hybrid_scaffolding_reading) dispute
 *   exactly this premise; they are authored as separate constraints per the
 *   ε-invariance principle, not folded into this one.
 *
 * KEY AGENTS:
 *   - indian_mathematical_tradition_priority_claim: first formalizers, historical priority holder, no ongoing extraction
 *   - global_mathematical_practice: universal downstream beneficiary of the mathematical fact, symmetric non-rivalrous benefit
 *   - european_mathematicians_post_transmission: later arrivers, treated here as independent-capable discoverers rather than passive recipients
 *   - historians_of_mathematics: analytical observers separating the empirical priority question from the ontological status question
 *   - contingent_thinkability_theorists: excluded voice, holders of the sibling reading that disputes independent-discoverability
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(zero_as_number_entry__universal_discovery_reading, 0.04).
domain_priors:suppression_score(zero_as_number_entry__universal_discovery_reading, 0.03).
domain_priors:theater_ratio(zero_as_number_entry__universal_discovery_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(zero_as_number_entry__universal_discovery_reading, extractiveness, 0.04).
narrative_ontology:constraint_metric(zero_as_number_entry__universal_discovery_reading, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(zero_as_number_entry__universal_discovery_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(zero_as_number_entry__universal_discovery_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(zero_as_number_entry__universal_discovery_reading, resistance, 0.12).

% --- Constraint claim ---
narrative_ontology:constraint_claim(zero_as_number_entry__universal_discovery_reading, mountain).
narrative_ontology:human_readable(zero_as_number_entry__universal_discovery_reading, "Zero-as-Number Is a Timeless Logical Consequence of Positional Notation (Universal Discovery Reading)").
narrative_ontology:topic_domain(zero_as_number_entry__universal_discovery_reading, "history_of_mathematics/philosophy_of_mathematics").

domain_priors:emerges_naturally(zero_as_number_entry__universal_discovery_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(zero_as_number_entry__universal_discovery_reading, '416809c4-f10a-48fc-8564-3b23b909da40').
narrative_ontology:cs_kernel_codification('416809c4-f10a-48fc-8564-3b23b909da40', distributed).
narrative_ontology:cs_authority_grounding('416809c4-f10a-48fc-8564-3b23b909da40', expertise).
narrative_ontology:cs_interpretation_layer_present('416809c4-f10a-48fc-8564-3b23b909da40').
narrative_ontology:cs_reading_relation('416809c4-f10a-48fc-8564-3b23b909da40', zero_as_number_entry__contingent_thinkability_reading, coexists_with).
narrative_ontology:cs_reading_relation('416809c4-f10a-48fc-8564-3b23b909da40', zero_as_number_entry__hybrid_scaffolding_reading, influences).
narrative_ontology:cs_axiom('416809c4-f10a-48fc-8564-3b23b909da40', foundational, priority_independent_ontological_status).
narrative_ontology:cs_axiom_status(priority_independent_ontological_status, holdable).
narrative_ontology:cs_axiom_grounding('416809c4-f10a-48fc-8564-3b23b909da40', priority_independent_ontological_status, deontological).
narrative_ontology:cs_axiom('416809c4-f10a-48fc-8564-3b23b909da40', foundational, mathematical_necessity_is_culture_independent).
narrative_ontology:cs_axiom_status(mathematical_necessity_is_culture_independent, holdable).
narrative_ontology:cs_axiom_grounding('416809c4-f10a-48fc-8564-3b23b909da40', mathematical_necessity_is_culture_independent, empirically_contingent).
narrative_ontology:cs_reference_frame('416809c4-f10a-48fc-8564-3b23b909da40', mathematical_platonist_discovery_model).
narrative_ontology:cs_drift_state('416809c4-f10a-48fc-8564-3b23b909da40', post_postcolonial_historiography_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('416809c4-f10a-48fc-8564-3b23b909da40', '').
narrative_ontology:cs_kernel_id(zero_as_number_entry__universal_discovery_reading, zero_as_number_entry).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(zero_as_number_entry__universal_discovery_reading, global_mathematical_practice).
narrative_ontology:constraint_beneficiary(zero_as_number_entry__universal_discovery_reading, indian_mathematical_tradition_priority_claim).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(zero_as_number_entry__universal_discovery_reading, european_mathematicians_post_transmission).
narrative_ontology:constraint_vindicates(zero_as_number_entry__universal_discovery_reading, mathematical_platonism_discovery_model).
narrative_ontology:constraint_vindicates(zero_as_number_entry__universal_discovery_reading, priority_independent_ontological_status_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Historically credited (Brahmagupta, and earlier Bakhshali/pre-Gupta zero-glyph usage) with the first formal arithmetic treatment of zero as a number with defined operational rules. Under this reading, that priority is a historical-discovery fact — first to arrive at a truth that was always there — rather than an act of invention that could have gone otherwise. The tradition benefits reputationally from being the first discoverer, but the discovery itself confers no ongoing extraction; it is a credit, not a rent.
narrative_ontology:constraint_stakeholder(zero_as_number_entry__universal_discovery_reading, indian_mathematical_tradition_priority_claim, beneficiary,
    analytical, civilizational, analytical, global).

% All subsequent mathematics that uses positional notation and arithmetic operations benefits from zero being available as a number, regardless of who found it or when. No mathematician anywhere pays a cost for zero's discovery; the benefit is symmetric and non-rivalrous. There is no exit from arithmetic truth and none is needed — the truth is simply used.
narrative_ontology:constraint_stakeholder(zero_as_number_entry__universal_discovery_reading, global_mathematical_practice, beneficiary,
    analytical, civilizational, arbitrage, universal).

% Encountered and adopted zero-as-number later (via Al-Khwarizmi's transmission of Indian arithmetic, then Fibonacci's Liber Abaci). Under this reading, their later arrival reflects historical timing of discovery, not an inability to have found it independently — the mathematical fact was equally available to them at any point they engaged seriously with positional arithmetic. Their situation is that of a second (or parallel) discoverer, not a recipient of a gift that altered what was ontologically true.
narrative_ontology:constraint_stakeholder(zero_as_number_entry__universal_discovery_reading, european_mathematicians_post_transmission, beneficiary,
    analytical, civilizational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(zero_as_number_entry__universal_discovery_reading, european_mathematicians_post_transmission, observer).

% Study the documentary record of when and where zero-as-number first appears in formalized arithmetic rules. Under this reading, their job is to correctly attribute priority of discovery — a historical-empirical question — while treating the ontological status of the mathematical fact as settled independently of that history. They can affirm Indian priority without that affirmation bearing on whether the fact itself was 'constructed' by any culture.
narrative_ontology:constraint_stakeholder(zero_as_number_entry__universal_discovery_reading, historians_of_mathematics, observer,
    analytical, generational, analytical, global).

% Hold that Aristotelian/Greek metaphysical commitments (the impossibility of void-as-quantity, resistance to treating 'nothing' as a countable magnitude) constituted a genuine conceptual barrier that indigenous European mathematics could not have crossed without contact. This reading treats their account as an alternative framing not adopted here — they would object that calling this a 'discovery' by both traditions independently erases a real asymmetry in conceptual availability. They are not part of this constraint's structural account; they are the sibling reading.
narrative_ontology:constraint_stakeholder(zero_as_number_entry__universal_discovery_reading, contingent_thinkability_theorists, excluded,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: No coordination problem is being solved by this constraint in the ordinary sense — it is a claim about the ontological status of a mathematical truth. Insofar as there is a coordination function, it is scholarly: the community of mathematicians and historians of mathematics coordinate around treating zero's mathematical necessity as independent of who formalized it first, which allows credit-attribution (a historical question) to be separated from validity-attribution (a logical question).
% TRANSFER_FUNCTION: Nothing material transfers. What is at stake is credit and narrative framing: historical priority accrues to the Indian mathematical tradition as a matter of documented fact, while the truth-status of zero-as-number accrues to no one — it is treated as available to any tradition that developed the requisite positional-notation arithmetic, symmetric across cultures.
% ABSENT_VOICES: The contingent_thinkability and hybrid_scaffolding readings are the primary absent voices here — they would argue that treating this as pure independent-parallel-discovery erases either a hard conceptual barrier (contingent_thinkability) or a scaffolding dependency that makes 'discovery' too weak a word for what Indian philosophical traditions contributed (hybrid_scaffolding). Those readings are not refuted by this one; they are simply a different constraint, authored separately.
% DISAPPEARANCE_RATIONALE: If this specific interpretive claim (mathematical universality, priority-independence) vanished from discourse tomorrow, mathematics itself would be entirely unaffected — zero-as-number would still function identically in every arithmetic system that uses it. What would change is only the historiographical framing debate; no computation, proof, or practical mathematical practice depends on which philosophical reading of zero's discovery history is accepted.
% FOUNDING_PROBLEM: The problem this reading addresses is a philosophical one: how to reconcile crediting a specific historical culture (India) with first formalizing zero-as-number, while avoiding the implication that the concept was therefore culturally relative, contingent, or 'invented' rather than discovered — i.e., preserving mathematical objectivity/Platonism against historicist or culturally-contingent accounts of mathematical truth.
% FOUNDING_PROBLEM_CORROBORATION: Mathematical Platonists and realist philosophers of mathematics (e.g., in the tradition of Frege, Gödel) attest to this problem independent of any stake in Indian mathematical history specifically — they defend priority-independent ontological status for mathematical objects generally, not to elevate or diminish any particular discovering culture. Historians of mathematics such as Kim Plofker (whose scholarship establishes Indian priority) generally treat the discovery/invention philosophical question as separate from and outside their historical-empirical claims, which is itself a form of outside corroboration that the two questions are logically severable.
narrative_ontology:disappearance_verdict(zero_as_number_entry__universal_discovery_reading, world_unchanged).
narrative_ontology:founding_problem_status(zero_as_number_entry__universal_discovery_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(zero_as_number_entry__universal_discovery_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(zero_as_number_entry__universal_discovery_reading, 'none', 1).
narrative_ontology:epsilon_provenance(zero_as_number_entry__universal_discovery_reading, 0.04, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(zero_as_number_entry__universal_discovery_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(zero_as_number_entry__universal_discovery_reading, ExtMetricName, E),
    domain_priors:suppression_score(zero_as_number_entry__universal_discovery_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(zero_as_number_entry__universal_discovery_reading),
    narrative_ontology:constraint_metric(zero_as_number_entry__universal_discovery_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(zero_as_number_entry__universal_discovery_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(zero_as_number_entry__universal_discovery_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored near-zero (0.03-0.04) because a mathematical necessity, by this reading's own premises, extracts nothing from anyone — no one pays a cost for zero being a number, and no one's exit options are constrained by it. Suppression is near-zero because there is no coercive apparatus maintaining zero's status as a number; it is not enforced, it is simply true and used. Accessibility collapse is authored high (0.88) because, once positional notation with arithmetic operations is adopted, the recognition of zero-as-number becomes essentially forced — there is no coherent alternative arithmetic that both uses positional place-value and refuses to treat the empty place as a number with operational rules. Resistance is authored low (0.12) because virtually no working mathematician disputes zero's number-status today; what little 'resistance' exists is entirely at the philosophical-historiographical level (the sibling readings), not at the level of mathematical practice. The theater ratio is low and essentially flat across the interval (0.08-0.10) because there is no performative maintenance apparatus around zero's number-status — it is not sustained by ritual or institutional theater, it simply operates.
 *
 * PERSPECTIVAL GAP:
 *   There is minimal seat divergence within this reading's own stakeholder set because no stakeholder here is positioned as a payer or victim — all named agents are beneficiaries or analytical observers. The real perspectival gap in this story is EXTERNAL to it: between this reading and its siblings. From this reading's seat, contingent_thinkability_theorists look like they are importing an unwarranted metaphysical claim about Greek conceptual limits into what is actually a straightforward historical-priority question; from their seat, this reading looks like it flattens a real asymmetry in conceptual readiness into a merely contingent timing difference.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are authored broadly and symmetrically: the Indian mathematical tradition receives historical credit (a reputational, non-extractive good), and global mathematical practice receives a usable truth with no distributional asymmetry. No victims are declared, consistent with the FSM guidance that a genuine mountain typically has none — this reading's core claim is precisely that no one is harmed by, nor gains rents from, a mathematical necessity's discovery timeline. Because beneficiaries ARE declared on a mountain claim (the priority-claim beneficiary and the vindicated propositions), FSM evaluation is triggered; the omegas below document the natural-law-vs-constructed ambiguity this requires. The declared beneficiaries here are credit-based rather than rent-based, which is the key structural fact distinguishing this from a false summit — but the schema correctly requires the ambiguity to be surfaced rather than assumed away.
 *
 * MANDATROPHY ANALYSIS:
 *   There is no mandatrophy question in the ordinary institutional sense here — mandatrophy concerns arrangements that outlive their founding function. This constraint is a philosophical/historiographical claim, not an institution with a lifecycle. The relevant analogue is whether the 'discovery' framing itself has outlived its usefulness as opposed to a more nuanced scaffolding account — that tension is precisely what the kernel's three readings exist to adjudicate, and is intentionally NOT resolved within this single reading.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    discovery_vs_construction_of_mathematical_objects,
    'Is zero-as-number a discovered timeless truth (Platonist ontology, this reading''s premise) or a human conceptual construction that different cultures arrived at via different cognitive/philosophical routes (constructivist ontology, closer to the sibling readings)?',
    'This is not empirically resolvable by historical evidence alone — it is a live question in philosophy of mathematics (Platonism vs. formalism vs. social constructivism) that historical priority data cannot adjudicate. Resolution mechanism would be philosophical argument about the nature of mathematical objects generally, of which zero is one instance among many (parallel debates exist for negative numbers, imaginary numbers, infinitesimals, set-theoretic foundations).',
    'If constructivist, this reading''s mountain classification would be undermined — zero-as-number would be better modeled as at least partially contingent on the conceptual resources a culture possesses, moving it toward the hybrid_scaffolding_reading''s structure. If Platonist, this reading''s classification holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(discovery_vs_construction_of_mathematical_objects, conceptual, 'Whether mathematical objects like zero are discovered (mountain) or constructed (contingent) — the core philosophical fork underlying all three kernel readings.').

omega_variable(
    beneficiary_status_of_priority_credit,
    'Does crediting the Indian mathematical tradition with priority constitute a genuine, non-extractive beneficiary relationship (mere historical credit, as authored here), or does downstream Eurocentric historiography that minimizes or omits that priority constitute a form of extraction (uncredited intellectual contribution) that this reading''s flat beneficiary structure fails to capture?',
    'Historiographical audit of how zero''s discovery has been credited across textbooks, encyclopedic sources, and mathematics curricula over the past two centuries — tracking whether Indian priority has been systematically underrepresented relative to the documentary record established by historians like Plofker and Ifrah.',
    'If systematic under-crediting is found, this reading''s beneficiary structure understates a real historical grievance — the ontological independence claim (zero was always true regardless of discoverer) could be functioning, in practice, as cover for erasing the discoverer''s credit, which would push this specific reading toward false-summit territory even though the underlying mathematical claim remains sound.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_status_of_priority_credit, empirical, 'Whether the priority-independence framing has been used to launder historical under-crediting of Indian mathematics in Western historiography — the FSM-relevant ambiguity for this mountain''s declared beneficiary.').

omega_variable(
    independent_vs_transmitted_european_arrival,
    'Did European mathematics arrive at zero-as-number via a genuinely independent parallel discovery process, or was it entirely transmission-dependent (via Arabic intermediaries) such that the ''independent discovery'' component of this reading''s premise is empirically false for the European case specifically?',
    'Documentary and manuscript-tradition analysis of the transmission chain (Indian sources -> Al-Khwarizmi''s Arabic synthesis -> Latin translations -> Fibonacci''s Liber Abaci) versus any evidence of European arithmetic independently developing zero-as-number absent this chain.',
    'The documentary record strongly favors transmission over independent parallel discovery for the European case. If transmission is total, this reading''s claim that ''priority of holder does not affect ontological status'' remains true as a philosophical claim, but its empirical gloss (''Europeans discovered it later via independent... path'') would need to drop the independent-discovery option and rest entirely on the transmitted-path branch, which is closer to what contingent_thinkability_reading and hybrid_scaffolding_reading dispute at the mechanism level, not the ontology level.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(independent_vs_transmitted_european_arrival, empirical, 'Whether the European arrival at zero-as-number was via genuine parallel independent discovery or solely via documented transmission — bears on this reading''s factual premise, not its ontological claim.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(zero_as_number_entry__universal_discovery_reading, 0, 1500).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(zero_tr_t0, zero_as_number_entry__universal_discovery_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(zero_tr_t300, zero_as_number_entry__universal_discovery_reading, theater_ratio, 300, 0.09).
narrative_ontology:measurement(zero_tr_t600, zero_as_number_entry__universal_discovery_reading, theater_ratio, 600, 0.1).
narrative_ontology:measurement(zero_tr_t900, zero_as_number_entry__universal_discovery_reading, theater_ratio, 900, 0.1).
narrative_ontology:measurement(zero_tr_t1200, zero_as_number_entry__universal_discovery_reading, theater_ratio, 1200, 0.1).
narrative_ontology:measurement(zero_tr_t1500, zero_as_number_entry__universal_discovery_reading, theater_ratio, 1500, 0.1).

% Extraction over time
narrative_ontology:measurement(zero_be_t0, zero_as_number_entry__universal_discovery_reading, base_extractiveness, 0, 0.03).
narrative_ontology:measurement(zero_be_t300, zero_as_number_entry__universal_discovery_reading, base_extractiveness, 300, 0.03).
narrative_ontology:measurement(zero_be_t600, zero_as_number_entry__universal_discovery_reading, base_extractiveness, 600, 0.04).
narrative_ontology:measurement(zero_be_t900, zero_as_number_entry__universal_discovery_reading, base_extractiveness, 900, 0.04).
narrative_ontology:measurement(zero_be_t1200, zero_as_number_entry__universal_discovery_reading, base_extractiveness, 1200, 0.04).
narrative_ontology:measurement(zero_be_t1500, zero_as_number_entry__universal_discovery_reading, base_extractiveness, 1500, 0.04).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(zero_as_number_entry__universal_discovery_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(zero_as_number_entry__universal_discovery_reading, zero_as_number_entry__contingent_thinkability_reading).
narrative_ontology:affects_constraint(zero_as_number_entry__universal_discovery_reading, zero_as_number_entry__hybrid_scaffolding_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the zero_as_number_entry kernel. universal_discovery_reading (this file) authors zero-as-number as a Mountain: timeless, priority-independent, negligible extraction, no victims, high accessibility collapse. contingent_thinkability_reading authors the same natural-language claim with substantially different structural data: it would declare a genuine conceptual barrier in the Greek/Aristotelian framework, meaning transmission was constitutive rather than incidental — that story's ε and suppression should register the dependency this one denies. hybrid_scaffolding_reading occupies a middle position: mathematically latent but requiring specific philosophical scaffolding, with contact functioning as recognition-trigger rather than transmission — its beneficiary/victim structure and accessibility_collapse values should sit between the other two. Per the ε-invariance principle, these are three separate constraints, not one constraint measured three ways; each carries its own ε and is linked here via affects_constraints rather than merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
