% ============================================================================
% CONSTRAINT STORY: zero_as_number_entry__universal_discovery_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
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
 *   human_readable: Zero-as-Number: Timeless Mathematical Availability (Universal Discovery Reading)
 *   domain: history_of_mathematics/philosophy_of_mathematics
 *
 * SUMMARY:
 *   This story instantiates the universal_discovery_reading of the
 *   zero_as_number_entry kernel as a single epsilon-invariant constraint:
 *   zero-as-number is a timeless logical consequence of positional notation
 *   together with the arithmetic operations - any system that writes
 *   quantities by place value and permits addition and subtraction contains
 *   zero as a number whether or not any human has noticed. On this reading
 *   the historical record (formalized zero-arithmetic rules in
 *   seventh-century India; consolidation and transmission through Islamic
 *   mathematics; European entry via Fibonacci's Liber Abaci and parallel
 *   paths; institutional resistance decaying by the early modern period) is a
 *   record of recognition events, not of construction: Indian mathematicians
 *   encountered the structure first, Europeans later, and holder priority
 *   changes credit, not ontological status. The constraint has no parties: no
 *   one pays for it, no one collects from it, and its yield - working
 *   positional arithmetic - accrues universally and symmetrically to every
 *   tradition that acquires it. It is therefore authored as a party-free
 *   mountain on the gravity pattern, with the beneficiary ambiguity the
 *   kernel hands down carried in omega variables rather than manufactured
 *   into a beneficiary list. This file is one member of a three-file
 *   constraint family; the sibling files instantiate the kernel's other
 *   readings and are linked through network.affects_constraints.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(zero_as_number_entry__universal_discovery_reading, 0.02).
domain_priors:suppression_score(zero_as_number_entry__universal_discovery_reading, 0.02).
domain_priors:theater_ratio(zero_as_number_entry__universal_discovery_reading, 0.02).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(zero_as_number_entry__universal_discovery_reading, extractiveness, 0.02).
narrative_ontology:constraint_metric(zero_as_number_entry__universal_discovery_reading, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(zero_as_number_entry__universal_discovery_reading, theater_ratio, 0.02).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(zero_as_number_entry__universal_discovery_reading, accessibility_collapse, 0.93).
narrative_ontology:constraint_metric(zero_as_number_entry__universal_discovery_reading, resistance, 0.06).

% --- Constraint claim ---
narrative_ontology:constraint_claim(zero_as_number_entry__universal_discovery_reading, mountain).
narrative_ontology:human_readable(zero_as_number_entry__universal_discovery_reading, "Zero-as-Number: Timeless Mathematical Availability (Universal Discovery Reading)").
narrative_ontology:topic_domain(zero_as_number_entry__universal_discovery_reading, "history_of_mathematics/philosophy_of_mathematics").

domain_priors:emerges_naturally(zero_as_number_entry__universal_discovery_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(zero_as_number_entry__universal_discovery_reading, 'b9e7bc7e-db7c-470e-ab96-b93099f34d34').
narrative_ontology:cs_kernel_codification('b9e7bc7e-db7c-470e-ab96-b93099f34d34', formalized).
narrative_ontology:cs_authority_grounding('b9e7bc7e-db7c-470e-ab96-b93099f34d34', expertise).
narrative_ontology:cs_interpretation_layer_present('b9e7bc7e-db7c-470e-ab96-b93099f34d34').
narrative_ontology:cs_reading_relation('b9e7bc7e-db7c-470e-ab96-b93099f34d34', zero_as_number_entry__contingent_thinkability_reading, coexists_with).
narrative_ontology:cs_reading_relation('b9e7bc7e-db7c-470e-ab96-b93099f34d34', zero_as_number_entry__hybrid_scaffolding_reading, coexists_with).
narrative_ontology:cs_axiom('b9e7bc7e-db7c-470e-ab96-b93099f34d34', foundational, zero_as_number_timelessly_available).
narrative_ontology:cs_axiom_status(zero_as_number_timelessly_available, holdable).
narrative_ontology:cs_axiom_grounding('b9e7bc7e-db7c-470e-ab96-b93099f34d34', zero_as_number_timelessly_available, empirically_contingent).
narrative_ontology:cs_axiom('b9e7bc7e-db7c-470e-ab96-b93099f34d34', foundational, discoverer_priority_ontologically_irrelevant).
narrative_ontology:cs_axiom_status(discoverer_priority_ontologically_irrelevant, holdable).
narrative_ontology:cs_axiom_grounding('b9e7bc7e-db7c-470e-ab96-b93099f34d34', discoverer_priority_ontologically_irrelevant, conventional).
narrative_ontology:cs_reference_frame('b9e7bc7e-db7c-470e-ab96-b93099f34d34', timeless_mathematical_availability).
narrative_ontology:cs_drift_state('b9e7bc7e-db7c-470e-ab96-b93099f34d34', contemporary_historiography, gap(repudiation_pressure, minor, true)).
narrative_ontology:cs_created_at('b9e7bc7e-db7c-470e-ab96-b93099f34d34', '').
narrative_ontology:cs_kernel_id(zero_as_number_entry__universal_discovery_reading, zero_as_number_entry).

% --- Structural relationships ---
narrative_ontology:constraint_vindicates(zero_as_number_entry__universal_discovery_reading, positional_place_value_principle).
narrative_ontology:constraint_vindicates(zero_as_number_entry__universal_discovery_reading, arithmetic_closure_under_subtraction).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Supplies a single uniform encoding of quantity: with zero as a number, positional notation runs one algorithm set - carrying, borrowing, long multiplication and division - across every magnitude, so calculation skill generalizes instead of being re-derived per order of magnitude or per abacus configuration.
% TRANSFER_FUNCTION: Nothing transfers. The constraint moves no money, work, attention, or status between parties because it has no parties; what it yields is capability - executable positional arithmetic - and that capability flows to every acquiring tradition at no one's expense.
% ABSENT_VOICES: At the European entry point, the missing voices belonged to traditions whose frameworks excluded zero: Aristotelian category theory (quantity requiring a substratum, which 'nothing' lacks) and the institutional arithmetic authorities whose standing rested on abacus mastery. Those voices are historically dissolved rather than presently silenced - no living constituency is kept out of the conversation about zero's status. Where exclusion occurred, it was imposed by contemporary municipal and guild institutions (the numeral bans), which are neighboring constraints, not this one.
% DISAPPEARANCE_RATIONALE: Without zero-as-number, positional notation loses the placeholder that computes, arithmetic loses closure under subtraction and its additive origin, and the written algorithms fail; double-entry bookkeeping, algebra, digital computation, and quantitative science lose their substrate and would reorganize around far costlier computational regimes, as the Roman-numeral and abacus economies were.
% FOUNDING_PROBLEM: Positional notation generates an unfilled place-value slot that must denote 'nothing in this place' while still participating in computation, and arithmetic demands closure under subtraction plus a distinguished additive origin. Zero-as-number answers both at once: it completes the numeral system so that written algorithms execute uniformly at every magnitude.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated entirely from outside any beneficiary set - trivially, since this reading declares none. Assyriology attests the placeholder-before-number stage (millennia of Babylonian sexagesimal blanks); the Sanskrit record attests formalized zero-arithmetic rules by the seventh century (Brahmagupta's Brahmasphutasiddhanta); the Latin translation lineages and the Liber Abaci diffusion record attest the transmission path into Europe; and the commercial-city numeral bans attest that what resisted resolution was the entry problem, not the fact. No party attests the genealogy from inside a benefiting position, because the reading holds there is no such position.
narrative_ontology:disappearance_verdict(zero_as_number_entry__universal_discovery_reading, world_rearranges).
narrative_ontology:founding_problem_status(zero_as_number_entry__universal_discovery_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(zero_as_number_entry__universal_discovery_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(zero_as_number_entry__universal_discovery_reading, 'none', 1).
narrative_ontology:epsilon_provenance(zero_as_number_entry__universal_discovery_reading, 0.02, 'stealth/ox-alpha', 'none', direct).

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
 *   Metrics are authored for what the constraint descriptively is under this reading. Extractiveness 0.02: the fact takes nothing from anyone; the residual is a conservative allowance for the unavoidable cost of acquiring the concept, booked as coordination-floor cost rather than extraction (it sits at the information_standard Boltzmann floor). Suppression 0.02: nothing enforces the fact - it propagates by logical consequence; the historical bans on Arabic numerals in European commercial cities were coercive constraints AGAINST the practice, operated by guild and municipal institutions, and belong to other constraint stories, not to this one (importing them here would break epsilon-invariance by measuring a different arrangement). Theater ratio 0.02: no one performs maintenance on a mathematical fact; pedagogy teaches it but maintains nothing. Accessibility collapse 0.93: once positional notation and the arithmetic operations are understood, the alternative - zero-not-a-number - collapses almost completely: place-value notation demands a placeholder that participates in computation, subtraction demands closure, and the written algorithms fail without it; the remaining sliver models traditions that used positional placeholders for centuries without promoting the placeholder to a number, showing collapse is fast but not instantaneous. Resistance 0.06: the fact itself meets no active resistance; the small residual honors the historical resistance to the practice, which decayed on contact with understanding rather than being defeated. The measurement series are deliberately flat at every shared time point (t=0..14, century units anchored at roughly 600 CE: t0 immediately preceding Brahmagupta's formalization, t3 Islamic-era consolidation, t6 the Liber Abaci and European entry, t9 early-modern acceptance, t12 the foundational formalization era, t14 present ubiquity): epsilon-invariance itself predicts flatness, since drift in the series would mean the observable had stopped measuring this constraint. The grid is shared across both tracked metrics by construction, and suppression_requirement is intentionally untracked because the constraint has no enforcement capacity whose rise or fall could be traced - the static enforcement picture is carried by the scalar.
 *
 * PERSPECTIVAL GAP:
 *   With no parties, there are no seats between which the engine should compute divergence - every hypothetical seat (a seventh-century Indian formalizer, a thirteenth-century Italian merchant, a nineteenth-century foundationalist, a contemporary analyst) confronts the same structure with the same negligible extraction, and uniform classification across seats is the mountain signature. The only perspectival texture available is diachronic: during the European adoption era the same fact was expensive to avow (institutional penalties attached to the numerals that carried it) and cheap to own a century later - but that cost gradient was imposed by neighboring institutional constraints, and modeling it here would fuse two arrangements into one epsilon. This story therefore computes no seat divergence by design; the divergence the kernel actually exhibits lives between readings, that is, between files, and is routed to the omega variables.
 *
 * DIRECTIONALITY LOGIC:
 *   No beneficiaries and no victims are declared, so the derivation chain has no asymmetry to read and no directionality overrides are needed. The structural reason: the constraint's yield (uniform positional arithmetic) flows to every acquiring tradition at no one's expense - benefit is universal and symmetric, which is not an identifiable beneficiary group but the absence of one. The kernel's handed-down ambiguity ('all mathematics benefits equally from truth') is documented in omega beneficiary_structure_ambiguity: declaring a beneficiary such as positional_notation_users would be authorable but would fire the false-summit signature on an otherwise clean mountain profile, manufacturing a tangled_rope signal out of a declaration rather than a structure. Priority of discovery (India first, Europe later) is likewise directionally inert under this reading: discoverers gain credit and capability, not rents, and the reading's foundational axiom makes holder priority ontologically irrelevant. Spatial scope is effectively universal - the structure binds any notation-using agent anywhere - but with epsilon at the coordination floor, the scope amplification of effective extraction has nothing to amplify.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (complete positional notation; close arithmetic under subtraction) is dead - solved permanently - while the arrangement persists maximally; read naively, dead-plus-world_rearranges is the capture/zombie signature, and this story is the controlled case for why that mismatch flag must be cross-checked against the theater path. Nothing is being maintained past its function: theater_ratio is 0.02, there is no agenda_setter, no administrator could change the constraint, and no one bears a fixable cost. Persistence after problem-death here is permanence of solution, not inertia of institution - exactly the distinction mandatrophy analysis exists to protect. Accordingly mandatrophy_resolved is authored false: the constraint's mandate has not outlived its function, because its function (making positional arithmetic executable) is exercised by every computation performed. The classification also blocks the opposite mislabel: the real coordination function (one algorithm set across all magnitudes) could invite a rope reading, but ropes are maintained for coordination by participants, whereas this structure is simply true and would coordinate nothing if it were not; the mountain claim and the coordination-type declaration coexist because the coordination is a consequence of the fact, not its purpose.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint is the universal_discovery_reading of the zero_as_number_entry kernel: zero-as-number as timeless logical consequence of positional notation plus arithmetic operations, with discoverer priority ontologically irrelevant. Which structural elements of the classification would change under the sibling readings?',
    'Compile the sibling files (contingent_thinkability_reading, hybrid_scaffolding_reading) and diff epsilon, beneficiary/victim sets, enforcement flags, and computed types against this story; the disagreement is located in the inference from logical availability to historical inevitability, not in the object-level status of zero.',
    'Under contingent_thinkability the classification shifts from timeless mountain toward a transmission-dependent construct with a possible victim set (traditions whose frameworks blocked indigenous emergence); under hybrid_scaffolding the mountain survives at the structural level but gains a scaffold-shaped historical layer with its own sunset logic. This file''s own classification is unchanged by either outcome.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: one reading of the zero_as_number_entry kernel; sibling readings relocate the disagreement to the availability-to-inevitability inference.').

omega_variable(
    disagreement_location_availability_vs_actualization,
    'Where exactly do the readings disagree - on zero''s ontological status as a number, or only on whether logical availability entails historical inevitability of discovery?',
    'Conceptual separation of the object-level claim (zero is a number; derivable from positional notation plus the arithmetic operations) from the modal-historical claim (any notation-using tradition would converge on it without transmission); test whether any sibling reading disputes the object-level claim itself.',
    'If the dispute is confined to the modal-historical layer, this reading''s mountain classification is stable across the whole kernel and the family shares one object-level constraint; if the object-level status is itself disputed, the kernel decomposes further and this file''s referent narrows.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(disagreement_location_availability_vs_actualization, conceptual, 'Locates the kernel disagreement between ontological status and modal-historical inevitability.').

omega_variable(
    beneficiary_structure_ambiguity,
    'The kernel hands down an ambiguous beneficiary set (''all mathematics benefits equally from truth''): is universal symmetric benefit a beneficiary structure at all, and would declaring beneficiaries change the classification?',
    'Author a probe variant declaring positional_notation_users as beneficiary and observe the false_summit_mountain signature: if it fires on this clean mountain profile, the declaration manufactures the false-summit signal rather than detecting one.',
    'Declaring beneficiaries would trigger FSM reclassification toward tangled_rope despite negligible extraction, coercion-free suppression, and no capturable gain; leaving the set empty preserves the genuine-mountain profile. The ambiguity is documented, not resolved.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_structure_ambiguity, conceptual, 'Universal symmetric benefit is documented as the absence of a beneficiary structure; the FSM-firing risk of declaring one is recorded.').

omega_variable(
    independent_vs_transmitted_european_path,
    'Was the European encounter with zero-as-number independent rediscovery, transmission through Islamic mathematics, or a mixture - and does the path bear on this reading''s claims?',
    'Manuscript philology: Latin lineages of al-Khwarizmi''s arithmetic, the diffusion record of Fibonacci''s Liber Abaci, and any evidence of autonomous European development of positional ideas; compare against claims of independent rediscovery.',
    'Under this reading, none: the foundational axiom makes holder priority and path ontologically irrelevant, so the classification is invariant across all resolutions. The omega is retained because the sibling readings'' classifications do turn on the path.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(independent_vs_transmitted_european_path, empirical, 'Transmission-path question is classification-invariant here but load-bearing for the sibling readings.').

omega_variable(
    availability_vs_cognitive_actualization,
    'Does ''logically available'' understate the barrier - cognitive research finds zero unusually hard to represent and acquire, so does latent availability entail achievable discovery for any tradition, or only for some?',
    'Cognitive arithmetic research (number-sense studies, developmental and neuroimaging work on zero) cross-referenced with the historical record of traditions that used positional placeholders for centuries without promoting the placeholder to a number.',
    'If availability does not entail actualizability, the reading''s inevitability gloss weakens and weight shifts toward the hybrid_scaffolding position; the ontological-status axiom (zero is a number; priority irrelevant) survives either way, so the mountain classification degrades gracefully rather than flipping.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(availability_vs_cognitive_actualization, empirical, 'Tests whether logical availability implies cognitive actualizability; threatens the inevitability gloss, not the ontological core.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(zero_as_number_entry__universal_discovery_reading, 0, 14).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(zero_universal_discovery_tr_t0, zero_as_number_entry__universal_discovery_reading, theater_ratio, 0, 0.02).
narrative_ontology:measurement_basis(zero_universal_discovery_tr_t0, observed).
narrative_ontology:measurement(zero_universal_discovery_tr_t3, zero_as_number_entry__universal_discovery_reading, theater_ratio, 3, 0.02).
narrative_ontology:measurement_basis(zero_universal_discovery_tr_t3, observed).
narrative_ontology:measurement(zero_universal_discovery_tr_t6, zero_as_number_entry__universal_discovery_reading, theater_ratio, 6, 0.02).
narrative_ontology:measurement_basis(zero_universal_discovery_tr_t6, observed).
narrative_ontology:measurement(zero_universal_discovery_tr_t9, zero_as_number_entry__universal_discovery_reading, theater_ratio, 9, 0.02).
narrative_ontology:measurement_basis(zero_universal_discovery_tr_t9, observed).
narrative_ontology:measurement(zero_universal_discovery_tr_t12, zero_as_number_entry__universal_discovery_reading, theater_ratio, 12, 0.02).
narrative_ontology:measurement_basis(zero_universal_discovery_tr_t12, observed).
narrative_ontology:measurement(zero_universal_discovery_tr_t14, zero_as_number_entry__universal_discovery_reading, theater_ratio, 14, 0.02).
narrative_ontology:measurement_basis(zero_universal_discovery_tr_t14, observed).

% Extraction over time
narrative_ontology:measurement(zero_universal_discovery_be_t0, zero_as_number_entry__universal_discovery_reading, base_extractiveness, 0, 0.02).
narrative_ontology:measurement_basis(zero_universal_discovery_be_t0, observed).
narrative_ontology:measurement(zero_universal_discovery_be_t3, zero_as_number_entry__universal_discovery_reading, base_extractiveness, 3, 0.02).
narrative_ontology:measurement_basis(zero_universal_discovery_be_t3, observed).
narrative_ontology:measurement(zero_universal_discovery_be_t6, zero_as_number_entry__universal_discovery_reading, base_extractiveness, 6, 0.02).
narrative_ontology:measurement_basis(zero_universal_discovery_be_t6, observed).
narrative_ontology:measurement(zero_universal_discovery_be_t9, zero_as_number_entry__universal_discovery_reading, base_extractiveness, 9, 0.02).
narrative_ontology:measurement_basis(zero_universal_discovery_be_t9, observed).
narrative_ontology:measurement(zero_universal_discovery_be_t12, zero_as_number_entry__universal_discovery_reading, base_extractiveness, 12, 0.02).
narrative_ontology:measurement_basis(zero_universal_discovery_be_t12, observed).
narrative_ontology:measurement(zero_universal_discovery_be_t14, zero_as_number_entry__universal_discovery_reading, base_extractiveness, 14, 0.02).
narrative_ontology:measurement_basis(zero_universal_discovery_be_t14, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(zero_as_number_entry__universal_discovery_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(zero_as_number_entry__universal_discovery_reading, information_standard).
narrative_ontology:affects_constraint(zero_as_number_entry__universal_discovery_reading, contingent_thinkability_reading).
narrative_ontology:affects_constraint(zero_as_number_entry__universal_discovery_reading, hybrid_scaffolding_reading).

% DUAL FORMULATION NOTE:
% Kernel zero_as_number_entry decomposes into three epsilon-invariant readings: this file (universal_discovery_reading - object-level timeless availability, epsilon at the coordination floor, mountain), contingent_thinkability_reading (transmission-contingent emergence; extraction concentrates on the blocked-indigenous counterfactual), and hybrid_scaffolding_reading (latent structure plus scaffolding trigger). The readings share a referent kernel but instantiate different constraints with different epsilon, beneficiary/victim structures, and types; they are linked because the universal reading's object-level claim is the established foundation the sibling readings argue from - upstream (the mathematical fact) structurally influencing downstream (the historiographical contest) without foreclosing it.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
