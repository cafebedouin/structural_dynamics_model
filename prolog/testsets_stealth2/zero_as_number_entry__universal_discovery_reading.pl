% ============================================================================
% CONSTRAINT STORY: zero_as_number_entry__universal_discovery_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-07
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
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: zero_as_number_entry__universal_discovery_reading
 *   human_readable: Zero-as-Number Entry — Universal Discovery Reading (Timeless Mathematical Availability)
 *   domain: history of mathematics / philosophy of mathematics / conceptual history
 *
 * SUMMARY:
 *   A kernel-reading story. The colloquial label 'the entry of zero into
 *   number' decomposes, per the epsilon-invariance principle, into three
 *   structurally distinct claims; this file authors one of them — the
 *   universal_discovery_reading: zero-as-number was always mathematically
 *   available, being a logical consequence of positional notation together
 *   with the arithmetic operations; Indian mathematicians formalized it first
 *   (codified rules for shunya by 628 CE), Europeans integrated it later by
 *   transmitted or partially independent path, and the priority of the holder
 *   does not affect the structure's ontological status. Epsilon's referent is
 *   the standing arrangement under contest — operative zero-as-number within
 *   positional arithmetic as historically instantiated — assessed by this
 *   reading's own lights; the reading endorses no rival arrangement, so no
 *   alternative-arrangement discount enters. No victim set exists (discovery
 *   has no losers) and no beneficiary set is declared: benefit is strictly
 *   universal and non-differential, so 'all of mathematics benefits equally
 *   from truth' is not a set of actors, and declaring a subset would
 *   misdescribe the structure and falsely present a differential-capture
 *   profile. The interest-ladenness of the availability framing itself is
 *   routed to an omega (fsm_interest_laden_framing) rather than settled by
 *   declaration. Linked to its two sibling readings via
 *   network.affects_constraints; see network.dual_formulation_note. KEY
 *   AGENTS (by structural relationship): - indian_mathematical_tradition:
 *   first-discoverer seat (moderate/mobile) — formalized operative zero;
 *   collects the structure's utility earliest in time; priority dates the
 *   discovery without altering its status -
 *   islamic_transmission_intermediaries: relay seat (institutional/mobile) —
 *   carried notation and algorithms westward; changed arrival speed, not
 *   structure - european_adopter_tradition: later-discoverer seat
 *   (moderate/mobile) — integrated operative zero after contact or partial
 *   rediscovery; same universal utility, later date -
 *   aristotelian_scholastic_tradition: excluded seat
 *   (institutional/identity_locked) — the framework that barred
 *   nothing-as-quantity; the historical object of this reading's
 *   counterfactual-irrelevance clause - historians_of_mathematics: analytical
 *   observer (analytical/analytical) — adjudicates priority and transmission;
 *   the only seat seeing the full cross-reading dispute -
 *   universal_computing_practice: diffuse user seat (organized/mobile) —
 *   every subsequent arithmetic civilization; collects usage-utility
 *   uniformly, captures nothing
 *
 * KEY AGENTS:
 *   - indian_mathematical_tradition: first-discoverer seat (moderate/mobile) — formalized operative zero within place-value decimal notation; collects the structure's utility earliest in time; under this reading priority fixes a date, not a status
 *   - islamic_transmission_intermediaries: relay seat (institutional/mobile) — Abbasid-era scholarship carried the notation and its algorithms westward; altered speed of arrival, not the structure
 *   - european_adopter_tradition: later-discoverer seat (moderate/mobile) — integrated operative zero centuries after formalization, by transmission or partial rediscovery; same universal utility, later date
 *   - aristotelian_scholastic_tradition: excluded seat (institutional/identity_locked) — the Greek-medieval framework barring nothing-as-quantity; historically objected, now outside the conversation
 *   - historians_of_mathematics: analytical observer (analytical/analytical) — adjudicates priority and transmission; sees the whole cross-reading dispute
 *   - universal_computing_practice: diffuse user seat (organized/mobile) — all subsequent calculating communities; uniform utility, no capture
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(zero_as_number_entry__universal_discovery_reading, 0.03).
domain_priors:suppression_score(zero_as_number_entry__universal_discovery_reading, 0.02).
domain_priors:theater_ratio(zero_as_number_entry__universal_discovery_reading, 0.03).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(zero_as_number_entry__universal_discovery_reading, extractiveness, 0.03).
narrative_ontology:constraint_metric(zero_as_number_entry__universal_discovery_reading, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(zero_as_number_entry__universal_discovery_reading, theater_ratio, 0.03).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(zero_as_number_entry__universal_discovery_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(zero_as_number_entry__universal_discovery_reading, resistance, 0.18).

% --- Constraint claim ---
narrative_ontology:constraint_claim(zero_as_number_entry__universal_discovery_reading, mountain).
narrative_ontology:human_readable(zero_as_number_entry__universal_discovery_reading, "Zero-as-Number Entry — Universal Discovery Reading (Timeless Mathematical Availability)").
narrative_ontology:topic_domain(zero_as_number_entry__universal_discovery_reading, "history of mathematics / philosophy of mathematics / conceptual history").

domain_priors:emerges_naturally(zero_as_number_entry__universal_discovery_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(zero_as_number_entry__universal_discovery_reading, 'c4aa1fbb-7559-47dc-9407-89e1637fb2ef').
narrative_ontology:cs_kernel_codification('c4aa1fbb-7559-47dc-9407-89e1637fb2ef', distributed).
narrative_ontology:cs_authority_grounding('c4aa1fbb-7559-47dc-9407-89e1637fb2ef', expertise).
narrative_ontology:cs_interpretation_layer_present('c4aa1fbb-7559-47dc-9407-89e1637fb2ef').
narrative_ontology:cs_reading_relation('c4aa1fbb-7559-47dc-9407-89e1637fb2ef', zero_as_number_entry__contingent_thinkability_reading, coexists_with).
narrative_ontology:cs_reading_relation('c4aa1fbb-7559-47dc-9407-89e1637fb2ef', zero_as_number_entry__hybrid_scaffolding_reading, coexists_with).
narrative_ontology:cs_axiom('c4aa1fbb-7559-47dc-9407-89e1637fb2ef', foundational, mathematical_truth_is_holder_invariant).
narrative_ontology:cs_axiom_status(mathematical_truth_is_holder_invariant, holdable).
narrative_ontology:cs_axiom_grounding('c4aa1fbb-7559-47dc-9407-89e1637fb2ef', mathematical_truth_is_holder_invariant, deontological).
narrative_ontology:cs_axiom('c4aa1fbb-7559-47dc-9407-89e1637fb2ef', foundational, positional_notation_entails_operative_zero).
narrative_ontology:cs_axiom_status(positional_notation_entails_operative_zero, holdable).
narrative_ontology:cs_axiom_grounding('c4aa1fbb-7559-47dc-9407-89e1637fb2ef', positional_notation_entails_operative_zero, empirically_contingent).
narrative_ontology:cs_reference_frame('c4aa1fbb-7559-47dc-9407-89e1637fb2ef', timeless_structural_availability).
narrative_ontology:cs_drift_state('c4aa1fbb-7559-47dc-9407-89e1637fb2ef', contemporary_historiography, gap(stable, minor, true)).
narrative_ontology:cs_created_at('c4aa1fbb-7559-47dc-9407-89e1637fb2ef', '').
narrative_ontology:cs_kernel_id(zero_as_number_entry__universal_discovery_reading, zero_as_number_entry).

% --- Structural relationships ---
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(zero_as_number_entry__universal_discovery_reading, indian_mathematical_tradition).
narrative_ontology:constraint_beneficiary(zero_as_number_entry__universal_discovery_reading, islamic_transmission_intermediaries).
narrative_ontology:constraint_beneficiary(zero_as_number_entry__universal_discovery_reading, european_adopter_tradition).
narrative_ontology:constraint_beneficiary(zero_as_number_entry__universal_discovery_reading, universal_computing_practice).
narrative_ontology:constraint_vindicates(zero_as_number_entry__universal_discovery_reading, ontological_invariance_of_mathematical_truth).
narrative_ontology:constraint_vindicates(zero_as_number_entry__universal_discovery_reading, priority_independence_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Court and monastic mathematicians of South Asia — Aryabhata's school, Jain calculator traditions, Brahmagupta's codification of rules for shunya (628 CE) — developed place-value decimal notation and made the empty place operable as a number. What flows to them is the same utility the structure affords every user: compact representation and closed algorithms. Nothing binds them to it (they could abandon the notation without penalty) and nothing they built charges anyone downstream; their priority fixes a date of formalization, and under this reading a date is not a status.
narrative_ontology:constraint_stakeholder(zero_as_number_entry__universal_discovery_reading, indian_mathematical_tradition, beneficiary,
    moderate, generational, mobile, regional).

% Abbasid-era scholars and practitioners — al-Khwarizmi's arithmetic, al-Uqlidisi's manuals, the scribal culture of Baghdad — absorbed Indian numerals and produced the texts that carried place-value zero westward. They collected the same usage-utility and relayed the technique; on this reading their role changes the speed of arrival in Europe, whether by transmission or alongside an independent path, and alters nothing about the structure itself.
narrative_ontology:constraint_stakeholder(zero_as_number_entry__universal_discovery_reading, islamic_transmission_intermediaries, beneficiary,
    institutional, generational, mobile, continental).

% Mediterranean merchants and later university mathematicians — Fibonacci's Liber Abaci (1202), the algorist party, the eventual integration into European analysis — took up operative zero centuries after its formalization, by transmission, rediscovery, or both. What flows to them is the same utility, later; their late arrival marks historical distance from the structure, not any difference in the structure.
narrative_ontology:constraint_stakeholder(zero_as_number_entry__universal_discovery_reading, european_adopter_tradition, beneficiary,
    moderate, generational, mobile, continental).

% The Greek and medieval Christian framework treated number as a plurality of units and barred treating nothing as a quantity; scholastic institutions inherited and taught the bar for centuries. They are the historical population this reading's counterfactual clause concerns: the tradition whose conceptual commitments the contingent reading treats as decisive and this reading treats as ontologically idle. They objected when the numerals arrived; they are no longer in the conversation, and their framework's identity was fused with the bar.
narrative_ontology:constraint_stakeholder(zero_as_number_entry__universal_discovery_reading, aristotelian_scholastic_tradition, excluded,
    institutional, civilizational, identity_locked, continental).

% Scholars who reconstruct transmission paths, date formalizations, and adjudicate priority claims among the Indian, Islamic, and European traditions. They collect nothing and pay nothing; their classifications decide which reading of the zero-entry question prevails, which makes them the only seat that sees the whole cross-reading dispute from inside this story.
narrative_ontology:constraint_stakeholder(zero_as_number_entry__universal_discovery_reading, historians_of_mathematics, observer,
    analytical, generational, analytical, global).

% Every subsequent community of calculation — ledger-keepers, scientists, engineers, software — uses place-value zero as the substrate of quantitative work. The utility is uniform across all of them: no user captures more than use itself affords, no user is charged beyond learning the notation, and the practice reproduces itself across generations without administration.
narrative_ontology:constraint_stakeholder(zero_as_number_entry__universal_discovery_reading, universal_computing_practice, beneficiary,
    organized, civilizational, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(zero_as_number_entry__universal_discovery_reading, diffuse).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the representation-and-computation problem of arithmetic: a finite symbol set expressing unbounded quantity through place value, with algorithms (carrying, borrowing, long multiplication and division) that close only if the empty place is itself a number. The structure coordinates every user of positional notation on a single encoding — the largest information standard in practice.
% TRANSFER_FUNCTION: Nothing material transfers between parties. What moves is articulable knowledge: formalization in India, manual-writing in the Abbasid world, adoption in Europe. No rents, obligations, or goods flow through the structure itself; use imposes no charge on anyone.
% ABSENT_VOICES: The Aristotelian-scholastic tradition is the structurally absent voice: it held that nothing cannot be a quantity and would reject the reading's premise outright; it is no longer in the conversation. Also absent is any Indian-priority advocacy seat — the reading's priority-irrelevance clause dissolves the practical stake that first-discovery might otherwise ground, and no seat inside this reading argues the contrary.
% DISAPPEARANCE_RATIONALE: Every arrangement built on positional arithmetic — commercial ledger-keeping, scientific calculation, digital computation — presupposes an operative zero; overnight removal would force reversion to additive notation and collapse algorithmic arithmetic and modern computation with it. The counterfactual is not merely disruptive but impossible to engineer, which is the structural signature this reading claims.
% FOUNDING_PROBLEM: How to express and manipulate unbounded quantities with a finite symbol set. Place-value notation generates the empty-place problem — the blank column must behave as a number for carrying and borrowing to close — and an operative zero is the structure the notation demands.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated outside any benefiting party by the derivation itself: that place-value arithmetic entails an operative zero is a checkable mathematical result, attested in the standard algebraic literature, and by convergent independent invention (Babylonian placeholder, Maya positional zero, Indian shunya) in unconnected traditions. No testimonial corroboration from any human seat is required — which is itself the reading's point.
narrative_ontology:disappearance_verdict(zero_as_number_entry__universal_discovery_reading, world_rearranges).
narrative_ontology:founding_problem_status(zero_as_number_entry__universal_discovery_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(zero_as_number_entry__universal_discovery_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(zero_as_number_entry__universal_discovery_reading, 'none', 1).
narrative_ontology:epsilon_provenance(zero_as_number_entry__universal_discovery_reading, 0.03, 'stealth/ox-alpha', 'none', direct).

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
 *   All five core metrics are authored from this reading's own lights against a fixed referent: operative zero-as-number within positional arithmetic as historically instantiated. Extractiveness 0.03 sits one point above the information_standard coordination floor (0.02): the only standing cost the structure imposes is the learning burden of the notation itself, borne identically by every newcomer regardless of tradition or date. Suppression 0.02: no enforcement machinery exists or ever did — the concept compels solely through proof and demonstrated utility; suppression is authored raw and unscaled, per the structural-property rule. Theater 0.03: mathematical validity is maintained by derivation, not performance; nothing here decays into ritual. Accessibility collapse 0.92: once place-value notation and the arithmetic operations are granted, an operative zero is forced — no alternative representation survives contact with the algorithms, which is the completeness signature of a logical consequence. Resistance 0.18: honest historical friction existed (the Aristotelian bar on nothing-as-quantity, abacist resistance, municipal bans on the foreign numerals), but it was friction of host frameworks against recognition, not against the structure, and it collapsed everywhere on demonstrated utility; the reading holds such friction ontologically inert. The measurement grid is deliberately flat across thirteen centuries on both tracked series: absence of drift IS the datum — no enforcement ratchet, no extraction accumulation, no theater growth. suppression_requirement is intentionally not serialized: the enforcement picture is static-null and fully captured by the scalar. Claim and metrics are independent authored facts: claimed_type mountain states this reading's structural verdict; the metrics describe operation; the engine computes per-seat types from the structural data.
 *
 * PERSPECTIVAL GAP:
 *   Within this reading, the human seats converge: every seat sits near the beneficiary side of symmetric because the structure taxes no one and subsidizes everyone identically — with no beneficiary/victim declarations the engine's canonical fallback lands each seat near d=0.5, which is the correct rendering of universal non-differential benefit. The perspectival action is BETWEEN readings, not within seats: the contingent_thinkability reading would split these same historical seats into blocked and unblocked populations (pre-contact European tradition as a framework-barred population; transmission as the unlocking channel), and the hybrid reading would insert a retireable scaffolding phase. The historians' seat is the only within-story seat that sees the full cross-reading dispute; the excluded scholastic seat would compute a different world altogether — one in which the premise that nothing can be a quantity is incoherent.
 *
 * DIRECTIONALITY LOGIC:
 *   No beneficiaries and no victims are declared, deliberately. Benefit here is strictly universal and non-differential — 'all of mathematics benefits equally from truth' is not a beneficiary set, and naming a subset (the Indian tradition, say) would misdescribe the structure and falsely present differential capture that this reading's lights do not warrant. With no structural declarations, the engine derives each seat's d from canonical power-atom fallbacks, landing all seats near symmetric — matching the structure. No directionality_overrides are authored: there is no seat whose derived d would be wrong. gain_flow is authored as 'diffuse' as an affirmative checked claim: every named seat was examined and none captures the structure's gains — each collects only what usage itself affords, uniformly. fixing_cost is deliberately omitted: its premise (some agent who could fix or remove the arrangement) fails for a logical necessity — no cost class is definable where no fixer exists, and authoring 'prohibitive' would misfile the story into a neglect cell describing institutional decay rather than mathematical permanence.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — finite-symbol representation of unbounded quantity — is perennially live and permanently solved by the same structure; there is no mandate to outlive its function, no sunset to declare, no atrophy to resolve, and mandatrophy_resolved is deliberately left undeclared. The classification guards two opposite errors. Read as a mere notation convention coordinating users, the structure's necessity is understated — this reading's point is that no alternative was ever really available once the notation is granted. Read as a contested extraction arrangement (priority disputes as rent-seeking), historiographical noise is mistaken for the structure's operation — under this reading, who found it first is ontologically idle, so the disputes generate nothing to classify. The flat temporal series is the positive evidence: nothing accumulates, nothing decays, nothing is performed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_provenance,
    'This constraint is one reading — universal_discovery_reading — of the zero_as_number_entry kernel; what structural differences would instantiating contingent_thinkability_reading or hybrid_scaffolding_reading instead produce?',
    'Author the two sibling stories against the same historical interval and compare computed types, epsilon, and stakeholder surfaces; the delta locates what the kernel contest actually turns on.',
    'The contingent reading would introduce a framework-barred population (pre-contact European tradition) and concentrate epsilon on transmission barriers; the hybrid reading would introduce a retireable scaffolding phase with sunset dynamics. Either would replace this story''s no-parties profile with a partitioned beneficiary/payer surface.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_provenance, conceptual, 'Committer-frame provenance: one of three readings of the zero-entry kernel; siblings would restructure the seat map and epsilon.').

omega_variable(
    availability_vs_inevitability_decomposition,
    'Does this reading conflate availability (zero-as-number is entailed by positional notation plus the arithmetic operations) with discovery-inevitability (any mature arithmetic tradition converges on it)? If epsilon or classification differs across the two observables, they are two constraints.',
    'Epsilon-invariance test: measure the constraint as the entailment claim (formal, reader-independent) and as the convergence claim (historical-counterfactual); if the measured profiles diverge, decompose into separate availability and inevitability stories linked by network.affects_constraints.',
    'Decomposition would leave the entailment claim a clean no-parties necessity and move the contested, higher-epsilon residue (counterfactual convergence, Maya and Mesopotamian comparisons) into a separate inevitability story with its own stakeholders.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(availability_vs_inevitability_decomposition, conceptual, 'Guard against folding two epsilon-distinct claims (entailment vs convergence) into one story.').

omega_variable(
    fsm_interest_laden_framing,
    'Is the timeless-availability framing itself interest-laden — does any program (Platonist philosophy of mathematics, internalist historiography, or the quiet dissolution of Indian-priority credit-claims) derive differential legitimacy or resources from presenting the structure as holder-independent?',
    'Search for a seat whose resource or legitimacy position depends on the availability thesis rather than on the mathematics; if one exists, re-author with that seat declared in base_properties.beneficiaries, which routes the story through false-summit evaluation.',
    'A found beneficiary would flip the story from a no-parties necessity to a false-summit candidate (engine reclassification toward tangled_rope), with the historiographical program as the capturing seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fsm_interest_laden_framing, conceptual, 'False-summit-shaped uncertainty held as an omega because this reading''s own lights warrant no declared beneficiary set.').

omega_variable(
    isolated_tradition_convergence_test,
    'Do unconnected notation traditions reliably reach an operative zero when positional representation matures, or is operative zero historically rare enough that transmission, not structure, explains its spread?',
    'Comparative history of independent inventions: Maya positional zero (unconnected to Afro-Eurasian development), Babylonian placeholder zero, Indian shunya; count convergence events against opportunities for convergence.',
    'Repeated independent convergence confirms the availability and inevitability profile this reading claims; a single-origin pattern would shift evidential weight to the contingent reading and raise effective epsilon on the transmission pathway.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(isolated_tradition_convergence_test, empirical, 'Empirical test of the convergence claim via independent-invention counts.').

omega_variable(
    notation_conditionality_of_necessity,
    'Is zero-as-number''s necessity absolute (flowing from logic and the arithmetic operations alone) or conditional on positional notation — itself a human artifact, albeit one under convergent selection pressure?',
    'Formal analysis: determine whether any comparably efficient non-positional arithmetic exists that does not require an operative zero, and whether computational efficiency can be grounded as necessary rather than merely preferred.',
    'If necessity is notation-conditional, the structure is better read as a convention-locked coordination layer atop a chosen notation and the necessity claim narrows to conditional form; if absolute, the timeless-availability claim stands unrestricted.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(notation_conditionality_of_necessity, conceptual, 'Scope of the necessity claim: unconditional versus notation-relative.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(zero_as_number_entry__universal_discovery_reading, 400, 1700).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(zero_tr_t400, zero_as_number_entry__universal_discovery_reading, theater_ratio, 400, 0.03).
narrative_ontology:measurement_basis(zero_tr_t400, observed).
narrative_ontology:measurement(zero_tr_t628, zero_as_number_entry__universal_discovery_reading, theater_ratio, 628, 0.03).
narrative_ontology:measurement_basis(zero_tr_t628, observed).
narrative_ontology:measurement(zero_tr_t900, zero_as_number_entry__universal_discovery_reading, theater_ratio, 900, 0.03).
narrative_ontology:measurement_basis(zero_tr_t900, observed).
narrative_ontology:measurement(zero_tr_t1202, zero_as_number_entry__universal_discovery_reading, theater_ratio, 1202, 0.03).
narrative_ontology:measurement_basis(zero_tr_t1202, observed).
narrative_ontology:measurement(zero_tr_t1500, zero_as_number_entry__universal_discovery_reading, theater_ratio, 1500, 0.03).
narrative_ontology:measurement_basis(zero_tr_t1500, observed).
narrative_ontology:measurement(zero_tr_t1700, zero_as_number_entry__universal_discovery_reading, theater_ratio, 1700, 0.03).
narrative_ontology:measurement_basis(zero_tr_t1700, observed).

% Extraction over time
narrative_ontology:measurement(zero_be_t400, zero_as_number_entry__universal_discovery_reading, base_extractiveness, 400, 0.03).
narrative_ontology:measurement_basis(zero_be_t400, observed).
narrative_ontology:measurement(zero_be_t628, zero_as_number_entry__universal_discovery_reading, base_extractiveness, 628, 0.03).
narrative_ontology:measurement_basis(zero_be_t628, observed).
narrative_ontology:measurement(zero_be_t900, zero_as_number_entry__universal_discovery_reading, base_extractiveness, 900, 0.03).
narrative_ontology:measurement_basis(zero_be_t900, observed).
narrative_ontology:measurement(zero_be_t1202, zero_as_number_entry__universal_discovery_reading, base_extractiveness, 1202, 0.03).
narrative_ontology:measurement_basis(zero_be_t1202, observed).
narrative_ontology:measurement(zero_be_t1500, zero_as_number_entry__universal_discovery_reading, base_extractiveness, 1500, 0.03).
narrative_ontology:measurement_basis(zero_be_t1500, observed).
narrative_ontology:measurement(zero_be_t1700, zero_as_number_entry__universal_discovery_reading, base_extractiveness, 1700, 0.03).
narrative_ontology:measurement_basis(zero_be_t1700, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(zero_as_number_entry__universal_discovery_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(zero_as_number_entry__universal_discovery_reading, information_standard).
narrative_ontology:affects_constraint(zero_as_number_entry__universal_discovery_reading, zero_as_number_entry__contingent_thinkability_reading).
narrative_ontology:affects_constraint(zero_as_number_entry__universal_discovery_reading, zero_as_number_entry__hybrid_scaffolding_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'the entry of zero into number'. The label conflates three structurally distinct claims: (1) availability — this story: zero-as-number is an unconditional entailment of positional notation plus the arithmetic operations, epsilon near the coordination floor, no binding parties; (2) thinkability — zero_as_number_entry__contingent_thinkability_reading: European arrival depended on transmission against conceptual barriers, epsilon concentrated on the barrier structure and its beneficiaries; (3) scaffolding — zero_as_number_entry__hybrid_scaffolding_reading: latent structure requiring tradition-specific scaffolding for operational uptake, with retireable-phase dynamics. Each story carries its own epsilon, its own stakeholder surface, and its own claimed type, linked via network.affects_constraints. This reading is upstream: its availability thesis sets the terms the other two negotiate, since both siblings concede the latent structure and dispute uptake.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
