% ============================================================================
% CONSTRAINT STORY: zero_as_number_entry__contingent_thinkability_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_zero_as_number_entry__contingent_thinkability_reading, []).

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
 *   constraint_id: zero_as_number_entry__contingent_thinkability_reading
 *   human_readable: Contingent Thinkability of Zero-as-Number in Europe (Transmission-Dependence Reading)
 *   domain: history of mathematics / philosophy of mathematics / conceptual history
 *
 * SUMMARY:
 *   This story instantiates ONE reading — contingent_thinkability_reading —
 *   of the contested kernel zero_as_number_entry: the claim that
 *   zero-as-number became thinkable in Europe only through contact with
 *   Indian and Islamic mathematics, and that absent this transmission the
 *   concept would not have emerged indigenously because the
 *   Greek/Aristotelian framework barred treating nothing as a quantity. The
 *   constraint modeled is the corrected attribution standard as an operative
 *   historiographical arrangement: it coordinates the history-of-mathematics
 *   community on a single evidence-anchored record (genuine coordination
 *   function) while asymmetrically extracting a dependency admission from the
 *   European mathematical tradition and transferring recognition to the
 *   Indian and Islamic traditions (asymmetric extraction), sustained by
 *   active enforcement through peer review, critical editions, and curriculum
 *   standards. EPSILON REFERENT: per the kernel-reading rule, epsilon is
 *   authored for the standing arrangement under contest — the operative
 *   attribution regime this reading instantiates and defends — assessed by
 *   this reading's own lights; it is NOT authored for the autonomy narrative
 *   this reading displaces, nor averaged across sibling readings. ASSUMPTIONS
 *   STATED: interval points index decades (t=0 -> 1920, t=100 -> 2020),
 *   spanning the peak of Eurocentric autonomy narratives to the contemporary
 *   corrective regime; metric trajectories are authored estimates of
 *   documented drift (publication patterns, curriculum revisions, enforcement
 *   intensity), not instrument readings. CLAIM/METRIC INDEPENDENCE:
 *   claimed_type=tangled_rope is authored from the structure (both
 *   coordination and asymmetric extraction, actively enforced); the metrics
 *   are authored from descriptive operation; the engine computes per-seat
 *   classifications and any divergence from the claim is signal, not error.
 *
 * KEY AGENTS:
 *   - european_mathematical_tradition: Primary target (institutional/identity_locked) — bears the dependency admission extracted by the corrected attribution standard
 *   - indian_mathematical_tradition: Primary beneficiary (organized/mobile) — collects priority recognition for zero-as-number's invention
 *   - islamic_mathematical_tradition: Secondary beneficiary (organized/mobile) — collects transmission and refinement recognition
 *   - history_of_mathematics_community: Agenda setter (institutional/constrained) — administers and enforces the attribution standard, collecting professional capital
 *   - national_curriculum_authorities: Secondary payer (institutional/constrained) — bears the concrete curricular adaptation costs
 *   - postcolonial_students_and_educators: Excluded voice (moderate/constrained) — lives inside the curricular legacy but outside the adjudicating venues
 *   - philosophy_of_mathematics_analysts: Analytical observer (analytical/analytical) — sees the full three-reading structure and collects nothing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(zero_as_number_entry__contingent_thinkability_reading, 0.7).
domain_priors:suppression_score(zero_as_number_entry__contingent_thinkability_reading, 0.35).
domain_priors:theater_ratio(zero_as_number_entry__contingent_thinkability_reading, 0.26).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(zero_as_number_entry__contingent_thinkability_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(zero_as_number_entry__contingent_thinkability_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(zero_as_number_entry__contingent_thinkability_reading, theater_ratio, 0.26).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(zero_as_number_entry__contingent_thinkability_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(zero_as_number_entry__contingent_thinkability_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(zero_as_number_entry__contingent_thinkability_reading, tangled_rope).
narrative_ontology:human_readable(zero_as_number_entry__contingent_thinkability_reading, "Contingent Thinkability of Zero-as-Number in Europe (Transmission-Dependence Reading)").
narrative_ontology:topic_domain(zero_as_number_entry__contingent_thinkability_reading, "history of mathematics / philosophy of mathematics / conceptual history").

domain_priors:requires_active_enforcement(zero_as_number_entry__contingent_thinkability_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(zero_as_number_entry__contingent_thinkability_reading, 'c4982f48-e361-4858-b422-6fb6231db16b').
narrative_ontology:cs_kernel_codification('c4982f48-e361-4858-b422-6fb6231db16b', distributed).
narrative_ontology:cs_authority_grounding('c4982f48-e361-4858-b422-6fb6231db16b', expertise).
narrative_ontology:cs_interpretation_layer_present('c4982f48-e361-4858-b422-6fb6231db16b').
narrative_ontology:cs_reading_relation('c4982f48-e361-4858-b422-6fb6231db16b', zero_as_number_entry__universal_discovery_reading, coexists_with).
narrative_ontology:cs_reading_relation('c4982f48-e361-4858-b422-6fb6231db16b', zero_as_number_entry__hybrid_scaffolding_reading, coexists_with).
narrative_ontology:cs_axiom('c4982f48-e361-4858-b422-6fb6231db16b', foundational, indigenous_emergence_impossible_without_transmission).
narrative_ontology:cs_axiom_status(indigenous_emergence_impossible_without_transmission, holdable).
narrative_ontology:cs_axiom_grounding('c4982f48-e361-4858-b422-6fb6231db16b', indigenous_emergence_impossible_without_transmission, empirically_contingent).
narrative_ontology:cs_axiom('c4982f48-e361-4858-b422-6fb6231db16b', secondary, aristotelian_framework_barred_nilpotent_quantity).
narrative_ontology:cs_axiom_status(aristotelian_framework_barred_nilpotent_quantity, holdable).
narrative_ontology:cs_axiom_grounding('c4982f48-e361-4858-b422-6fb6231db16b', aristotelian_framework_barred_nilpotent_quantity, empirically_contingent).
narrative_ontology:cs_reference_frame('c4982f48-e361-4858-b422-6fb6231db16b', transmission_dependent_reception).
narrative_ontology:cs_drift_state('c4982f48-e361-4858-b422-6fb6231db16b', contemporary_historiography, gap(axiom_overriding, minor, true)).
narrative_ontology:cs_created_at('c4982f48-e361-4858-b422-6fb6231db16b', '').
narrative_ontology:cs_kernel_id(zero_as_number_entry__contingent_thinkability_reading, zero_as_number_entry).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(zero_as_number_entry__contingent_thinkability_reading, indian_mathematical_tradition).
narrative_ontology:constraint_beneficiary(zero_as_number_entry__contingent_thinkability_reading, islamic_mathematical_tradition).
narrative_ontology:constraint_victim(zero_as_number_entry__contingent_thinkability_reading, european_mathematical_tradition).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(zero_as_number_entry__contingent_thinkability_reading, national_curriculum_authorities).
narrative_ontology:constraint_vindicates(zero_as_number_entry__contingent_thinkability_reading, cultural_contingency_of_mathematical_concepts).
narrative_ontology:constraint_vindicates(zero_as_number_entry__contingent_thinkability_reading, transmission_dependence_of_european_zero).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Carries the constraint's admission cost. The tradition's inherited self-account — mathematics as an internally driven Greek-to-modern continuum — must be revised to register that one of its foundational arithmetical concepts arrived from outside, through Latin translations of Arabic works ultimately drawing on Sanskrit sources. The dependency is constitutive of what the tradition is: it cannot exit its own genesis, and the admission is extracted anew with each survey edition, curriculum revision, and commemoration. What flows from it is narrative sovereignty over its own origins; what flows to it is the corrected record it nonetheless teaches.
narrative_ontology:constraint_stakeholder(zero_as_number_entry__contingent_thinkability_reading, european_mathematical_tradition, payer,
    institutional, civilizational, identity_locked, continental).

% Holds the priority core: the Sanskrit positional decimal system and Brahmagupta's seventh-century rules treating zero as a number operable in arithmetic. Under the corrected attribution standard it collects formal recognition — citation priority, curricular presence, commemoration — that earlier Eurocentric accounts withheld. It administers nothing and needs do nothing to keep collecting; recognition flows whether or not South Asian institutions press the claim. Exit would mean declining the recognition, which no institution in the tradition seeks.
narrative_ontology:constraint_stakeholder(zero_as_number_entry__contingent_thinkability_reading, indian_mathematical_tradition, beneficiary,
    organized, civilizational, mobile, regional).

% Holds the transmission-and-refinement credit: al-Khwarizmi's arithmetic and algebra, the Arabic numeral corpus, and the translation movement that carried Indian zero westward into Latin Europe. Collects mediation and development recognition under the corrected account. Like the Indian tradition it collects without running anything; its recognition share is secondary to the priority core but materially larger than under autonomy narratives.
narrative_ontology:constraint_stakeholder(zero_as_number_entry__contingent_thinkability_reading, islamic_mathematical_tradition, beneficiary,
    organized, civilizational, mobile, continental).

% Journal editors, learned-society officers, survey-textbook authors, and critical-edition projects that administer the attribution standard: manuscripts, philology, and peer review decide what the record says, and the community enforces the corrected attribution against residual autonomy narratives. It also collects professional capital — the corrective program funds careers, journals, and anniversary volumes. It cannot exit its stewardship without abandoning the discipline's evidentiary machinery.
narrative_ontology:constraint_stakeholder(zero_as_number_entry__contingent_thinkability_reading, history_of_mathematics_community, agenda_setter,
    institutional, generational, constrained, global).

% Ministry and examination bodies that must rewrite syllabi, textbooks, and teacher training whenever the attribution standard shifts. They bear the concrete adaptation cost of the dependency admission — reprint cycles, public hearings, contested standards disputes — and have no exit short of teaching a record their own universities refute.
narrative_ontology:constraint_stakeholder(zero_as_number_entry__contingent_thinkability_reading, national_curriculum_authorities, payer,
    institutional, generational, constrained, national).

% Learners and teachers in formerly colonized education systems who live inside the curricular legacy the constraint adjudicates but sit outside the journals and societies where the reading is argued. They would press for the recognition to reach their classrooms rather than remain a metropolitan scholarly correction; their absence keeps much of the constraint's benefit concentrated in the historiographical center.
narrative_ontology:constraint_stakeholder(zero_as_number_entry__contingent_thinkability_reading, postcolonial_students_and_educators, excluded,
    moderate, biographical, constrained, global).

% Philosophers assessing what the dispute shows about whether mathematical concepts are culturally indexed or discovery-independent. They take testimony from every seat, distinguish ontological availability from historical thinkability, and collect nothing and pay nothing whichever reading prevails.
narrative_ontology:constraint_stakeholder(zero_as_number_entry__contingent_thinkability_reading, philosophy_of_mathematics_analysts, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(zero_as_number_entry__contingent_thinkability_reading, indian_mathematical_tradition).
narrative_ontology:fixing_cost_class(zero_as_number_entry__contingent_thinkability_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a single integrated, evidence-anchored record of how positional arithmetic and zero-as-number entered European practice, so that credit assignment, research lineages, and teaching across communities track actual transmission paths instead of fragmenting into parochial national narratives.
% TRANSFER_FUNCTION: Moves recognition and priority-status from the European mathematical tradition to the Indian and Islamic mathematical traditions; moves curricular revision labor and reprint costs onto European-derived education systems; and moves professional capital — careers, publications, commemorations — to the corrective historiography community that administers the standard.
% ABSENT_VOICES: The original Sanskrit and Arabic authors cannot testify and are represented only through modern philology; the medieval transmission agents (translator-scholars, merchant-mathematicians of the Fibonacci era) left no seat at the table; and educators and students in formerly colonized systems, who carry the curricular legacy most directly, are largely absent from the venues where the reading is adjudicated.
% DISAPPEARANCE_RATIONALE: If the transmission-dependence account vanished overnight, textbook narratives would drift back toward autonomous-European-genius framings, the priority-recognition gains of the Indian and Islamic traditions would evaporate, curriculum authorities would halt revision cycles, and the hybrid and universalist readings would lose their principal foil — the historiographical economy would reorganize around the autonomy narrative the corrective exists to displace.
% FOUNDING_PROBLEM: The erasure problem: nineteenth- and twentieth-century Eurocentric historiography attributed zero's entry into European mathematics to European ingenuity or dismissed it as a mere notational convenience, concealing both the Indian invention of zero-as-number and the Islamic transmission machinery that delivered it.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated substantially from OUTSIDE the beneficiary set: the philological manuscript record itself (Latin translations of al-Khwarizmi's arithmetic and algebra; the documented debts of Liber Abaci), and — decisively — historians of mathematics working inside the European tradition, who concede the transmission dependence against their own tradition's narrative interest. An interested party conceding against interest is the strongest corroboration seat available; no corroboration exists for the displaced autonomy narrative outside legacy textbooks.
narrative_ontology:disappearance_verdict(zero_as_number_entry__contingent_thinkability_reading, world_rearranges).
narrative_ontology:founding_problem_status(zero_as_number_entry__contingent_thinkability_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(zero_as_number_entry__contingent_thinkability_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(zero_as_number_entry__contingent_thinkability_reading, 'none', 1).
narrative_ontology:epsilon_provenance(zero_as_number_entry__contingent_thinkability_reading, 0.7, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(zero_as_number_entry__contingent_thinkability_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(zero_as_number_entry__contingent_thinkability_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(zero_as_number_entry__contingent_thinkability_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.70 at interval end) because the admission the constraint extracts is deep: it reaches the tradition's account of its own genesis, recurs with every survey edition and curriculum cycle, and cannot be paid off once — the dependency is constitutive. Suppression is moderate-low (0.35): this is scholarly-space enforcement (gatekeeping, standards, review), not coercive exclusion; the sibling readings remain fully publishable and vigorously argued, so alternatives are not suppressed. Theater ratio is low-moderate (0.26): a growing share of acknowledgment is performative — ritual citations of 'the Arabs gave us zero' in prefaces without curricular or citational integration — but the core corrective function (critical editions, philology, revised surveys) is real work. Accessibility collapse is low (0.30): understanding the transmission evidence does NOT collapse the alternatives — the universalist and hybrid readings remain coherent live positions, which is precisely why this is a contested kernel rather than a settled fact. Resistance is moderate-high (0.55): autonomy narratives retain defenders, curriculum reforms meet public hearings and political pushback, and the corrective must be continuously maintained. The measurement series run on ONE shared grid (t = 0,20,40,60,80,100) with all three metrics authored at every point; the trajectories are monotonic, not cyclical — enforcement capacity ratcheted upward as the corrective professionalized (postcolonial historiography, world-history initiatives), which is why suppression_requirement is traced here: the story's dynamic IS the maturing of enforcement machinery, not merely extraction shift.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute sharply different types from identical structural data. From the european_mathematical_tradition seat, the arrangement operates as enforced extraction: an institution with identity-locked exit paying a recurring narrative tax it cannot escape — a snare-flavored experience. From the two beneficiary-tradition seats, the same arrangement is overdue recognition arriving without administration burden — subsidy-flavored. From the history_of_mathematics_community seat, it is ordinary scholarly correction it built and stewards — rope-flavored, with the professional-capital side-payment mostly invisible to it. The engine computes this divergence from power, exit, and directional position; the authored claim does not adjudicate which experience is 'real.'
 *
 * DIRECTIONALITY LOGIC:
 *   The two beneficiary traditions sit near the full-beneficiary end (d near 0): recognition flows to them, they administer nothing, and their exit is mobile — nothing binds them to the arrangement. The european_mathematical_tradition sits near the full-target end (d near 1): it bears the transfer, and its exit is identity_locked — the dependency admission concerns its own constitution, so exit is not merely blocked but unthinkable without dissolving the tradition's self-concept; identity-lock amplifies effective extraction beyond what a mobile payer at the same power level would experience. The history_of_mathematics_community derives a low-to-moderate d: it enforces the standard and collects professional capital from its operation, placing it beneficiary-side despite not appearing in the beneficiaries array. National curriculum authorities derive a high d as secondary payers with constrained exit. No directionality_overrides are used: the derivation from declared beneficiaries, victims, and exit options produces the correct relationships, and a power-atom-level override would wrongly sweep across the multiple institutional seats this story deliberately differentiates by role.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against mislabeling in both directions. Read as pure justice, the corrective looks like a rope — everyone nets a benefit from accurate history — which erases the real asymmetric cost borne by the European tradition and the curriculum systems; the tangled_rope classification forces the extraction into view. Read cynically as grievance politics, it looks like a snare — status confiscation dressed as scholarship — which erases the genuine coordination function: a single evidence-anchored record solves a real collective problem that parochial national histories cannot. Mandatrophy status: the founding problem (erasure of transmission dependence) is LIVE — popular narratives and many curricula still carry autonomy framings — so the mandate has not outlived its function and no resolution is declared. The R5 mismatch consumer reads founding_problem_status=live x disappearance_verdict=world_rearranges: consistent, no zombie flag. Fixing_cost is authored prohibitive on independent evidence: the corrective cannot be retired without the erasure recurring, because the philological record re-anchors it — removal cost exceeds any benefit to the paying seat. Gain_flow is authored as indian_mathematical_tradition because the recognition flow demonstrably lands on named seats and the priority core (zero-as-number's invention itself) accrues there; the Islamic tradition collects a real but secondary transmission share, so no single-seat/diffuse ambiguity remains unchecked.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This constraint is ONE reading (contingent_thinkability_reading) of the kernel zero_as_number_entry; what would the sibling readings change structurally if adopted as the operative account?',
    'Comparative classification across the three reading files in the family: classify universal_discovery_reading and hybrid_scaffolding_reading independently and diff the victim/beneficiary sets and epsilon values against this story.',
    'Under universal_discovery_reading the dependency admission disappears (priority of holder does not affect ontological status), the victim set collapses, and epsilon falls toward coordination-only levels; under hybrid_scaffolding_reading the victim set splits between barrier-hardness and scaffolding-absence and epsilon lands intermediate. The classification of THIS story is indexical to the reading, not to the topic.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer structure: reading-indexed classification of the zero-entry kernel; sibling readings are other constraints, not parts of this one.').

omega_variable(
    indigenous_emergence_counterfactual,
    'Can the counterfactual at this reading''s core — that Europe could not have generated zero-as-number indigenously absent transmission — be established at all from single-history evidence?',
    'Comparative analysis of barrier mechanisms (Aristotelian prohibition of nilpotent quantity, absence of positional-notation incentives in Latin commercial practice) against documented near-misses (abacus computists manipulating empty columns, Gerbertine counters) to estimate how hard the barrier actually was.',
    'If viable indigenous near-paths existed, the reading degrades toward the hybrid scaffolding position and epsilon falls; if the metaphysical barrier was hard, the foundational axiom holds and epsilon stays high.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(indigenous_emergence_counterfactual, empirical, 'Establishability of the no-indigenous-emergence counterfactual under single-history evidence.').

omega_variable(
    recognition_transfer_materiality,
    'Does the priority recognition collected by the Indian and Islamic traditions constitute genuine benefit transfer, or symbolic accounting with no material effect?',
    'Trace funding flows, curricular inclusion rates, institutional partnerships, and prestige metrics for South Asian and Middle Eastern scientific institutions following attribution reforms and World-History-of-Mathematics initiatives.',
    'If purely symbolic, the beneficiary seats'' directionality rises toward symmetric and the constraint reads closer to a pure rope; if material, the asymmetric extraction reading is confirmed and the tangled_rope classification is stabilized.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(recognition_transfer_materiality, empirical, 'Materiality of recognition transfer to the beneficiary traditions.').

omega_variable(
    barrier_vs_incentive_cause,
    'Was the European delay in zero-thinkability caused by Aristotelian metaphysical prohibition, or by mundane economic factors (Roman numerals adequate for commerce until double-entry bookkeeping made positional computation profitable)?',
    'Economic history of medieval computation practice cross-checked against scholastic text analysis of nilpotent-quantity prohibitions: if commercial demand preceded any relaxation of the metaphysical bar, incentive explains the timing better than metaphysics.',
    'An incentive-based cause weakens the ''metaphysical barrier'' framing that grounds this reading''s foundational axiom, shifting credit dynamics and lowering the dependency admission''s depth; a metaphysical cause confirms the reading''s mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(barrier_vs_incentive_cause, empirical, 'Whether the blocking mechanism was metaphysical or economic-incentive based.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(zero_as_number_entry__contingent_thinkability_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(zero_tr_t0, zero_as_number_entry__contingent_thinkability_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(zero_tr_t20, zero_as_number_entry__contingent_thinkability_reading, theater_ratio, 20, 0.14).
narrative_ontology:measurement(zero_tr_t40, zero_as_number_entry__contingent_thinkability_reading, theater_ratio, 40, 0.18).
narrative_ontology:measurement(zero_tr_t60, zero_as_number_entry__contingent_thinkability_reading, theater_ratio, 60, 0.22).
narrative_ontology:measurement(zero_tr_t80, zero_as_number_entry__contingent_thinkability_reading, theater_ratio, 80, 0.24).
narrative_ontology:measurement(zero_tr_t100, zero_as_number_entry__contingent_thinkability_reading, theater_ratio, 100, 0.26).

% Extraction over time
narrative_ontology:measurement(zero_be_t0, zero_as_number_entry__contingent_thinkability_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(zero_be_t20, zero_as_number_entry__contingent_thinkability_reading, base_extractiveness, 20, 0.5).
narrative_ontology:measurement(zero_be_t40, zero_as_number_entry__contingent_thinkability_reading, base_extractiveness, 40, 0.58).
narrative_ontology:measurement(zero_be_t60, zero_as_number_entry__contingent_thinkability_reading, base_extractiveness, 60, 0.64).
narrative_ontology:measurement(zero_be_t80, zero_as_number_entry__contingent_thinkability_reading, base_extractiveness, 80, 0.68).
narrative_ontology:measurement(zero_be_t100, zero_as_number_entry__contingent_thinkability_reading, base_extractiveness, 100, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(zero_su_t0, zero_as_number_entry__contingent_thinkability_reading, suppression_requirement, 0, 0.12).
narrative_ontology:measurement(zero_su_t20, zero_as_number_entry__contingent_thinkability_reading, suppression_requirement, 20, 0.17).
narrative_ontology:measurement(zero_su_t40, zero_as_number_entry__contingent_thinkability_reading, suppression_requirement, 40, 0.23).
narrative_ontology:measurement(zero_su_t60, zero_as_number_entry__contingent_thinkability_reading, suppression_requirement, 60, 0.28).
narrative_ontology:measurement(zero_su_t80, zero_as_number_entry__contingent_thinkability_reading, suppression_requirement, 80, 0.32).
narrative_ontology:measurement(zero_su_t100, zero_as_number_entry__contingent_thinkability_reading, suppression_requirement, 100, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(zero_as_number_entry__contingent_thinkability_reading, identity_coordination).
narrative_ontology:affects_constraint(zero_as_number_entry__contingent_thinkability_reading, universal_discovery_reading).
narrative_ontology:affects_constraint(zero_as_number_entry__contingent_thinkability_reading, hybrid_scaffolding_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'how zero entered European mathematics' decomposes into three structurally distinct readings of the kernel zero_as_number_entry, each with its own epsilon, beneficiary/victim structure, and classification. This file instantiates contingent_thinkability_reading (transmission was necessary; Europe could not have generated the concept indigenously; high epsilon on cultural contingency; European tradition as victim of the dependency admission, non-Western traditions as beneficiaries of priority recognition). The siblings are separate stories: universal_discovery_reading (availability is ontological, priority incidental — expect near-zero victim set and low epsilon) and hybrid_scaffolding_reading (latent structure plus external trigger — expect a split victim set and intermediate epsilon). Each file links the others via network.affects_constraints; upstream-downstream pressure runs from the universalist reading (the standing default narrative) toward the two corrective readings, which exist as critiques of it.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
