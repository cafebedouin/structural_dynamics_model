% ============================================================================
% CONSTRAINT STORY: zero_mathematical_status__placeholder_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_zero_mathematical_status__placeholder_reading, []).

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
 *   constraint_id: zero_mathematical_status__placeholder_reading
 *   human_readable: Placeholder-Only Regime for Zero: Notation Without Arithmetic
 *   domain: history_of_mathematics/philosophy_of_mathematics/conceptual_history
 *
 * SUMMARY:
 *   Across the ancient and late-ancient world, the mark for the empty place
 *   in positional notation was permitted to exist but forbidden to act.
 *   Babylonian sexagesimal scribes left a blank or cut a separating wedge;
 *   Hellenistic astronomers wrote a small circle in fractional tables; Indian
 *   calculators dotted the vacant decimal column; Chinese counting-board
 *   offices simply removed the rods. In every tradition the mark made
 *   column-position carry magnitude, and in every tradition the mark answered
 *   no operational question: nothing could be added to it, subtracted from
 *   it, or divided by it, and a computation landing on it stopped there. This
 *   story instantiates ONE reading of the contested kernel
 *   zero_mathematical_status — the placeholder reading, on which zero is a
 *   notational device and not a number — and authors epsilon for THAT
 *   standing arrangement only, by its own lights: it delivered real
 *   computational efficiency while denying full arithmetic closure to the
 *   people whose problems crossed zero. The claim/metric gap is deliberate:
 *   the reading is CLAIMED as tangled_rope (genuine coordination plus
 *   asymmetric cost) while the metrics describe moderately extractive,
 *   increasingly enforced operation; the engine measures the divergence.
 *   Sibling readings (number_reading, parmenidean_rejection) are separate
 *   constraints in separate files, linked by network edges. KEY AGENTS (by
 *   structural relationship): - babylonian_sexagesimal_scribes: Primary
 *   beneficiary (organized/constrained) — collects calculation efficiency
 *   from the inert mark - indian_algebraic_astronomers: Primary target
 *   (moderate/constrained) — bears the lost closure when problems produce
 *   null quantities - peripatetic_philosophers: Doctrinal enforcer
 *   (institutional/identity_locked) — supplies the prohibition that nothing
 *   is a quantity - scribal_training_establishments: Administrative enforcer
 *   (institutional/constrained) — transmits the convention and disciplines
 *   deviation - syzygy_prediction_computers: Secondary target
 *   (moderate/constrained) — stalls at zero crossings in conjunction tables -
 *   historians_of_mathematics: Analytical observer — sees the full
 *   three-reading structure
 *
 * KEY AGENTS:
 *   - babylonian_sexagesimal_scribes: Primary beneficiary (organized/constrained) — temple and palace accountants whose base-60 tables run on an empty-place mark they never operate on
 *   - indian_algebraic_astronomers: Primary target (moderate/constrained) — equation solvers and planetary computers whose null results the convention lets them write but not use
 *   - peripatetic_philosophers: Doctrinal enforcer (institutional/identity_locked) — hold that nothing is not a quantity; their ontology is load-bearing for their physics
 *   - scribal_training_establishments: Administrative enforcer (institutional/constrained) — certify table literacy, decide how the empty place is written, sustain demand via the convention's awkward cases
 *   - hellenistic_alexandrian_astronomers: Beneficiary with secondary payer position (organized/constrained) — compact tabular computation; stall at exact-zero elongations
 *   - indian_decimal_calculators: Beneficiary with secondary payer position (organized/constrained) — enormous compression from the dotted vacancy; no textbook procedure when a result lands on it
 *   - chinese_rod_calculation_offices: Beneficiary with secondary payer position (organized/constrained) — the blank row is the absence of rods, not a rod; null results carried as skipped steps
 *   - syzygy_prediction_computers: Secondary target (moderate/constrained) — interpolate around zero crossings because increments cannot be added to the mark
 *   - historians_of_mathematics: Analytical observer (analytical/analytical) — reconstruct all three readings from tablets, papyri, and manuscripts; take no side
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(zero_mathematical_status__placeholder_reading, 0.58).
domain_priors:suppression_score(zero_mathematical_status__placeholder_reading, 0.6).
domain_priors:theater_ratio(zero_mathematical_status__placeholder_reading, 0.26).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(zero_mathematical_status__placeholder_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(zero_mathematical_status__placeholder_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(zero_mathematical_status__placeholder_reading, theater_ratio, 0.26).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(zero_mathematical_status__placeholder_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(zero_mathematical_status__placeholder_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(zero_mathematical_status__placeholder_reading, tangled_rope).
narrative_ontology:human_readable(zero_mathematical_status__placeholder_reading, "Placeholder-Only Regime for Zero: Notation Without Arithmetic").
narrative_ontology:topic_domain(zero_mathematical_status__placeholder_reading, "history_of_mathematics/philosophy_of_mathematics/conceptual_history").

domain_priors:requires_active_enforcement(zero_mathematical_status__placeholder_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(zero_mathematical_status__placeholder_reading, '7e5862dd-8852-4c71-8101-6dbe8edfe48d').
narrative_ontology:cs_kernel_codification('7e5862dd-8852-4c71-8101-6dbe8edfe48d', distributed).
narrative_ontology:cs_authority_grounding('7e5862dd-8852-4c71-8101-6dbe8edfe48d', practice).
narrative_ontology:cs_interpretation_layer_present('7e5862dd-8852-4c71-8101-6dbe8edfe48d').
narrative_ontology:cs_reading_relation('7e5862dd-8852-4c71-8101-6dbe8edfe48d', zero_mathematical_status__number_reading, forecloses).
narrative_ontology:cs_reading_relation('7e5862dd-8852-4c71-8101-6dbe8edfe48d', zero_mathematical_status__parmenidean_rejection, coexists_with).
narrative_ontology:cs_axiom('7e5862dd-8852-4c71-8101-6dbe8edfe48d', foundational, zero_is_operationally_inert_mark).
narrative_ontology:cs_axiom_status(zero_is_operationally_inert_mark, holdable).
narrative_ontology:cs_axiom_grounding('7e5862dd-8852-4c71-8101-6dbe8edfe48d', zero_is_operationally_inert_mark, conventional).
narrative_ontology:cs_axiom('7e5862dd-8852-4c71-8101-6dbe8edfe48d', secondary, positional_efficiency_requires_inert_zero).
narrative_ontology:cs_axiom_status(positional_efficiency_requires_inert_zero, holdable).
narrative_ontology:cs_axiom_grounding('7e5862dd-8852-4c71-8101-6dbe8edfe48d', positional_efficiency_requires_inert_zero, instrumental).
narrative_ontology:cs_reference_frame('7e5862dd-8852-4c71-8101-6dbe8edfe48d', operationally_inert_empty_place_marker).
narrative_ontology:cs_drift_state('7e5862dd-8852-4c71-8101-6dbe8edfe48d', brahmagupta_synthesis, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('7e5862dd-8852-4c71-8101-6dbe8edfe48d', '').
narrative_ontology:cs_kernel_id(zero_mathematical_status__placeholder_reading, zero_mathematical_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(zero_mathematical_status__placeholder_reading, babylonian_sexagesimal_scribes).
narrative_ontology:constraint_beneficiary(zero_mathematical_status__placeholder_reading, hellenistic_alexandrian_astronomers).
narrative_ontology:constraint_beneficiary(zero_mathematical_status__placeholder_reading, indian_decimal_calculators).
narrative_ontology:constraint_beneficiary(zero_mathematical_status__placeholder_reading, chinese_rod_calculation_offices).
narrative_ontology:constraint_beneficiary(zero_mathematical_status__placeholder_reading, scribal_training_establishments).
narrative_ontology:constraint_beneficiary(zero_mathematical_status__placeholder_reading, peripatetic_philosophers).
narrative_ontology:constraint_victim(zero_mathematical_status__placeholder_reading, indian_algebraic_astronomers).
narrative_ontology:constraint_victim(zero_mathematical_status__placeholder_reading, syzygy_prediction_computers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(zero_mathematical_status__placeholder_reading, hellenistic_alexandrian_astronomers).
narrative_ontology:constraint_victim(zero_mathematical_status__placeholder_reading, indian_decimal_calculators).
narrative_ontology:constraint_victim(zero_mathematical_status__placeholder_reading, chinese_rod_calculation_offices).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Temple and palace accountants maintaining multiplication, reciprocal, and coefficient tables in base sixty. From the late periods they leave a blank space or cut a slanted wedge for an empty sexagesimal place, letting column position carry magnitude so a small symbol set suffices for any quantity. They never add, subtract, or divide the mark itself: trailing empty places are trimmed, and a result landing mid-number demands care the tables never formalize. Leaving scribal service forfeits their livelihood; the convention is the medium of their entire craft.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__placeholder_reading, babylonian_sexagesimal_scribes, beneficiary,
    organized, generational, constrained, regional).

% Computers of chord tables and planetary longitudes working in sexagesimal fractions, in the tradition that writes a small circle for an empty fractional place in the manner of Ptolemy's tables. They gain compact tabular computation of great precision. When a computed elongation lands exactly on zero — at a conjunction — the mark states the value, but the apparatus offers no rule for carrying it into the next addition, so they restart from tabular entries or annotate around the case.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__placeholder_reading, hellenistic_alexandrian_astronomers, beneficiary,
    organized, biographical, constrained, continental).
narrative_ontology:stakeholder_secondary_role(zero_mathematical_status__placeholder_reading, hellenistic_alexandrian_astronomers, payer).

% Hold that nothing is not a quantity: the void is denied, and a symbol naming absence cannot enter reckoning without corrupting the distinction between quantity and lack. Their arguments supply the prohibition the calculating trades inherit, and the regime's smooth operation continuously confirms their doctrine. They collect coherence and educational authority. Exit would mean dismantling the ontology their physics stands on — motion, place, and continuum all presuppose the plenum — so the position is not one they can step out of without ceasing to be what they are.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__placeholder_reading, peripatetic_philosophers, agenda_setter,
    institutional, generational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(zero_mathematical_status__placeholder_reading, peripatetic_philosophers, beneficiary).

% By the middle of the first millennium CE they compute in decimal place value with a dot or small circle — shunya — for the vacant column, compressing astronomical handbook computation and commercial arithmetic enormously. The same mark answers no operational question: a computation that yields an empty column stops there, and the era's textbooks give no procedure for it. They could revert to older non-positional methods only at a crushing loss of speed, so the convention is effectively their only road.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__placeholder_reading, indian_decimal_calculators, beneficiary,
    organized, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(zero_mathematical_status__placeholder_reading, indian_decimal_calculators, payer).

% Counting-board bureaus of the Han bureaucracy and successors. An empty row on the board marks zero place value, and the physical removal of rods makes the empty place natural to handle. But the blank is the absence of rods, not a rod: no move in the canon operates on it, and texts carry null results as skipped steps or verbal notes rather than as values that continue the computation.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__placeholder_reading, chinese_rod_calculation_offices, beneficiary,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(zero_mathematical_status__placeholder_reading, chinese_rod_calculation_offices, payer).

% Temple, palace, and later monastic schools that train calculators and certify table literacy. They transmit the convention, decide how the empty place is written in their house style, and discipline deviation from it. The convention's awkward cases — trimmed vacancies, mid-number blanks, results that land on the mark — sustain steady demand for certified instruction. Relaxing the boundary between writing the mark and computing with it would dissolve part of the gatekeeping role their standing rests on.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__placeholder_reading, scribal_training_establishments, agenda_setter,
    institutional, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(zero_mathematical_status__placeholder_reading, scribal_training_establishments, beneficiary).

% Computers of planetary mean and true longitudes and solvers of equations in which terms cancel. Their problems routinely produce null quantities: a longitude difference of exactly zero, a coefficient that vanishes. The convention gives them a way to write such a result and no way to use it, so they improvise — treating the mark as a quantity ad hoc — and eventually codify what improvisation taught them: the rules for shunya operations set down in Brahmagupta's chapter, which the wider scribal world absorbs slowly and imperfectly.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__placeholder_reading, indian_algebraic_astronomers, payer,
    moderate, biographical, constrained, continental).

% Predictors of conjunctions and oppositions who tabulate elongations passing through exact zero. The placeholder marks the zero crossing legibly, but stepping the table through the crossing — adding daily increments to a zero state — requires operating on the mark, which the convention forbids. They interpolate around the crossing from nonzero neighbors on either side, accepting error near the very events their tables exist to catch.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__placeholder_reading, syzygy_prediction_computers, payer,
    moderate, biographical, constrained, continental).

% Reconstruct the settlement over the mark's status from Babylonian tablets, Greek papyri, Indian manuscripts, and Chinese texts. They see the placeholder regime as one reading among three of a single contested kernel, watch the enforcement ratchet and the improvisations that breached it, and take no side in the dispute over what zero is.
narrative_ontology:constraint_stakeholder(zero_mathematical_status__placeholder_reading, historians_of_mathematics, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(zero_mathematical_status__placeholder_reading, diffuse).
narrative_ontology:fixing_cost_class(zero_mathematical_status__placeholder_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Makes positional notation usable: a mark for the empty place lets a column's value depend on position alone, so a fixed symbol set serves arbitrarily large magnitudes, tables compose, and algorithms scale without new signs per order of magnitude. Stated without evaluation.
% TRANSFER_FUNCTION: Moves computational labor savings to every user of positional notation, and moves the cost of incomplete arithmetic closure onto those whose computations produce or require null quantities — the algebraic astronomers and zero-crossing table computers — while preserving, at no operational price to the beneficiary seats, the doctrine that nothing is not a quantity.
% ABSENT_VOICES: The would-be advocates of operating on the mark were structurally absent for most of the interval: the convention consolidated centuries before anyone systematically needed zero-arithmetic, and no forum existed in which notation conventions were adjudicated across civilizations. When the need finally produced spokesmen — the algebraic-astronomer lineage — they spoke from inside the tradition and had to invent the rules themselves rather than contest the convention in any standing assembly. The excluded seat is therefore temporal as much as institutional: the objection arrived before its audience did.
% DISAPPEARANCE_RATIONALE: If the placeholder-only regime vanished overnight — if the mark were freed to carry operations — the entire downstream edifice reorganizes: difference tables step through zero crossings instead of interpolating around them, cancelled terms become first-class quantities, and the algebra of equations opens onto the manipulation of null results. Conversely, if the mark itself (under the parmenidean sibling) were banned outright, positional computation collapses for lack of a column disambiguator. Either way the world rearranges: the arrangement of ancient computation depends on this settlement having been made one way rather than another.
% FOUNDING_PROBLEM: Positional notation requires marking empty places, or column values become ambiguous (a blank between digits cannot be distinguished from a blank ending the number); the arrangement was built to solve that ambiguity while committing to no arithmetic object for 'nothing,' which the reigning ontological doctrines forbade.
% FOUNDING_PROBLEM_CORROBORATION: The disambiguation half of the founding problem is attested by the universal adoption of some empty-place device across every independent positional tradition. The exclusion half — that the mark must stay operationally inert — is attested by no one outside the beneficiary set: the payer-side textual record refutes it, since Brahmagupta's rules demonstrate the operations were definable all along, and historians of mathematics reading the Babylonian, Greek, and Indian records confirm the exclusion was a doctrinal inheritance rather than a technical necessity. The beneficiary seats themselves framed the exclusion as ontological necessity; no external source corroborates that framing.
narrative_ontology:disappearance_verdict(zero_mathematical_status__placeholder_reading, world_rearranges).
narrative_ontology:founding_problem_status(zero_mathematical_status__placeholder_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(zero_mathematical_status__placeholder_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(zero_mathematical_status__placeholder_reading, 'none', 1).
narrative_ontology:epsilon_provenance(zero_mathematical_status__placeholder_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(zero_mathematical_status__placeholder_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(zero_mathematical_status__placeholder_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(zero_mathematical_status__placeholder_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is intermediate (0.58 at interval end) and rising: the referent arrangement delivered genuine efficiency, but as computational ambition grew — difference tables, equation solving, ephemerides — the cost of denied closure grew with it, and the convention offered no compensation to the seats that paid it. Suppression (0.60) tracks the enforcement ratchet: the prohibition was never self-executing, and it hardened precisely as the number reading became viable — philosophical consolidation of the no-void doctrine, then curriculum-level policing of how the empty place might be written. Theater stays low (0.14 to 0.26): the placeholder function was real and heavily used; the growing theatrical share is ritual avoidance — rhetorical denunciation of nothingness, trimming of trailing vacancies, verbal notes substituting for operations — not the core activity. Accessibility_collapse is low-moderate (0.38): the alternatives (operating on the mark; refusing the mark entirely) remained live and were eventually exercised, so understanding the convention did not foreclose its rivals. Resistance is substantial (0.62): improvised operations on the mark, workaround annotation, and finally the codified rules of the shunya chapter are the record of sustained push. The three measurement series share one grid (T = 0, 200, ..., 1400; one unit = one year; T=0 approximates the consolidation of the Babylonian empty-place practice around the 7th century BCE, T=1400 falls one lifetime after Brahmagupta's rules of 628 CE entered circulation), so every metric is authored at every examined point. The trajectories are monotonic, not cyclical: this regime drifted, it did not oscillate — tension accumulated toward the codification event rather than cycling through crisis and reconciliation. Suppression_requirement is authored because the story specifically traces enforcement-capacity change (the ratchet), not merely shifting extraction. Suppression is authored as a raw structural property; only extractiveness is scaled by the engine.
 *
 * PERSPECTIVAL GAP:
 *   The payer and agenda-setter seats should compute divergent types from identical structural data. From the scribal-training seat, the convention is craft competence: the awkward cases are professional puzzles that sustain the value of certified instruction, and the boundary between writing a mark and operating on one is simply what mastery means. From the algebraic-astronomer seat, the same boundary is a wall across the road: the tool writes the answer and refuses to hand it over. Same-level divergence matters too: indian_decimal_calculators and indian_algebraic_astronomers are nominal peers in the same civilization at adjacent power levels, yet the former rides the convention (their problems stay positive) while the latter pays for it (their problems cross zero) — the differentiator is problem class, not rank. The engine computes these per-seat classifications; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality for the four practitioner seats and the two enforcer seats: the scribes, Alexandrian astronomers, decimal calculators, and rod offices collect efficiency (damped chi, subsidy-flavored at the beneficiary end), while the scribal establishments and peripatetic philosophers collect gatekeeping value and doctrinal coherence respectively. Victim declarations drive high directionality for the algebraic astronomers and syzygy computers: they bear the transfer (lost closure, stalled tables, improvised workarounds) with constrained exit — there is no non-positional route to their problems, and their professional identity is constituted through the very practice the constraint governs. The peripatetic philosophers are the one seat whose derived directionality understates their position: they benefit not by collecting rents but by vindication — the regime's operation continuously confirms the no-void doctrine listed under vindicated_propositions, and a vindicated proposition collects no rents even though the philosophers holding it collect authority. No directionality overrides are used: the role declarations plus exit differentiation already separate the same-power actors (organized beneficiaries versus moderate payers; constrained institutions versus the identity_locked philosophical school).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — disambiguating column values in positional notation — is permanently solved by any empty-place mark, and the mark alone solves it. The exclusion half (keeping the mark operationally inert) outlived its function the moment operations on shunya were demonstrably definable; the mandate had died but the arrangement persisted for centuries in transmission lag, curriculum inertia, and doctrinal habit, which is why mandatrophy_resolved is declared true. The R5 mismatch consumer will read founding_problem_status=dead against disappearance_verdict=world_rearranges and raise the capture/zombie flag; notably this is a sincere fossilization, not a theatrical one — theater_ratio stays low because the regime was never mostly performance, it was mostly function that refused to update. The classification prevents mislabeling in both directions: a pure-snare reading would erase the real, large efficiency gains delivered to every calculator seat (gains that made the regime durable and widely adopted); a pure-rope reading would erase the asymmetric closure cost borne by the algebraic lineage, who received the coordination benefit and were billed for its limits. Tangled rope holds both facts in one structure: coordinated efficiency, asymmetric payment, active enforcement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint is the placeholder_reading instantiation of kernel zero_mathematical_status: what structurally changes under the sibling readings number_reading and parmenidean_rejection, and where exactly is the disagreement located?',
    'Comparative structural analysis across the three sibling stories: number_reading defines operations on the mark (closure achieved, the lost-closure victim set dissolves, extraction falls toward the coordination floor); parmenidean_rejection removes the mark even from notation (every positional-calculator beneficiary seat converts to a victim, since positional efficiency is surrendered outright).',
    'Adopting number_reading eliminates this reading''s victim structure entirely; adopting parmenidean_rejection converts the beneficiary seats into victims. The disagreement is located in one structural element: whether the empty-place mark admits operations. This story authors epsilon only for the placeholder-only arrangement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: one reading of the zero-status kernel; siblings would redistribute the beneficiary and victim sets.').

omega_variable(
    placeholder_stage_contingency,
    'Is the notation-only stage a cognitively forced waystation (any civilization entering positional notation must first treat the empty-place mark as operationally inert) or a contingent construction reinforced by void-doctrines and scribal gatekeeping?',
    'Cross-cultural comparison: the Maya developed an explicit zero glyph embedded in calendrical computation without a documented inert-placeholder interlude; if multiple independent lineages reach operational zero directly, the placeholder stage is contingent rather than forced.',
    'If contingent, the regime''s multi-century persistence attributes to enforcement rather than cognitive necessity, raising the effective weight of suppression in classification; if forced, part of the measured extraction is the unavoidable price of concept formation and the constraint sits closer to a genuine developmental limit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(placeholder_stage_contingency, empirical, 'Whether the notation/number separation was inevitable or constructed.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression structural (doctrinal prohibition plus scribal gatekeeping and curriculum control) or internalized (calculators themselves unable to conceive of operating on the empty mark)?',
    'Examine improvisation episodes against transmission lag: where practitioners ad hoc operated on the mark despite doctrine (the road that leads to Brahmagupta''s rules), suppression was structural; where communities with access to defined rules continued blank-handling and trimming for generations, an internalized residue persisted after the structural barrier was gone.',
    'If internalized, effective suppression exceeds the structural measure: the constraint travels inside its targets and outlives its enforcement machinery, which matches the observed centuries-long transmission lag between the definition of shunya operations and their general adoption.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural versus internalized suppression mechanism in a conceptual constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(zero_mathematical_status__placeholder_reading, 0, 1400).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(zms_placeholder_tr_t0, zero_mathematical_status__placeholder_reading, theater_ratio, 0, 0.14).
narrative_ontology:measurement(zms_placeholder_tr_t200, zero_mathematical_status__placeholder_reading, theater_ratio, 200, 0.15).
narrative_ontology:measurement(zms_placeholder_tr_t400, zero_mathematical_status__placeholder_reading, theater_ratio, 400, 0.17).
narrative_ontology:measurement(zms_placeholder_tr_t600, zero_mathematical_status__placeholder_reading, theater_ratio, 600, 0.19).
narrative_ontology:measurement(zms_placeholder_tr_t800, zero_mathematical_status__placeholder_reading, theater_ratio, 800, 0.21).
narrative_ontology:measurement(zms_placeholder_tr_t1000, zero_mathematical_status__placeholder_reading, theater_ratio, 1000, 0.23).
narrative_ontology:measurement(zms_placeholder_tr_t1200, zero_mathematical_status__placeholder_reading, theater_ratio, 1200, 0.25).
narrative_ontology:measurement(zms_placeholder_tr_t1400, zero_mathematical_status__placeholder_reading, theater_ratio, 1400, 0.26).

% Extraction over time
narrative_ontology:measurement(zms_placeholder_be_t0, zero_mathematical_status__placeholder_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(zms_placeholder_be_t200, zero_mathematical_status__placeholder_reading, base_extractiveness, 200, 0.34).
narrative_ontology:measurement(zms_placeholder_be_t400, zero_mathematical_status__placeholder_reading, base_extractiveness, 400, 0.38).
narrative_ontology:measurement(zms_placeholder_be_t600, zero_mathematical_status__placeholder_reading, base_extractiveness, 600, 0.42).
narrative_ontology:measurement(zms_placeholder_be_t800, zero_mathematical_status__placeholder_reading, base_extractiveness, 800, 0.46).
narrative_ontology:measurement(zms_placeholder_be_t1000, zero_mathematical_status__placeholder_reading, base_extractiveness, 1000, 0.5).
narrative_ontology:measurement(zms_placeholder_be_t1200, zero_mathematical_status__placeholder_reading, base_extractiveness, 1200, 0.54).
narrative_ontology:measurement(zms_placeholder_be_t1400, zero_mathematical_status__placeholder_reading, base_extractiveness, 1400, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(zms_placeholder_su_t0, zero_mathematical_status__placeholder_reading, suppression_requirement, 0, 0.36).
narrative_ontology:measurement(zms_placeholder_su_t200, zero_mathematical_status__placeholder_reading, suppression_requirement, 200, 0.4).
narrative_ontology:measurement(zms_placeholder_su_t400, zero_mathematical_status__placeholder_reading, suppression_requirement, 400, 0.43).
narrative_ontology:measurement(zms_placeholder_su_t600, zero_mathematical_status__placeholder_reading, suppression_requirement, 600, 0.47).
narrative_ontology:measurement(zms_placeholder_su_t800, zero_mathematical_status__placeholder_reading, suppression_requirement, 800, 0.5).
narrative_ontology:measurement(zms_placeholder_su_t1000, zero_mathematical_status__placeholder_reading, suppression_requirement, 1000, 0.53).
narrative_ontology:measurement(zms_placeholder_su_t1200, zero_mathematical_status__placeholder_reading, suppression_requirement, 1200, 0.57).
narrative_ontology:measurement(zms_placeholder_su_t1400, zero_mathematical_status__placeholder_reading, suppression_requirement, 1400, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(zero_mathematical_status__placeholder_reading, information_standard).
narrative_ontology:affects_constraint(zero_mathematical_status__placeholder_reading, zero_mathematical_status__number_reading).
narrative_ontology:affects_constraint(zero_mathematical_status__placeholder_reading, zero_mathematical_status__parmenidean_rejection).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial notion 'zero' conflates three structurally distinct claims about one symbol's status. This file is the placeholder reading (notation yes, operations no; intermediate epsilon — efficiency gained, closure denied). zero_mathematical_status__number_reading is the upstream-established reading (operations defined and vindicated; epsilon near the coordination floor). zero_mathematical_status__parmenidean_rejection is the ontological-refusal reading (mark disallowed entirely; positional efficiency sacrificed, victim set maximal). The placeholder reading sits between them structurally and historically: it inherits the Babylonian-Greek notational practice cited as evidence by both siblings, and its enforcement ratchet is what the number reading had to overcome. All three files link one another via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
