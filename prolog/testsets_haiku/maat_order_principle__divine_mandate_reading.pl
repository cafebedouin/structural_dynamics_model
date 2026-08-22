% ============================================================================
% CONSTRAINT STORY: maat_order_principle__divine_mandate_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_maat_order_principle__divine_mandate_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: maat_order_principle__divine_mandate_reading
 *   human_readable: Ma'at Divine Mandate: Pharaoh as Cosmic Source
 *   domain: political_philosophy/religious_authority/ancient_governance
 *
 * SUMMARY:
 *   Ancient Egypt's divine mandate reading of Ma'at (cosmic order) positioned
 *   the Pharaoh as the sole terrestrial channel through which Ma'at flows
 *   from the cosmos into society. By definition, the Pharaoh embodies Ma'at
 *   and cannot violate it — any apparent violation must be a misunderstanding
 *   of cosmic necessity. This reading concentrates authority absolutely while
 *   immunizing it from constraint: the ruler cannot be bound by rules because
 *   the ruler IS the source of rule. The reading justifies massive extraction
 *   (labor, resources, devotion) as the price of cosmic maintenance. It
 *   suppresses alternative readings — the reciprocity reading (which would
 *   place mutual obligations on the Pharaoh) and the distributed-maintenance
 *   reading (which would distribute responsibility across society) — by
 *   treating them as threats to cosmic order itself. The constraint story
 *   author assesses this reading's structural form: high extraction justified
 *   through cosmological narrative, suppression enforced through theological
 *   impossibility of dissent, and a narrow theater ratio (much functional
 *   governance, but increasingly performative theodicy as actual
 *   environmental variation challenges the claim of perfect cosmic
 *   synchronization).
 *
 * KEY AGENTS:
 *   - Pharaonic authority: sole mediator of cosmic order, immune from constraint, maximum extraction
 *   - Priestly administrative apparatus: interprets Pharaonic will, benefits from the arrangement, constrained by dependence on royal legitimacy
 *   - Subject population: pays labor and resources, identity-locked through religious obligation, maximum suppression
 *   - Dissenting theological voices: would articulate alternative readings, structurally silenced, excluded from the conversation itself
 *   - Analytical observer: reconstructs the reading's structure and its extractive mechanisms
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(maat_order_principle__divine_mandate_reading, 0.82).
domain_priors:suppression_score(maat_order_principle__divine_mandate_reading, 0.89).
domain_priors:theater_ratio(maat_order_principle__divine_mandate_reading, 0.71).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(maat_order_principle__divine_mandate_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(maat_order_principle__divine_mandate_reading, suppression_requirement, 0.89).
narrative_ontology:constraint_metric(maat_order_principle__divine_mandate_reading, theater_ratio, 0.71).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(maat_order_principle__divine_mandate_reading, accessibility_collapse, 0.91).
narrative_ontology:constraint_metric(maat_order_principle__divine_mandate_reading, resistance, 0.34).

% --- Constraint claim ---
narrative_ontology:constraint_claim(maat_order_principle__divine_mandate_reading, snare).
narrative_ontology:human_readable(maat_order_principle__divine_mandate_reading, "Ma'at Divine Mandate: Pharaoh as Cosmic Source").
narrative_ontology:topic_domain(maat_order_principle__divine_mandate_reading, "political_philosophy/religious_authority/ancient_governance").

domain_priors:requires_active_enforcement(maat_order_principle__divine_mandate_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(maat_order_principle__divine_mandate_reading, 'e029556d-8aee-43b4-bad9-87b06a67fe30').
narrative_ontology:cs_kernel_codification('e029556d-8aee-43b4-bad9-87b06a67fe30', formalized).
narrative_ontology:cs_authority_grounding('e029556d-8aee-43b4-bad9-87b06a67fe30', extraction).
narrative_ontology:cs_interpretation_layer_present('e029556d-8aee-43b4-bad9-87b06a67fe30').
narrative_ontology:cs_reading_relation('e029556d-8aee-43b4-bad9-87b06a67fe30', maat_order_principle__reciprocity_reading, forecloses).
narrative_ontology:cs_reading_relation('e029556d-8aee-43b4-bad9-87b06a67fe30', maat_order_principle__distributed_maintenance_reading, coexists_with).
narrative_ontology:cs_axiom('e029556d-8aee-43b4-bad9-87b06a67fe30', foundational, pharaonic_immunity_from_constraint).
narrative_ontology:cs_axiom_status(pharaonic_immunity_from_constraint, holdable).
narrative_ontology:cs_axiom_grounding('e029556d-8aee-43b4-bad9-87b06a67fe30', pharaonic_immunity_from_constraint, deontological).
narrative_ontology:cs_axiom('e029556d-8aee-43b4-bad9-87b06a67fe30', foundational, cosmic_order_flows_through_singular_source).
narrative_ontology:cs_axiom_status(cosmic_order_flows_through_singular_source, holdable).
narrative_ontology:cs_axiom_grounding('e029556d-8aee-43b4-bad9-87b06a67fe30', cosmic_order_flows_through_singular_source, theological).
narrative_ontology:cs_reference_frame('e029556d-8aee-43b4-bad9-87b06a67fe30', cosmic_pharaonic_intermediation).
narrative_ontology:cs_drift_state('e029556d-8aee-43b4-bad9-87b06a67fe30', environmental_falsification_accumulation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e029556d-8aee-43b4-bad9-87b06a67fe30', '').
narrative_ontology:cs_kernel_id(maat_order_principle__divine_mandate_reading, maat_order_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(maat_order_principle__divine_mandate_reading, pharaonic_authority).
narrative_ontology:constraint_beneficiary(maat_order_principle__divine_mandate_reading, priestly_administrative_apparatus).
narrative_ontology:constraint_victim(maat_order_principle__divine_mandate_reading, subject_population).
narrative_ontology:constraint_victim(maat_order_principle__divine_mandate_reading, local_administrators).
narrative_ontology:constraint_victim(maat_order_principle__divine_mandate_reading, dissenting_theological_voices).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(maat_order_principle__divine_mandate_reading, successor_pharaohs).
narrative_ontology:constraint_victim(maat_order_principle__divine_mandate_reading, priestly_administrative_apparatus).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Positioned as the sole conduit through which cosmic Ma'at (divine order) flows to the terrestrial realm. By definition embodies Ma'at — cannot violate it because violation would require Ma'at to contradict itself. Makes all decisions on behalf of cosmic balance. Collects tribute, labor, and sacred authority as compensation for bearing this cosmological burden. Their position is immune to internal critique: any challenge to the Pharaoh's action is by definition a misunderstanding of Ma'at's manifestation through them.
narrative_ontology:constraint_stakeholder(maat_order_principle__divine_mandate_reading, pharaonic_authority, agenda_setter,
    institutional, civilizational, trapped, universal).

% Obligated to obey the Pharaoh's will as the expression of cosmic order. Their labor, crops, and loyalty sustain the state and ceremonial apparatus. Any deprivation is framed as cosmic necessity or their own failure to properly maintain Ma'at through obedience. Exit means death, exile, or religious condemnation as one who rejects cosmic order itself. Their costs are immense and perpetual; their voice in defining Ma'at is categorically excluded by the reading's own structure.
narrative_ontology:constraint_stakeholder(maat_order_principle__divine_mandate_reading, subject_population, payer,
    powerless, biographical, trapped, universal).

% Administers and interprets the Pharaoh's decrees as expressions of Ma'at. Receives ritual authority, land grants, and administrative privilege in exchange. They benefit from the arrangement but are also constrained by it — their legitimacy depends entirely on affirming the Pharaoh's cosmological role. They pay through intellectual subordination and institutional risk: if the Pharaoh's rule visibly fails, they must either rationalize the failure as cosmic correction or face replacement.
narrative_ontology:constraint_stakeholder(maat_order_principle__divine_mandate_reading, priestly_administrative_apparatus, beneficiary,
    organized, generational, constrained, universal).
narrative_ontology:stakeholder_secondary_role(maat_order_principle__divine_mandate_reading, priestly_administrative_apparatus, payer).

% Implement Pharaonic will in provinces. Bear responsibility for extracting resources and enforcing obedience to royal decree. They occupy a precarious position: they benefit from delegated authority but can be blamed or executed if outcomes displease the center. They pay through the constant risk of scapegoating and through enforcement costs that fall on their jurisdiction.
narrative_ontology:constraint_stakeholder(maat_order_principle__divine_mandate_reading, local_administrators, payer,
    moderate, biographical, constrained, regional).

% Would articulate alternative readings of Ma'at — that it is distributed, reciprocal, or subject to constraints even the Pharaoh must observe. They are structurally silenced: to speak is to be labeled as denying cosmic order itself, rendering the speaker not just wrong but cosmologically dangerous. Their identity as priests or thinkers depends on participation in the theological system; exit means loss of institutional voice and social standing.
narrative_ontology:constraint_stakeholder(maat_order_principle__divine_mandate_reading, dissenting_theological_voices, excluded,
    moderate, biographical, identity_locked, universal).

% Inherit the cosmological position but also inherit the constraint that they cannot violate Ma'at — because they ARE Ma'at. They benefit from absolute authority but are also trapped in it: they cannot claim to be subject to external law or alternative voices without immediately losing the reading's main legitimacy claim. Each Pharaoh must maintain the fiction of unsullied cosmic embodiment or admit to a regime-ending vulnerability.
narrative_ontology:constraint_stakeholder(maat_order_principle__divine_mandate_reading, successor_pharaohs, beneficiary,
    institutional, civilizational, trapped, universal).

% Reconstructs the historical and conceptual structure of the divine mandate reading from textual and administrative evidence, noting how power accumulates when authority claims immunity from critique by locating itself in cosmic law.
narrative_ontology:constraint_stakeholder(maat_order_principle__divine_mandate_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(maat_order_principle__divine_mandate_reading, pharaonic_authority).
narrative_ontology:fixing_cost_class(maat_order_principle__divine_mandate_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a unified cosmological framework that justifies centralized authority and obligation to the state. Solves the coordination problem of how to organize vast agricultural surplus and labor toward monumental works and ceremonial maintenance — by claiming the center is the cosmic hub through which all order flows.
% TRANSFER_FUNCTION: Moves labor, agricultural surplus, and devotional attention from the subject population and local administrators to the Pharaonic authority and its priestly apparatus, justified as the price of cosmic maintenance and the Pharaoh's cosmological burden.
% ABSENT_VOICES: Alternative theological readings that would place constraints even on Pharaonic action (the reciprocity and distributed-maintenance readings); philosophers or priests who question whether Ma'at truly demands absolute obedience; foreign or conquered populations who would dispute whether this cosmic order applies to them; women's roles in Ma'at (largely absent from the official theological surface); slaves and the enslaved, whose cosmological status is peripheral.
% DISAPPEARANCE_RATIONALE: If the divine mandate reading vanished — if the Pharaoh were suddenly understood not to embody Ma'at but to be bound by it — the entire legitimacy structure of centralized Pharaonic authority would collapse. Resources would cease flowing; the priestly apparatus would need to reconstitute itself outside royal patronage; local administrators would face a crisis of legitimacy. The successor readings (reciprocity, distributed maintenance) would immediately reorganize the state's understanding of obligation and authority.
% FOUNDING_PROBLEM: How to maintain cosmic order and ensure the Nile's fertility, which sustains all life. The founding problem is presented as: celestial/terrestrial synchronization requires a single earthly intermediary through whom divine will flows, and that intermediary cannot be constrained by terrestrial law without breaking the connection.
% FOUNDING_PROBLEM_CORROBORATION: The Pharaonic apparatus and priesthood attest the founding problem is live and unsolved except through the divine mandate. Historians and some surviving non-official texts suggest the problem was theological cover for centralized resource extraction — the Nile's behavior is naturalistic and does not require a cosmic intermediary. No external corroboration from outside the benefiting parties attests the founding problem in the divine mandate form; peasant and worker sources are largely absent from the surviving record, a structural silence enforced by the reading itself.
narrative_ontology:disappearance_verdict(maat_order_principle__divine_mandate_reading, world_rearranges).
narrative_ontology:founding_problem_status(maat_order_principle__divine_mandate_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(maat_order_principle__divine_mandate_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(maat_order_principle__divine_mandate_reading, 'none', 1).
narrative_ontology:epsilon_provenance(maat_order_principle__divine_mandate_reading, 0.82, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(maat_order_principle__divine_mandate_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(maat_order_principle__divine_mandate_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(maat_order_principle__divine_mandate_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   This reading scores high on extractiveness (0.82) because extraction is justified as cosmological necessity rather than negotiable policy — the Pharaoh does not merely take resources, the Pharaoh takes what cosmic order requires. Suppression is very high (0.89) because dissent is not merely illegal, it is cosmologically incoherent — to question the Pharaoh's will is to deny the cosmic order itself, placing the questioner outside legitimacy. Theater ratio is high (0.71) because as time passes and the Nile floods or fails according to natural cycles not perfectly coordinated with royal decree, more energy goes into explaining away the apparent failures and into ceremonial reaffirmation of the reading's premises — the gap between the promised perfect synchronization and actual variation is bridged theatrically rather than functionally. Resistance is low (0.34) because the populace lacks a coherent alternative framework and faces identity/religious annihilation for adopting one. The temporal series show extractiveness and suppression rising through the interval, plateauing, then stabilizing — this reflects institutional hardening: as the reading faces more environmental evidence against it, the state invests more in enforcement and ceremonial performance to maintain the fiction.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter (Pharaonic authority) and the payers (subject population, local administrators) compute radically different constraint experiences from this same reading. For the Pharaoh, the divine mandate is liberation — absolute authority unconstrained by law because law and authority are identical. For the subject, it is a snare — extraction justified through theological impossibility of resistance. The priestly apparatus occupies an intermediate position: they benefit from the reading but are also constrained by dependence on Pharaonic legitimacy. A priest cannot reframe Ma'at without immediately losing institutional voice; the reading traps them in its logic. The analytical observer sees the reading not as cosmic fact but as a power structure dressed in theological language. The engine computes these divergences from the stakeholder structure: power asymmetry (institutional vs. powerless), exit options (trapped vs. analytical), and role distribution (agenda-setter collecting vs. payers bearing costs).
 *
 * DIRECTIONALITY LOGIC:
 *   Pharaonic authority sits at d ≈ 0.0 (full beneficiary): collects extraction, sets the rules, cannot be constrained. Subject population sits at d ≈ 1.0 (full target): bears costs, has no legitimate exit, identity-locked into obedience through religious teaching. Priestly apparatus sits at d ≈ 0.35 (net beneficiary with substantial constraint): benefits from authority and land grants but is trapped in theological subordination to the Pharaoh; they have moderate exit (could theoretically articulate alternative theology) but identity-locked (doing so destroys their institutional role). Local administrators sit at d ≈ 0.65 (net payers with some power): benefit from delegated authority but bear scapegoating risk and enforcement burden; they have constrained exit (removing themselves means losing status and administrative position). The dissenting theological voices are structurally excluded — they have moderate power in the abstract but trapped exit (any voice means religious annihilation). The reading's structure itself enforces directionality asymmetry by making power and immunity inversely coupled: those with maximum power (Pharaoh) have maximum directionality advantage (d ≈ 0.0) because their position is defined as beyond constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   The divine mandate reading does not exhibit mandatrophy in the full sense (loss of original function while institution persists), but it is temporally trajectory shows a movement toward mandatrophy: the founding problem was presented as how to maintain cosmic order and ensure Nile fertility through celestial-terrestrial synchronization. Over the interval, as empirical variation in Nile floods (driven by climate and hydrology, not by Pharaonic piety) becomes harder to explain, the reading's functional claim weakens — the Pharaoh's decrees visibly do not control the Nile. Rather than abandoning the reading, the state intensifies theatrical performance (ceremonies, monuments, reaffirmations of cosmic connection) while the extractive function persists and even hardens. The theater_ratio rises from 0.58 to 0.71, indicating that by the interval's end, the reading operates partly as resource extraction justified through inert mythology, not as a live claim about cosmic causation. The reading is on a path toward pitonhood — the maintenance function (ensuring cosmic balance through royal action) atrophies, but the extraction function (collecting labor and resources) persists, supported by sunk institutional investment and the suppression machinery that prevents alternative frameworks from forming. The suppression_requirement remains high throughout, because the state must actively prevent the competing readings (reciprocity, distributed maintenance) from crystallizing into institutional alternatives.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cosmic_synchronization_vs_natural_variation,
    'Does the Nile''s flooding behavior actually correlate with Pharaonic piety and action, or is it determined by natural climate and hydrology independent of royal decree?',
    'Long-term empirical observation of flood cycles in relation to royal actions and theological claims; reconstruction of paleoclimate data and Nile records showing causation independent of royal behavior.',
    'If natural and independent: the founding problem''s stated solution (cosmic synchronization through the Pharaoh) is false, and the reading''s functional justification collapses — extraction persists as pure institutional inertia (piton trajectory). If correlated: the reading retains empirical force and functional claim (true coordination function). This omega''s resolution determines whether the constraint is a snare of power dressed in theology or a genuine (if unequally distributed) coordination mechanism.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cosmic_synchronization_vs_natural_variation, empirical, 'Whether Pharaonic action actually controls cosmic order or the correlation is illusory.').

omega_variable(
    foreclosure_vs_suppression,
    'Does the divine mandate reading logically foreclose the reciprocity reading (they cannot both be true in any framework), or does it merely suppress the reciprocity reading through institutional power (the readings are logically incompatible but can be held by different parties)?',
    'Logical analysis: can a framework simultaneously hold that the Pharaoh embodies Ma''at (and thus cannot violate it) AND that the Pharaoh is bound by reciprocal Ma''at obligations? If yes, they coexist (suppression). If no, they foreclose (logical incompatibility).',
    'If foreclosure: this reading''s core axiom (pharaonic_immunity_from_constraint) directly contradicts the reciprocity reading''s core axiom (pharaonic_reciprocal_obligation), and one reading''s falsification would settle the contest. If suppression: the readings occupy different parties'' belief systems, and the contest is political/institutional, not logical — defeating the divine-mandate reading requires defeating the institution, not just the argument.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(foreclosure_vs_suppression, conceptual, 'Logical structure of the reading relation between divine mandate and reciprocity readings.').

omega_variable(
    dissent_suppression_mechanism,
    'Is the high suppression of dissenting voices structurally enforced (legal penalties, execution, exile) or internalized (believers genuinely unable to conceive of the alternative without psychological rupture)?',
    'Analysis of historical evidence: how many dissenters are explicitly punished vs. how many sources indicate self-censorship or cognitive inability to articulate alternatives? Post-exit trajectory: what happens to ex-priests who abandon the theological framework — do they recover the ability to articulate alternatives, or do they carry the suppression with them?',
    'If structural: removing the enforcement apparatus would allow dissenting voices to crystallize, and the reading''s dominance would face immediate challenge. If internalized: the reading has colonized cognition itself, and even without enforcement, alternatives would be epistemically invisible. If mixed: suppression is amplified (structural enforcement reinforces internalized barriers), and the true suppression level may be higher than the measured 0.89.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dissent_suppression_mechanism, empirical, 'Suppression mechanism: structural vs. internalized in the dissenting voice exclusion.').

omega_variable(
    kernel_identity_vs_reading_choice,
    'Is Ma''at itself a reading-independent concept that all three readings are attempting to capture, or is Ma''at constituted by the readings — different concepts that happen to share a name?',
    'Theological and linguistic history: do pre-dynastic or alternative cultural sources use a ''Ma''at'' concept that is independent of the three readings? Or does the concept''s meaning shift entirely depending on which reading is active?',
    'If independent: the three readings are competing truth claims about the same reality, and empirical evidence could adjudicate. If constituted-by-readings: there is no kernel outside the readings, and the concept of ''Ma''at'' itself is a battleground — the contest is not about what Ma''at requires, but about what gets to count as Ma''at.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_identity_vs_reading_choice, conceptual, 'Ontological status of Ma''at as kernel vs. contingent on readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(maat_order_principle__divine_mandate_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(maat_tr_t0, maat_order_principle__divine_mandate_reading, theater_ratio, 0, 0.58).
narrative_ontology:measurement_basis(maat_tr_t0, observed).
narrative_ontology:measurement(maat_tr_t20, maat_order_principle__divine_mandate_reading, theater_ratio, 20, 0.62).
narrative_ontology:measurement_basis(maat_tr_t20, observed).
narrative_ontology:measurement(maat_tr_t40, maat_order_principle__divine_mandate_reading, theater_ratio, 40, 0.68).
narrative_ontology:measurement_basis(maat_tr_t40, observed).
narrative_ontology:measurement(maat_tr_t60, maat_order_principle__divine_mandate_reading, theater_ratio, 60, 0.72).
narrative_ontology:measurement_basis(maat_tr_t60, observed).
narrative_ontology:measurement(maat_tr_t80, maat_order_principle__divine_mandate_reading, theater_ratio, 80, 0.71).
narrative_ontology:measurement_basis(maat_tr_t80, observed).
narrative_ontology:measurement(maat_tr_t100, maat_order_principle__divine_mandate_reading, theater_ratio, 100, 0.71).
narrative_ontology:measurement_basis(maat_tr_t100, observed).

% Extraction over time
narrative_ontology:measurement(maat_be_t0, maat_order_principle__divine_mandate_reading, base_extractiveness, 0, 0.76).
narrative_ontology:measurement_basis(maat_be_t0, observed).
narrative_ontology:measurement(maat_be_t20, maat_order_principle__divine_mandate_reading, base_extractiveness, 20, 0.79).
narrative_ontology:measurement_basis(maat_be_t20, observed).
narrative_ontology:measurement(maat_be_t40, maat_order_principle__divine_mandate_reading, base_extractiveness, 40, 0.81).
narrative_ontology:measurement_basis(maat_be_t40, observed).
narrative_ontology:measurement(maat_be_t60, maat_order_principle__divine_mandate_reading, base_extractiveness, 60, 0.83).
narrative_ontology:measurement_basis(maat_be_t60, observed).
narrative_ontology:measurement(maat_be_t80, maat_order_principle__divine_mandate_reading, base_extractiveness, 80, 0.82).
narrative_ontology:measurement_basis(maat_be_t80, observed).
narrative_ontology:measurement(maat_be_t100, maat_order_principle__divine_mandate_reading, base_extractiveness, 100, 0.82).
narrative_ontology:measurement_basis(maat_be_t100, observed).

% Suppression requirement over time
narrative_ontology:measurement(maat_su_t0, maat_order_principle__divine_mandate_reading, suppression_requirement, 0, 0.84).
narrative_ontology:measurement_basis(maat_su_t0, observed).
narrative_ontology:measurement(maat_su_t20, maat_order_principle__divine_mandate_reading, suppression_requirement, 20, 0.86).
narrative_ontology:measurement_basis(maat_su_t20, observed).
narrative_ontology:measurement(maat_su_t40, maat_order_principle__divine_mandate_reading, suppression_requirement, 40, 0.88).
narrative_ontology:measurement_basis(maat_su_t40, observed).
narrative_ontology:measurement(maat_su_t60, maat_order_principle__divine_mandate_reading, suppression_requirement, 60, 0.9).
narrative_ontology:measurement_basis(maat_su_t60, observed).
narrative_ontology:measurement(maat_su_t80, maat_order_principle__divine_mandate_reading, suppression_requirement, 80, 0.89).
narrative_ontology:measurement_basis(maat_su_t80, observed).
narrative_ontology:measurement(maat_su_t100, maat_order_principle__divine_mandate_reading, suppression_requirement, 100, 0.89).
narrative_ontology:measurement_basis(maat_su_t100, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(maat_order_principle__divine_mandate_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(maat_order_principle__divine_mandate_reading, 0.18).
narrative_ontology:affects_constraint(maat_order_principle__divine_mandate_reading, maat_order_principle__reciprocity_reading).
narrative_ontology:affects_constraint(maat_order_principle__divine_mandate_reading, maat_order_principle__distributed_maintenance_reading).

% DUAL FORMULATION NOTE:
% The kernel 'maat_order_principle' decomposes into three structurally distinct constraint stories: divine_mandate_reading (this constraint, where Ma'at flows through the Pharaoh who cannot violate it), reciprocity_reading (where Ma'at imposes mutual obligations), and distributed_maintenance_reading (where Ma'at maintenance is collective responsibility). Each reading instantiates different beneficiaries, victims, and suppression mechanisms from the same kernel. The readings are linked by reading_relations (foreclosure, coexistence, influence) declared in cs_structure. This story's instantiation is the divine-mandate reading; the sibling readings carry their own ε values, stakeholder structures, and temporal dynamics. The divine-mandate reading influences both siblings by controlling institutional vocabulary and suppressing alternative theological articulation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
