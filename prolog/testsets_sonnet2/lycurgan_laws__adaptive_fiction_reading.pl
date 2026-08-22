% ============================================================================
% CONSTRAINT STORY: lycurgan_laws__adaptive_fiction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_lycurgan_laws__adaptive_fiction_reading, []).

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
 *   constraint_id: lycurgan_laws__adaptive_fiction_reading
 *   human_readable: Lycurgan Constitutional Immutability (Adaptive Fiction Reading)
 *   domain: political/constitutional/religious
 *
 * SUMMARY:
 *   This story reads Sparta's constitutional myth of Lycurgan immutability as
 *   a noble lie: a legitimating fiction that concealed continuous,
 *   elite-managed adaptation of citizenship, land, and military rules. On
 *   this reading, the ephors and kings never actually operated under a fixed
 *   code — they reinterpreted the ancestral rhetra whenever demographic and
 *   political pressure required it (tightening or loosening helot control,
 *   adjusting the kleros/citizenship threshold, reallocating land through
 *   inheritance manipulation), while publicly insisting nothing had changed
 *   since Lycurgus. The coordination function (stable succession and land
 *   settlement) is real, but the extraction (elite land concentration,
 *   citizenship-stripping of the hypomeiones, and opportunistic helot
 *   subjugation) rides on the same immutability claim that supplies the
 *   coordination cover. Demographic collapse (oliganthropia) is read here as
 *   a symptom of enforcement failure and elite capture, NOT as an inevitable
 *   consequence of rigid rule-following, distinguishing this reading sharply
 *   from the sibling demographic_trap_reading.
 *
 * KEY AGENTS:
 *   - spartan_dual_kings: primary beneficiary and agenda-setter, institutional/arbitrage — invokes immutability while quietly reinterpreting succession and land rules
 *   - ephorate: co-agenda-setter, institutional/mobile — administers the fluid citizenship boundary under a claim of fixed law
 *   - hypomeiones_declining_citizens: primary target, powerless/trapped — loses citizenship as the 'unchanging' threshold is actually adjusted
 *   - helot_population: primary target, powerless/trapped — subjugation intensity varies opportunistically despite being framed as eternal
 *   - later_greek_historians: analytical observer — documents the gap between claimed fixity and observed institutional drift
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(lycurgan_laws__adaptive_fiction_reading, 0.58).
domain_priors:suppression_score(lycurgan_laws__adaptive_fiction_reading, 0.62).
domain_priors:theater_ratio(lycurgan_laws__adaptive_fiction_reading, 0.71).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lycurgan_laws__adaptive_fiction_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(lycurgan_laws__adaptive_fiction_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(lycurgan_laws__adaptive_fiction_reading, theater_ratio, 0.71).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(lycurgan_laws__adaptive_fiction_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(lycurgan_laws__adaptive_fiction_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(lycurgan_laws__adaptive_fiction_reading, tangled_rope).
narrative_ontology:human_readable(lycurgan_laws__adaptive_fiction_reading, "Lycurgan Constitutional Immutability (Adaptive Fiction Reading)").
narrative_ontology:topic_domain(lycurgan_laws__adaptive_fiction_reading, "political/constitutional/religious").

domain_priors:requires_active_enforcement(lycurgan_laws__adaptive_fiction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(lycurgan_laws__adaptive_fiction_reading, '9909ef04-eb49-46c7-be08-d7752dab6b56').
narrative_ontology:cs_kernel_codification('9909ef04-eb49-46c7-be08-d7752dab6b56', fixed_text).
narrative_ontology:cs_authority_grounding('9909ef04-eb49-46c7-be08-d7752dab6b56', extraction).
narrative_ontology:cs_interpretation_layer_present('9909ef04-eb49-46c7-be08-d7752dab6b56').
narrative_ontology:cs_reading_relation('9909ef04-eb49-46c7-be08-d7752dab6b56', lycurgan_laws__sacral_fidelity_reading, forecloses).
narrative_ontology:cs_reading_relation('9909ef04-eb49-46c7-be08-d7752dab6b56', lycurgan_laws__demographic_trap_reading, influences).
narrative_ontology:cs_axiom('9909ef04-eb49-46c7-be08-d7752dab6b56', foundational, immutability_claim_is_instrumental_legitimation).
narrative_ontology:cs_axiom_status(immutability_claim_is_instrumental_legitimation, holdable).
narrative_ontology:cs_axiom_grounding('9909ef04-eb49-46c7-be08-d7752dab6b56', immutability_claim_is_instrumental_legitimation, instrumental).
narrative_ontology:cs_axiom('9909ef04-eb49-46c7-be08-d7752dab6b56', secondary, institutional_drift_is_elite_managed_not_emergent).
narrative_ontology:cs_axiom_status(institutional_drift_is_elite_managed_not_emergent, holdable).
narrative_ontology:cs_axiom_grounding('9909ef04-eb49-46c7-be08-d7752dab6b56', institutional_drift_is_elite_managed_not_emergent, empirically_contingent).
narrative_ontology:cs_reference_frame('9909ef04-eb49-46c7-be08-d7752dab6b56', lycurgan_founding_rhetra).
narrative_ontology:cs_drift_state('9909ef04-eb49-46c7-be08-d7752dab6b56', late_classical_oliganthropia_crisis, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('9909ef04-eb49-46c7-be08-d7752dab6b56', '').
narrative_ontology:cs_kernel_id(lycurgan_laws__adaptive_fiction_reading, lycurgan_laws).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(lycurgan_laws__adaptive_fiction_reading, spartan_dual_kings).
narrative_ontology:constraint_beneficiary(lycurgan_laws__adaptive_fiction_reading, ephorate).
narrative_ontology:constraint_beneficiary(lycurgan_laws__adaptive_fiction_reading, gerousia_elders).
narrative_ontology:constraint_victim(lycurgan_laws__adaptive_fiction_reading, hypomeiones_declining_citizens).
narrative_ontology:constraint_victim(lycurgan_laws__adaptive_fiction_reading, perioikoi_communities).
narrative_ontology:constraint_victim(lycurgan_laws__adaptive_fiction_reading, helot_population).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(lycurgan_laws__adaptive_fiction_reading, delphic_oracle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Invoke Lycurgus's supposed divine oath binding all Spartans to unalterable law, while quietly reinterpreting inheritance, military obligation, and land-tenure rules to consolidate estates and manage succession crises. Their public posture is strict fidelity to the ancestral constitution; their practice is continuous, selective reinterpretation that preserves their own prerogatives.
narrative_ontology:constraint_stakeholder(lycurgan_laws__adaptive_fiction_reading, spartan_dual_kings, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(lycurgan_laws__adaptive_fiction_reading, spartan_dual_kings, beneficiary).

% Annually elected overseers who interpret and enforce the ancestral laws, including expelling citizens who fail the agoge or fall below the land-share threshold. They present themselves as neutral guardians of an unchanging code but exercise wide discretion over who counts as a full citizen, effectively adapting the citizenship boundary to demographic and political pressure while denying that any adaptation occurs.
narrative_ontology:constraint_stakeholder(lycurgan_laws__adaptive_fiction_reading, ephorate, agenda_setter,
    institutional, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(lycurgan_laws__adaptive_fiction_reading, ephorate, beneficiary).

% The council of elders holds veto power over legislative proposals by claiming fidelity to Lycurgus's original design. In practice their rulings shift with the political needs of the ruling families, but the claim of immutability shields their discretionary judgments from being seen as ordinary politics.
narrative_ontology:constraint_stakeholder(lycurgan_laws__adaptive_fiction_reading, gerousia_elders, beneficiary,
    powerful, generational, constrained, national).

% Spartiates who lose their land allotment (kleros) through debt, inheritance division, or failure to meet syssitia contribution requirements are quietly stripped of full citizenship. The law is enforced as though it were an immutable natural boundary, but the threshold and its application shift as elite land concentration accelerates — they bear the cost of a 'fixed' rule that is actually adjusted to elite convenience.
narrative_ontology:constraint_stakeholder(lycurgan_laws__adaptive_fiction_reading, hypomeiones_declining_citizens, payer,
    powerless, biographical, trapped, local).

% Free but non-citizen communities subject to Spartan military levy and foreign-policy control, justified by the same ancestral-constitution rhetoric. Their obligations are adjusted upward whenever Spartan manpower shortages worsen, while the constitutional narrative insists nothing has changed.
narrative_ontology:constraint_stakeholder(lycurgan_laws__adaptive_fiction_reading, perioikoi_communities, payer,
    moderate, generational, constrained, regional).

% Enserfed agricultural laborers whose subjugation is framed as part of the eternal Lycurgan order (including the annually renewed ritual declaration of war on them by the ephors). The intensity of control over them is tightened or loosened opportunistically as Spartiate numbers fall, even while officially declared unchanging.
narrative_ontology:constraint_stakeholder(lycurgan_laws__adaptive_fiction_reading, helot_population, payer,
    powerless, generational, trapped, local).

% The oracle's endorsement of Lycurgus's laws (the rhetra) supplies the divine warrant that makes the immutability claim credible. Its priesthood benefits from being the institution whose authority is cited whenever kings or ephors need to legitimize a reinterpretation as merely 'clarifying' the original divine command.
narrative_ontology:constraint_stakeholder(lycurgan_laws__adaptive_fiction_reading, delphic_oracle, beneficiary,
    institutional, civilizational, analytical, regional).

% Writers such as Xenophon, Aristotle, and Plutarch document the gap between Sparta's claimed constitutional fixity and its observable institutional drift (land laws, citizenship thresholds, military obligations), providing the retrospective evidence that the immutability claim functioned as legitimating cover rather than descriptive fact.
narrative_ontology:constraint_stakeholder(lycurgan_laws__adaptive_fiction_reading, later_greek_historians, observer,
    analytical, civilizational, analytical, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(lycurgan_laws__adaptive_fiction_reading, spartan_dual_kings).
narrative_ontology:fixing_cost_class(lycurgan_laws__adaptive_fiction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Anchoring political and military obligations to a single, unquestionable ancestral authority (Lycurgus, ratified by Delphi) solves a genuine coordination problem: it prevents recurring legitimacy contests every time land, citizenship, or military rules need adjustment, by letting elites frame necessary changes as 'restoration' rather than innovation.
% TRANSFER_FUNCTION: Moves land, citizenship status, and labor extraction rights from declining Spartiate households and subject populations toward the ruling kings, ephors, and landed elite, under cover of a rule that is publicly declared never to change.
% ABSENT_VOICES: The hypomeiones who lost citizenship, and the helots whose subjugation intensified with each Spartiate manpower crisis, have no formal voice in the Gerousia or Apella in a form that could challenge the 'ancestral law' framing; their exclusion is precisely what allows the elite to reinterpret the law without acknowledging it as reinterpretation.
% DISAPPEARANCE_RATIONALE: If the fiction of Lycurgan immutability were openly abandoned, the kings, ephors, and Gerousia would need to justify citizenship stripping, land redistribution favoring elites, and helot subjugation as ordinary contested policy rather than sacred inheritance — this would open all three to political challenge and likely accelerate demands for land redistribution (as later reformers like Agis IV and Cleomenes III in fact attempted, explicitly by claiming to 'restore' the true Lycurgan order).
% FOUNDING_PROBLEM: Archaic Sparta needed to resolve the Messenian land settlement, the balance between the two royal houses, and the integration of a conquered helot population into a stable military-agricultural order, without a written code that could be reopened by every subsequent political faction.
% FOUNDING_PROBLEM_CORROBORATION: Aristotle's Politics and Plutarch's Lycurgus (writing centuries later, with access to earlier sources and to Sparta's own visible institutional decline) both note that the actual land tenure, citizenship, and military systems had diverged substantially from any single founding settlement by the classical period, while the Spartan state itself continued to assert unbroken adherence — corroboration comes from outside the Spartan ruling apparatus, not from ephors or kings themselves.
narrative_ontology:disappearance_verdict(lycurgan_laws__adaptive_fiction_reading, world_rearranges).
narrative_ontology:founding_problem_status(lycurgan_laws__adaptive_fiction_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(lycurgan_laws__adaptive_fiction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(lycurgan_laws__adaptive_fiction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(lycurgan_laws__adaptive_fiction_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(lycurgan_laws__adaptive_fiction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(lycurgan_laws__adaptive_fiction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(lycurgan_laws__adaptive_fiction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) and theater ratio (0.71, rising over the interval) are both moderately high because the story's central claim is that a growing share of 'constitutional fidelity' activity was performative cover for adaptation that elites needed anyway — the theater ratio rises as the gap between rhetoric and practice widens across the classical period. Suppression (0.62) reflects the ephors' real coercive machinery (agoge enforcement, krypteia against helots, citizenship-stripping) but is authored below the extraction ceiling because much of the constraint's hold operates through the LEGITIMACY of the immutability claim rather than raw force alone. Accessibility collapse is moderate (0.45) — alternatives to the Lycurgan framing existed and were periodically proposed (Agis IV, Cleomenes III) but were suppressed as impious rather than genuinely foreclosed. Resistance is moderate-low (0.4): most resistance came generations later once the fiction had visibly failed, not continuously across the interval.
 *
 * DIRECTIONALITY LOGIC:
 *   Kings, ephors, and Gerousia elders sit near the full-beneficiary end: they administer the reinterpretation and capture its gains (land, prerogative, political control) while bearing minimal cost themselves — hence d is derived low despite formally being 'subject to the same laws.' The hypomeiones, perioikoi, and helots sit near the full-target end: trapped or heavily constrained exit, bearing the costs of boundary adjustments they cannot contest without appearing impious. The delphic_oracle is a genuine beneficiary of the arrangement's legitimating function but is a non-human institution; its directionality is derived from its structural role as legitimation-supplier rather than any land or labor stake.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resolves a mandatrophy risk directly: a naive reading of Sparta's own self-description would classify the Lycurgan system as a Mountain (immutable natural/divine law). The adaptive_fiction_reading instead classifies it as a Tangled Rope — a genuine coordination function (stable succession, integrated land settlement) fused with asymmetric extraction (elite capture of the citizenship/land threshold) that REQUIRES active enforcement (ephor discretion, krypteia, expulsion of the hypomeiones) to sustain the fiction. Treating the surface mountain-claim as literal would have hidden the extraction; treating the whole system as pure Snare would miss the real coordination problem (preventing endless succession disputes) that the fiction genuinely solved for a period.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fiction_vs_genuine_belief,
    'Did Spartan elites (kings, ephors, Gerousia) consciously understand the immutability claim as a legitimating fiction, or did they sincerely believe in Lycurgan sacrality while nonetheless drifting in practice through motivated reasoning rather than deliberate deception?',
    'Comparative analysis of contemporaneous (5th-4th century BCE) versus later (Hellenistic/Roman-era, e.g. Plutarch) source layers to distinguish original Spartan self-understanding from retrospective rationalization; examination of whether reform attempts (Agis IV, Cleomenes III) framed their departures as restoration (suggesting sincere belief in an original that had been corrupted) or as necessary adaptation (suggesting conscious fiction-management).',
    'If elites sincerely believed in immutability while drifting via motivated reasoning, the ''noble lie'' framing is too strong — this would be better modeled as institutionalized self-deception, which lowers confidence in requires_active_enforcement being deliberate rather than emergent. If deliberate, the tangled_rope classification with high theater_ratio is strongly supported as-authored.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(fiction_vs_genuine_belief, conceptual, 'Whether the immutability claim was consciously instrumentalized or sincerely (if inconsistently) held.').

omega_variable(
    kernel_reading_selection_evidence,
    'What in the source record specifically supports reading Lycurgan immutability as adaptive fiction rather than sacral fidelity or demographic trap — is the evidence for covert adaptation strong enough to be the PRIMARY structural claim, or is it one strand among genuinely fixed elements?',
    'Systematic cataloguing of documented rule changes (land law reforms attributed to Epitadeus, shifting citizenship thresholds, evolving military obligations) against the total set of Lycurgan-attributed institutions, to establish what proportion of the ''constitution'' actually drifted versus remained stable.',
    'If most institutions were genuinely stable and only a few adapted, this reading may overstate its case relative to sacral_fidelity_reading; if drift was pervasive, this reading''s tangled_rope classification is the more defensible structural read of the three.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_evidence, empirical, 'How much of the historical record actually supports the adaptive-fiction structural claim versus the sibling readings.').

omega_variable(
    demographic_causation_attribution,
    'Is Spartan demographic collapse (oliganthropia) better explained by enforcement failure and elite land concentration (this reading) or by the system''s brittleness under literal unrevisability (the demographic_trap_reading)?',
    'Quantitative reconstruction of land distribution patterns and citizen-count trajectories from Herodotus through Aristotle''s Politics (which explicitly discusses Spartan land concentration as a cause of decline), cross-referenced against known military losses (e.g. Leuctra) to separate structural rigidity effects from elite-capture effects.',
    'Determines which of the two sibling readings (this one or demographic_trap_reading) better fits the causal record; the readings are not mutually exclusive but this omega documents where the primary causal weight should sit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(demographic_causation_attribution, empirical, 'Whether demographic collapse is better attributed to elite capture (this reading) or systemic rigidity (sibling reading).').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(lycurgan_laws__adaptive_fiction_reading, 0, 400).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lycu_tr_t0, lycurgan_laws__adaptive_fiction_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(lycu_tr_t60, lycurgan_laws__adaptive_fiction_reading, theater_ratio, 60, 0.45).
narrative_ontology:measurement(lycu_tr_t120, lycurgan_laws__adaptive_fiction_reading, theater_ratio, 120, 0.55).
narrative_ontology:measurement(lycu_tr_t200, lycurgan_laws__adaptive_fiction_reading, theater_ratio, 200, 0.62).
narrative_ontology:measurement(lycu_tr_t280, lycurgan_laws__adaptive_fiction_reading, theater_ratio, 280, 0.68).
narrative_ontology:measurement(lycu_tr_t340, lycurgan_laws__adaptive_fiction_reading, theater_ratio, 340, 0.71).
narrative_ontology:measurement(lycu_tr_t400, lycurgan_laws__adaptive_fiction_reading, theater_ratio, 400, 0.71).

% Extraction over time
narrative_ontology:measurement(lycu_be_t0, lycurgan_laws__adaptive_fiction_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(lycu_be_t60, lycurgan_laws__adaptive_fiction_reading, base_extractiveness, 60, 0.36).
narrative_ontology:measurement(lycu_be_t120, lycurgan_laws__adaptive_fiction_reading, base_extractiveness, 120, 0.42).
narrative_ontology:measurement(lycu_be_t200, lycurgan_laws__adaptive_fiction_reading, base_extractiveness, 200, 0.49).
narrative_ontology:measurement(lycu_be_t280, lycurgan_laws__adaptive_fiction_reading, base_extractiveness, 280, 0.54).
narrative_ontology:measurement(lycu_be_t340, lycurgan_laws__adaptive_fiction_reading, base_extractiveness, 340, 0.57).
narrative_ontology:measurement(lycu_be_t400, lycurgan_laws__adaptive_fiction_reading, base_extractiveness, 400, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(lycu_su_t0, lycurgan_laws__adaptive_fiction_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(lycu_su_t60, lycurgan_laws__adaptive_fiction_reading, suppression_requirement, 60, 0.44).
narrative_ontology:measurement(lycu_su_t120, lycurgan_laws__adaptive_fiction_reading, suppression_requirement, 120, 0.5).
narrative_ontology:measurement(lycu_su_t200, lycurgan_laws__adaptive_fiction_reading, suppression_requirement, 200, 0.55).
narrative_ontology:measurement(lycu_su_t280, lycurgan_laws__adaptive_fiction_reading, suppression_requirement, 280, 0.6).
narrative_ontology:measurement(lycu_su_t340, lycurgan_laws__adaptive_fiction_reading, suppression_requirement, 340, 0.62).
narrative_ontology:measurement(lycu_su_t400, lycurgan_laws__adaptive_fiction_reading, suppression_requirement, 400, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(lycurgan_laws__adaptive_fiction_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(lycurgan_laws__adaptive_fiction_reading, 0.1).
narrative_ontology:affects_constraint(lycurgan_laws__adaptive_fiction_reading, lycurgan_laws__sacral_fidelity_reading).
narrative_ontology:affects_constraint(lycurgan_laws__adaptive_fiction_reading, lycurgan_laws__demographic_trap_reading).

% DUAL FORMULATION NOTE:
% This story is one of three readings of the lycurgan_laws kernel. sacral_fidelity_reading treats the immutability claim as genuine sacred law (Mountain-flavored, near-zero extraction by that reading's own lights). demographic_trap_reading treats the same claim as literally binding and structurally brittle, causing demographic collapse through unrevisability rather than elite manipulation. This reading (adaptive_fiction_reading) treats the immutability claim as legitimating theater covering continuous elite-managed adaptation — Tangled Rope, moderate-high extraction, rising theater_ratio. All three share the same kernel (the Lycurgan constitutional claim) but instantiate structurally distinct constraints with different ε values, different beneficiary/victim structures, and different classifications, per the ε-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
