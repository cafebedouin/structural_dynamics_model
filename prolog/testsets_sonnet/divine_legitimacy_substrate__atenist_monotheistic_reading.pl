% ============================================================================
% CONSTRAINT STORY: divine_legitimacy_substrate__atenist_monotheistic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_divine_legitimacy_substrate__atenist_monotheistic_reading, []).

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
 *   constraint_id: divine_legitimacy_substrate__atenist_monotheistic_reading
 *   human_readable: Atenist Exclusive Revelation Doctrine (Pharaonic Monopoly)
 *   domain: religious/political economy
 *
 * SUMMARY:
 *   This story instantiates the Atenist reading of the contested
 *   divine-legitimacy kernel in New Kingdom Egypt: Pharaoh Akhenaten's
 *   declaration that Aten alone is divine and that he alone is the legitimate
 *   channel of Aten's revelation. This is structurally distinct from the
 *   Amun-priesthood reading (legitimacy through established priestly
 *   interpretation of a multi-deity cosmology) and the folk-syncretistic
 *   reading (legitimacy through pragmatic household/village multi-deity
 *   practice) — those are separate constraints, not alternate measurements of
 *   this one. The Atenist reading concentrates interpretive authority
 *   exclusively in the pharaoh's person, dismantles the rival temple economy
 *   that previously distributed religious-political power, and requires
 *   continuous enforcement (temple closures, monument erasure, doctrinal
 *   promulgation) to hold. Its rapid reversal after Akhenaten's death is
 *   itself evidence of how enforcement-dependent, rather than naturally
 *   persistent, this particular reading was.
 *
 * KEY AGENTS:
 *   - pharaoh_akhenaten: agenda_setter/beneficiary (institutional/arbitrage) — declares exclusive revelation and redirects temple wealth to the crown
 *   - new_royal_administrative_cadre: beneficiary (powerful/mobile) — rises on loyalty to the new doctrine
 *   - amun_priesthood: payer (organized/trapped) — loses institutional standing and income
 *   - temple_dependent_laborers: payer (powerless/trapped) — loses livelihood tied to temple estates
 *   - provincial_cult_communities: payer/excluded (powerless/constrained) — worship criminalized by decree
 *   - later_restoration_dynasty: observer (institutional/analytical) — reverses the doctrine, evidencing its instability
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(divine_legitimacy_substrate__atenist_monotheistic_reading, 0.81).
domain_priors:suppression_score(divine_legitimacy_substrate__atenist_monotheistic_reading, 0.88).
domain_priors:theater_ratio(divine_legitimacy_substrate__atenist_monotheistic_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(divine_legitimacy_substrate__atenist_monotheistic_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__atenist_monotheistic_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__atenist_monotheistic_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(divine_legitimacy_substrate__atenist_monotheistic_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__atenist_monotheistic_reading, resistance, 0.79).

% --- Constraint claim ---
narrative_ontology:constraint_claim(divine_legitimacy_substrate__atenist_monotheistic_reading, tangled_rope).
narrative_ontology:human_readable(divine_legitimacy_substrate__atenist_monotheistic_reading, "Atenist Exclusive Revelation Doctrine (Pharaonic Monopoly)").
narrative_ontology:topic_domain(divine_legitimacy_substrate__atenist_monotheistic_reading, "religious/political economy").

domain_priors:requires_active_enforcement(divine_legitimacy_substrate__atenist_monotheistic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(divine_legitimacy_substrate__atenist_monotheistic_reading, '07660445-03cf-41a5-9380-3f712527c51f').
narrative_ontology:cs_kernel_codification('07660445-03cf-41a5-9380-3f712527c51f', formalized).
narrative_ontology:cs_authority_grounding('07660445-03cf-41a5-9380-3f712527c51f', extraction).
narrative_ontology:cs_interpretation_layer_present('07660445-03cf-41a5-9380-3f712527c51f').
narrative_ontology:cs_reading_relation('07660445-03cf-41a5-9380-3f712527c51f', divine_legitimacy_substrate__amun_polytheistic_reading, forecloses).
narrative_ontology:cs_reading_relation('07660445-03cf-41a5-9380-3f712527c51f', divine_legitimacy_substrate__folk_syncretistic_reading, forecloses).
narrative_ontology:cs_axiom('07660445-03cf-41a5-9380-3f712527c51f', foundational, aten_sole_true_deity).
narrative_ontology:cs_axiom_status(aten_sole_true_deity, holdable).
narrative_ontology:cs_axiom_grounding('07660445-03cf-41a5-9380-3f712527c51f', aten_sole_true_deity, theological).
narrative_ontology:cs_axiom('07660445-03cf-41a5-9380-3f712527c51f', foundational, pharaoh_exclusive_mediator_of_revelation).
narrative_ontology:cs_axiom_status(pharaoh_exclusive_mediator_of_revelation, overridden).
narrative_ontology:cs_axiom_grounding('07660445-03cf-41a5-9380-3f712527c51f', pharaoh_exclusive_mediator_of_revelation, conventional).
narrative_ontology:cs_reference_frame('07660445-03cf-41a5-9380-3f712527c51f', exclusive_pharaonic_revelation_of_aten).
narrative_ontology:cs_drift_state('07660445-03cf-41a5-9380-3f712527c51f', post_akhenaten_restoration, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('07660445-03cf-41a5-9380-3f712527c51f', '').
narrative_ontology:cs_kernel_id(divine_legitimacy_substrate__atenist_monotheistic_reading, divine_legitimacy_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__atenist_monotheistic_reading, pharaoh_akhenaten).
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__atenist_monotheistic_reading, new_royal_administrative_cadre).
narrative_ontology:constraint_victim(divine_legitimacy_substrate__atenist_monotheistic_reading, amun_priesthood).
narrative_ontology:constraint_victim(divine_legitimacy_substrate__atenist_monotheistic_reading, temple_dependent_laborers).
narrative_ontology:constraint_victim(divine_legitimacy_substrate__atenist_monotheistic_reading, provincial_cult_communities).
narrative_ontology:constraint_vindicates(divine_legitimacy_substrate__atenist_monotheistic_reading, pharaonic_sole_mediatorship_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Declares himself the sole legitimate channel through which Aten's will is known, relocates the capital to Akhetaten, orders the closure and defunding of other temples, and redirects their landholdings and labor to the new cult and crown administration. Collects both religious authority and the material wealth previously distributed across temple networks.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__atenist_monotheistic_reading, pharaoh_akhenaten, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(divine_legitimacy_substrate__atenist_monotheistic_reading, pharaoh_akhenaten, beneficiary).

% Officials elevated by loyalty to the new cult rather than by traditional priestly lineage; they receive appointments, land grants, and status that depend entirely on the doctrine's continuation and on Akhenaten's personal favor.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__atenist_monotheistic_reading, new_royal_administrative_cadre, beneficiary,
    powerful, biographical, mobile, national).

% Stripped of temple income, ritual authority, and institutional standing as Amun and other cults are declared false and their temples closed. Cannot practice openly, cannot appeal to a competing legitimating authority since the pharaoh has monopolized revelation, and has nowhere else to go within Egypt's political structure.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__atenist_monotheistic_reading, amun_priesthood, payer,
    organized, generational, trapped, national).

% Farmers, artisans, and workers whose livelihoods were organized around temple estates lose employment and social insurance functions the temples provided when those estates are confiscated or abandoned. They have no say in the doctrinal change and no alternative economic structure to move into locally.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__atenist_monotheistic_reading, temple_dependent_laborers, payer,
    powerless, biographical, trapped, regional).

% Local and household worship of traditional deities is delegitimized by decree; communities continue informal practice at personal risk because open participation in the new state cult is required for civic standing, while their own devotional life is officially designated as worship of false gods.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__atenist_monotheistic_reading, provincial_cult_communities, payer,
    powerless, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(divine_legitimacy_substrate__atenist_monotheistic_reading, provincial_cult_communities, excluded).

% Record and transmit the new doctrine's texts and hymns, positioned to see both the theological claims and the administrative reallocation of temple wealth that accompanies them, without independent power to alter the arrangement.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__atenist_monotheistic_reading, royal_court_scribes, observer,
    moderate, biographical, constrained, national).

% The successor administration that reverses the doctrine after Akhenaten's death, restoring Amun's temples and erasing Atenist monuments — their subsequent actions retroactively evidence how contested and unstable the exclusive-revelation claim was.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__atenist_monotheistic_reading, later_restoration_dynasty, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(divine_legitimacy_substrate__atenist_monotheistic_reading, pharaoh_akhenaten).
narrative_ontology:fixing_cost_class(divine_legitimacy_substrate__atenist_monotheistic_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, unambiguous locus of religious-political authority, eliminating the need to arbitrate among competing priestly claims and cosmological traditions — in principle simplifying legitimacy disputes to a single revealed channel.
% TRANSFER_FUNCTION: Moves temple landholdings, agricultural surplus, ritual labor obligations, and political patronage away from the Amun priesthood and provincial cult networks toward the pharaoh's person and the newly created administrative cadre loyal to the Aten cult.
% ABSENT_VOICES: The Amun priesthood and provincial worshippers are structurally silenced — the doctrine itself declares their gods false, so there is no legitimate discursive space within the system for them to object; their objection can only register outside the system, in later restoration.
% DISAPPEARANCE_RATIONALE: The doctrine's disappearance (which is exactly what happened after Akhenaten's death) triggered immediate, large-scale rearrangement: temple estates were restored, Akhetaten was abandoned, monuments were defaced, and the prior priestly economic and religious order was reconstituted almost in full — demonstrating the arrangement's dependence on active enforcement rather than natural persistence.
% FOUNDING_PROBLEM: Framed by the pharaoh as resolving theological confusion and consolidating fragmented, temple-based political power (particularly the wealth and influence of the Amun priesthood) under a single unifying revelation.
% FOUNDING_PROBLEM_CORROBORATION: Akhenaten's own inscriptions attest the problem as theological (falseness of other gods) and civilizational renewal. Later restoration-era inscriptions and priestly records — produced by the very institutions the doctrine displaced, and thus not disinterested, but external to the Atenist court — characterize the episode as a seizure of temple wealth rather than a genuine theological correction; no source independent of both court and priesthood survives to adjudicate between these attestations.
narrative_ontology:disappearance_verdict(divine_legitimacy_substrate__atenist_monotheistic_reading, world_rearranges).
narrative_ontology:founding_problem_status(divine_legitimacy_substrate__atenist_monotheistic_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(divine_legitimacy_substrate__atenist_monotheistic_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(divine_legitimacy_substrate__atenist_monotheistic_reading, 'none', 1).
narrative_ontology:epsilon_provenance(divine_legitimacy_substrate__atenist_monotheistic_reading, 0.81, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(divine_legitimacy_substrate__atenist_monotheistic_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(divine_legitimacy_substrate__atenist_monotheistic_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(divine_legitimacy_substrate__atenist_monotheistic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises sharply (0.38 to 0.81) over the interval as temple confiscations accelerate and crown/administrative wealth concentration deepens — this is not a claim about theology's truth but about the material transfer riding on the doctrinal declaration. Suppression tracks closely (0.45 to 0.88) because the arrangement's persistence depends on active measures: temple closures, iconoclasm against other cults, promulgation of the exclusive-revelation doctrine through court texts and monuments. Theater ratio is moderate (0.25 to 0.42): genuine theological content and genuine administrative reallocation coexist with escalating monumental and hymnic performance of the doctrine's truth, which grows as a legitimating overlay on the underlying transfer.
 *
 * PERSPECTIVAL GAP:
 *   From Akhenaten's seat, the arrangement is coordination: resolving theological plurality into a single coherent revealed truth, ending fragmented priestly authority. From the Amun priesthood's seat, structurally identical acts (temple closure, doctrinal promulgation) register as extraction backed by suppression — their institutional destruction and the confiscation of their economic base. The engine computing divergent per-seat types from the same structural facts is exactly the point: the coordination story and the extraction story are not competing interpretations of different events, they are the same events read from opposite ends of the transfer.
 *
 * DIRECTIONALITY LOGIC:
 *   Akhenaten sits at the extreme beneficiary end: he is simultaneously the doctrine's sole legitimate interpreter and the direct recipient of the wealth and authority redirected from other cults — an unusually clean case of agenda-setter and beneficiary collapsing into one seat. The Amun priesthood and provincial worshippers sit near the full-target end: organized/powerless populations whose prior standing is directly negated by the doctrine's core claim (their gods are false), with trapped or constrained exit because there is no alternative legitimating authority within the system to appeal to. The administrative cadre occupies an intermediate position — beneficiaries of the transfer but dependent on the doctrine's survival, which explains their rapid disappearance from the historical record once the doctrine collapsed.
 *
 * MANDATROPHY ANALYSIS:
 *   The doctrine's own genealogy after Akhenaten's death — swift, near-total reversal by the restoration dynasty — is diagnostic. If the arrangement had been solving a genuine, still-live coordination problem (reconciling incompatible theological traditions), its collapse should have provoked crisis or partial retention of its functional elements. Instead the restoration reconstituted the prior temple economy almost wholesale, indicating the 'founding problem' the doctrine claimed to solve (theological confusion, fragmented power) was substantially a pretext for concentrating resources in the crown, not a persistent structural need the society required solving. This prevents mislabeling the doctrine's rapid, near-complete unwinding as a tragedy of good coordination lost — the evidence points toward extraction whose scaffolding fell when its enforcer died.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    genuine_revelation_vs_political_instrument,
    'Was Akhenaten''s exclusive-revelation claim a sincerely held theological conviction that happened to have redistributive consequences, or was the theological claim instrumentally adopted (or amplified) to dismantle a rival power center (the Amun priesthood)?',
    'Comparative analysis of the timing and sequencing of theological promulgation versus temple confiscation orders; examination of whether doctrinal intensity tracks administrative need for resource consolidation across the reign.',
    'If primarily sincere theology with incidental transfer effects, the coordination reading (resolving cosmological plurality) carries more analytical weight even though the metrics remain extractive. If primarily instrumental, the tangled_rope classification''s extraction component dominates and the coordination framing is closer to cover story.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(genuine_revelation_vs_political_instrument, conceptual, 'Whether the exclusivity doctrine was sincere theology or instrumentalized power consolidation.').

omega_variable(
    kernel_reading_selection_ambiguity,
    'Is the atenist_monotheistic_reading the correct structural decomposition of ''Egyptian divine legitimacy'' for this period, or should it instead be modeled as a temporary deviation absorbed within a single longer-running amun_polytheistic_reading constraint (i.e., a brief interruption rather than a distinct kernel reading)?',
    'Assess whether the doctrinal, economic, and institutional discontinuity during the Amarna period was sufficient to constitute a structurally distinct legitimacy regime versus a transient royal policy within a persistent underlying system.',
    'If modeled as a transient deviation rather than a distinct reading, the network relationship to amun_polytheistic_reading should be recharacterized from coexisting sibling to a temporary override, changing how contamination/restoration dynamics propagate between the two stories.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, conceptual, 'Whether the Atenist period is a distinct kernel reading or a transient deviation within the Amun reading''s persistence.').

omega_variable(
    beneficiary_scope_of_administrative_cadre,
    'Did the new administrative cadre benefit net-positively across the whole episode, given that many were likely purged or lost standing during the restoration, or were they merely short-term beneficiaries with eventual negative net exposure?',
    'Prosopographic tracing of named Amarna-period officials into the post-restoration record, where available, to determine survival and status outcomes.',
    'If the cadre suffered net losses after restoration, their directionality should shift closer to symmetric or even target rather than pure beneficiary, since their gains were conditional and short-lived.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_scope_of_administrative_cadre, empirical, 'Whether the administrative beneficiaries'' gains were durable or wiped out by the restoration.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(divine_legitimacy_substrate__atenist_monotheistic_reading, 0, 17).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(divi_tr_t0, divine_legitimacy_substrate__atenist_monotheistic_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(divi_tr_t3, divine_legitimacy_substrate__atenist_monotheistic_reading, theater_ratio, 3, 0.3).
narrative_ontology:measurement(divi_tr_t6, divine_legitimacy_substrate__atenist_monotheistic_reading, theater_ratio, 6, 0.34).
narrative_ontology:measurement(divi_tr_t9, divine_legitimacy_substrate__atenist_monotheistic_reading, theater_ratio, 9, 0.38).
narrative_ontology:measurement(divi_tr_t12, divine_legitimacy_substrate__atenist_monotheistic_reading, theater_ratio, 12, 0.4).
narrative_ontology:measurement(divi_tr_t17, divine_legitimacy_substrate__atenist_monotheistic_reading, theater_ratio, 17, 0.42).

% Extraction over time
narrative_ontology:measurement(divi_be_t0, divine_legitimacy_substrate__atenist_monotheistic_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(divi_be_t3, divine_legitimacy_substrate__atenist_monotheistic_reading, base_extractiveness, 3, 0.55).
narrative_ontology:measurement(divi_be_t6, divine_legitimacy_substrate__atenist_monotheistic_reading, base_extractiveness, 6, 0.68).
narrative_ontology:measurement(divi_be_t9, divine_legitimacy_substrate__atenist_monotheistic_reading, base_extractiveness, 9, 0.75).
narrative_ontology:measurement(divi_be_t12, divine_legitimacy_substrate__atenist_monotheistic_reading, base_extractiveness, 12, 0.79).
narrative_ontology:measurement(divi_be_t17, divine_legitimacy_substrate__atenist_monotheistic_reading, base_extractiveness, 17, 0.81).

% Suppression requirement over time
narrative_ontology:measurement(divi_su_t0, divine_legitimacy_substrate__atenist_monotheistic_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(divi_su_t3, divine_legitimacy_substrate__atenist_monotheistic_reading, suppression_requirement, 3, 0.62).
narrative_ontology:measurement(divi_su_t6, divine_legitimacy_substrate__atenist_monotheistic_reading, suppression_requirement, 6, 0.74).
narrative_ontology:measurement(divi_su_t9, divine_legitimacy_substrate__atenist_monotheistic_reading, suppression_requirement, 9, 0.81).
narrative_ontology:measurement(divi_su_t12, divine_legitimacy_substrate__atenist_monotheistic_reading, suppression_requirement, 12, 0.85).
narrative_ontology:measurement(divi_su_t17, divine_legitimacy_substrate__atenist_monotheistic_reading, suppression_requirement, 17, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(divine_legitimacy_substrate__atenist_monotheistic_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(divine_legitimacy_substrate__atenist_monotheistic_reading, 0.08).
narrative_ontology:affects_constraint(divine_legitimacy_substrate__atenist_monotheistic_reading, amun_polytheistic_reading).
narrative_ontology:affects_constraint(divine_legitimacy_substrate__atenist_monotheistic_reading, folk_syncretistic_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the divine_legitimacy_substrate kernel. amun_polytheistic_reading models legitimacy flowing through established priestly interpretation of the pre-existing multi-deity cosmology (higher accessibility, lower suppression, broader distributed benefit across a priestly class). folk_syncretistic_reading models legitimacy through pragmatic household/village practice incorporating multiple deities (minimal enforcement, low extraction, closer to genuine rope). This atenist reading has substantially higher extractiveness and suppression than either sibling because it uniquely claims interpretive exclusivity concentrated in a single person and requires active dismantling of the prior distributed structure to hold. The three readings are not the same constraint measured three ways — each has a distinct ε, distinct beneficiary/victim structure, and distinct persistence mechanism; they are linked here because they compete for the same legitimating role within the same historical system.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
