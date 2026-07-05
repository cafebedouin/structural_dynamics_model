% ============================================================================
% CONSTRAINT STORY: lycurgan_laws__sacral_fidelity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_lycurgan_laws__sacral_fidelity_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: lycurgan_laws__sacral_fidelity_reading
 *   human_readable: Lycurgan Laws as Sacred, Unchangeable Divine Ordinance (Sacral Fidelity Reading)
 *   domain: political_philosophy/constitutional_theory
 *
 * SUMMARY:
 *   This story instantiates the sacral fidelity reading of the Lycurgan
 *   kernel: the claim, held internally by Spartan civic ideology and its
 *   magistracies, that the constitution attributed to Lycurgus is divinely
 *   sanctioned, fixed, and beyond legitimate human revision. Within this
 *   reading, immutability is not a design flaw but the very source of the
 *   constitution's legitimacy and stability — deviation from ancestral custom
 *   is treated as impiety and civic corruption, and Sparta's eventual decline
 *   is attributed, from within this frame, to external military pressures
 *   (Theban resurgence, the loss of Messenian helot labor after Leuctra) or
 *   to citizen moral decay (luxury, foreign gold), never to the design of the
 *   constitution itself. This is a distinct constraint from the
 *   demographic_trap_reading (which locates Spartan collapse causally in the
 *   oliganthropia produced by rigid inheritance and citizenship rules) and
 *   the adaptive_fiction_reading (which treats the immutability claim itself
 *   as a functional fiction covering real, ongoing adaptation). Each reading
 *   has its own epsilon and its own classification; they are linked here only
 *   via the kernel, not merged.
 *
 * KEY AGENTS:
 *   - spartan_gerousia: agenda_setter (institutional/identity_locked) — administers and enforces the sacral reading of the constitution
 *   - ephorate: agenda_setter/beneficiary (institutional/identity_locked) — derives its entire prosecutorial authority from the immutability claim
 *   - homoioi_citizen_class: beneficiary/payer (organized/identity_locked) — formed from childhood to hold the belief, gains status from it, bears its austerity
 *   - helot_population: excluded (powerless/trapped) — entirely outside the sacral framing's field of moral concern
 *   - constitutional_historians: observer (analytical/analytical) — sees the doctrinal claim as one reading among several, contested from outside
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(lycurgan_laws__sacral_fidelity_reading, 0.42).
domain_priors:suppression_score(lycurgan_laws__sacral_fidelity_reading, 0.71).
domain_priors:theater_ratio(lycurgan_laws__sacral_fidelity_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lycurgan_laws__sacral_fidelity_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(lycurgan_laws__sacral_fidelity_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(lycurgan_laws__sacral_fidelity_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(lycurgan_laws__sacral_fidelity_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(lycurgan_laws__sacral_fidelity_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(lycurgan_laws__sacral_fidelity_reading, mountain).
narrative_ontology:human_readable(lycurgan_laws__sacral_fidelity_reading, "Lycurgan Laws as Sacred, Unchangeable Divine Ordinance (Sacral Fidelity Reading)").
narrative_ontology:topic_domain(lycurgan_laws__sacral_fidelity_reading, "political_philosophy/constitutional_theory").

domain_priors:requires_active_enforcement(lycurgan_laws__sacral_fidelity_reading).
domain_priors:emerges_naturally(lycurgan_laws__sacral_fidelity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(lycurgan_laws__sacral_fidelity_reading, '4ee4ac31-f1a4-4886-bf44-ee3c364b9a25').
narrative_ontology:cs_kernel_codification('4ee4ac31-f1a4-4886-bf44-ee3c364b9a25', fixed_text).
narrative_ontology:cs_authority_grounding('4ee4ac31-f1a4-4886-bf44-ee3c364b9a25', lineage).
narrative_ontology:cs_interpretation_layer_present('4ee4ac31-f1a4-4886-bf44-ee3c364b9a25').
narrative_ontology:cs_reading_relation('4ee4ac31-f1a4-4886-bf44-ee3c364b9a25', lycurgan_laws__adaptive_fiction_reading, coexists_with).
narrative_ontology:cs_reading_relation('4ee4ac31-f1a4-4886-bf44-ee3c364b9a25', lycurgan_laws__demographic_trap_reading, influences).
narrative_ontology:cs_axiom('4ee4ac31-f1a4-4886-bf44-ee3c364b9a25', foundational, immutability_is_divinely_sanctioned_virtue).
narrative_ontology:cs_axiom_status(immutability_is_divinely_sanctioned_virtue, holdable).
narrative_ontology:cs_axiom_grounding('4ee4ac31-f1a4-4886-bf44-ee3c364b9a25', immutability_is_divinely_sanctioned_virtue, theological).
narrative_ontology:cs_axiom('4ee4ac31-f1a4-4886-bf44-ee3c364b9a25', secondary, decline_caused_by_external_vice_not_design).
narrative_ontology:cs_axiom_status(decline_caused_by_external_vice_not_design, holdable).
narrative_ontology:cs_axiom_grounding('4ee4ac31-f1a4-4886-bf44-ee3c364b9a25', decline_caused_by_external_vice_not_design, conventional).
narrative_ontology:cs_reference_frame('4ee4ac31-f1a4-4886-bf44-ee3c364b9a25', divine_founder_sanction).
narrative_ontology:cs_drift_state('4ee4ac31-f1a4-4886-bf44-ee3c364b9a25', post_leuctra_decline, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('4ee4ac31-f1a4-4886-bf44-ee3c364b9a25', '').
narrative_ontology:cs_kernel_id(lycurgan_laws__sacral_fidelity_reading, lycurgan_laws).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(lycurgan_laws__sacral_fidelity_reading, spartan_gerousia).
narrative_ontology:constraint_beneficiary(lycurgan_laws__sacral_fidelity_reading, ephorate).
narrative_ontology:constraint_beneficiary(lycurgan_laws__sacral_fidelity_reading, homoioi_citizen_class).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(lycurgan_laws__sacral_fidelity_reading, homoioi_citizen_class).
narrative_ontology:constraint_vindicates(lycurgan_laws__sacral_fidelity_reading, divine_founder_authority_doctrine).
narrative_ontology:constraint_vindicates(lycurgan_laws__sacral_fidelity_reading, constitutional_immutability_as_virtue).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The council of elders administers and interprets the Lycurgan rhetra, treating it as fixed divine law delivered through Apollo's oracle at Delphi. They enforce adherence through the agoge, the mess-halls, and social sanction, and understand their own authority as flowing directly from fidelity to the unaltered ordinance rather than from any discretionary power of their own.
narrative_ontology:constraint_stakeholder(lycurgan_laws__sacral_fidelity_reading, spartan_gerousia, agenda_setter,
    institutional, civilizational, identity_locked, regional).

% The five annually-elected ephors police compliance with the ancestral constitution, prosecuting kings and citizens alike for departures from custom. Their office exists only because the laws are held immutable; they collect the authority to judge precisely because the standard they judge by cannot be renegotiated.
narrative_ontology:constraint_stakeholder(lycurgan_laws__sacral_fidelity_reading, ephorate, agenda_setter,
    institutional, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(lycurgan_laws__sacral_fidelity_reading, ephorate, beneficiary).

% Full Spartan citizens are formed from childhood by the agoge into believing the laws are sacred and unalterable; this belief secures their standing, land allotment, and communal identity, but also binds them to austerity, lifelong military obligation, and prohibition on commerce, from which there is no honorable exit without forfeiting citizenship itself.
narrative_ontology:constraint_stakeholder(lycurgan_laws__sacral_fidelity_reading, homoioi_citizen_class, beneficiary,
    organized, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(lycurgan_laws__sacral_fidelity_reading, homoioi_citizen_class, payer).

% The oracular sanction attributed to Lycurgus's founding is invoked as the ultimate warrant for the constitution's unchangeability. It is not itself an actor but the doctrinal anchor the sacral reading points to whenever revision is proposed and refused.
narrative_ontology:constraint_stakeholder(lycurgan_laws__sacral_fidelity_reading, delphic_oracle_tradition, observer,
    institutional, civilizational, analytical, regional).
narrative_ontology:stakeholder_non_agent(lycurgan_laws__sacral_fidelity_reading, delphic_oracle_tradition).

% The subjugated agricultural labor force whose surplus sustains the leisure-for-military-training economy the Lycurgan system requires is entirely outside the sacral framing's field of concern; the sacral reading treats the constitution's legitimacy as a matter between citizens and divine ordinance, not as a question about who bears the productive cost of that arrangement.
narrative_ontology:constraint_stakeholder(lycurgan_laws__sacral_fidelity_reading, helot_population, excluded,
    powerless, biographical, trapped, regional).

% Later observers, ancient and modern, examine whether the immutability claim was ever historically accurate or whether it functioned as retrospective legitimation for laws that in fact changed. From the sacral reading's own internal premises, this observation is treated as irrelevant or hostile, since it questions the very naturalness the reading asserts.
narrative_ontology:constraint_stakeholder(lycurgan_laws__sacral_fidelity_reading, constitutional_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(lycurgan_laws__sacral_fidelity_reading, diffuse).
narrative_ontology:fixing_cost_class(lycurgan_laws__sacral_fidelity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides Sparta with a single, non-negotiable civic and military discipline: every citizen is raised under one code, eliminating factional dispute over what the law requires and binding the whole citizen body to a common martial standard.
% TRANSFER_FUNCTION: Moves discretion away from any living lawgiver, magistrate, or assembly and toward a fixed, sacralized text/custom; citizens surrender the capacity to renegotiate terms of citizenship in exchange for a stable, unquestionable civic identity and status within the homoioi class.
% ABSENT_VOICES: The helot population, whose forced agricultural labor underwrites the entire leisure-class military economy the laws presuppose, has no standing within the sacral framing at all — the reading's field of legitimate concern begins and ends with citizens.
% DISAPPEARANCE_RATIONALE: If the divine-ordinance framing collapsed, the ephorate's authority to prosecute deviation would lose its warrant, the agoge's total claim on citizen formation would become contestable, and citizens could argue for revision of land allotment, military obligation, or civic rights on ordinary political grounds rather than being foreclosed by sacred unrevisability.
% FOUNDING_PROBLEM: Archaic Sparta faced factional instability, land redistribution conflict, and a need to bind a small citizen class into total military readiness against a much larger subjugated population; the Lycurgan rhetra is presented as solving this by fixing the terms of citizenship as divinely settled and beyond dispute.
% FOUNDING_PROBLEM_CORROBORATION: The Gerousia and ephorate, from inside the tradition, attest the founding problem (civic factionalism, divine sanction needed) as still live and the ordinance as still necessary. Outside the benefiting institutions, ancient historians (Thucydides on Spartan secrecy and rigidity, Aristotle's Politics critique of the Spartan constitution) and modern demographic historians attest that the sacral framing is itself a retrospective legitimation device and that the underlying problem the laws claim to solve was neither singular nor permanently settled by any one founding act.
narrative_ontology:disappearance_verdict(lycurgan_laws__sacral_fidelity_reading, world_rearranges).
narrative_ontology:founding_problem_status(lycurgan_laws__sacral_fidelity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(lycurgan_laws__sacral_fidelity_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(lycurgan_laws__sacral_fidelity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(lycurgan_laws__sacral_fidelity_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(lycurgan_laws__sacral_fidelity_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(lycurgan_laws__sacral_fidelity_reading, ExtMetricName, E),
    domain_priors:suppression_score(lycurgan_laws__sacral_fidelity_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(lycurgan_laws__sacral_fidelity_reading),
    narrative_ontology:constraint_metric(lycurgan_laws__sacral_fidelity_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(lycurgan_laws__sacral_fidelity_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(lycurgan_laws__sacral_fidelity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored moderate (0.42) rather than low, because even within this reading the sacralization of the constitution serves the concentrated interest of the Gerousia and ephorate in insulating their own authority from renegotiation — this is a real, if modest, asymmetric benefit riding on a genuine coordination function (civic unity, martial discipline). Suppression is authored high (0.71) because the sacral reading depends on the agoge's total formation of belief and on the ephorate's active prosecutorial enforcement against deviation; this is not passive natural law, it requires continuous institutional maintenance to hold. Accessibility collapse is authored high (0.80) because, from within the citizen class, the sacralized frame genuinely does foreclose alternative civic imaginaries almost completely — a citizen raised in the agoge has few felt options. Resistance is authored low-moderate (0.35): overt resistance from citizens is rare precisely because suppression operates mostly through identity formation rather than external coercion, though ephor prosecutions of kings (Pausanias, others) show the enforcement machinery was regularly invoked. Theater ratio rises across the measured interval (0.30 to 0.58) because as Sparta's actual military and demographic position weakened after the 4th century BCE, the sacral rhetoric of unchangeable perfection increasingly outran the system's functional performance — a classic Goodhart drift where the doctrine's performative assertion intensifies as its substantive success declines.
 *
 * DIRECTIONALITY LOGIC:
 *   The Gerousia and ephorate sit closest to the beneficiary end: their institutional authority is generated by, not merely protected by, the immutability claim, and they have no meaningful exit from advocating it since their office has no other warrant. The homoioi citizen class is genuinely dual-positioned — real coordination benefit (civic status, land allotment, communal identity) alongside real cost (lifelong military obligation, austerity, foreclosed alternatives), which the identity_locked exit option reflects: leaving the belief system means leaving citizenship itself. Helots are excluded from the frame entirely rather than positioned as payers within it, because the sacral reading's moral universe does not extend to them — their exploitation is invisible to this reading's own terms, which is itself a notable structural fact about the reading rather than a victim relationship the reading acknowledges.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy question under this reading is suppressed by design: because the constitution is held sacred and unrevisable, the very question of whether its founding function (early civic stabilization against factional land conflict) remains live is treated as impious to ask. The founding_problem_status is authored 'contested' rather than 'dead' precisely because the sacral reading's own adherents insist the problem (divine sanction, civic unity) is permanently, not just currently, solved — a claim of permanent solution is one of the diagnostic markers separating this reading from the demographic_trap and adaptive_fiction readings, which both treat the founding problem as having drifted or failed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sacral_reading_naturalness_vs_construction,
    'Is the Lycurgan constitution''s unrevisability a genuine natural-law-like feature of Spartan civic order (as the sacral reading holds), or a constructed doctrinal claim that benefits the Gerousia and ephorate by insulating their authority from renegotiation?',
    'Comparative institutional history: examine whether comparable Greek poleis without a sacralized founder-myth achieved comparable civic stability, and whether the rhetra''s content is independently attested pre-5th-century or reconstructed retrospectively by later sources (Plutarch, writing centuries after the fact).',
    'If the immutability claim is shown to be a retrospective construction serving the magistracies'' authority, this reading is a false summit (FSM) — a mountain claim covering an identifiable beneficiary structure — and the engine''s reclassification toward tangled_rope would be the diagnostically correct outcome. If genuinely naturalized through long, uncontested practice, the mountain claim holds more strongly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sacral_reading_naturalness_vs_construction, conceptual, 'Whether sacral immutability is natural civic order or constructed magisterial insulation.').

omega_variable(
    kernel_reading_disagreement_location,
    'Where exactly do the three Lycurgan-kernel readings (sacral_fidelity, demographic_trap, adaptive_fiction) locate their disagreement?',
    'The three readings disagree on causal attribution for Spartan decline (external/vice vs. internal design flaw vs. covert-adaptation-masking-decline) and on whether zero revision capacity is a virtue, a fatal design defect, or a fiction that was never fully real. Resolving this requires independent demographic data on citizen-count decline (oliganthropia) cross-referenced against the dating of attested legal changes (e.g., Epitadeus''s rhetra on property alienation) that the sacral reading''s own tradition sometimes acknowledges while denying they constitute ''revision.''',
    'If demographic and legal-change evidence show substantial covert revision occurred while immutability was publicly asserted, the sacral reading is descriptively false as history even if it remains a coherent doctrinal claim held by Spartan magistracies — the adaptive_fiction_reading would then carry the stronger empirical warrant, and this reading''s status would shift from ''the historical truth'' to ''the doctrine Spartans held about themselves.''',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_location, empirical, 'Locates where the three sibling readings of the Lycurgan kernel actually diverge and what would adjudicate between them.').

omega_variable(
    helot_exclusion_from_sacral_frame,
    'Is the sacral reading''s total silence on helot subjugation a neutral scope limitation (the doctrine is about citizens, full stop) or itself evidence that the doctrine functions to naturalize an extraction relationship it declines to examine?',
    'Compare the sacral tradition''s treatment of helot status to its treatment of citizen status: if helot subjugation is also framed as divinely/naturally ordained (rather than simply unaddressed), this supports the extraction-naturalization reading; if it is genuinely absent from the doctrinal corpus rather than affirmatively justified, the silence is closer to scope limitation.',
    'Determines whether helot_population belongs in this story''s victim set at all, or whether their absence from base_properties.victims correctly reflects that this reading''s kernel does not even purport to address them.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(helot_exclusion_from_sacral_frame, conceptual, 'Whether the sacral reading''s silence on helots is scope limitation or naturalized extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(lycurgan_laws__sacral_fidelity_reading, 0, 400).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lycu_tr_t0, lycurgan_laws__sacral_fidelity_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(lycu_tr_t60, lycurgan_laws__sacral_fidelity_reading, theater_ratio, 60, 0.38).
narrative_ontology:measurement(lycu_tr_t120, lycurgan_laws__sacral_fidelity_reading, theater_ratio, 120, 0.45).
narrative_ontology:measurement(lycu_tr_t200, lycurgan_laws__sacral_fidelity_reading, theater_ratio, 200, 0.5).
narrative_ontology:measurement(lycu_tr_t280, lycurgan_laws__sacral_fidelity_reading, theater_ratio, 280, 0.55).
narrative_ontology:measurement(lycu_tr_t400, lycurgan_laws__sacral_fidelity_reading, theater_ratio, 400, 0.58).

% Extraction over time
narrative_ontology:measurement(lycu_be_t0, lycurgan_laws__sacral_fidelity_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(lycu_be_t60, lycurgan_laws__sacral_fidelity_reading, base_extractiveness, 60, 0.33).
narrative_ontology:measurement(lycu_be_t120, lycurgan_laws__sacral_fidelity_reading, base_extractiveness, 120, 0.36).
narrative_ontology:measurement(lycu_be_t200, lycurgan_laws__sacral_fidelity_reading, base_extractiveness, 200, 0.39).
narrative_ontology:measurement(lycu_be_t280, lycurgan_laws__sacral_fidelity_reading, base_extractiveness, 280, 0.41).
narrative_ontology:measurement(lycu_be_t400, lycurgan_laws__sacral_fidelity_reading, base_extractiveness, 400, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(lycu_su_t0, lycurgan_laws__sacral_fidelity_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(lycu_su_t60, lycurgan_laws__sacral_fidelity_reading, suppression_requirement, 60, 0.63).
narrative_ontology:measurement(lycu_su_t120, lycurgan_laws__sacral_fidelity_reading, suppression_requirement, 120, 0.66).
narrative_ontology:measurement(lycu_su_t200, lycurgan_laws__sacral_fidelity_reading, suppression_requirement, 200, 0.68).
narrative_ontology:measurement(lycu_su_t280, lycurgan_laws__sacral_fidelity_reading, suppression_requirement, 280, 0.7).
narrative_ontology:measurement(lycu_su_t400, lycurgan_laws__sacral_fidelity_reading, suppression_requirement, 400, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(lycurgan_laws__sacral_fidelity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(lycurgan_laws__sacral_fidelity_reading, 0.1).
narrative_ontology:affects_constraint(lycurgan_laws__sacral_fidelity_reading, lycurgan_laws__demographic_trap_reading).
narrative_ontology:affects_constraint(lycurgan_laws__sacral_fidelity_reading, lycurgan_laws__adaptive_fiction_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the Lycurgan kernel, decomposed per the ε-invariance principle because the label 'the Lycurgan constitution' covers structurally distinct claims with different epsilon values: this reading (sacral_fidelity) authors moderate extraction (0.42) concentrated on magisterial authority-insulation, treats immutability as virtue, and attributes decline to external causes. demographic_trap_reading authors a different epsilon (unrevisability itself as the causal mechanism of civic collapse via oliganthropia) and a different victim structure (the citizen class itself as victim of the system's brittleness). adaptive_fiction_reading authors yet a third epsilon (the gap between the public immutability claim and actual covert legal adaptation, e.g. Epitadeus's rhetra). All three share the same kernel_id (lycurgan_laws) but are not the same constraint — each carries its own beneficiary/victim declarations and its own classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
