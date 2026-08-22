% ============================================================================
% CONSTRAINT STORY: lycurgan_laws__sacral_fidelity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   constraint_id: lycurgan_laws__sacral_fidelity_reading
 *   human_readable: Lycurgan Laws as Sacred, Unrevisable Divine Ordinance
 *   domain: political_philosophy/constitutional_theory
 *
 * SUMMARY:
 *   This story instantiates the sacral-fidelity reading of the Lycurgan
 *   kernel: the position, held by the Spartiate citizen class and the
 *   institutions administering the ordinance (ephorate, gerousia, Delphic
 *   oracle), that the Great Rhetra is a divinely sanctioned, unamendable
 *   constitutional order rather than a human policy choice. Within this
 *   framework, zero revision capacity is not a design flaw but the point —
 *   the laws' permanence IS their legitimacy, and any decline Sparta
 *   experienced is attributed to external military pressure (Thebes,
 *   Macedon), demographic misfortune, or citizen moral decay (failure to live
 *   up to the ordinance), never to the ordinance's own structure. This is a
 *   distinct constraint from the demographic_trap_reading (which locates
 *   Spartan collapse in the unrevisability itself, via oliganthropia) and the
 *   adaptive_fiction_reading (which treats the immutability claim as a noble
 *   lie covering real covert adaptation) — those are different constraints
 *   with different ε values, linked here via network only, not folded into
 *   this one.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(lycurgan_laws__sacral_fidelity_reading, 0.28).
domain_priors:suppression_score(lycurgan_laws__sacral_fidelity_reading, 0.62).
domain_priors:theater_ratio(lycurgan_laws__sacral_fidelity_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lycurgan_laws__sacral_fidelity_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(lycurgan_laws__sacral_fidelity_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(lycurgan_laws__sacral_fidelity_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(lycurgan_laws__sacral_fidelity_reading, accessibility_collapse, 0.82).
narrative_ontology:constraint_metric(lycurgan_laws__sacral_fidelity_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(lycurgan_laws__sacral_fidelity_reading, mountain).
narrative_ontology:human_readable(lycurgan_laws__sacral_fidelity_reading, "Lycurgan Laws as Sacred, Unrevisable Divine Ordinance").
narrative_ontology:topic_domain(lycurgan_laws__sacral_fidelity_reading, "political_philosophy/constitutional_theory").

domain_priors:emerges_naturally(lycurgan_laws__sacral_fidelity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(lycurgan_laws__sacral_fidelity_reading, '8e1dc789-6eee-48d1-8ded-4c6fa7d0d7ed').
narrative_ontology:cs_kernel_codification('8e1dc789-6eee-48d1-8ded-4c6fa7d0d7ed', fixed_text).
narrative_ontology:cs_authority_grounding('8e1dc789-6eee-48d1-8ded-4c6fa7d0d7ed', lineage).
narrative_ontology:cs_interpretation_layer_present('8e1dc789-6eee-48d1-8ded-4c6fa7d0d7ed').
narrative_ontology:cs_reading_relation('8e1dc789-6eee-48d1-8ded-4c6fa7d0d7ed', lycurgan_laws__demographic_trap_reading, coexists_with).
narrative_ontology:cs_reading_relation('8e1dc789-6eee-48d1-8ded-4c6fa7d0d7ed', lycurgan_laws__adaptive_fiction_reading, influences).
narrative_ontology:cs_axiom('8e1dc789-6eee-48d1-8ded-4c6fa7d0d7ed', foundational, rhetra_is_divinely_ratified_and_beyond_amendment).
narrative_ontology:cs_axiom_status(rhetra_is_divinely_ratified_and_beyond_amendment, holdable).
narrative_ontology:cs_axiom_grounding('8e1dc789-6eee-48d1-8ded-4c6fa7d0d7ed', rhetra_is_divinely_ratified_and_beyond_amendment, theological).
narrative_ontology:cs_axiom('8e1dc789-6eee-48d1-8ded-4c6fa7d0d7ed', secondary, spartan_decline_attributable_to_external_or_moral_causes_not_system_design).
narrative_ontology:cs_axiom_status(spartan_decline_attributable_to_external_or_moral_causes_not_system_design, holdable).
narrative_ontology:cs_axiom_grounding('8e1dc789-6eee-48d1-8ded-4c6fa7d0d7ed', spartan_decline_attributable_to_external_or_moral_causes_not_system_design, conventional).
narrative_ontology:cs_reference_frame('8e1dc789-6eee-48d1-8ded-4c6fa7d0d7ed', delphic_ratified_lycurgan_order).
narrative_ontology:cs_drift_state('8e1dc789-6eee-48d1-8ded-4c6fa7d0d7ed', post_leuctra_decline, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('8e1dc789-6eee-48d1-8ded-4c6fa7d0d7ed', '').
narrative_ontology:cs_kernel_id(lycurgan_laws__sacral_fidelity_reading, lycurgan_laws).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(lycurgan_laws__sacral_fidelity_reading, spartiate_citizen_class).
narrative_ontology:constraint_beneficiary(lycurgan_laws__sacral_fidelity_reading, ephorate_and_gerousia).
narrative_ontology:constraint_beneficiary(lycurgan_laws__sacral_fidelity_reading, delphic_oracle_authority).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(lycurgan_laws__sacral_fidelity_reading, spartiate_citizen_class).
narrative_ontology:constraint_vindicates(lycurgan_laws__sacral_fidelity_reading, kosmos_of_lycurgus_is_divinely_sanctioned).
narrative_ontology:constraint_vindicates(lycurgan_laws__sacral_fidelity_reading, rhetra_is_beyond_human_amendment).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Full citizens whose entire identity, land allotment (kleros), education (agoge), and standing in the syssitia derive from unbroken adherence to the Rhetra. They understand the laws as Apollo's binding word delivered through Lycurgus and ratified at Delphi, not as a human policy choice open to debate. Their exit from the system is not merely costly but unthinkable within their own framework — abandoning the laws would mean abandoning what makes one Spartan at all.
narrative_ontology:constraint_stakeholder(lycurgan_laws__sacral_fidelity_reading, spartiate_citizen_class, beneficiary,
    powerful, civilizational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(lycurgan_laws__sacral_fidelity_reading, spartiate_citizen_class, payer).

% The ephors and the council of elders administer and enforce the ancestral constitution, policing deviation (including the periodic krypteia actions against helots and the scrutiny of citizen conduct). From this seat, they are not legislators but guardians of a fixed sacred trust; their authority is entirely derivative of fidelity to the unrevisable ordinance, so they have no framework within which to treat it as negotiable.
narrative_ontology:constraint_stakeholder(lycurgan_laws__sacral_fidelity_reading, ephorate_and_gerousia, agenda_setter,
    institutional, civilizational, identity_locked, regional).

% The oracle's historical ratification of the Rhetra is the cited source of the laws' sacred, unamendable status. Its authority and the laws' authority are mutually reinforcing: the oracle validated the laws, and the laws' permanence testifies to the oracle's divine reliability. Neither can be revised without implicating the other.
narrative_ontology:constraint_stakeholder(lycurgan_laws__sacral_fidelity_reading, delphic_oracle_authority, agenda_setter,
    institutional, civilizational, analytical, regional).
narrative_ontology:stakeholder_secondary_role(lycurgan_laws__sacral_fidelity_reading, delphic_oracle_authority, beneficiary).

% The subjugated agricultural laboring population whose forced labor underwrites the leisure that makes the agoge and syssitia possible. Within the sacral-fidelity reading, their subjection is treated as part of the same divinely sanctioned order rather than as a policy choice; they have no standing to be heard on whether the ordinance should be questioned, and their objection (were it voiced) is outside the frame this reading recognizes as relevant.
narrative_ontology:constraint_stakeholder(lycurgan_laws__sacral_fidelity_reading, helot_population, excluded,
    powerless, generational, trapped, regional).

% Plutarch, Xenophon, and others recorded and largely transmitted the sacral-fidelity framing centuries after Sparta's decline, often for their own didactic or nostalgic purposes. They report Spartan self-understanding but write from outside the lived commitment, at a distance that lets the framework's origins be examined rather than simply inhabited.
narrative_ontology:constraint_stakeholder(lycurgan_laws__sacral_fidelity_reading, later_greek_historians, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides Sparta with a single, uncontestable foundation for civic order — land tenure, military training, citizen conduct, and political authority all trace to one settled, non-negotiable source, eliminating the need for ongoing constitutional argument.
% TRANSFER_FUNCTION: The ordinance secures the Spartiate citizen class's privileged standing (land, leisure, martial status) by fixing helot subjection and rigid citizen discipline as permanent features of a sacred order rather than as revisable arrangements.
% ABSENT_VOICES: The helot population and any citizen faction favoring reform (such as later reformist kings like Agis IV and Cleomenes III) are excluded from the sacral-fidelity frame entirely — the reading treats questioning the ordinance as impiety, not as a policy position with standing to be weighed.
% DISAPPEARANCE_RATIONALE: If the sacred-and-unrevisable status of the Rhetra were disbelieved overnight, land redistribution, citizenship criteria, military obligations, and the ephors' authority would all become contestable questions rather than settled facts — the entire Spartan civic order was built on treating them as closed.
% FOUNDING_PROBLEM: Early Sparta faced acute internal instability — stasis over land distribution and citizen unity — that Lycurgus's ordinance (in this reading, divinely delivered and ratified) was meant to resolve permanently by placing the core arrangements beyond future political dispute.
% FOUNDING_PROBLEM_CORROBORATION: Within the sacral-fidelity tradition itself (Plutarch's Lycurgus, the Great Rhetra as transmitted), the founding problem is treated as permanently resolved by divine sanction. Outside corroboration is thin: modern historians and epigraphic analysis of the Rhetra's actual (much narrower) textual content suggest the totalizing sacred-unrevisability claim was itself a later reconstruction, not evidence from a source independent of those who benefited from the claim's authority.
narrative_ontology:disappearance_verdict(lycurgan_laws__sacral_fidelity_reading, world_rearranges).
narrative_ontology:founding_problem_status(lycurgan_laws__sacral_fidelity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(lycurgan_laws__sacral_fidelity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(lycurgan_laws__sacral_fidelity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(lycurgan_laws__sacral_fidelity_reading, 0.28, 'claude-sonnet-5', 'none', direct).

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
 *   Extraction is authored low (0.28 at interval end) because, from this reading's own lights, the ordinance is not experienced by its principal citizen constituency as extraction at all — it is the foundation of their status, land, and identity. Suppression is authored moderate-high (0.62) because real coercive machinery (the krypteia, ephor scrutiny, agoge discipline) enforces conformity even within this reading, though the reading interprets that enforcement as sacred guardianship rather than extraction. Accessibility collapse is high (0.82) and resistance is low (0.2): from inside the sacral-fidelity frame, alternatives to the ordinance are not merely suppressed but conceptually unavailable — questioning the Rhetra is impiety, not policy disagreement, which is precisely why this reading can claim mountain status internally even though outside observers (the sibling readings) see a constructed, contestable arrangement.
 *
 * DIRECTIONALITY LOGIC:
 *   The Spartiate citizen class and the ephorate/gerousia sit near the beneficiary end of directionality: the ordinance's stability is what secures their entire social position, and within this reading they experience zero cost from its unrevisability because unrevisability is definitionally sacred rather than a constraint on their agency. The Delphic oracle is also a structural beneficiary — the ordinance's sanctity and the oracle's authority validate each other. The helot population, though structurally central to the extraction the arrangement enables, does not register within this reading's own accounting at all; their exclusion from consideration is itself part of what the reading is.
 *
 * MANDATROPHY ANALYSIS:
 *   The sacral-fidelity reading forecloses mandatrophy analysis by design: if the ordinance is divine and eternal, it cannot have a founding problem that becomes obsolete, because obsolescence would imply the ordinance was ever a contingent human solution to a contingent human problem. The R5 corroboration field surfaces this directly — no source independent of the tradition's own transmission (Plutarch et al., writing centuries later) corroborates the totalizing sacred-unrevisability claim, which is the mismatch this reading's own structure predicts.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sacral_reading_committer_structure,
    'Is the Lycurgan ordinance''s sacred-unrevisability status a genuine natural-law-like feature of Spartan constitutional order, or is it a constructed authority claim that benefits the Spartiate citizen class, the ephorate/gerousia, and the Delphic oracle by placing the arrangement beyond contest?',
    'Compare this reading against the sibling demographic_trap_reading (which treats the same unrevisability as the causal mechanism of Spartan demographic collapse) and the sibling adaptive_fiction_reading (which treats the sacred framing as a noble lie masking documented covert legal adaptations, e.g. changes to inheritance and citizenship practice under demographic pressure). If independent epigraphic or comparative evidence shows the ordinance was practically revised while being publicly declared immutable, the mountain claim in this reading is a false summit.',
    'If the constructed-authority-claim account is correct, this Mountain reading is a false summit whose beneficiaries are exactly the parties who administer and are legitimated by the sacred framing — reclassification pressure toward tangled_rope in that case falls on the sibling readings, not this one, since ε-invariance requires each reading to keep its own stable ε.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sacral_reading_committer_structure, conceptual, 'Whether the sacred-immutability claim is genuine natural law (from this reading''s own lights) or a constructed authority claim serving identifiable beneficiaries — the central committer-axis ambiguity of the Lycurgan kernel.').

omega_variable(
    helot_exclusion_from_the_frame,
    'Does the sacral-fidelity reading''s complete exclusion of helot testimony and interest from its own accounting reflect the reading''s genuine internal consistency, or does it mask an extraction relationship that any complete structural account would have to register?',
    'Examine whether any Spartiate-authored or oracle-authored source from within the tradition itself ever registers helot subjection as a cost requiring justification, versus treating it as simply given — silence itself is evidence about whether the exclusion is structural to the reading or an oversight.',
    'If the tradition''s own sources never treat helot subjection as needing justification, that corroborates the reading''s internal coherence but also confirms the reading structurally cannot see its own extraction — which is exactly the asymmetry the sibling readings are built to name.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(helot_exclusion_from_the_frame, empirical, 'Whether the reading''s exclusion of helot interests is a genuine feature of the sacral framework or evidence the framework cannot register its own extractive base.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(lycurgan_laws__sacral_fidelity_reading, 0, 400).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lycu_tr_t0, lycurgan_laws__sacral_fidelity_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(lycu_tr_t80, lycurgan_laws__sacral_fidelity_reading, theater_ratio, 80, 0.1).
narrative_ontology:measurement(lycu_tr_t160, lycurgan_laws__sacral_fidelity_reading, theater_ratio, 160, 0.12).
narrative_ontology:measurement(lycu_tr_t240, lycurgan_laws__sacral_fidelity_reading, theater_ratio, 240, 0.13).
narrative_ontology:measurement(lycu_tr_t320, lycurgan_laws__sacral_fidelity_reading, theater_ratio, 320, 0.14).
narrative_ontology:measurement(lycu_tr_t400, lycurgan_laws__sacral_fidelity_reading, theater_ratio, 400, 0.15).

% Extraction over time
narrative_ontology:measurement(lycu_be_t0, lycurgan_laws__sacral_fidelity_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(lycu_be_t80, lycurgan_laws__sacral_fidelity_reading, base_extractiveness, 80, 0.2).
narrative_ontology:measurement(lycu_be_t160, lycurgan_laws__sacral_fidelity_reading, base_extractiveness, 160, 0.24).
narrative_ontology:measurement(lycu_be_t240, lycurgan_laws__sacral_fidelity_reading, base_extractiveness, 240, 0.26).
narrative_ontology:measurement(lycu_be_t320, lycurgan_laws__sacral_fidelity_reading, base_extractiveness, 320, 0.28).
narrative_ontology:measurement(lycu_be_t400, lycurgan_laws__sacral_fidelity_reading, base_extractiveness, 400, 0.28).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(lycurgan_laws__sacral_fidelity_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(lycurgan_laws__sacral_fidelity_reading, lycurgan_laws__demographic_trap_reading).
narrative_ontology:affects_constraint(lycurgan_laws__sacral_fidelity_reading, lycurgan_laws__adaptive_fiction_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraints decomposed from the single natural-language label 'the Lycurgan laws.' sacral_fidelity_reading (this story) authors the ordinance as the Spartiate/ephorate/oracle self-understanding: genuinely natural-law-like from within, low authored extraction, high accessibility collapse, decline attributed externally. demographic_trap_reading authors the same textual ordinance as a brittle, unrevisable system whose rigidity directly caused oliganthropia and citizen-class collapse — high extraction, tangled_rope or snare territory. adaptive_fiction_reading authors the immutability claim itself as a noble lie covering documented covert legal adaptation — its ε tracks the gap between public claim and private practice. All three share the kernel (the Rhetra and its claimed status) but are structurally distinct constraints per the ε-invariance principle; they are not to be merged or averaged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
