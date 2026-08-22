% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_transmission__symbol_continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_transmission__symbol_continuity_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: catastrophe_memory_transmission__symbol_continuity_reading
 *   human_readable: Catastrophe-Memory Ritual: Symbolic Continuity Reading
 *   domain: religious_studies/collective_memory/ritual_studies
 *
 * SUMMARY:
 *   This story instantiates the symbol_continuity_reading of the
 *   catastrophe_memory_transmission kernel: the claim that ritual
 *   mourning-practice preserves communal identity as an intrinsic good in
 *   itself, and that transmitting the exact symbolic form — not any encoded
 *   operational content — is what carries the community through catastrophe
 *   and its aftermath. Under this reading, fidelity to form is the point, not
 *   a vehicle for something else. This is distinct from the sibling
 *   operational_competence_reading (which treats the ritual as covertly
 *   encoding survival pattern-recognition) and the hybrid_embedded_reading
 *   (which treats form and competence as inseparable). Under
 *   symbol_continuity_reading alone, the ritual's rigidity is functioning as
 *   designed even where it sacrifices adaptive capacity, because adaptive
 *   capacity was never what the ritual was for.
 *
 * KEY AGENTS:
 *   - elder_ritual_custodians: Primary agenda-setter and beneficiary (institutional/identity_locked) — authority derives from certifying correct form
 *   - younger_generation_practitioners: Primary payer (moderate/constrained) — bear the cost of rigid transmission against changed material circumstances
 *   - displaced_diaspora_members: Excluded voice (powerless/trapped) — cannot meet the fidelity bar and are read out of legitimate participation
 *   - external_disaster_researchers: Analytical observer — documents the gap between stated and actual function
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_transmission__symbol_continuity_reading, 0.58).
domain_priors:suppression_score(catastrophe_memory_transmission__symbol_continuity_reading, 0.61).
domain_priors:theater_ratio(catastrophe_memory_transmission__symbol_continuity_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_transmission__symbol_continuity_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__symbol_continuity_reading, suppression_requirement, 0.61).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__symbol_continuity_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_transmission__symbol_continuity_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__symbol_continuity_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_transmission__symbol_continuity_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_transmission__symbol_continuity_reading, "Catastrophe-Memory Ritual: Symbolic Continuity Reading").
narrative_ontology:topic_domain(catastrophe_memory_transmission__symbol_continuity_reading, "religious_studies/collective_memory/ritual_studies").

domain_priors:requires_active_enforcement(catastrophe_memory_transmission__symbol_continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_transmission__symbol_continuity_reading, 'ef25a06a-088c-417f-8b04-0a5ce891d266').
narrative_ontology:cs_kernel_codification('ef25a06a-088c-417f-8b04-0a5ce891d266', implicit).
narrative_ontology:cs_authority_grounding('ef25a06a-088c-417f-8b04-0a5ce891d266', lineage).
narrative_ontology:cs_interpretation_layer_present('ef25a06a-088c-417f-8b04-0a5ce891d266').
narrative_ontology:cs_reading_relation('ef25a06a-088c-417f-8b04-0a5ce891d266', catastrophe_memory_transmission__operational_competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('ef25a06a-088c-417f-8b04-0a5ce891d266', catastrophe_memory_transmission__hybrid_embedded_reading, coexists_with).
narrative_ontology:cs_axiom('ef25a06a-088c-417f-8b04-0a5ce891d266', foundational, symbolic_form_is_the_good_not_a_vehicle).
narrative_ontology:cs_axiom_status(symbolic_form_is_the_good_not_a_vehicle, holdable).
narrative_ontology:cs_axiom_grounding('ef25a06a-088c-417f-8b04-0a5ce891d266', symbolic_form_is_the_good_not_a_vehicle, deontological).
narrative_ontology:cs_axiom('ef25a06a-088c-417f-8b04-0a5ce891d266', secondary, fidelity_to_form_constitutes_communal_continuity).
narrative_ontology:cs_axiom_status(fidelity_to_form_constitutes_communal_continuity, holdable).
narrative_ontology:cs_axiom_grounding('ef25a06a-088c-417f-8b04-0a5ce891d266', fidelity_to_form_constitutes_communal_continuity, conventional).
narrative_ontology:cs_reference_frame('ef25a06a-088c-417f-8b04-0a5ce891d266', post_catastrophe_founding_transmission).
narrative_ontology:cs_drift_state('ef25a06a-088c-417f-8b04-0a5ce891d266', third_generation_diaspora_dispersal, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ef25a06a-088c-417f-8b04-0a5ce891d266', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_transmission__symbol_continuity_reading, catastrophe_memory_transmission).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_transmission__symbol_continuity_reading, communal_identity_continuity).
narrative_ontology:constraint_beneficiary(catastrophe_memory_transmission__symbol_continuity_reading, elder_ritual_custodians).
narrative_ontology:constraint_victim(catastrophe_memory_transmission__symbol_continuity_reading, adaptive_response_capacity).
narrative_ontology:constraint_victim(catastrophe_memory_transmission__symbol_continuity_reading, younger_generation_practitioners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold and transmit the correct forms of mourning practice — the sequence of words, gestures, timing, and material objects that must be reproduced exactly for the ritual to 'count.' Their standing in the community derives entirely from being the authoritative keepers of correct form. They resist any modification to the ritual sequence, including modifications proposed in response to new material conditions (displacement, changed calendars, unavailable ritual materials), treating fidelity itself as the good being protected.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__symbol_continuity_reading, elder_ritual_custodians, agenda_setter,
    institutional, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_transmission__symbol_continuity_reading, elder_ritual_custodians, beneficiary).

% Not itself an actor but the collective good the ritual is said to secure: a continuous thread of shared identity across the catastrophe that the ritual commemorates. The community's sense of being 'still itself' after loss is sustained by the visible unbroken transmission of the same symbolic forms across generations.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__symbol_continuity_reading, communal_identity_continuity, beneficiary,
    institutional, civilizational, analytical, regional).
narrative_ontology:stakeholder_non_agent(catastrophe_memory_transmission__symbol_continuity_reading, communal_identity_continuity).

% Are required to learn and perform the ritual exactly as transmitted, including elements that no longer correspond to their material circumstances (invoking geographic features they no longer inhabit, materials that must now be imported at cost, timings tied to a calendar drifted from lived seasons). Deviation risks being read as a failure of piety or a rupture in communal identity, not as adaptive improvement. Many privately find the ritual's rigidity a burden layered on top of the grief it is meant to hold.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__symbol_continuity_reading, younger_generation_practitioners, payer,
    moderate, biographical, constrained, local).

% The community's collective ability to update its catastrophe-response knowledge — what to do differently next time — is the abstract casualty of prioritizing symbolic fidelity. Because the ritual's authority rests on being unchanging, the practical knowledge of past mitigation failures and successes is not encoded into the transmitted form; whatever operational lessons the original catastrophe generated are carried, if at all, outside the ritual channel and decay faster than the ritual itself.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__symbol_continuity_reading, adaptive_response_capacity, payer,
    powerless, generational, trapped, regional).
narrative_ontology:stakeholder_non_agent(catastrophe_memory_transmission__symbol_continuity_reading, adaptive_response_capacity).

% Live far from the ritual's home site and lack access to the specific materials, locations, or elder lineages required for the fidelity standard the custodians enforce. Their attempts to perform adapted versions are frequently deemed illegitimate by the custodial authority, effectively excluding them from full participation in identity continuity despite carrying the same catastrophe in family memory.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__symbol_continuity_reading, displaced_diaspora_members, excluded,
    powerless, biographical, trapped, global).

% Study the ritual as a case of memory transmission and note the divergence between its stated function (survival preparedness) and its actual operative content (symbolic fidelity with minimal encoded operational content). They are not party to the ritual's authority structure and their findings circulate mainly in academic literature rather than back into the community's practice.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__symbol_continuity_reading, external_disaster_researchers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_transmission__symbol_continuity_reading, elder_ritual_custodians).
narrative_ontology:fixing_cost_class(catastrophe_memory_transmission__symbol_continuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a community's shared identity and collective mourning around a catastrophic loss, giving dispersed or generationally distant members a common symbolic anchor and a socially legible way to grieve together.
% TRANSFER_FUNCTION: Moves interpretive and performative authority from the community at large to the custodial lineage that certifies correct form, and moves adaptive flexibility away from younger and displaced practitioners toward the preservation of an unchanging symbolic sequence.
% ABSENT_VOICES: Displaced diaspora members who cannot meet the fidelity standard are effectively unheard in disputes over what counts as legitimate mourning; disaster-preparedness specialists who might argue the ritual channel could carry more operational content are outside the custodial conversation entirely.
% DISAPPEARANCE_RATIONALE: Custodians and much of the older community would say the world rearranges catastrophically — communal identity dissolves without the unbroken symbolic thread. Younger practitioners and diaspora members are divided: some would experience relief at the loosening of a rigid obligation, others fear a real loss of connection to ancestors. Because the parties genuinely disagree about how much of communal identity is actually load-bearing on this specific symbolic form versus transferable to other forms, the verdict is contested rather than settled.
% FOUNDING_PROBLEM: The original catastrophe produced mass grief and a fractured sense of communal continuity; the ritual was built to give survivors and descendants a shared, repeatable act that affirmed the community had not been destroyed.
% FOUNDING_PROBLEM_CORROBORATION: Elder custodians attest the founding problem — loss of communal continuity — remains permanently live and requires undiminished fidelity to address. External disaster researchers and some diaspora members, from outside the custodial lineage, attest that the acute continuity crisis of the founding generation has substantially resolved into a different, ongoing question of intergenerational transmission cost, and that the ritual's insistence on unchanging form now functions more to preserve custodial authority than to address the original rupture.
narrative_ontology:disappearance_verdict(catastrophe_memory_transmission__symbol_continuity_reading, contested).
narrative_ontology:founding_problem_status(catastrophe_memory_transmission__symbol_continuity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_transmission__symbol_continuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(catastrophe_memory_transmission__symbol_continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_transmission__symbol_continuity_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_transmission__symbol_continuity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophe_memory_transmission__symbol_continuity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(catastrophe_memory_transmission__symbol_continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored moderate-high and rising (0.35 to 0.58) because the cost of enforced fidelity — material, temporal, psychological — accumulates as circumstances drift further from the ritual's original conditions while the custodial authority resists corresponding adaptation. Suppression is authored substantial (0.61 at interval end) because deviation is treated as a legitimacy threat rather than an improvement, and the custodial lineage actively polices correctness. Theater ratio rises to 0.42 because an increasing share of the performed ritual's content is symbolic maintenance disconnected from any operational referent — this is expected and appropriate under this reading, since the reading holds the symbolic content IS the function, not a decayed proxy for a lost operational one.
 *
 * PERSPECTIVAL GAP:
 *   From the custodial seat, unwavering fidelity is coordination succeeding exactly as intended — the ritual holds the community together across generations. From the younger-practitioner and diaspora seats, the same unwavering fidelity is an imposed cost with declining relevance to their lived circumstances, enforced through social and sometimes economic sanction. The engine's per-seat computation should reflect this divergence structurally rather than resolve it — both readings of the lived experience are accurate to their seat.
 *
 * DIRECTIONALITY LOGIC:
 *   Elder custodians sit near the beneficiary end: their authority and status are constituted by the fidelity requirement itself, and they bear none of the adaptation costs directly. Communal identity continuity is the coordination good the arrangement genuinely produces — this is not manufactured cover; under this reading it is real. Younger practitioners and displaced diaspora members sit toward the target end: they bear the transmission cost (time, material burden, exclusion risk) without commensurate control over the form. Adaptive response capacity, as a non-agent casualty, is the clearest victim: it simply does not accumulate because the transmission channel is not built to carry it.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (fractured post-catastrophe communal identity) may be substantially resolved for the generation that lived through it, while the arrangement persists at full enforcement intensity for descendants who did not. This is exactly the tangled_rope signature: coordination function (identity continuity) is real and ongoing, but it is now inseparably bundled with extraction (imposed rigidity cost on those least equipped to bear it) via active custodial enforcement. Declaring this tangled_rope rather than mountain or rope prevents two errors: treating the ritual as pure sacred/natural (erasing the real cost to younger and diaspora practitioners) and treating it as pure extraction (erasing the real identity-continuity good the custodians are not wrong to defend).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    symbol_vs_competence_referent,
    'Is the community''s actual survival mechanism the preservation of symbolic form (this reading) or the covert transmission of operational competence embedded in that form (the sibling operational_competence_reading)?',
    'Compare outcomes across communities with high-fidelity ritual transmission but divergent catastrophe-preparedness outcomes; if fidelity and preparedness are uncorrelated, the symbol-only reading gains support. Ethnographic tracing of whether elders explicitly teach the ritual''s elements as containing practical lessons, versus purely as required form, would further discriminate.',
    'If competence turns out to be embedded (supporting hybrid_embedded_reading or operational_competence_reading instead), the victim set here (adaptive_response_capacity) would be misidentified — the capacity would not actually be sacrificed but transmitted in non-propositional form, and this story''s extraction estimate would be overstated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(symbol_vs_competence_referent, conceptual, 'Whether the ritual''s survival function is symbolic continuity alone or embeds operational competence.').

omega_variable(
    custodial_authority_natural_or_constructed,
    'Is the custodial lineage''s exclusive authority over ''correct form'' an intrinsic feature of how communal ritual transmission must work, or a constructed monopoly that could be distributed more broadly without loss of the coordination good?',
    'Comparative study of communities that distribute ritual-correctness authority more broadly (e.g., across multiple lineages or open community adjudication) versus centralized-custodian models, tracking whether communal identity continuity metrics differ.',
    'If authority concentration is not required for the coordination good, the extraction component attributable to elder_ritual_custodians as sole beneficiaries is higher than structurally necessary, strengthening the tangled_rope reading toward snare; if concentration is functionally required for coherence, the current balance is closer to justified coordination cost.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(custodial_authority_natural_or_constructed, conceptual, 'Whether custodial monopoly on ritual correctness is structurally necessary or an extractable surplus.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_transmission__symbol_continuity_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_transmission__symbol_continuity_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(cata_tr_t8, catastrophe_memory_transmission__symbol_continuity_reading, theater_ratio, 8, 0.26).
narrative_ontology:measurement(cata_tr_t16, catastrophe_memory_transmission__symbol_continuity_reading, theater_ratio, 16, 0.31).
narrative_ontology:measurement(cata_tr_t24, catastrophe_memory_transmission__symbol_continuity_reading, theater_ratio, 24, 0.36).
narrative_ontology:measurement(cata_tr_t32, catastrophe_memory_transmission__symbol_continuity_reading, theater_ratio, 32, 0.39).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_transmission__symbol_continuity_reading, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_transmission__symbol_continuity_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(cata_be_t8, catastrophe_memory_transmission__symbol_continuity_reading, base_extractiveness, 8, 0.41).
narrative_ontology:measurement(cata_be_t16, catastrophe_memory_transmission__symbol_continuity_reading, base_extractiveness, 16, 0.47).
narrative_ontology:measurement(cata_be_t24, catastrophe_memory_transmission__symbol_continuity_reading, base_extractiveness, 24, 0.52).
narrative_ontology:measurement(cata_be_t32, catastrophe_memory_transmission__symbol_continuity_reading, base_extractiveness, 32, 0.55).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_transmission__symbol_continuity_reading, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_transmission__symbol_continuity_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(cata_su_t8, catastrophe_memory_transmission__symbol_continuity_reading, suppression_requirement, 8, 0.5).
narrative_ontology:measurement(cata_su_t16, catastrophe_memory_transmission__symbol_continuity_reading, suppression_requirement, 16, 0.54).
narrative_ontology:measurement(cata_su_t24, catastrophe_memory_transmission__symbol_continuity_reading, suppression_requirement, 24, 0.57).
narrative_ontology:measurement(cata_su_t32, catastrophe_memory_transmission__symbol_continuity_reading, suppression_requirement, 32, 0.59).
narrative_ontology:measurement(cata_su_t40, catastrophe_memory_transmission__symbol_continuity_reading, suppression_requirement, 40, 0.61).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_transmission__symbol_continuity_reading, identity_coordination).
narrative_ontology:affects_constraint(catastrophe_memory_transmission__symbol_continuity_reading, catastrophe_memory_transmission__operational_competence_reading).
narrative_ontology:affects_constraint(catastrophe_memory_transmission__symbol_continuity_reading, catastrophe_memory_transmission__hybrid_embedded_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the catastrophe_memory_transmission kernel. symbol_continuity_reading (this story) authors ritual fidelity as the terminal good, producing a tangled_rope classification where identity continuity is the coordination function and sacrificed adaptive capacity is the extraction. operational_competence_reading authors the same ritual as a covert survival-training mechanism, which would substantially lower or eliminate the adaptive-capacity victim class this story declares. hybrid_embedded_reading treats the two as inseparable, producing an intermediate classification. Each reading carries its own ε, beneficiary/victim structure, and claimed_type per the ε-invariance principle; they are linked here rather than merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
