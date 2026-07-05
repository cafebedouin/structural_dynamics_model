% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_transmission__symbol_continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   human_readable: Catastrophe Memory Transmission — Symbolic Continuity Reading
 *   domain: religious_studies/collective_memory/ritual_studies
 *
 * SUMMARY:
 *   This story instantiates the symbol_continuity_reading of the
 *   catastrophe_memory_transmission kernel: the claim that post-catastrophe
 *   ritual preserves identity and mourning-practice as an intrinsic communal
 *   good in itself, and that transmitting the symbolic form correctly IS the
 *   survival mechanism — survival of the community as a continuous identity,
 *   not survival in the operational sense of hazard response. Under this
 *   reading, fidelity to ritual form is the point; adaptation of the form for
 *   practical efficiency is treated as a loss, not an improvement. This is a
 *   distinct constraint from the operational_competence_reading (which holds
 *   the ritual transmits practical survival skill through pattern recognition
 *   and coordination rehearsal) and from the hybrid_embedded_reading (which
 *   holds the two functions are inseparable). Each reading has its own
 *   beneficiary/victim structure and its own epsilon; they are linked here
 *   only through network.affects_constraints and are not to be merged.
 *
 * KEY AGENTS:
 *   - elder_custodians: agenda_setter/beneficiary (institutional/identity_locked) — certify correct ritual form, resist adaptation
 *   - communal_identity_continuity: beneficiary, non-agent (institutional/analytical) — the abstract good invoked to justify fidelity
 *   - ritual_specialists: beneficiary/agenda_setter (organized/identity_locked) — professional identity constituted by exact performance
 *   - younger_generation_practical_responders: payer (moderate/constrained) — bear the opportunity cost of fidelity requirements during actual recovery
 *   - adaptive_capacity_of_the_community: payer, non-agent (powerless/trapped) — the atrophied capacity sacrificed to preserve form
 *   - reform_minded_practitioners: excluded (moderate/constrained) — proposals for adaptation not admitted to the ritual agenda
 *   - ethnographic_observers: observer (analytical/analytical) — compares fidelity vs adaptation outcomes across communities
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
narrative_ontology:human_readable(catastrophe_memory_transmission__symbol_continuity_reading, "Catastrophe Memory Transmission — Symbolic Continuity Reading").
narrative_ontology:topic_domain(catastrophe_memory_transmission__symbol_continuity_reading, "religious_studies/collective_memory/ritual_studies").

domain_priors:requires_active_enforcement(catastrophe_memory_transmission__symbol_continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_transmission__symbol_continuity_reading, '21496b24-476d-4f9e-b95f-c699abf1e17c').
narrative_ontology:cs_kernel_codification('21496b24-476d-4f9e-b95f-c699abf1e17c', distributed).
narrative_ontology:cs_authority_grounding('21496b24-476d-4f9e-b95f-c699abf1e17c', practice).
narrative_ontology:cs_interpretation_layer_present('21496b24-476d-4f9e-b95f-c699abf1e17c').
narrative_ontology:cs_reading_relation('21496b24-476d-4f9e-b95f-c699abf1e17c', catastrophe_memory_transmission__operational_competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('21496b24-476d-4f9e-b95f-c699abf1e17c', catastrophe_memory_transmission__hybrid_embedded_reading, forecloses).
narrative_ontology:cs_axiom('21496b24-476d-4f9e-b95f-c699abf1e17c', foundational, symbolic_form_is_the_survival_mechanism).
narrative_ontology:cs_axiom_status(symbolic_form_is_the_survival_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('21496b24-476d-4f9e-b95f-c699abf1e17c', symbolic_form_is_the_survival_mechanism, deontological).
narrative_ontology:cs_axiom('21496b24-476d-4f9e-b95f-c699abf1e17c', foundational, operational_competence_is_severable_from_ritual_form).
narrative_ontology:cs_axiom_status(operational_competence_is_severable_from_ritual_form, holdable).
narrative_ontology:cs_axiom_grounding('21496b24-476d-4f9e-b95f-c699abf1e17c', operational_competence_is_severable_from_ritual_form, conventional).
narrative_ontology:cs_reference_frame('21496b24-476d-4f9e-b95f-c699abf1e17c', post_founding_catastrophe_rite).
narrative_ontology:cs_drift_state('21496b24-476d-4f9e-b95f-c699abf1e17c', contemporary_reduced_threat_environment, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('21496b24-476d-4f9e-b95f-c699abf1e17c', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_transmission__symbol_continuity_reading, catastrophe_memory_transmission).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_transmission__symbol_continuity_reading, communal_identity_continuity).
narrative_ontology:constraint_beneficiary(catastrophe_memory_transmission__symbol_continuity_reading, ritual_specialists).
narrative_ontology:constraint_beneficiary(catastrophe_memory_transmission__symbol_continuity_reading, elder_custodians).
narrative_ontology:constraint_victim(catastrophe_memory_transmission__symbol_continuity_reading, adaptive_capacity_of_the_community).
narrative_ontology:constraint_victim(catastrophe_memory_transmission__symbol_continuity_reading, younger_generation_practical_responders).
narrative_ontology:constraint_vindicates(catastrophe_memory_transmission__symbol_continuity_reading, symbolic_form_as_survival_mechanism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold and transmit the correct form of the mourning rite: sequence, chant, garment, timing. They adjudicate fidelity — whether a performance 'counts' — and their standing in the community derives from being the ones who know the form. They resist proposals to shorten, merge, or adapt the ritual for practical reasons (fewer participants, changed terrain, faster recovery needs), treating deviation as a threat to communal continuity rather than a reasonable adaptation.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__symbol_continuity_reading, elder_custodians, agenda_setter,
    institutional, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_transmission__symbol_continuity_reading, elder_custodians, beneficiary).

% The abstract good the ritual is said to preserve — a continuous sense of who this community is across generations and catastrophes. It is not an actor but the named collective interest the elder custodians invoke to justify strict fidelity; it accrues symbolic value each time the form is performed correctly.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__symbol_continuity_reading, communal_identity_continuity, beneficiary,
    institutional, civilizational, analytical, regional).
narrative_ontology:stakeholder_non_agent(catastrophe_memory_transmission__symbol_continuity_reading, communal_identity_continuity).

% Train for years to perform the correct forms of mourning and commemoration. Their social role, livelihood, and self-concept are constituted by being the ones the community turns to after catastrophe. A shift toward operational adaptation (triage-focused, less symbolically elaborate practice) would devalue their specific expertise.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__symbol_continuity_reading, ritual_specialists, beneficiary,
    organized, biographical, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_transmission__symbol_continuity_reading, ritual_specialists, agenda_setter).

% Live through the next catastrophe and must decide how much time, labor, and resource to devote to correct ritual performance versus practical response — evacuation logistics, resource redistribution, structural rebuilding. Diverting effort to satisfy fidelity requirements delays practical response; refusing to perform the rite risks communal sanction and being read as having abandoned the community's identity. They bear the cost of the tradeoff without controlling its terms.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__symbol_continuity_reading, younger_generation_practical_responders, payer,
    moderate, biographical, constrained, regional).

% The community's practical capacity to adjust survival strategy to a changing hazard environment (new climate patterns, new settlement geography, new technology). Because resources, attention, and legitimacy are channeled toward preserving ritual form exactly, capacity to develop or adopt new operational responses atrophies; it is not an actor but the named casualty of the fidelity requirement.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__symbol_continuity_reading, adaptive_capacity_of_the_community, payer,
    powerless, generational, trapped, regional).
narrative_ontology:stakeholder_non_agent(catastrophe_memory_transmission__symbol_continuity_reading, adaptive_capacity_of_the_community).

% Would argue that some elements of the mourning rite could be shortened or substituted without loss of communal meaning, freeing resources for practical rebuilding. Their proposals are rarely brought to the elder custodians as legitimate agenda items; raising them publicly risks being framed as disrespecting the dead or abandoning identity.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__symbol_continuity_reading, reform_minded_practitioners, excluded,
    moderate, biographical, constrained, regional).

% Study the community's post-catastrophe ritual practice comparatively across cultures and disasters. They can trace whether ritual fidelity correlates with better or worse practical outcomes in subsequent catastrophes, without a stake in either the ritual's preservation or its reform.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__symbol_continuity_reading, ethnographic_observers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_transmission__symbol_continuity_reading, elder_custodians).
narrative_ontology:fixing_cost_class(catastrophe_memory_transmission__symbol_continuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The rite provides a shared, legible way for the community to grieve together, mark the catastrophe as a communal rather than merely individual event, and reaffirm who belongs to the community across a disruption that could otherwise fragment it.
% TRANSFER_FUNCTION: Moves time, labor, and material resources from the general community — especially the generation tasked with practical recovery — to the maintenance of correct ritual form, and moves social standing and authority to the elder custodians and ritual specialists who certify that form.
% ABSENT_VOICES: Reform-minded practitioners who would trade some ritual elaboration for faster practical recovery are structurally excluded from setting the ritual agenda; raising the question publicly is read as a loyalty test they are likely to fail.
% DISAPPEARANCE_RATIONALE: Elder custodians and ritual specialists would say the world rearranges catastrophically — communal identity dissolves without the transmitted form. Younger practical responders and adaptive-capacity advocates would say practical recovery outcomes might improve, or at minimum would not worsen, if resources currently devoted to strict fidelity were redirected; they contest whether the identity function requires exact formal preservation at all.
% FOUNDING_PROBLEM: After an early catastrophic loss, the community needed a way to grieve collectively, hold the community together against fragmentation, and mark continuity of identity in the face of existential rupture.
% FOUNDING_PROBLEM_CORROBORATION: Elder custodians and ritual specialists attest the founding problem is still live and requires exact fidelity to solve. Ethnographic observers, studying comparable communities where ritual form loosened without loss of communal cohesion, report that identity continuity does not appear to require the specific formal elements currently insisted upon — suggesting the founding problem may be substantially solved by looser forms, corroboration coming from outside the beneficiary set.
narrative_ontology:disappearance_verdict(catastrophe_memory_transmission__symbol_continuity_reading, contested).
narrative_ontology:founding_problem_status(catastrophe_memory_transmission__symbol_continuity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_transmission__symbol_continuity_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
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
 *   Extraction (0.58 at interval end) reflects the resource and labor cost extracted from the community's practical-response capacity to fund symbolic fidelity, decoupled from any operational payoff — this is the reading's defining feature versus its siblings, where the same resource flow would be justified as skill transmission. Suppression (0.61) captures the social sanction attached to deviation: reform proposals are treated as identity betrayal, not policy disagreement. Theater ratio (0.42) is substantial and rising because as the catastrophe recedes in living memory, an increasing share of ritual performance function is symbolic self-reference (proving the community still performs it right) rather than active grief processing — a genuine Goodhart-style drift toward proxy over function. Accessibility collapse (0.5) and resistance (0.55) sit at moderate-high: alternatives to the exact form are conceivable and occasionally proposed (not a mountain), but proposing them meets real social resistance from the custodial seats.
 *
 * PERSPECTIVAL GAP:
 *   From the elder custodians' seat, the arrangement is coordination in the purest sense — preserving what makes the community itself. From the younger practical responders' seat, the same arrangement computes as an enforced transfer: labor and resource extracted from recovery to fund symbolic performance whose necessity they increasingly doubt. The engine should compute these divergently from the same structural data; the claimed_type (tangled_rope) is authored to hold both readings in view rather than resolve them.
 *
 * DIRECTIONALITY LOGIC:
 *   Elder custodians and ritual specialists sit near the full-beneficiary end: their authority, livelihood, and self-concept are produced by the fidelity requirement, and their exit from the arrangement is identity-locked rather than merely inconvenient — leaving the custodial role would mean abandoning who they are within the community. Younger practical responders and the abstract adaptive-capacity casualty sit near the full-target end: they bear the diverted resources and delayed practical response, and their exit is constrained (they cannot simply opt out of communal ritual obligations without social cost) or fully trapped (adaptive capacity itself has no exit option — it is a capacity, not an actor). Reform-minded practitioners are excluded rather than coordinated: their structural position is to have a stake but no seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — grief needs a communal container after catastrophe, and the community needs a way to reaffirm it is still one community — is real and was genuinely solved by early ritual practice. Under the symbol_continuity_reading, however, the mandate for EXACT formal fidelity has plausibly outlived any function that fidelity itself serves. The comparative ethnographic evidence (communities with looser forms retaining comparable identity cohesion) suggests the founding problem could be solved with less prescriptive fidelity, at lower cost to adaptive capacity. Classifying this as tangled_rope rather than pure snare preserves the genuine coordination function (real grief processing, real identity affirmation) while still naming the asymmetric extraction (adaptive capacity sacrificed disproportionately to benefit custodial standing) — collapsing it to snare would deny the real coordination good; collapsing it to rope would deny the real cost imposed on practical responders and the excluded reformers.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_symbol_continuity_vs_siblings,
    'Is the ritual''s survival function genuinely separable into a pure symbolic-identity function (this reading), or does the symbolic form encode operational survival competence that this reading''s framing discards (the operational_competence_reading and hybrid_embedded_reading)?',
    'Comparative ethnographic and disaster-response outcome studies: if communities that reduce ritual fidelity show no degradation in either identity cohesion or practical hazard response, the symbol-continuity and operational-competence functions are separable and this reading is well-founded in isolation. If practical response outcomes degrade specifically among communities that abandon ritual form, the hybrid_embedded_reading is better supported and this reading understates the ritual''s operational content.',
    'If the functions are genuinely separable, this reading''s tangled_rope classification (identity maintenance vs. sacrificed adaptive capacity) is structurally sound as authored. If they are inseparable, some of what this reading counts as pure extraction (resources diverted from adaptive capacity) may actually be misallocated coordination cost, shifting the classification toward rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_symbol_continuity_vs_siblings, conceptual, 'Whether the symbol-continuity function of ritual is structurally separable from operational survival competence, or whether that separation is itself the contested claim distinguishing the three kernel readings.').

omega_variable(
    elder_custodian_beneficiary_or_natural_authority,
    'Do elder custodians benefit from fidelity requirements as an extractive byproduct of their institutional position, or does their authority arise naturally and inevitably from possessing specialized, hard-won knowledge that anyone in their position would hold?',
    'Examine whether custodial authority persists when knowledge is made widely accessible (e.g., through documentation, training programs open to non-custodial community members) versus whether custodians actively restrict access to the knowledge itself.',
    'If custodians actively gatekeep the knowledge, the beneficiary structure is constructed and extraction is well-founded. If the knowledge is freely transmissible and authority derives purely from demonstrated competence, the tangled_rope framing may overstate the extraction relative to genuine, non-restricted expertise recognition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(elder_custodian_beneficiary_or_natural_authority, empirical, 'Whether custodial authority over ritual form is maintained through restricted access (extractive) or emerges from open, contestable expertise (non-extractive).').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_transmission__symbol_continuity_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_transmission__symbol_continuity_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(cata_tr_t8, catastrophe_memory_transmission__symbol_continuity_reading, theater_ratio, 8, 0.27).
narrative_ontology:measurement(cata_tr_t16, catastrophe_memory_transmission__symbol_continuity_reading, theater_ratio, 16, 0.32).
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
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_transmission__symbol_continuity_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(cata_su_t8, catastrophe_memory_transmission__symbol_continuity_reading, suppression_requirement, 8, 0.46).
narrative_ontology:measurement(cata_su_t16, catastrophe_memory_transmission__symbol_continuity_reading, suppression_requirement, 16, 0.51).
narrative_ontology:measurement(cata_su_t24, catastrophe_memory_transmission__symbol_continuity_reading, suppression_requirement, 24, 0.55).
narrative_ontology:measurement(cata_su_t32, catastrophe_memory_transmission__symbol_continuity_reading, suppression_requirement, 32, 0.58).
narrative_ontology:measurement(cata_su_t40, catastrophe_memory_transmission__symbol_continuity_reading, suppression_requirement, 40, 0.61).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_transmission__symbol_continuity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(catastrophe_memory_transmission__symbol_continuity_reading, 0.08).
narrative_ontology:affects_constraint(catastrophe_memory_transmission__symbol_continuity_reading, catastrophe_memory_transmission__operational_competence_reading).
narrative_ontology:affects_constraint(catastrophe_memory_transmission__symbol_continuity_reading, catastrophe_memory_transmission__hybrid_embedded_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three linked readings of the catastrophe_memory_transmission kernel. symbol_continuity_reading (this story) claims ritual fidelity preserves communal identity as an end in itself, with adaptive capacity as the sacrificed victim — tangled_rope. operational_competence_reading claims the ritual transmits practical survival skill through embedded pattern rehearsal — a different beneficiary/victim structure and likely a different classification (rope or tangled_rope depending on whether skill transmission is captured by specialists). hybrid_embedded_reading claims the two functions are inseparable, which if true would undercut this story's victim declaration (adaptive capacity sacrificed) since the sacrifice would not be real if competence rides along with form. Each story retains its own epsilon per the epsilon-invariance principle; they are not to be merged or averaged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
