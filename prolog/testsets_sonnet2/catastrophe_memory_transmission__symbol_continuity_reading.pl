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
 *   human_readable: Catastrophe-Memory Ritual Transmission — Symbolic Continuity Reading
 *   domain: religious_studies/collective_memory/ritual_studies
 *
 * SUMMARY:
 *   This story instantiates the symbol_continuity_reading of the
 *   catastrophe_memory_transmission kernel: a community that survived a
 *   historical catastrophe maintains a fixed mourning ritual whose declared
 *   purpose is to preserve communal identity and honor the dead, with
 *   fidelity to precise symbolic form treated as the survival mechanism
 *   itself. As environmental and social conditions around the community have
 *   changed over the interval, the ritual's exact form has been held constant
 *   while the community's practical adaptive capacity to respond to
 *   present-day risk has not been renewed through the ritual channel — the
 *   two goods (identity continuity and adaptive capacity) increasingly trade
 *   off against one another rather than reinforcing each other, which is the
 *   structural signature of tangled_rope rather than rope or mountain.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_transmission__symbol_continuity_reading, 0.61).
domain_priors:suppression_score(catastrophe_memory_transmission__symbol_continuity_reading, 0.58).
domain_priors:theater_ratio(catastrophe_memory_transmission__symbol_continuity_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_transmission__symbol_continuity_reading, extractiveness, 0.61).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__symbol_continuity_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__symbol_continuity_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_transmission__symbol_continuity_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__symbol_continuity_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_transmission__symbol_continuity_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_transmission__symbol_continuity_reading, "Catastrophe-Memory Ritual Transmission — Symbolic Continuity Reading").
narrative_ontology:topic_domain(catastrophe_memory_transmission__symbol_continuity_reading, "religious_studies/collective_memory/ritual_studies").

domain_priors:requires_active_enforcement(catastrophe_memory_transmission__symbol_continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_transmission__symbol_continuity_reading, 'a1ed5ad0-9034-4659-b91d-040c18ac7a4e').
narrative_ontology:cs_kernel_codification('a1ed5ad0-9034-4659-b91d-040c18ac7a4e', implicit).
narrative_ontology:cs_authority_grounding('a1ed5ad0-9034-4659-b91d-040c18ac7a4e', lineage).
narrative_ontology:cs_interpretation_layer_present('a1ed5ad0-9034-4659-b91d-040c18ac7a4e').
narrative_ontology:cs_reading_relation('a1ed5ad0-9034-4659-b91d-040c18ac7a4e', catastrophe_memory_transmission__operational_competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('a1ed5ad0-9034-4659-b91d-040c18ac7a4e', catastrophe_memory_transmission__hybrid_embedded_reading, influences).
narrative_ontology:cs_axiom('a1ed5ad0-9034-4659-b91d-040c18ac7a4e', foundational, symbolic_fidelity_is_the_survival_mechanism).
narrative_ontology:cs_axiom_status(symbolic_fidelity_is_the_survival_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('a1ed5ad0-9034-4659-b91d-040c18ac7a4e', symbolic_fidelity_is_the_survival_mechanism, conventional).
narrative_ontology:cs_axiom('a1ed5ad0-9034-4659-b91d-040c18ac7a4e', foundational, identity_continuity_is_severable_from_operational_competence).
narrative_ontology:cs_axiom_status(identity_continuity_is_severable_from_operational_competence, holdable).
narrative_ontology:cs_axiom_grounding('a1ed5ad0-9034-4659-b91d-040c18ac7a4e', identity_continuity_is_severable_from_operational_competence, empirically_contingent).
narrative_ontology:cs_reference_frame('a1ed5ad0-9034-4659-b91d-040c18ac7a4e', post_catastrophe_founding_rite).
narrative_ontology:cs_drift_state('a1ed5ad0-9034-4659-b91d-040c18ac7a4e', contemporary_diaspora_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a1ed5ad0-9034-4659-b91d-040c18ac7a4e', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_transmission__symbol_continuity_reading, catastrophe_memory_transmission).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_transmission__symbol_continuity_reading, communal_identity_continuity).
narrative_ontology:constraint_beneficiary(catastrophe_memory_transmission__symbol_continuity_reading, ritual_lineage_holders).
narrative_ontology:constraint_victim(catastrophe_memory_transmission__symbol_continuity_reading, adaptive_response_capacity).
narrative_ontology:constraint_victim(catastrophe_memory_transmission__symbol_continuity_reading, younger_generation_practitioners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Elders, clergy, or designated keepers who administer the mourning-rite calendar, correct deviations in performance, and train successors. Their authority and social standing derive entirely from being the recognized transmitters of correct form; they collect prestige, material support (offerings, stipends), and communal deference for maintaining fidelity. They have no exit from the role without forfeiting the identity that gives them standing.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__symbol_continuity_reading, ritual_lineage_holders, agenda_setter,
    institutional, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_transmission__symbol_continuity_reading, ritual_lineage_holders, beneficiary).

% The abstract communal good the ritual is said to preserve — a shared sense of who 'we' are across generations, anchored in the memory of the catastrophe. It is not an actor that collects anything itself but is invoked to justify fidelity to symbolic form even when the ritual's practical utility for crisis response has atrophied.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__symbol_continuity_reading, communal_identity_continuity, beneficiary,
    institutional, civilizational, analytical, regional).
narrative_ontology:stakeholder_non_agent(catastrophe_memory_transmission__symbol_continuity_reading, communal_identity_continuity).

% Community members required to learn and perform the ritual's exact sequence, chants, and mourning gestures. They inherit the obligation to reproduce form precisely, at real cost in time, labor, and foreclosed alternative practices, while the environmental or crisis conditions the ritual originally responded to have often changed beyond what the fixed form addresses. Leaving the practice risks being read as abandoning the dead and severing communal belonging.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__symbol_continuity_reading, younger_generation_practitioners, payer,
    moderate, biographical, constrained, local).

% The community's practical capacity to update its crisis-response knowledge — resource stockpiling logic, warning signs, coordination procedures — as conditions change. Where ritual fidelity is prioritized over revision, this capacity does not get renewed through the ritual channel and instead must be rebuilt elsewhere or is simply lost, even as the ritual continues to be performed with full solemnity.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__symbol_continuity_reading, adaptive_response_capacity, payer,
    powerless, generational, trapped, regional).
narrative_ontology:stakeholder_non_agent(catastrophe_memory_transmission__symbol_continuity_reading, adaptive_response_capacity).

% Ethnographers, disaster-risk-reduction specialists, and public health researchers who study the community's catastrophe memory and could offer comparative evidence on whether the ritual still transmits useful operational content. They are typically not consulted on ritual form, and their findings, when produced, rarely feed back into how the rite is administered.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__symbol_continuity_reading, outside_disaster_researchers, excluded,
    analytical, biographical, analytical, national).

% Descendants who left the region and maintain looser, adapted versions of the mourning practice. They would argue for a more flexible transmission model that keeps meaning while dropping rigid form, but their adapted practice is often treated by lineage holders as diminished or illegitimate, and they have no vote in how the home community's ritual calendar is set.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__symbol_continuity_reading, diaspora_descendants, excluded,
    moderate, biographical, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_transmission__symbol_continuity_reading, ritual_lineage_holders).
narrative_ontology:fixing_cost_class(catastrophe_memory_transmission__symbol_continuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The ritual coordinates a community's collective act of remembering a past catastrophe and its dead, providing a shared calendar, shared vocabulary of grief, and a mechanism by which the event is not forgotten and the community's continuity across the rupture is affirmed.
% TRANSFER_FUNCTION: Time, labor, and deference flow from younger practitioners and the wider community to ritual lineage holders, in exchange for the maintenance of symbolic continuity; adaptive attention that could go toward updating practical crisis-response knowledge is spent instead on preserving exact ritual form.
% ABSENT_VOICES: Outside disaster researchers who could assess whether the transmitted content still carries operational value are not part of the ritual's administration. Diaspora descendants who have already adapted the form are treated as having departed from correct practice rather than as a legitimate alternative reading, and are excluded from decisions about the home ritual's fidelity standards.
% DISAPPEARANCE_RATIONALE: Lineage holders and many community members would say the world rearranges catastrophically overnight — identity, mourning obligation to the dead, and communal cohesion unravel without the rite. Adaptive-capacity advocates and outside observers would say comparatively little practical crisis response changes, since (on this reading) the ritual's operational content has already been sacrificed to formal fidelity; what would actually collapse is a specific vector of belonging, not a survival mechanism in the literal sense. The two camps genuinely disagree about what 'the world' includes here.
% FOUNDING_PROBLEM: The community needed a way to process collective grief after a catastrophic event and to ensure that the event, and the dead, would not be forgotten by subsequent generations — a problem of memory and belonging, not (on this reading) a problem of operational preparedness.
% FOUNDING_PROBLEM_CORROBORATION: Ritual lineage holders and many community elders attest the founding problem — preserving memory and communal identity around loss — remains fully live and is the ritual's entire point. Outside disaster researchers and some diaspora descendants, corroborating from outside the benefiting lineage-holder group, note that if the ritual once also encoded practical threat-response knowledge, that function is not evident in current transmitted content, which now reads as almost purely commemorative and formally fixed.
narrative_ontology:disappearance_verdict(catastrophe_memory_transmission__symbol_continuity_reading, contested).
narrative_ontology:founding_problem_status(catastrophe_memory_transmission__symbol_continuity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_transmission__symbol_continuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(catastrophe_memory_transmission__symbol_continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_transmission__symbol_continuity_reading, 0.61, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness rises modestly across the interval (0.42 to 0.61) as the gap between fixed ritual form and changed material conditions widens, so more of what younger practitioners give up (time, alternative preparation, flexible response) buys decreasing marginal identity-continuity benefit relative to its cost. Theater ratio climbs in parallel (0.22 to 0.42) as performance of correct form increasingly substitutes for content that once may have had operational relevance under other readings of the kernel. Suppression rises moderately (0.38 to 0.58) as deviations from form are treated more strictly as threats to communal cohesion rather than as legitimate adaptation. Accessibility collapse and resistance are both mid-range (0.5, 0.55): unlike a mountain, workable alternative forms of mourning exist and are visibly practiced elsewhere (e.g., by diaspora descendants), but invoking them within the home community meets real social friction.
 *
 * PERSPECTIVAL GAP:
 *   From the ritual lineage holder seat, the arrangement is pure coordination: a sacred duty to the dead and to communal continuity, with no serious internal cost. From the younger-practitioner and adaptive-capacity seats, the same arrangement increasingly reads as extraction of time and flexibility in service of a form whose connection to any practical survival function (on THIS reading) was never load-bearing. The engine computes these divergent per-seat classifications from the declared power/exit/scope data; the story does not adjudicate which seat is 'right' — that adjudication is exactly the kernel-level contest among the three readings.
 *
 * DIRECTIONALITY LOGIC:
 *   Ritual lineage holders sit near the beneficiary end: they administer the standard, collect deference and material support, and have no structural cost from continued fidelity — their identity is fused with correct transmission (identity_locked exit). Younger practitioners and the abstract good of adaptive response capacity sit near the target end: they bear the transfer of labor and foreclosed alternatives, with constrained or trapped exit respectively. Communal identity continuity is named as a beneficiary but is a non-agent good (agent: false) — it collects no rents itself; it is invoked to justify the transfer, which is exactly the structure this reading claims to identify.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (collective grief processing, memory preservation) is genuinely contested as live vs. dead: for lineage holders it is fully live, for outside observers it looks like a problem the community has already solved and is now maintaining formal machinery for at a rising adaptive-capacity cost. Classifying this as tangled_rope rather than snare or mountain prevents two mislabelings: calling it a snare would deny the real coordination good (memory, mourning, belonging) it demonstrably provides; calling it a mountain (as an unquestioned natural necessity of communal survival) would hide the beneficiary/victim structure the symbol_continuity_reading is specifically built to surface — that identity maintenance and adaptive responsiveness have been decoupled and now compete for the same scarce communal attention.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    symbolic_vs_operational_content_ambiguity,
    'Does the ritual''s transmitted content carry any latent operational survival knowledge (as the sibling readings claim), or is its function genuinely exhausted by identity/memory preservation as this reading holds?',
    'Structured ethnographic and disaster-risk-reduction analysis of the ritual''s specific gestures, sequences, and narrated content, cross-checked against documented historical crisis-response practices of the community, to determine whether any operational pattern-recognition or coordination content survives inside the symbolic form.',
    'If latent operational content is found, this story''s classification collapses toward the hybrid_embedded_reading or operational_competence_reading and the victim declaration (adaptive_response_capacity) would need substantial revision; if no such content is found, the symbol_continuity_reading''s tangled_rope classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(symbolic_vs_operational_content_ambiguity, empirical, 'Whether the ritual''s fixed form is purely symbolic or carries embedded operational knowledge — the central point of contest among the three kernel readings.').

omega_variable(
    identity_continuity_as_actor_or_proxy,
    'Is ''communal identity continuity'' a genuine collective good the community values for its own sake, or is it a proxy invoked by lineage holders to legitimate their own institutional position and the transfer of labor/deference they collect?',
    'Compare community sentiment (surveyed independently of lineage-holder framing) about the ritual''s meaning against the material and status benefits accruing specifically to lineage holders; a large gap between broad communal valuation and concentrated lineage-holder benefit would support the proxy reading.',
    'If the good is genuinely diffuse and widely held, the tangled_rope coordination function is robust. If it functions mainly as legitimating cover for lineage-holder benefit with little diffuse communal uptake, this reading shifts closer to snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_continuity_as_actor_or_proxy, conceptual, 'Whether the declared beneficiary (communal identity continuity) is a real diffuse good or a legitimating proxy for a concentrated beneficiary.').

omega_variable(
    kernel_framing_choice_disclosure,
    'Given that the same underlying ritual practice could be authored under any of three structurally distinct readings (symbol_continuity, operational_competence, hybrid_embedded), what specific evidence in this community''s case motivated selecting the symbol_continuity framing rather than one of the siblings?',
    'Document the specific ethnographic signals used: absence of demonstrable operational content in current ritual performance, explicit community discourse framing the rite as commemorative rather than preparatory, and lineage-holder self-description of the ritual''s purpose as memorial rather than practical.',
    'If those signals were weak or absent, the hybrid_embedded_reading may be the more defensible framing for this specific community, and this story''s ε/type would not transfer to that case without re-authoring.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_choice_disclosure, conceptual, 'Documents the framing choice among the kernel''s three readings and what would change it, per the CS-framing under-determination guidance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_transmission__symbol_continuity_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_transmission__symbol_continuity_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(cata_tr_t10, catastrophe_memory_transmission__symbol_continuity_reading, theater_ratio, 10, 0.27).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_transmission__symbol_continuity_reading, theater_ratio, 20, 0.31).
narrative_ontology:measurement(cata_tr_t30, catastrophe_memory_transmission__symbol_continuity_reading, theater_ratio, 30, 0.35).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_transmission__symbol_continuity_reading, theater_ratio, 40, 0.38).
narrative_ontology:measurement(cata_tr_t50, catastrophe_memory_transmission__symbol_continuity_reading, theater_ratio, 50, 0.4).
narrative_ontology:measurement(cata_tr_t60, catastrophe_memory_transmission__symbol_continuity_reading, theater_ratio, 60, 0.42).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_transmission__symbol_continuity_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(cata_be_t10, catastrophe_memory_transmission__symbol_continuity_reading, base_extractiveness, 10, 0.47).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_transmission__symbol_continuity_reading, base_extractiveness, 20, 0.51).
narrative_ontology:measurement(cata_be_t30, catastrophe_memory_transmission__symbol_continuity_reading, base_extractiveness, 30, 0.55).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_transmission__symbol_continuity_reading, base_extractiveness, 40, 0.58).
narrative_ontology:measurement(cata_be_t50, catastrophe_memory_transmission__symbol_continuity_reading, base_extractiveness, 50, 0.6).
narrative_ontology:measurement(cata_be_t60, catastrophe_memory_transmission__symbol_continuity_reading, base_extractiveness, 60, 0.61).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_transmission__symbol_continuity_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(cata_su_t10, catastrophe_memory_transmission__symbol_continuity_reading, suppression_requirement, 10, 0.43).
narrative_ontology:measurement(cata_su_t20, catastrophe_memory_transmission__symbol_continuity_reading, suppression_requirement, 20, 0.47).
narrative_ontology:measurement(cata_su_t30, catastrophe_memory_transmission__symbol_continuity_reading, suppression_requirement, 30, 0.5).
narrative_ontology:measurement(cata_su_t40, catastrophe_memory_transmission__symbol_continuity_reading, suppression_requirement, 40, 0.53).
narrative_ontology:measurement(cata_su_t50, catastrophe_memory_transmission__symbol_continuity_reading, suppression_requirement, 50, 0.56).
narrative_ontology:measurement(cata_su_t60, catastrophe_memory_transmission__symbol_continuity_reading, suppression_requirement, 60, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_transmission__symbol_continuity_reading, identity_coordination).
narrative_ontology:affects_constraint(catastrophe_memory_transmission__symbol_continuity_reading, catastrophe_memory_transmission__operational_competence_reading).
narrative_ontology:affects_constraint(catastrophe_memory_transmission__symbol_continuity_reading, catastrophe_memory_transmission__hybrid_embedded_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the catastrophe_memory_transmission kernel, each authored as a separate constraint story per the ε-invariance principle: the ritual practice colloquially called 'catastrophe memory transmission' conflates three structurally distinct claims about what the ritual actually does and who it actually serves. symbol_continuity_reading (this story) authors ε=0.61 and tangled_rope on the premise that identity preservation and adaptive capacity have been decoupled; operational_competence_reading and hybrid_embedded_reading author their own independent ε values on the premise that operational content is or is not embedded in the same form. All three link to each other via affects_constraints; none is the 'true' reading — they are three constraints sharing a label.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
