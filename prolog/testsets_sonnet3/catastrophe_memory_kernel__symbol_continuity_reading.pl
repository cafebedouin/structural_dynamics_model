% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_kernel__symbol_continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_kernel__symbol_continuity_reading, []).

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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: catastrophe_memory_kernel__symbol_continuity_reading
 *   human_readable: Catastrophe-Memory Ritual as Symbolic Continuity and Collective Identity
 *   domain: religious_studies/collective_memory
 *
 * SUMMARY:
 *   In the generations following a communal catastrophe, a mourning-rite
 *   becomes fixed in form — specific liturgy, calendar, permitted variation —
 *   and its continuation becomes read as the mark of the community's
 *   persistence as a single entity across time. This story treats that
 *   continuity-marking function as the constraint's core claim, distinct from
 *   three sibling readings of the same underlying kernel (ritual as
 *   boundary-enforcer, as adaptive-capacity transmitter, as
 *   trauma-warning-system). Here the beneficiary is the abstraction of
 *   tradition-continuity itself and those whose standing derives from
 *   performing it faithfully; the cost falls on those who would modify the
 *   symbolic form to fit changed circumstances, and on those at generational
 *   or geographic remove for whom the fixed symbol set no longer maps cleanly
 *   onto lived experience. Extraction is low: no one is siphoning resources
 *   through the rite's persistence, only interpretive authority and the cost
 *   of rigidity are transferred.
 *
 * KEY AGENTS:
 *   - elder_ritual_authorities: sets and enforces liturgical form (institutional/identity_locked)
 *   - communal_identity_holders: primary beneficiaries of stable identity marker (organized/constrained)
 *   - adaptive_modification_advocates: bear the cost of rigidity (moderate/constrained)
 *   - intermarried_and_diaspora_descendants: symbol set no longer fits their circumstances (powerless/constrained)
 *   - younger_generation_members: inherit the obligation without design input (excluded)
 *   - comparative_religion_scholars: analytical observers of drift and persistence across parallel traditions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_kernel__symbol_continuity_reading, 0.22).
domain_priors:suppression_score(catastrophe_memory_kernel__symbol_continuity_reading, 0.38).
domain_priors:theater_ratio(catastrophe_memory_kernel__symbol_continuity_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__symbol_continuity_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__symbol_continuity_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__symbol_continuity_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__symbol_continuity_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__symbol_continuity_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_kernel__symbol_continuity_reading, rope).
narrative_ontology:human_readable(catastrophe_memory_kernel__symbol_continuity_reading, "Catastrophe-Memory Ritual as Symbolic Continuity and Collective Identity").
narrative_ontology:topic_domain(catastrophe_memory_kernel__symbol_continuity_reading, "religious_studies/collective_memory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_kernel__symbol_continuity_reading, 'fc22b771-859c-4977-9104-f2a77112068a').
narrative_ontology:cs_kernel_codification('fc22b771-859c-4977-9104-f2a77112068a', distributed).
narrative_ontology:cs_authority_grounding('fc22b771-859c-4977-9104-f2a77112068a', lineage).
narrative_ontology:cs_interpretation_layer_present('fc22b771-859c-4977-9104-f2a77112068a').
narrative_ontology:cs_reading_relation('fc22b771-859c-4977-9104-f2a77112068a', catastrophe_memory_kernel__boundary_maintenance_reading, coexists_with).
narrative_ontology:cs_reading_relation('fc22b771-859c-4977-9104-f2a77112068a', catastrophe_memory_kernel__survival_competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('fc22b771-859c-4977-9104-f2a77112068a', catastrophe_memory_kernel__trauma_encoding_reading, influences).
narrative_ontology:cs_axiom('fc22b771-859c-4977-9104-f2a77112068a', foundational, symbolic_form_persistence_constitutes_identity_continuity).
narrative_ontology:cs_axiom_status(symbolic_form_persistence_constitutes_identity_continuity, holdable).
narrative_ontology:cs_axiom_grounding('fc22b771-859c-4977-9104-f2a77112068a', symbolic_form_persistence_constitutes_identity_continuity, conventional).
narrative_ontology:cs_axiom('fc22b771-859c-4977-9104-f2a77112068a', secondary, fidelity_to_inherited_form_outweighs_adaptive_fit).
narrative_ontology:cs_axiom_status(fidelity_to_inherited_form_outweighs_adaptive_fit, holdable).
narrative_ontology:cs_axiom_grounding('fc22b771-859c-4977-9104-f2a77112068a', fidelity_to_inherited_form_outweighs_adaptive_fit, instrumental).
narrative_ontology:cs_reference_frame('fc22b771-859c-4977-9104-f2a77112068a', unbroken_transmission_chain_from_witnesses).
narrative_ontology:cs_drift_state('fc22b771-859c-4977-9104-f2a77112068a', third_and_fourth_generation_post_catastrophe, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('fc22b771-859c-4977-9104-f2a77112068a', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_kernel__symbol_continuity_reading, catastrophe_memory_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__symbol_continuity_reading, tradition_continuity_itself).
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__symbol_continuity_reading, communal_identity_holders).
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__symbol_continuity_reading, elder_ritual_authorities).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__symbol_continuity_reading, adaptive_modification_advocates).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__symbol_continuity_reading, intermarried_and_diaspora_descendants).
narrative_ontology:constraint_vindicates(catastrophe_memory_kernel__symbol_continuity_reading, collective_memory_persists_through_symbolic_repetition).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Not an actor but the standing of the tradition as an unbroken symbolic line — every faithful performance of the mourning-rite adds another link. The rite's persistence is itself what accrues; no one administers it on its own behalf but its continuation is what the ritual is measured against.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__symbol_continuity_reading, tradition_continuity_itself, beneficiary,
    institutional, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(catastrophe_memory_kernel__symbol_continuity_reading, tradition_continuity_itself).

% Members who perform the annual mourning-rite gain a stable marker of who they are relative to a remembered catastrophe. Participation confers belonging and a legible place in an intergenerational chain; declining to participate is possible but carries a felt cost of standing outside the collective.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__symbol_continuity_reading, communal_identity_holders, beneficiary,
    organized, generational, constrained, national).

% Custodians of the correct form — the liturgy, the calendar, the permitted variations. They decide what counts as faithful transmission and correct departures from it. Their authority is constituted by the rite's continuation; their professional and personal identity has fused with being its keepers.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__symbol_continuity_reading, elder_ritual_authorities, agenda_setter,
    institutional, generational, identity_locked, national).

% Community members who want the mourning-practice updated — shorter observance, altered language, accommodation for changed circumstances (new geography, new threats, blended households). They bear the cost of the rite's rigidity: their proposals are treated as erosion rather than adaptation, and pressing them risks being read as betraying continuity itself.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__symbol_continuity_reading, adaptive_modification_advocates, payer,
    moderate, biographical, constrained, national).

% Descendants at cultural or geographic remove from the originating community, for whom the rite's exact symbolic vocabulary (specific place-names, language, kinship forms) no longer maps cleanly onto their lived circumstances. They can participate in a form that partially fits them or step outside identity-marking entirely; there is no third option that lets them modify the symbol set and keep standing.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__symbol_continuity_reading, intermarried_and_diaspora_descendants, payer,
    powerless, biographical, constrained, global).

% Children and young adults raised inside the practice who inherit the obligation to continue it without having been consulted on its form. They would, if asked, likely favor some adaptation to make the symbols legible to their own experience, but the consultation channel does not exist — continuity is transmitted downward, not negotiated.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__symbol_continuity_reading, younger_generation_members, excluded,
    powerless, biographical, constrained, national).

% Study the rite's form and persistence across communities and generations, documenting drift, revival, and variation without a stake in any particular liturgical outcome.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__symbol_continuity_reading, comparative_religion_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_kernel__symbol_continuity_reading, diffuse).
narrative_ontology:fixing_cost_class(catastrophe_memory_kernel__symbol_continuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The mourning-rite provides a shared, repeatable symbolic vocabulary that lets a dispersed or generationally extended group recognize itself as one continuous entity across time, without requiring everyone to agree on doctrine, history, or politics — the form itself carries the continuity.
% TRANSFER_FUNCTION: Moves interpretive authority and the right to define 'faithful practice' toward those who hold and administer the traditional form (elder ritual authorities), and moves the cost of formal rigidity onto members whose circumstances have diverged from the form's original context (diaspora descendants, modification advocates).
% ABSENT_VOICES: Younger members raised inside the practice, and descendants at cultural remove, would likely propose adapting the symbolic vocabulary to fit their lived circumstances; they are structurally downstream recipients of the form rather than parties to its design, and the transmission channel runs one direction.
% DISAPPEARANCE_RATIONALE: Elder ritual authorities and communal identity holders would say the world rearranges sharply — the community loses its primary mechanism for marking itself as continuous with the past. Modification advocates and diaspora descendants would say much less changes than claimed: informal memory, family narrative, and looser commemorative practice would likely persist and could better fit their situations; the dispute is exactly over how much of the claimed continuity function is irreplaceable versus performed.
% FOUNDING_PROBLEM: A community faced with catastrophic loss needed a way to keep the memory of what happened, and who was lost, from dissolving as direct witnesses aged and died — a durable symbolic anchor that would outlast any single generation's memory.
% FOUNDING_PROBLEM_CORROBORATION: Elder ritual authorities and communal identity holders attest the founding problem remains live — direct witnesses are gone, and the rite is now the primary transmission mechanism. Comparative religion scholars, working from outside the benefiting community, corroborate that the memory-transmission function is real but note the specific liturgical FORM has in many parallel cases decoupled from transmission efficacy, persisting as identity-marker independent of whether it still transmits the original memory content most effectively.
narrative_ontology:disappearance_verdict(catastrophe_memory_kernel__symbol_continuity_reading, contested).
narrative_ontology:founding_problem_status(catastrophe_memory_kernel__symbol_continuity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_kernel__symbol_continuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(catastrophe_memory_kernel__symbol_continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_kernel__symbol_continuity_reading, 0.22, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_kernel__symbol_continuity_reading_tests).
:- end_tests(catastrophe_memory_kernel__symbol_continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.22 by interval end) because nothing material is being extracted through the rite's operation — no rents, no resource transfer beyond the interpretive-authority and identity-standing transfer described above. Suppression is moderate (0.38): there is real social cost to declining or modifying, but no coercive enforcement apparatus comparable to a legal or economic sanction. Theater ratio rises over the interval (0.12 to 0.30) reflecting a slow drift toward performed correctness as the founding witnesses recede and the rite's connection to lived memory of the actual catastrophe thins — the form persists more for its own sake and less as live transmission, a signal worth tracking even though it does not, on this reading, push the constraint out of rope territory.
 *
 * DIRECTIONALITY LOGIC:
 *   Tradition-continuity itself and elder ritual authorities sit near the beneficiary end: the former is what accrues with every faithful performance, the latter derive standing and authority from administering it. Communal identity holders are a genuine but softer beneficiary — real coordination value, low cost. Adaptive modification advocates and diaspora descendants sit toward the target end: they bear the cost of the form's rigidity without commensurate benefit, and their exit options are constrained rather than mobile — leaving the practice means forfeiting the identity-marking function entirely, there is no partial-exit path that preserves standing while modifying the form.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preserving memory of catastrophic loss as direct witnesses die out) is contested as live: elder authorities say the transmission function is more necessary than ever given attrition of firsthand memory; scholars corroborate the transmission function is real but note the specific liturgical form has in comparable traditions decoupled from transmission efficacy. This is not full mandatrophy (the founding problem persists in the outside-corroborated view too), but the classification as low-extraction rope depends on the coordination function remaining substantially intact — the rising theater_ratio series is the early-warning signal that this reading's own metrics would need to keep tracking should the form's link to memory-transmission continue thinning.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    continuity_vs_boundary_disambiguation,
    'When the mourning-rite excludes non-conforming members or those who modify its form, is that exclusion incidental to preserving symbolic continuity (this reading), or is boundary-enforcement the actual mechanism doing the work (the sibling boundary_maintenance_reading)?',
    'Compare cases where the rite is practiced without any exclusionary consequence attached (private commemoration, diaspora communities with no enforcement capacity) against cases with active social sanction for non-participation; if the continuity-marking function persists robustly in the no-sanction cases, the readings are genuinely separable.',
    'If continuity-marking cannot be observed independent of boundary-enforcement in practice, this reading''s low-extraction claim would need revision toward the sibling''s higher-suppression profile — the two readings would be shown to be causally entangled rather than merely two lenses on the same low-stakes practice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(continuity_vs_boundary_disambiguation, conceptual, 'Whether symbolic continuity is separable in practice from boundary-enforcement, or the same mechanism under two labels.').

omega_variable(
    natural_memory_vs_constructed_continuity_claim,
    'Is the felt necessity of ritual FORM (as opposed to memory content) for continuity a natural feature of how collective identity persists across generations, or is the specific liturgical fixity a constructed claim that benefits those who administer the fixed form?',
    'Cross-tradition comparison: communities that have deliberately updated ritual form while explicitly retaining memory content, tracked for whether collective identity and continuity markers persist as robustly as in fixed-form traditions.',
    'If updated-form communities show equally robust continuity, the fixed-form claim is substantially administrator-serving rather than functionally necessary, which would raise this reading''s suppression and extractiveness scores and move it toward tangled_rope; if fixed form proves functionally load-bearing, the rope classification and low extraction are supported.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(natural_memory_vs_constructed_continuity_claim, empirical, 'Whether ritual fixity itself, versus memory content, is functionally necessary for continuity — bears on whether elder-authority benefit is incidental or extractive.').

omega_variable(
    founding_problem_genealogy_reading_dependence,
    'Does the founding-problem genealogy (preserving memory as witnesses die) actually support THIS reading''s low-extraction rope claim, or does the same genealogy equally support the trauma_encoding_reading''s higher-extraction claim depending on which aspect of ''memory'' is emphasized?',
    'Examine whether corroborating outside sources (comparative religion scholars) distinguish symbolic-identity-continuity transmission from trauma-warning transmission when describing the same historical rite, or treat them as inseparable.',
    'If scholarly corroboration cannot cleanly separate the two functions, the ε-invariance decomposition into separate stories may be under-motivated for this specific kernel, though the framework''s authoring rule still requires the split per constraint identity discipline.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_genealogy_reading_dependence, conceptual, 'Whether the shared founding-problem narrative differentially supports this reading versus the trauma_encoding sibling reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_kernel__symbol_continuity_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_kernel__symbol_continuity_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_kernel__symbol_continuity_reading, theater_ratio, 20, 0.17).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_kernel__symbol_continuity_reading, theater_ratio, 40, 0.21).
narrative_ontology:measurement(cata_tr_t60, catastrophe_memory_kernel__symbol_continuity_reading, theater_ratio, 60, 0.25).
narrative_ontology:measurement(cata_tr_t80, catastrophe_memory_kernel__symbol_continuity_reading, theater_ratio, 80, 0.28).
narrative_ontology:measurement(cata_tr_t100, catastrophe_memory_kernel__symbol_continuity_reading, theater_ratio, 100, 0.3).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_kernel__symbol_continuity_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_kernel__symbol_continuity_reading, base_extractiveness, 20, 0.14).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_kernel__symbol_continuity_reading, base_extractiveness, 40, 0.17).
narrative_ontology:measurement(cata_be_t60, catastrophe_memory_kernel__symbol_continuity_reading, base_extractiveness, 60, 0.19).
narrative_ontology:measurement(cata_be_t80, catastrophe_memory_kernel__symbol_continuity_reading, base_extractiveness, 80, 0.21).
narrative_ontology:measurement(cata_be_t100, catastrophe_memory_kernel__symbol_continuity_reading, base_extractiveness, 100, 0.22).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(catastrophe_memory_kernel__symbol_continuity_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_kernel__symbol_continuity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(catastrophe_memory_kernel__symbol_continuity_reading, 0.08).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__symbol_continuity_reading, catastrophe_memory_kernel__boundary_maintenance_reading).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__symbol_continuity_reading, catastrophe_memory_kernel__survival_competence_reading).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__symbol_continuity_reading, catastrophe_memory_kernel__trauma_encoding_reading).

% DUAL FORMULATION NOTE:
% This story is one of four constraints decomposed from a single natural-language concept ('ritual preserves memory of catastrophe') per the ε-invariance principle: each sibling reading (boundary_maintenance, survival_competence, symbol_continuity, trauma_encoding) names a structurally distinct claim about what the ritual is doing, with distinct beneficiary/victim structures and distinct ε. This reading (symbol_continuity) is authored as the lowest-extraction member of the family — pure identity-marking coordination with rigidity costs borne by those seeking adaptation, as opposed to the coercive-boundary or trauma-reinforcing mechanisms hypothesized in the sibling readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
