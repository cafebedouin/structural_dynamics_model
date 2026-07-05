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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   human_readable: Catastrophe Commemoration Ritual — Symbolic Continuity Reading
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   This story instantiates the symbol_continuity reading of the
 *   catastrophe_memory_kernel: a community's fixed mourning ritual, examined
 *   specifically as a mechanism for preserving symbolic continuity and
 *   collective identity across dispersal and time, independent of whether it
 *   also functions as survival-skill transmission, trauma-warning encoding,
 *   or boundary policing (those are separate constraints in this family).
 *   Under this reading the coordination good is legibility-to-one-another
 *   across a dispersed population; the cost is borne by anyone whose life no
 *   longer fits the fixed symbolic vocabulary — reformers proposing
 *   adaptation, and especially mixed-heritage descendants whose family
 *   configuration was never contemplated by the form. Extraction is
 *   comparatively low because the ritual moves no material resource at scale;
 *   what it moves is interpretive latitude and belonging-recognition,
 *   concentrated with the custodial class and withheld from those seeking
 *   modification.
 *
 * KEY AGENTS:
 *   - elder_ritual_custodians: agenda_setter/beneficiary (institutional/identity_locked) — administer fidelity, cannot exit their role without losing communal identity
 *   - tradition_continuity_institution: non-agent beneficiary — the abstract good of continuity itself, invoked to justify fixed form
 *   - diaspora_identity_cohesion: beneficiary (organized/constrained) — gains synchronized legibility across geography
 *   - adaptive_modification_advocates: payer (moderate/constrained) — bear cost of being read as unfaithful
 *   - intermarried_and_mixed_heritage_descendants: payer (powerless/trapped) — form was not built to admit their configuration
 *   - younger_reform_minded_practitioners: payer (moderate/identity_locked) — torn between belonging and felt disconnection
 *   - comparative_religion_scholars: observer (analytical) — cross-community comparative view
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_kernel__symbol_continuity_reading, 0.28).
domain_priors:suppression_score(catastrophe_memory_kernel__symbol_continuity_reading, 0.42).
domain_priors:theater_ratio(catastrophe_memory_kernel__symbol_continuity_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__symbol_continuity_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__symbol_continuity_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__symbol_continuity_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__symbol_continuity_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__symbol_continuity_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_kernel__symbol_continuity_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_kernel__symbol_continuity_reading, "Catastrophe Commemoration Ritual — Symbolic Continuity Reading").
narrative_ontology:topic_domain(catastrophe_memory_kernel__symbol_continuity_reading, "religious_studies/collective_memory/ritual_practice").

domain_priors:requires_active_enforcement(catastrophe_memory_kernel__symbol_continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_kernel__symbol_continuity_reading, 'f20f27ff-b512-49d2-8f68-c4549519578e').
narrative_ontology:cs_kernel_codification('f20f27ff-b512-49d2-8f68-c4549519578e', distributed).
narrative_ontology:cs_authority_grounding('f20f27ff-b512-49d2-8f68-c4549519578e', lineage).
narrative_ontology:cs_interpretation_layer_present('f20f27ff-b512-49d2-8f68-c4549519578e').
narrative_ontology:cs_reading_relation('f20f27ff-b512-49d2-8f68-c4549519578e', catastrophe_memory_kernel__survival_competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('f20f27ff-b512-49d2-8f68-c4549519578e', catastrophe_memory_kernel__trauma_encoding_reading, coexists_with).
narrative_ontology:cs_reading_relation('f20f27ff-b512-49d2-8f68-c4549519578e', catastrophe_memory_kernel__boundary_maintenance_reading, influences).
narrative_ontology:cs_axiom('f20f27ff-b512-49d2-8f68-c4549519578e', foundational, fixed_symbolic_form_is_the_continuity_mechanism).
narrative_ontology:cs_axiom_status(fixed_symbolic_form_is_the_continuity_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('f20f27ff-b512-49d2-8f68-c4549519578e', fixed_symbolic_form_is_the_continuity_mechanism, conventional).
narrative_ontology:cs_axiom('f20f27ff-b512-49d2-8f68-c4549519578e', secondary, adaptive_modification_degrades_transmissible_identity).
narrative_ontology:cs_axiom_status(adaptive_modification_degrades_transmissible_identity, holdable).
narrative_ontology:cs_axiom_grounding('f20f27ff-b512-49d2-8f68-c4549519578e', adaptive_modification_degrades_transmissible_identity, empirically_contingent).
narrative_ontology:cs_reference_frame('f20f27ff-b512-49d2-8f68-c4549519578e', post_catastrophe_founding_transmission).
narrative_ontology:cs_drift_state('f20f27ff-b512-49d2-8f68-c4549519578e', contemporary_diaspora_dispersal, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f20f27ff-b512-49d2-8f68-c4549519578e', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_kernel__symbol_continuity_reading, catastrophe_memory_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__symbol_continuity_reading, tradition_continuity_institution).
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__symbol_continuity_reading, elder_ritual_custodians).
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__symbol_continuity_reading, diaspora_identity_cohesion).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__symbol_continuity_reading, adaptive_modification_advocates).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__symbol_continuity_reading, intermarried_and_mixed_heritage_descendants).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__symbol_continuity_reading, younger_reform_minded_practitioners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer the calendar, liturgy, and correct performance of the mourning ritual, deciding what counts as faithful transmission versus corruption. Their communal standing and life's meaning are constituted by being the ones who carry the practice forward unchanged; they cannot renounce the custodial role without dissolving their own social identity.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__symbol_continuity_reading, elder_ritual_custodians, agenda_setter,
    institutional, civilizational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_kernel__symbol_continuity_reading, elder_ritual_custodians, beneficiary).

% The abstract continuity of symbolic transmission itself is what the ritual is built to serve — it collects nothing as an actor but is the standing good the custodians and diaspora invoke to justify fidelity to fixed form over adaptation.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__symbol_continuity_reading, tradition_continuity_institution, beneficiary,
    institutional, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(catastrophe_memory_kernel__symbol_continuity_reading, tradition_continuity_institution).

% Dispersed communities use synchronized performance of the ritual to remain legible to one another as one people despite geographic separation. Participation confers belonging and mutual recognition; opting out risks being read as having left the community rather than merely having changed a practice.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__symbol_continuity_reading, diaspora_identity_cohesion, beneficiary,
    organized, generational, constrained, global).

% Propose updating elements of the mourning practice — shortened observances, translated liturgy, altered timing to fit contemporary life — to keep the practice alive for people it no longer fits as performed. They bear the cost of being labeled unfaithful or assimilationist for suggesting change, and their proposals are routinely rejected on continuity grounds regardless of practical merit.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__symbol_continuity_reading, adaptive_modification_advocates, payer,
    moderate, biographical, constrained, national).

% Occupy an ambiguous position relative to the ritual's boundary-drawing form; the rigid symbolic vocabulary was not built to admit their mixed household configurations, so full participation is either denied or requires erasing part of their own family history. They cannot exit without being read as having abandoned the group, and cannot fully enter without contorting their actual situation.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__symbol_continuity_reading, intermarried_and_mixed_heritage_descendants, payer,
    powerless, biographical, trapped, national).

% Feel the pull of continued belonging but experience the fixed ritual form as increasingly disconnected from their lived circumstances. Leaving the practice risks rupturing family and communal ties built around it; staying means absorbing the cost of performing forms that feel emptied of felt meaning for them personally.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__symbol_continuity_reading, younger_reform_minded_practitioners, payer,
    moderate, biographical, identity_locked, national).

% Study how mourning rituals function as identity-transmission mechanisms across catastrophe-surviving populations, comparing this community's insistence on formal fidelity to other groups' more adaptive transmission strategies.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__symbol_continuity_reading, comparative_religion_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Synchronizes symbolic performance across a dispersed population so that shared identity and continuity with the past remain legible and mutually recognizable without requiring physical proximity or centralized governance.
% TRANSFER_FUNCTION: Moves flexibility and adaptive latitude away from individuals whose circumstances no longer match the ritual's fixed symbolic vocabulary, and concentrates interpretive authority over 'faithful transmission' with the custodial class; in return it moves a stable, legible marker of belonging to everyone who performs the form correctly.
% ABSENT_VOICES: Intermarried descendants and reform-minded younger members raise the loudest objections but are structurally positioned as questioners of fidelity rather than participants in defining the practice — their proposed modifications are heard, when heard at all, as threats to continuity rather than as legitimate transmission strategy.
% DISAPPEARANCE_RATIONALE: If the fixed ritual form vanished overnight, the diaspora would lose its principal synchronized marker of shared identity; custodians would lose their structural role; and constrained descendants and reformers would gain latitude to construct forms that fit their actual lives, at the cost of some loss of cross-community legibility in the short term.
% FOUNDING_PROBLEM: A dispersed population, after catastrophic rupture, needed a way to remain recognizable to itself and to transmit a shared symbolic identity across generations and geography without central governance.
% FOUNDING_PROBLEM_CORROBORATION: Custodians and diaspora organizations attest the problem remains fully live — dispersal and assimilation pressure continue. Comparative religion scholars, working from outside the beneficiary set, attest the coordination need for SOME shared marker is live but that the specific fixed symbolic vocabulary has increasingly diverged from the felt lived circumstances of a growing share of the population, corroborated by documented rising rates of quiet non-participation among intermarried and younger members.
narrative_ontology:disappearance_verdict(catastrophe_memory_kernel__symbol_continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_kernel__symbol_continuity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_kernel__symbol_continuity_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(catastrophe_memory_kernel__symbol_continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_kernel__symbol_continuity_reading, 0.28, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is low-moderate (0.28 at interval end) because under this reading nothing material is extracted — the ritual moves symbolic/interpretive goods, not resources, and even those flows are diffuse. Suppression (0.42) is present but moderate: rejection of proposed adaptations and social costs to noncompliance, not coercive force. Theater ratio rises substantially over the interval (0.20 to 0.45) reflecting a genuine drift signal specific to THIS reading: as dispersal continues and the population that the fixed form was built for shrinks relative to those it doesn't fit, an increasing share of ritual performance is maintained for its symbolic-continuity display value rather than for any operational function it once carried under other readings (survival competence, trauma warning) — those functions are captured in sibling constraints, not here.
 *
 * PERSPECTIVAL GAP:
 *   Custodians and diaspora organizations experience the fixed form as the coordination good itself — a rope. Adaptive-modification advocates and mixed-heritage descendants experience the same fixed form as a tangled rope: real continuity function exists, but it also extracts flexibility and belonging-recognition from anyone whose life diverges from the form's assumptions, and this extraction requires active social enforcement (labeling proposed changes as unfaithful) to persist.
 *
 * DIRECTIONALITY LOGIC:
 *   Custodians and diaspora cohesion sit near the beneficiary end: they collect legibility and continuity-value from the arrangement and administer or benefit from its fixed form. Adaptive advocates and reformers sit toward the target end: constrained exit, real cost from social labeling. Mixed-heritage descendants sit closest to full-target: trapped exit options, since leaving reads as abandoning heritage entirely rather than modifying a practice, and the form's symbolic vocabulary structurally excludes their actual family configuration.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (post-catastrophe dispersal needing a shared identity marker) is contested as live: custodians and diaspora organizations say dispersal pressure continues and justifies fixed fidelity; scholars and the rising non-participation data suggest the coordination NEED persists but the SPECIFIC fixed vocabulary has drifted from fitting the actual population it serves. This is not classified as full mandatrophy (the coordination function is not dead) but the rising theater ratio signals the fixed-form mechanism increasingly substituting symbolic performance for the adaptive continuity it was meant to secure — a partial drift the classification should register as tangled rope rather than clean rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    continuity_function_separability,
    'Can the symbolic-continuity function of the ritual be preserved under an adapted form, or does adaptation necessarily degrade the very continuity the ritual exists to provide?',
    'Comparative study of communities that have modified analogous mourning rituals (e.g., translated liturgy, shortened observance) and measurement of whether cross-community legibility and generational transmission rates held, declined, or were unaffected.',
    'If continuity survives adaptation, the custodians'' fidelity requirement is closer to pure extraction of flexibility with no coordination offset; if continuity genuinely depends on fixed form, the rigidity is closer to a necessary coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(continuity_function_separability, empirical, 'Whether symbolic continuity requires literal formal fixity or survives modification.').

omega_variable(
    kernel_reading_boundary_location,
    'Where exactly does the symbol-continuity function end and the boundary-maintenance function begin, given that both readings point at the same observable practice (excluding non-conforming participants)?',
    'Compare this reading''s cost structure (adaptive modification suppressed) against the boundary_maintenance_reading''s cost structure (exclusion of out-group members) using independent victim-group data; overlap in victim identity across readings would indicate the two constructs are less separable than the decomposition assumes.',
    'If the victim groups for symbol_continuity and boundary_maintenance turn out to be structurally identical rather than merely overlapping, the ε-invariance decomposition into four separate constraints may need to collapse two of them into one story with a documented dual reading rather than four fully independent files.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_boundary_location, conceptual, 'Whether the symbol-continuity and boundary-maintenance readings are genuinely structurally distinct or two labels for one mechanism.').

omega_variable(
    custodial_identity_lock_reversibility,
    'Is the custodians'' identity-lock to the custodial role permanent, or would it dissolve if an alternative, equally prestigious continuity-preserving role emerged?',
    'Observe whether custodians who adopt adapted forms elsewhere retain communal standing, or whether standing is contingent specifically on defending the unmodified form.',
    'If standing transfers to adapted-form custodianship, the identity-lock is a contingent social fact rather than an intrinsic feature of the role, weakening the case that fixed form is required by the coordination function itself.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(custodial_identity_lock_reversibility, conceptual, 'Whether custodial identity-lock is intrinsic to fidelity or a contingent social arrangement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_kernel__symbol_continuity_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_kernel__symbol_continuity_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(cata_tr_t10, catastrophe_memory_kernel__symbol_continuity_reading, theater_ratio, 10, 0.25).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_kernel__symbol_continuity_reading, theater_ratio, 20, 0.31).
narrative_ontology:measurement(cata_tr_t30, catastrophe_memory_kernel__symbol_continuity_reading, theater_ratio, 30, 0.35).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_kernel__symbol_continuity_reading, theater_ratio, 40, 0.39).
narrative_ontology:measurement(cata_tr_t50, catastrophe_memory_kernel__symbol_continuity_reading, theater_ratio, 50, 0.43).
narrative_ontology:measurement(cata_tr_t60, catastrophe_memory_kernel__symbol_continuity_reading, theater_ratio, 60, 0.45).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_kernel__symbol_continuity_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(cata_be_t10, catastrophe_memory_kernel__symbol_continuity_reading, base_extractiveness, 10, 0.18).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_kernel__symbol_continuity_reading, base_extractiveness, 20, 0.21).
narrative_ontology:measurement(cata_be_t30, catastrophe_memory_kernel__symbol_continuity_reading, base_extractiveness, 30, 0.24).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_kernel__symbol_continuity_reading, base_extractiveness, 40, 0.26).
narrative_ontology:measurement(cata_be_t50, catastrophe_memory_kernel__symbol_continuity_reading, base_extractiveness, 50, 0.27).
narrative_ontology:measurement(cata_be_t60, catastrophe_memory_kernel__symbol_continuity_reading, base_extractiveness, 60, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_kernel__symbol_continuity_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(cata_su_t10, catastrophe_memory_kernel__symbol_continuity_reading, suppression_requirement, 10, 0.33).
narrative_ontology:measurement(cata_su_t20, catastrophe_memory_kernel__symbol_continuity_reading, suppression_requirement, 20, 0.36).
narrative_ontology:measurement(cata_su_t30, catastrophe_memory_kernel__symbol_continuity_reading, suppression_requirement, 30, 0.38).
narrative_ontology:measurement(cata_su_t40, catastrophe_memory_kernel__symbol_continuity_reading, suppression_requirement, 40, 0.4).
narrative_ontology:measurement(cata_su_t50, catastrophe_memory_kernel__symbol_continuity_reading, suppression_requirement, 50, 0.41).
narrative_ontology:measurement(cata_su_t60, catastrophe_memory_kernel__symbol_continuity_reading, suppression_requirement, 60, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_kernel__symbol_continuity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(catastrophe_memory_kernel__symbol_continuity_reading, 0.1).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__symbol_continuity_reading, catastrophe_memory_kernel__survival_competence_reading).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__symbol_continuity_reading, catastrophe_memory_kernel__trauma_encoding_reading).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__symbol_continuity_reading, catastrophe_memory_kernel__boundary_maintenance_reading).

% DUAL FORMULATION NOTE:
% This story is one of four sibling readings of the catastrophe_memory_kernel, each instantiating a structurally distinct claim about the same observed ritual practice: symbol_continuity_reading (this file — identity transmission, low extraction), survival_competence_reading (adaptive-capacity transmission), trauma_encoding_reading (intergenerational warning encoding), and boundary_maintenance_reading (group-boundary enforcement). Each carries its own ε, beneficiary/victim structure, and classification per the ε-invariance principle; none averages or hedges across the others. Network edges here are declared bidirectionally-in-spirit across the family (each sibling should also link back to this file and to the other two).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
