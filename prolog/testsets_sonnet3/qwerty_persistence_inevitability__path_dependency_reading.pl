% ============================================================================
% CONSTRAINT STORY: qwerty_persistence_inevitability__path_dependency_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_qwerty_persistence_inevitability__path_dependency_reading, []).

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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: qwerty_persistence_inevitability__path_dependency_reading
 *   human_readable: QWERTY Keyboard Layout Persistence (Path Dependency Reading)
 *   domain: technology_history/political_economy/institutional_analysis
 *
 * SUMMARY:
 *   This story instantiates the path-dependency reading of the contested
 *   QWERTY-persistence kernel: the layout was fixed by a mechanical
 *   constraint (jamming) that no longer applies, and its persistence since is
 *   explained entirely by network-effect coordination among typists,
 *   manufacturers, and instructors, none of whom extract rent from the
 *   specific letters being where they are. A sibling reading
 *   (strategic_lock_in_reading, not this file) holds that manufacturer
 *   cartels and training-industry partnerships actively engineered and
 *   enforced the lock-in for commercial advantage — that is a different
 *   constraint with a different beneficiary structure and a different ε,
 *   generated separately.
 *
 * KEY AGENTS:
 *   - incumbent_typing_population: benefits from shared standard, generational time horizon, constrained exit due to retraining cost
 *   - keyboard_manufacturers: demand-followers in this reading, mobile exit (low technical switching cost)
 *   - efficiency_researchers: excluded voice, proposals exist but cannot overcome coordination inertia
 *   - general_public: bears diffuse externality, no identifiable extractor
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qwerty_persistence_inevitability__path_dependency_reading, 0.08).
domain_priors:suppression_score(qwerty_persistence_inevitability__path_dependency_reading, 0.05).
domain_priors:theater_ratio(qwerty_persistence_inevitability__path_dependency_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__path_dependency_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__path_dependency_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__path_dependency_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__path_dependency_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__path_dependency_reading, resistance, 0.12).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qwerty_persistence_inevitability__path_dependency_reading, mountain).
narrative_ontology:human_readable(qwerty_persistence_inevitability__path_dependency_reading, "QWERTY Keyboard Layout Persistence (Path Dependency Reading)").
narrative_ontology:topic_domain(qwerty_persistence_inevitability__path_dependency_reading, "technology_history/political_economy/institutional_analysis").

domain_priors:emerges_naturally(qwerty_persistence_inevitability__path_dependency_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qwerty_persistence_inevitability__path_dependency_reading, 'd55a8aa5-5615-4f8f-bac3-fd4d4cabe223').
narrative_ontology:cs_kernel_codification('d55a8aa5-5615-4f8f-bac3-fd4d4cabe223', distributed).
narrative_ontology:cs_authority_grounding('d55a8aa5-5615-4f8f-bac3-fd4d4cabe223', distributed).
narrative_ontology:cs_reading_relation('d55a8aa5-5615-4f8f-bac3-fd4d4cabe223', qwerty_persistence_inevitability__strategic_lock_in_reading, coexists_with).
narrative_ontology:cs_axiom('d55a8aa5-5615-4f8f-bac3-fd4d4cabe223', foundational, persistence_explained_by_uncoordinated_network_effects).
narrative_ontology:cs_axiom_status(persistence_explained_by_uncoordinated_network_effects, holdable).
narrative_ontology:cs_axiom_grounding('d55a8aa5-5615-4f8f-bac3-fd4d4cabe223', persistence_explained_by_uncoordinated_network_effects, empirically_contingent).
narrative_ontology:cs_axiom('d55a8aa5-5615-4f8f-bac3-fd4d4cabe223', foundational, no_identifiable_agent_captures_switching_cost_rent).
narrative_ontology:cs_axiom_status(no_identifiable_agent_captures_switching_cost_rent, holdable).
narrative_ontology:cs_axiom_grounding('d55a8aa5-5615-4f8f-bac3-fd4d4cabe223', no_identifiable_agent_captures_switching_cost_rent, empirically_contingent).
narrative_ontology:cs_reference_frame('d55a8aa5-5615-4f8f-bac3-fd4d4cabe223', mechanical_jamming_constraint_1873).
narrative_ontology:cs_drift_state('d55a8aa5-5615-4f8f-bac3-fd4d4cabe223', contemporary_electronic_keyboard_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('d55a8aa5-5615-4f8f-bac3-fd4d4cabe223', '').
narrative_ontology:cs_kernel_id(qwerty_persistence_inevitability__path_dependency_reading, qwerty_persistence_inevitability).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(qwerty_persistence_inevitability__path_dependency_reading, incumbent_typing_population).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(qwerty_persistence_inevitability__path_dependency_reading, general_public).
narrative_ontology:constraint_vindicates(qwerty_persistence_inevitability__path_dependency_reading, network_effects_produce_lockin_without_design).
narrative_ontology:constraint_vindicates(qwerty_persistence_inevitability__path_dependency_reading, coordination_equilibria_can_be_accidental).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Already trained on QWERTY, this population benefits from every other typist and every keyboard manufacturer sharing the same standard — retraining cost is the only 'cost' at issue, and it is borne diffusely and voluntarily-incurred rather than extracted by any party. Their continued use of QWERTY is what maintains the network effect, not a rent flowing to them from anyone else.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__path_dependency_reading, incumbent_typing_population, beneficiary,
    moderate, generational, constrained, global).

% Manufacture whatever layout the market demands. In this reading they are demand-followers, not cartel enforcers: switching production to an alternative layout is technically trivial and manufacturers who tried alternative layouts historically found no buyer base, not a blocked market. Their exit option is mobile because the technical switching cost to them specifically is low; what is fixed is the demand side.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__path_dependency_reading, keyboard_manufacturers, agenda_setter,
    organized, biographical, mobile, global).

% Teach whichever layout has the largest existing base of teachers, textbooks, and job-market expectation. They did not originate the standard and do not extract from maintaining it; they are downstream adapters to the equilibrium, in this reading's account of events.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__path_dependency_reading, typing_instruction_institutions, observer,
    institutional, generational, mobile, national).

% Researchers who have proposed alternative layouts (Dvorak and others) and demonstrated modest efficiency gains in some studies. Their proposals are not suppressed by any enforcing party in this reading — they simply cannot overcome the coordination problem of switching an entire trained population and installed hardware base simultaneously. Their voice is present in the literature but structurally unable to move the equilibrium.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__path_dependency_reading, efficiency_researchers, excluded,
    moderate, biographical, mobile, global).

% Bears a small, diffuse efficiency loss (marginally slower typing than an optimized layout might allow) as an externality of the coordination equilibrium, not as a transfer captured by any identifiable party. No one collects what the public loses; it is dissipated, not extracted.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__path_dependency_reading, general_public, payer,
    powerless, civilizational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(qwerty_persistence_inevitability__path_dependency_reading, diffuse).
narrative_ontology:fixing_cost_class(qwerty_persistence_inevitability__path_dependency_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the genuine collective-action problem of universal keyboard interoperability: every typist, keyboard, typewriter/computer, and typing curriculum needs to agree on one layout, and once one is adopted at scale the cost of any individual or firm deviating exceeds the benefit, regardless of the original layout's merits.
% TRANSFER_FUNCTION: There is no directed transfer under this reading. Any efficiency loss relative to a hypothetical optimal layout is diffused across the entire typing population as a shared externality; no party collects it as a rent.
% ABSENT_VOICES: Alternative-layout researchers and early adopters (Dvorak advocates) are present in the historical and academic record but structurally unable to coordinate a mass switch — their absence from influence is a collective-action failure, not an exclusion enforced by a beneficiary.
% DISAPPEARANCE_RATIONALE: If the QWERTY standard vanished overnight (all trained muscle memory and manufactured stock reset), the population would re-coordinate on some layout, quite possibly a different one — the arrangement is structurally load-bearing for interoperability, even though this reading holds no party is currently extracting rent from its specific shape.
% FOUNDING_PROBLEM: Early typewriter mechanisms (1870s) jammed when common letter pairs were struck in rapid succession from adjacent keys; the layout was arranged to separate frequently sequential letters and reduce mechanical jamming, and the resulting arrangement then locked in as the trained population and manufactured base grew.
% FOUNDING_PROBLEM_CORROBORATION: Historians of technology (e.g. Paul David's path-dependency scholarship, and mechanical-typewriter engineering histories written independently of any keyboard manufacturer or typing-instruction body) attest that the original jamming problem no longer applies to electronic keyboards, yet the layout persists purely as a coordination equilibrium — this corroboration comes from academic economic historians outside any party that benefits from the standard's persistence.
narrative_ontology:disappearance_verdict(qwerty_persistence_inevitability__path_dependency_reading, world_rearranges).
narrative_ontology:founding_problem_status(qwerty_persistence_inevitability__path_dependency_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qwerty_persistence_inevitability__path_dependency_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(qwerty_persistence_inevitability__path_dependency_reading, 'none', 1).
narrative_ontology:epsilon_provenance(qwerty_persistence_inevitability__path_dependency_reading, 0.08, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(qwerty_persistence_inevitability__path_dependency_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(qwerty_persistence_inevitability__path_dependency_reading, ExtMetricName, E),
    domain_priors:suppression_score(qwerty_persistence_inevitability__path_dependency_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(qwerty_persistence_inevitability__path_dependency_reading),
    narrative_ontology:constraint_metric(qwerty_persistence_inevitability__path_dependency_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(qwerty_persistence_inevitability__path_dependency_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(qwerty_persistence_inevitability__path_dependency_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.08) because under this reading no party captures a transfer from the arrangement's persistence — the efficiency loss (if any) is a diffuse externality, not a rent. Suppression is near-zero (0.05) because nothing actively blocks alternative layouts from being taught or adopted; the barrier is coordination cost, not coercion. Accessibility collapse is high (0.88) because once the standard is set, individually rational actors cannot profitably deviate — that is the signature of a genuine coordination equilibrium, not of enforced exclusivity. Resistance is low (0.12): there is some academic advocacy for alternatives but no organized movement contesting the standard as illegitimate.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (incumbent typists) benefit only in the weak, non-extractive sense of avoiding retraining cost — this is closer to a subsidy from the equilibrium than a rent collected from anyone. No victims are declared: the efficiency loss borne by the general public is diffuse and uncaptured, which is precisely why this reading assigns it a `payer` role without a corresponding beneficiary that collects what is paid. This is the structural signature the schema flags as mountain-with-beneficiary (FSM candidate) rather than tangled_rope, because the beneficiary relationship is incidental to the coordination function, not the product of enforcement.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (mechanical jamming) is dead, yet the arrangement persists — this is exactly the pattern mandatrophy analysis exists to catch. This reading's claim is that the persistence is NOT mandatrophy in the extractive sense (no one benefits from maintaining a now-obsolete justification) but rather ordinary network-effect inertia: the coordination function has migrated from 'prevent jamming' to 'preserve universal interoperability,' and the second function is still live even though the first is dead. The sibling reading disputes this migration story and holds that named institutional actors actively work to keep the equilibrium from tipping for their own benefit.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    accidental_vs_engineered_origin,
    'Was the persistence of QWERTY after the jamming problem became obsolete (roughly 1920s onward, with electric and then electronic keyboards) driven purely by decentralized network-effect coordination, or did manufacturers and typing-instruction institutions actively coordinate (formally or tacitly) to preserve the standard for commercial advantage?',
    'Archival research into typewriter-manufacturer trade association records, typing-school curriculum contracts, and any documented coordination agreements between manufacturers and instructional institutions in the 1920s-1950s period when alternative layouts (e.g. Dvorak, patented 1936) were proposed and commercially tested.',
    'If coordination/agreement evidence is found, this reading is undermined and the sibling strategic_lock_in_reading becomes the better-supported account for that period — the constraint would carry real beneficiaries extracting from enforced standardization rather than diffuse network effects.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(accidental_vs_engineered_origin, empirical, 'Whether QWERTY''s post-jamming persistence is decentralized coordination or coordinated manufacturer lock-in.').

omega_variable(
    beneficiary_incidental_or_structural,
    'Is the ''benefit'' incumbent typists receive from standard persistence properly classified as an incidental non-extractive network benefit, or does it constitute a structural rent once training industries and certification bodies built revenue models around the standard''s continuation?',
    'Trace revenue flows of typing-certification and vocational-training institutions tied specifically to QWERTY competency, and assess whether these institutions lobbied against alternative-layout adoption in any documented policy or curriculum-standard-setting venue.',
    'If certification-revenue lobbying is documented, part of this reading''s ''diffuse externality'' framing should migrate to a captured-rent framing, moving the classification toward tangled_rope for at least the training-institution seat.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_incidental_or_structural, conceptual, 'Whether the beneficiary declared here is genuinely incidental or has become a captured structural interest.').

omega_variable(
    efficiency_loss_magnitude,
    'How large is the actual typing-efficiency loss from QWERTY relative to well-studied alternative layouts, given contested and methodologically disputed efficiency studies (including the disputed Navy Dvorak studies)?',
    'Meta-analysis of rigorously controlled typing-speed studies across layouts, controlling for training time and typist selection effects.',
    'A larger, well-established efficiency loss would strengthen the case that the externality is significant enough to warrant coordinated intervention, without by itself establishing a beneficiary/extraction structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(efficiency_loss_magnitude, empirical, 'Magnitude of the diffuse efficiency externality claimed under this reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qwerty_persistence_inevitability__path_dependency_reading, 1873, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qwer_tr_t1873, qwerty_persistence_inevitability__path_dependency_reading, theater_ratio, 1873, 0.02).
narrative_ontology:measurement(qwer_tr_t1920, qwerty_persistence_inevitability__path_dependency_reading, theater_ratio, 1920, 0.03).
narrative_ontology:measurement(qwer_tr_t1960, qwerty_persistence_inevitability__path_dependency_reading, theater_ratio, 1960, 0.04).
narrative_ontology:measurement(qwer_tr_t1990, qwerty_persistence_inevitability__path_dependency_reading, theater_ratio, 1990, 0.04).
narrative_ontology:measurement(qwer_tr_t2010, qwerty_persistence_inevitability__path_dependency_reading, theater_ratio, 2010, 0.05).
narrative_ontology:measurement(qwer_tr_t2024, qwerty_persistence_inevitability__path_dependency_reading, theater_ratio, 2024, 0.05).

% Extraction over time
narrative_ontology:measurement(qwer_be_t1873, qwerty_persistence_inevitability__path_dependency_reading, base_extractiveness, 1873, 0.03).
narrative_ontology:measurement(qwer_be_t1920, qwerty_persistence_inevitability__path_dependency_reading, base_extractiveness, 1920, 0.05).
narrative_ontology:measurement(qwer_be_t1960, qwerty_persistence_inevitability__path_dependency_reading, base_extractiveness, 1960, 0.06).
narrative_ontology:measurement(qwer_be_t1990, qwerty_persistence_inevitability__path_dependency_reading, base_extractiveness, 1990, 0.07).
narrative_ontology:measurement(qwer_be_t2010, qwerty_persistence_inevitability__path_dependency_reading, base_extractiveness, 2010, 0.08).
narrative_ontology:measurement(qwer_be_t2024, qwerty_persistence_inevitability__path_dependency_reading, base_extractiveness, 2024, 0.08).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(qwerty_persistence_inevitability__path_dependency_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qwerty_persistence_inevitability__path_dependency_reading, information_standard).
narrative_ontology:boltzmann_floor_override(qwerty_persistence_inevitability__path_dependency_reading, 0.02).
narrative_ontology:affects_constraint(qwerty_persistence_inevitability__path_dependency_reading, qwerty_persistence_inevitability__strategic_lock_in_reading).

% DUAL FORMULATION NOTE:
% This story and qwerty_persistence_inevitability__strategic_lock_in_reading are sibling readings of the same kernel (qwerty_persistence_inevitability). This reading (path_dependency_reading) authors ε=0.08, claimed_type=mountain, no victim set, and beneficiaries limited to an incidental, non-extractive incumbent-population effect. The sibling reading authors a substantially higher ε, a real victim set (alternative-layout innovators, consumers paying an unrecognized efficiency tax), required active enforcement, and claimed_type=tangled_rope or snare depending on its own metrics. Both stories share the historical QWERTY substrate but diverge entirely on beneficiary/enforcement structure — per the ε-invariance principle, they are authored as two separate constraints rather than one story with a contested parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
