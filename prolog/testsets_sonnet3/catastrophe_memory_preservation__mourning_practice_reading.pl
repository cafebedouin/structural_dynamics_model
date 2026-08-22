% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_preservation__mourning_practice_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-14
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_preservation__mourning_practice_reading, []).

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
 *   constraint_id: catastrophe_memory_preservation__mourning_practice_reading
 *   human_readable: Catastrophe Commemoration Ritual as Symbolic Continuity Practice
 *   domain: religious_studies/collective_memory
 *
 * SUMMARY:
 *   This story authors ONE reading of the catastrophe_memory_preservation
 *   kernel: the mourning_practice reading, which holds that ritual
 *   commemoration of a historical catastrophe functions as symbolic
 *   continuity and identity-work, not as a channel for transmitting
 *   operational threat-recognition competence. Under this reading, the
 *   ritual's coordination function is real (synchronizing collective memory
 *   and belonging across a dispersed, generationally-renewing group) but its
 *   extraction is low, its suppression near-absent, and participation is
 *   opt-in with no material or protective stakes riding on compliance. This
 *   is a rope: coordination without meaningful coercion. The sibling readings
 *   — survival_competence_reading (ritual preserves operational capacity) and
 *   hybrid_atrophy_reading (ritual once did, but has degraded to this state)
 *   — are separate constraints with their own ε and structural data; they are
 *   not blended into this one.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_preservation__mourning_practice_reading, 0.22).
domain_priors:suppression_score(catastrophe_memory_preservation__mourning_practice_reading, 0.12).
domain_priors:theater_ratio(catastrophe_memory_preservation__mourning_practice_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_preservation__mourning_practice_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__mourning_practice_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__mourning_practice_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_preservation__mourning_practice_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__mourning_practice_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_preservation__mourning_practice_reading, rope).
narrative_ontology:human_readable(catastrophe_memory_preservation__mourning_practice_reading, "Catastrophe Commemoration Ritual as Symbolic Continuity Practice").
narrative_ontology:topic_domain(catastrophe_memory_preservation__mourning_practice_reading, "religious_studies/collective_memory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_preservation__mourning_practice_reading, 'f46b0d3d-1702-4ed3-a86f-c14e2ce6c680').
narrative_ontology:cs_kernel_codification('f46b0d3d-1702-4ed3-a86f-c14e2ce6c680', distributed).
narrative_ontology:cs_authority_grounding('f46b0d3d-1702-4ed3-a86f-c14e2ce6c680', practice).
narrative_ontology:cs_interpretation_layer_present('f46b0d3d-1702-4ed3-a86f-c14e2ce6c680').
narrative_ontology:cs_reading_relation('f46b0d3d-1702-4ed3-a86f-c14e2ce6c680', catastrophe_memory_preservation__survival_competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('f46b0d3d-1702-4ed3-a86f-c14e2ce6c680', catastrophe_memory_preservation__hybrid_atrophy_reading, influences).
narrative_ontology:cs_axiom('f46b0d3d-1702-4ed3-a86f-c14e2ce6c680', foundational, ritual_function_is_symbolic_not_operational).
narrative_ontology:cs_axiom_status(ritual_function_is_symbolic_not_operational, holdable).
narrative_ontology:cs_axiom_grounding('f46b0d3d-1702-4ed3-a86f-c14e2ce6c680', ritual_function_is_symbolic_not_operational, empirically_contingent).
narrative_ontology:cs_axiom('f46b0d3d-1702-4ed3-a86f-c14e2ce6c680', secondary, symbolic_continuity_was_original_not_residual).
narrative_ontology:cs_axiom_status(symbolic_continuity_was_original_not_residual, holdable).
narrative_ontology:cs_axiom_grounding('f46b0d3d-1702-4ed3-a86f-c14e2ce6c680', symbolic_continuity_was_original_not_residual, conventional).
narrative_ontology:cs_reference_frame('f46b0d3d-1702-4ed3-a86f-c14e2ce6c680', identity_continuity_through_shared_narrative).
narrative_ontology:cs_drift_state('f46b0d3d-1702-4ed3-a86f-c14e2ce6c680', contemporary_diaspora_practice, gap(stable, minor, true)).
narrative_ontology:cs_created_at('f46b0d3d-1702-4ed3-a86f-c14e2ce6c680', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_preservation__mourning_practice_reading, catastrophe_memory_preservation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_preservation__mourning_practice_reading, commemorating_community).
narrative_ontology:constraint_beneficiary(catastrophe_memory_preservation__mourning_practice_reading, communal_identity_institutions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(catastrophe_memory_preservation__mourning_practice_reading, younger_generation_members).
narrative_ontology:constraint_vindicates(catastrophe_memory_preservation__mourning_practice_reading, collective_identity_persistence_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gathers annually (or at prescribed intervals) to observe the ritual marking a historical catastrophe — fasting, recitation, communal assembly, symbolic reenactment. Participants are not extracting practical survival information from the observance; they are renewing a shared narrative that binds the group's members to one another and to their forebears. Attendance is voluntary and social cost of non-attendance is low to moderate — mostly a matter of feeling less connected, not material loss.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__mourning_practice_reading, commemorating_community, beneficiary,
    organized, generational, mobile, national).

% Religious congregations, cultural associations, and diaspora organizations schedule, liturgically structure, and transmit the ritual calendar. They benefit from the cohesion the ritual generates (membership, continuity of transmission, social capital) but exercise essentially no coercive enforcement — members who stop attending face social distance, not sanction, exile, or material penalty.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__mourning_practice_reading, communal_identity_institutions, agenda_setter,
    institutional, civilizational, mobile, national).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_preservation__mourning_practice_reading, communal_identity_institutions, beneficiary).

% Inherit the ritual as a marker of belonging rather than as instruction in operational threat-response. Many participate for identity and family reasons even when they find the historical referent distant; some drift away entirely without practical consequence, and their absence changes the ritual's texture but not their access to resources, protection, or opportunity.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__mourning_practice_reading, younger_generation_members, beneficiary,
    moderate, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_preservation__mourning_practice_reading, younger_generation_members, excluded).

% Scholars of religion and collective memory studying whether the ritual functions as symbolic identity-work or as latent operational transmission. They document participation patterns, generational drift, and the content of the ritual's instructional versus commemorative elements.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__mourning_practice_reading, outside_observers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_preservation__mourning_practice_reading, diffuse).
narrative_ontology:fixing_cost_class(catastrophe_memory_preservation__mourning_practice_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a recurring, low-cost occasion for a dispersed group to reaffirm shared identity and historical narrative, synchronizing memory and belonging across generations without requiring any operational skill transfer.
% TRANSFER_FUNCTION: Moves symbolic capital (shared narrative, felt belonging, intergenerational continuity) from the practice of observance to participants; does not move material resources, threat-recognition competence, or protective capability from anyone to anyone.
% ABSENT_VOICES: Descendants who have fully assimilated into host cultures and no longer observe the ritual are not represented in the commemorating community's self-account; their silence is read by the community as loss rather than as a dissenting position, so their reasons for exit go largely undocumented.
% DISAPPEARANCE_RATIONALE: Communal identity institutions would say the world rearranges significantly — a channel of intergenerational cohesion and historical memory disappears, weakening diaspora or in-group solidarity. Assimilated descendants and outside observers might say the world is largely unchanged in practical terms, since the ritual under this reading carries no operational function whose loss would be materially felt. The verdict is contested precisely because it turns on whether symbolic cohesion counts as a real-world dependency.
% FOUNDING_PROBLEM: A historical catastrophe threatened to fracture the group's continuity; the ritual was instituted (or evolved) to ensure the event and the identity it forged would not be forgotten as the generation with direct memory passed.
% FOUNDING_PROBLEM_CORROBORATION: Communal identity institutions and elder generation members attest the founding problem — intergenerational forgetting — remains live and the ritual actively addresses it. Outside scholars of collective memory partially corroborate this (documented continuity effects in diaspora studies) but also note that the operational content of the original catastrophe response, if any existed, is not being transmitted through this practice; no fully outside, non-participant attestation of the ritual's necessity exists beyond academic description.
narrative_ontology:disappearance_verdict(catastrophe_memory_preservation__mourning_practice_reading, contested).
narrative_ontology:founding_problem_status(catastrophe_memory_preservation__mourning_practice_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_preservation__mourning_practice_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(catastrophe_memory_preservation__mourning_practice_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_preservation__mourning_practice_reading, 0.22, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_preservation__mourning_practice_reading_tests).
:- end_tests(catastrophe_memory_preservation__mourning_practice_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.22 at interval end, rising only slightly) because under this reading nothing material is extracted from participants — no protective capability, no resource transfer, no threat-relevant instruction is being withheld or monetized. Suppression is authored near-zero (0.12) because exit is genuinely low-cost: no sanction beyond mild social distance attaches to non-participation. Theater ratio is moderate and rising (0.28 to 0.40) because this reading holds that most of the ritual's content IS performative/symbolic by design — that is not decay under this reading, it is the ritual functioning as intended, so a moderate-to-elevated theater ratio is consistent with a healthy rope rather than a red flag. Accessibility collapse is low (0.25): alternative ways of maintaining group identity (secular commemoration, informal storytelling, digital archives) remain visibly available and are not suppressed.
 *
 * DIRECTIONALITY LOGIC:
 *   The commemorating community and communal identity institutions are the clear beneficiaries — they gain cohesion, continuity, and social capital from the practice, with no identifiable victim class under this reading (participation is opt-in and non-participation carries no penalty beyond softened belonging). Younger generation members occupy a dual position: beneficiaries when they participate, effectively self-excluding when they don't, but in neither case do they pay a structural cost — this is why no victims array is authored. Directionality sits close to the symmetric-to-beneficiary end for all engaged parties.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resists mislabeling the ritual as pure extraction or pure theater precisely by keeping the coordination function (identity continuity) and the absence of coercion both in view simultaneously — a naive read might see the rising theater_ratio and conclude decay, but under this reading's own lights a high theater share is the successful operation of a symbolic-continuity mechanism, not a symptom of atrophy. That degeneration narrative belongs to the sibling hybrid_atrophy_reading, not here.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    operational_content_residue,
    'Does the ritual, despite this reading''s claim of pure symbolic function, still transmit any residual operational threat-recognition content (e.g., embedded warnings, precautionary practices coded into ritual form) that participants absorb without recognizing it as such?',
    'Ethnographic content analysis of ritual liturgy/practice compared against documented historical catastrophe-response behaviors; interviews with practitioners about perceived practical takeaways.',
    'If residual operational content is found, this reading''s claim of zero operational transfer weakens and the constraint drifts toward the survival_competence_reading or hybrid_atrophy_reading''s territory — potentially requiring reclassification or an explicit hybrid story.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(operational_content_residue, empirical, 'Whether claimed pure-symbolic ritual content secretly carries operational residue.').

omega_variable(
    reading_choice_location,
    'Is the mourning_practice_reading the correct framing for THIS specific ritual tradition, or does the evidence better fit hybrid_atrophy (a degraded survival function) — and where exactly does the disagreement between the three kernel readings actually locate itself?',
    'Comparative historical analysis of the ritual''s earliest documented form against its current form: if early forms contained explicit operational instruction (evacuation routes, warning signs, resource caching practices) that have since dropped out, hybrid_atrophy is better supported; if the ritual was symbolic from its earliest attested form, mourning_practice is better supported.',
    'Selecting the wrong reading for this specific tradition misattributes ε and beneficiary structure — a genuinely atrophied survival mechanism misclassified as originally-symbolic would understate the loss represented by the atrophy.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_choice_location, conceptual, 'Where the three-way kernel disagreement is actually located for this specific ritual case, and whether this reading was correctly selected.').

omega_variable(
    assimilated_descendant_silence,
    'Is the low resistance/low accessibility-collapse profile genuinely reflective of costless exit, or does it undercount descendants who exited under earlier, more coercive versions of the norm and whose absence is now invisible to the current community''s self-report?',
    'Longitudinal or oral-history tracing of families who ceased observance across generations, examining whether their exit was low-cost at the time or costly-then-normalized-now.',
    'If historical exit was costlier than current observation suggests, the rope classification may understate historical suppression even while accurately describing the present low-coercion state.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(assimilated_descendant_silence, empirical, 'Whether currently-low suppression conceals a historically higher-suppression exit cost now obscured by generational distance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_preservation__mourning_practice_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_preservation__mourning_practice_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(cata_tr_t8, catastrophe_memory_preservation__mourning_practice_reading, theater_ratio, 8, 0.31).
narrative_ontology:measurement(cata_tr_t16, catastrophe_memory_preservation__mourning_practice_reading, theater_ratio, 16, 0.34).
narrative_ontology:measurement(cata_tr_t24, catastrophe_memory_preservation__mourning_practice_reading, theater_ratio, 24, 0.36).
narrative_ontology:measurement(cata_tr_t32, catastrophe_memory_preservation__mourning_practice_reading, theater_ratio, 32, 0.38).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_preservation__mourning_practice_reading, theater_ratio, 40, 0.4).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_preservation__mourning_practice_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(cata_be_t8, catastrophe_memory_preservation__mourning_practice_reading, base_extractiveness, 8, 0.17).
narrative_ontology:measurement(cata_be_t16, catastrophe_memory_preservation__mourning_practice_reading, base_extractiveness, 16, 0.19).
narrative_ontology:measurement(cata_be_t24, catastrophe_memory_preservation__mourning_practice_reading, base_extractiveness, 24, 0.2).
narrative_ontology:measurement(cata_be_t32, catastrophe_memory_preservation__mourning_practice_reading, base_extractiveness, 32, 0.21).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_preservation__mourning_practice_reading, base_extractiveness, 40, 0.22).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(catastrophe_memory_preservation__mourning_practice_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_preservation__mourning_practice_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(catastrophe_memory_preservation__mourning_practice_reading, 0.1).
narrative_ontology:affects_constraint(catastrophe_memory_preservation__mourning_practice_reading, catastrophe_memory_preservation__survival_competence_reading).
narrative_ontology:affects_constraint(catastrophe_memory_preservation__mourning_practice_reading, catastrophe_memory_preservation__hybrid_atrophy_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraints decomposed from the natural-language concept 'ritual commemoration of catastrophe' under the ε-invariance principle. mourning_practice_reading (this file) authors low ε and rope classification on the premise that the ritual's function is purely symbolic-identity work. survival_competence_reading authors a different ε on the premise that operational threat-recognition capacity is genuinely transmitted. hybrid_atrophy_reading authors a temporally-drifting ε on the premise that the ritual began as survival_competence and decayed into mourning_practice. The three are linked via affects_constraints rather than merged, per DP-001 ε-invariance: each has a stable, single ε from its own reading's lights.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
