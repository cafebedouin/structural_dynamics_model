% ============================================================================
% CONSTRAINT STORY: honor_settlement_legitimacy__drop_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_settlement_legitimacy__drop_reading, []).

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
 *   constraint_id: honor_settlement_legitimacy__drop_reading
 *   human_readable: Persistence of Dueling in Fringe Honor Cultures (Drop Reading)
 *   domain: historical_sociology/legal_history/cultural_anthropology
 *
 * SUMMARY:
 *   This constraint represents the 'drop reading' of the honor settlement
 *   legitimacy kernel, focusing on the persistence of dueling as a fringe
 *   practice within specific, identity-locked honor cultures, despite its
 *   formal legal proscription. It highlights how dueling, while suppressed,
 *   was not entirely eliminated from the normative repertoire of these
 *   groups, maintaining a low-level, actively enforced, and moderately
 *   extractive function for its adherents. The claimed type is Tangled Rope,
 *   reflecting both a coordination function (dispute resolution within the
 *   honor code) and asymmetric extraction (physical risk to participants,
 *   social costs to families, and erosion of state legal authority).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_settlement_legitimacy__drop_reading, 0.4).
domain_priors:suppression_score(honor_settlement_legitimacy__drop_reading, 0.7).
domain_priors:theater_ratio(honor_settlement_legitimacy__drop_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_settlement_legitimacy__drop_reading, extractiveness, 0.4).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__drop_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__drop_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_settlement_legitimacy__drop_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__drop_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_settlement_legitimacy__drop_reading, tangled_rope).
narrative_ontology:human_readable(honor_settlement_legitimacy__drop_reading, "Persistence of Dueling in Fringe Honor Cultures (Drop Reading)").
narrative_ontology:topic_domain(honor_settlement_legitimacy__drop_reading, "historical_sociology/legal_history/cultural_anthropology").

domain_priors:requires_active_enforcement(honor_settlement_legitimacy__drop_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_settlement_legitimacy__drop_reading, '6ead6fa7-aabd-48b6-942a-f8073ff98192').
narrative_ontology:cs_kernel_codification('6ead6fa7-aabd-48b6-942a-f8073ff98192', implicit).
narrative_ontology:cs_authority_grounding('6ead6fa7-aabd-48b6-942a-f8073ff98192', practice).
narrative_ontology:cs_interpretation_layer_present('6ead6fa7-aabd-48b6-942a-f8073ff98192').
narrative_ontology:cs_reading_relation('6ead6fa7-aabd-48b6-942a-f8073ff98192', honor_settlement_legitimacy__contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('6ead6fa7-aabd-48b6-942a-f8073ff98192', honor_settlement_legitimacy__composite_reading, coexists_with).
narrative_ontology:cs_axiom('6ead6fa7-aabd-48b6-942a-f8073ff98192', foundational, honor_demands_direct_satisfaction).
narrative_ontology:cs_axiom_status(honor_demands_direct_satisfaction, holdable).
narrative_ontology:cs_axiom_grounding('6ead6fa7-aabd-48b6-942a-f8073ff98192', honor_demands_direct_satisfaction, conventional).
narrative_ontology:cs_axiom('6ead6fa7-aabd-48b6-942a-f8073ff98192', foundational, state_law_insufficient_for_honor).
narrative_ontology:cs_axiom_status(state_law_insufficient_for_honor, holdable).
narrative_ontology:cs_axiom_grounding('6ead6fa7-aabd-48b6-942a-f8073ff98192', state_law_insufficient_for_honor, conventional).
narrative_ontology:cs_reference_frame('6ead6fa7-aabd-48b6-942a-f8073ff98192', persistent_honor_code).
narrative_ontology:cs_drift_state('6ead6fa7-aabd-48b6-942a-f8073ff98192', mid_20th_century, gap(stable, minor, true)).
narrative_ontology:cs_created_at('6ead6fa7-aabd-48b6-942a-f8073ff98192', '').
narrative_ontology:cs_kernel_id(honor_settlement_legitimacy__drop_reading, honor_settlement_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_settlement_legitimacy__drop_reading, residual_honor_adherents).
narrative_ontology:constraint_beneficiary(honor_settlement_legitimacy__drop_reading, local_authorities_tolerating_duels).
narrative_ontology:constraint_victim(honor_settlement_legitimacy__drop_reading, duel_participants).
narrative_ontology:constraint_victim(honor_settlement_legitimacy__drop_reading, families_of_duelists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals who maintain a strong personal and social identity tied to traditional honor codes, for whom dueling, though legally proscribed, remains a legitimate means of dispute resolution or status defense within their specific social niche. They benefit from the perceived restoration of honor.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__drop_reading, residual_honor_adherents, beneficiary,
    moderate, biographical, identity_locked, local).

% Individuals compelled by honor codes to participate in duels, facing physical harm, death, or legal consequences. Their participation is often a last resort to avoid social ostracism or perceived dishonor, making their exit options severely constrained.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__drop_reading, duel_participants, payer,
    powerless, immediate, trapped, local).

% Local law enforcement or judicial figures who, due to cultural pressures, political expediency, or resource limitations, tacitly allow dueling to persist as a 'private' matter within certain communities, often turning a blind eye to its occurrence or imposing minimal penalties. They benefit from maintaining social order without direct confrontation.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__drop_reading, local_authorities_tolerating_duels, agenda_setter,
    institutional, biographical, constrained, local).

% Bear the social stigma, emotional trauma, and economic costs associated with dueling, including loss of life or legal repercussions. Their identity is often tied to the community's honor culture, making it difficult to exit the system or challenge its norms.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__drop_reading, families_of_duelists, payer,
    powerless, generational, identity_locked, local).

% The overarching legal framework that formally prohibits dueling and views it as a criminal act. It struggles to enforce its prohibition uniformly in areas where local honor cultures persist, leading to a gap between de jure and de facto legality.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__drop_reading, national_legal_system, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Within specific social niches, it provides a recognized, albeit illegal, mechanism for individuals to settle disputes, restore perceived honor, and maintain social standing according to traditional codes, preventing other forms of violence or social breakdown within that specific cultural context.
% TRANSFER_FUNCTION: Transfers the burden of dispute resolution and honor defense onto individuals through physical risk, and transfers social legitimacy to traditional honor codes at the expense of formal legal authority in specific contexts.
% ABSENT_VOICES: Victims of dueling (those killed or injured) and their immediate families, who often lack the power or social standing to challenge the honor code directly. Also, proponents of universal legal equality and state monopoly on violence, whose perspectives are marginalized in these specific cultural enclaves.
% DISAPPEARANCE_RATIONALE: If the cultural acceptance and tacit tolerance of dueling vanished overnight, these fringe honor cultures would face a crisis in how they manage disputes and maintain social status. Individuals would either be forced into the formal legal system, or new, potentially more violent, informal mechanisms would emerge to fill the void, fundamentally altering the social fabric of these communities.
% FOUNDING_PROBLEM: The need for individuals to defend their personal and family honor against perceived slights or insults in a society where formal legal recourse was insufficient or culturally inappropriate for such matters.
% FOUNDING_PROBLEM_CORROBORATION: Adherents of residual honor cultures attest that the problem of honor defense remains live, as formal legal systems often fail to address the deeply personal and social dimensions of honor. Anthropological studies and historical accounts from outside the benefiting parties corroborate the persistence of these honor codes and the perceived inadequacy of external legal frameworks for their adherents.
narrative_ontology:disappearance_verdict(honor_settlement_legitimacy__drop_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_settlement_legitimacy__drop_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_settlement_legitimacy__drop_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(honor_settlement_legitimacy__drop_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_settlement_legitimacy__drop_reading, 0.4, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_settlement_legitimacy__drop_reading_tests).
:- end_tests(honor_settlement_legitimacy__drop_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.4) because while dueling carries high individual risk, its overall social cost within these fringe groups is contained compared to its historical peak. Suppression is high (0.7) because the state actively, though often ineffectively, tries to suppress dueling, and the honor culture itself suppresses alternatives to dueling for its adherents. Theater ratio is low (0.2) as the practice, while fringe, is still functionally meaningful for its participants, not merely performative. Accessibility collapse is moderate (0.6) as formal legal alternatives exist but are culturally inaccessible or unacceptable to honor adherents. Resistance is low (0.3) from within the honor culture, as the practice is upheld by its beneficiaries, but high from the national legal system (not captured in this metric, which focuses on internal resistance).
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of residual honor adherents, dueling is a necessary, albeit risky, mechanism for maintaining social order and personal dignity within their cultural framework. From the perspective of the national legal system, it is an illegal and anachronistic practice that undermines the state's monopoly on violence. The engine's classification will reflect this divergence based on the structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Residual honor adherents benefit from the perceived restoration of honor and maintenance of their cultural identity (low d). Duel participants and their families bear the direct costs and risks (high d). Local authorities, by tacitly tolerating dueling, benefit from avoiding direct conflict with entrenched cultural norms (low d). The national legal system is an observer, attempting to suppress the practice but not directly benefiting or paying from its persistence.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (honor defense) is still 'live' for its adherents, preventing a full mandatrophy classification. However, the 'contested' status of the founding problem, coupled with the persistence of the practice despite legal proscription, suggests a form of cultural inertia where the constraint continues to operate in a niche, even as its broader societal justification has eroded. The classification as Tangled Rope captures this hybrid state, where a coordination function persists for a specific group, but at an extractive cost maintained by active enforcement and identity lock-in.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cultural_vs_legal_legitimacy,
    'To what extent does the cultural legitimacy of dueling within these fringe groups override or coexist with its legal illegitimacy, and what are the mechanisms of this interaction?',
    'Detailed ethnographic studies and legal case analyses of specific dueling incidents, examining the outcomes for participants and the responses of local vs. national authorities.',
    'If cultural legitimacy strongly overrides legal illegitimacy, the constraint''s effective suppression from the state is lower than measured, and its persistence is more deeply rooted in identity. If they coexist in tension, the constraint is more fragile and susceptible to external pressure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cultural_vs_legal_legitimacy, empirical, 'Ambiguity in the interplay between cultural acceptance and legal prohibition.').

omega_variable(
    identity_lock_strength,
    'How strong is the ''identity_locked'' exit option for residual honor adherents and their families? Is it a genuine internal commitment or a social pressure that could be overcome with sufficient external support?',
    'Longitudinal studies of individuals who successfully exited such honor cultures, identifying the catalysts and support structures that enabled their departure.',
    'If identity lock is primarily internal, the constraint is more resilient to external suppression. If it''s primarily external social pressure, targeted interventions could significantly reduce its persistence and extractiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_strength, empirical, 'The nature and strength of identity-based constraint on participants.').

omega_variable(
    reading_framing_divergence,
    'Is the ''drop_reading'' a distinct structural claim, or is it a specific manifestation of the ''composite_reading'' where some mechanisms of decline were less effective in certain niches?',
    'Comparative historical analysis across multiple honor cultures, testing whether the ''drop'' is a unique pattern or a regional variation of broader trends.',
    'If a distinct structural claim, it reinforces the idea that honor culture can persist as a live option. If a variation of the composite, it suggests the overall decline mechanisms were more pervasive, with local exceptions.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_framing_divergence, conceptual, 'Whether the ''drop_reading'' represents a fundamentally different structural pattern or a localized variant of other decline narratives.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_settlement_legitimacy__drop_reading, 1850, 1950).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t1850, honor_settlement_legitimacy__drop_reading, theater_ratio, 1850, 0.15).
narrative_ontology:measurement(hono_tr_t1875, honor_settlement_legitimacy__drop_reading, theater_ratio, 1875, 0.18).
narrative_ontology:measurement(hono_tr_t1900, honor_settlement_legitimacy__drop_reading, theater_ratio, 1900, 0.2).
narrative_ontology:measurement(hono_tr_t1925, honor_settlement_legitimacy__drop_reading, theater_ratio, 1925, 0.2).
narrative_ontology:measurement(hono_tr_t1950, honor_settlement_legitimacy__drop_reading, theater_ratio, 1950, 0.2).

% Extraction over time
narrative_ontology:measurement(hono_be_t1850, honor_settlement_legitimacy__drop_reading, base_extractiveness, 1850, 0.45).
narrative_ontology:measurement(hono_be_t1875, honor_settlement_legitimacy__drop_reading, base_extractiveness, 1875, 0.4).
narrative_ontology:measurement(hono_be_t1900, honor_settlement_legitimacy__drop_reading, base_extractiveness, 1900, 0.38).
narrative_ontology:measurement(hono_be_t1925, honor_settlement_legitimacy__drop_reading, base_extractiveness, 1925, 0.39).
narrative_ontology:measurement(hono_be_t1950, honor_settlement_legitimacy__drop_reading, base_extractiveness, 1950, 0.4).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t1850, honor_settlement_legitimacy__drop_reading, suppression_requirement, 1850, 0.65).
narrative_ontology:measurement(hono_su_t1875, honor_settlement_legitimacy__drop_reading, suppression_requirement, 1875, 0.68).
narrative_ontology:measurement(hono_su_t1900, honor_settlement_legitimacy__drop_reading, suppression_requirement, 1900, 0.7).
narrative_ontology:measurement(hono_su_t1925, honor_settlement_legitimacy__drop_reading, suppression_requirement, 1925, 0.7).
narrative_ontology:measurement(hono_su_t1950, honor_settlement_legitimacy__drop_reading, suppression_requirement, 1950, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_settlement_legitimacy__drop_reading, identity_coordination).
narrative_ontology:affects_constraint(honor_settlement_legitimacy__drop_reading, honor_settlement_legitimacy__contraction_reading).
narrative_ontology:affects_constraint(honor_settlement_legitimacy__drop_reading, honor_settlement_legitimacy__composite_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'honor_settlement_legitimacy' kernel. The 'drop_reading' emphasizes the persistence of dueling in fringe honor cultures, contrasting with the 'contraction_reading' (cognitive unthinkability) and 'composite_reading' (overdetermined decline). All three are linked to capture the full complexity of the kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
