% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_function__survival_competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_function__survival_competence_reading, []).

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
 *   constraint_id: catastrophe_memory_function__survival_competence_reading
 *   human_readable: Passover as Transmitted Survival-Competence (D5 Reading)
 *   domain: religious_studies/ritual_theory/collective_memory
 *
 * SUMMARY:
 *   This story instantiates the survival_competence_reading (D5) of the
 *   catastrophe_memory_function kernel: Passover's seder is read as a
 *   mechanism for transmitting operational, adaptive institutional knowledge
 *   — how to reconstitute law, worship, and communal organization without
 *   centralized institutions — across generations and across dispersed,
 *   unpredictable host environments. This is deliberately narrower than the
 *   sibling hybrid_transformation_reading, which holds the ritual encodes
 *   BOTH mourning-practice and survival-competence, and structurally
 *   different from mourning_practice_reading, which holds the ritual's core
 *   function is memorial obligation and boundary maintenance rather than
 *   adaptive-capacity transmission. Under this reading alone, ε stays low and
 *   stable: the coordination function (competence transfer) is doing
 *   essentially all the work, with no identified victim class extracting cost
 *   from another party. The sibling readings are separate constraints, not
 *   alternate measurements of this one — per the ε-invariance principle, each
 *   carries its own ε and stakeholder set.
 *
 * KEY AGENTS:
 *   - household_ritual_leaders: transmission node (moderate/constrained) — administers the embodied procedure each cycle
 *   - successor_generations: primary recipients (powerless/constrained) — receive adaptive competence via repeated participation
 *   - diaspora_communities: distributed beneficiary network (organized/mobile) — use the decentralized script as portable coordination infrastructure
 *   - ritual_theorists: analytical observer (analytical/analytical) — assesses whether the transmission mechanism genuinely carries adaptive competence versus other functions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_function__survival_competence_reading, 0.22).
domain_priors:suppression_score(catastrophe_memory_function__survival_competence_reading, 0.28).
domain_priors:theater_ratio(catastrophe_memory_function__survival_competence_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_function__survival_competence_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(catastrophe_memory_function__survival_competence_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(catastrophe_memory_function__survival_competence_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_function__survival_competence_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(catastrophe_memory_function__survival_competence_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_function__survival_competence_reading, rope).
narrative_ontology:human_readable(catastrophe_memory_function__survival_competence_reading, "Passover as Transmitted Survival-Competence (D5 Reading)").
narrative_ontology:topic_domain(catastrophe_memory_function__survival_competence_reading, "religious_studies/ritual_theory/collective_memory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_function__survival_competence_reading, 'd9d99140-0d5f-4904-9059-4d63280d12f4').
narrative_ontology:cs_kernel_codification('d9d99140-0d5f-4904-9059-4d63280d12f4', fixed_text).
narrative_ontology:cs_authority_grounding('d9d99140-0d5f-4904-9059-4d63280d12f4', practice).
narrative_ontology:cs_interpretation_layer_present('d9d99140-0d5f-4904-9059-4d63280d12f4').
narrative_ontology:cs_reading_relation('d9d99140-0d5f-4904-9059-4d63280d12f4', catastrophe_memory_function__mourning_practice_reading, coexists_with).
narrative_ontology:cs_reading_relation('d9d99140-0d5f-4904-9059-4d63280d12f4', catastrophe_memory_function__hybrid_transformation_reading, influences).
narrative_ontology:cs_axiom('d9d99140-0d5f-4904-9059-4d63280d12f4', foundational, ritual_rehearsal_transmits_adaptive_institutional_capacity).
narrative_ontology:cs_axiom_status(ritual_rehearsal_transmits_adaptive_institutional_capacity, holdable).
narrative_ontology:cs_axiom_grounding('d9d99140-0d5f-4904-9059-4d63280d12f4', ritual_rehearsal_transmits_adaptive_institutional_capacity, empirically_contingent).
narrative_ontology:cs_axiom('d9d99140-0d5f-4904-9059-4d63280d12f4', secondary, decentralized_practice_survives_absence_of_central_institution).
narrative_ontology:cs_axiom_status(decentralized_practice_survives_absence_of_central_institution, holdable).
narrative_ontology:cs_axiom_grounding('d9d99140-0d5f-4904-9059-4d63280d12f4', decentralized_practice_survives_absence_of_central_institution, empirically_contingent).
narrative_ontology:cs_reference_frame('d9d99140-0d5f-4904-9059-4d63280d12f4', decentralized_post_temple_reconstitution).
narrative_ontology:cs_drift_state('d9d99140-0d5f-4904-9059-4d63280d12f4', contemporary_diaspora, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('d9d99140-0d5f-4904-9059-4d63280d12f4', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_function__survival_competence_reading, catastrophe_memory_function).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__survival_competence_reading, diaspora_communities).
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__survival_competence_reading, household_ritual_leaders).
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__survival_competence_reading, successor_generations).
narrative_ontology:constraint_vindicates(catastrophe_memory_function__survival_competence_reading, ritual_transmits_adaptive_institutional_knowledge).
narrative_ontology:constraint_vindicates(catastrophe_memory_function__survival_competence_reading, decentralized_practice_survives_centralized_collapse).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Convenes and narrates the seder each cycle, deciding how the script is adapted to present circumstances. Holds no centralized office — authority is distributed household by household — but functions as the transmission point where the embodied procedure (retelling, symbolic foods, structured questions) is actually passed to the next cohort. Exit from the role is possible but exit from the underlying knowledge transfer would mean the household stops being a node in the transmission network.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__survival_competence_reading, household_ritual_leaders, agenda_setter,
    moderate, generational, constrained, global).

% Children and newer members of the household receive, through repeated embodied participation, a rehearsed template for institutional discontinuity: how a people organizes worship, law, and identity without a temple, a land, or a central authority. They did not choose to receive this, but what they receive is capacity, not debt — an operating template usable if their own institutions collapse.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__survival_competence_reading, successor_generations, beneficiary,
    powerless, biographical, constrained, global).

% Dispersed communities across radically different host societies use the shared, decentralized ritual script as a coordination mechanism that requires no central institution to execute — any household anywhere can run the full procedure from memory and text. This is precisely the adaptive capacity the reading identifies: portable, replicable, requiring no temple or state to function.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__survival_competence_reading, diaspora_communities, beneficiary,
    organized, civilizational, mobile, global).

% Surrounding states and institutions observe the practice from outside; some historically restricted or persecuted it, but under this reading their reaction is not the constraint's function — they are not coordinated by it and do not pay into it. They appear as context, not as parties.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__survival_competence_reading, host_society_institutions, observer,
    institutional, generational, analytical, national).

% Scholars studying the transmission mechanism itself, assessing whether the embodied rehearsal genuinely functions to preserve adaptive institutional competence (this reading) as opposed to functioning primarily as mourning-practice or boundary-maintenance (the sibling readings). They do not participate in the ritual's operation; they assess its function from outside.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__survival_competence_reading, ritual_theorists, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The ritual solves a genuine transmission problem across generations and across geographically dispersed, institutionally unmoored communities: how to pass down a working model of decentralized survival — self-organized worship, law-keeping, and identity maintenance — without requiring any surviving central institution (temple, monarchy, unified polity) to carry it. Embodied repetition (retelling, structured question-and-answer, symbolic action) encodes this template more durably than written instruction alone.
% TRANSFER_FUNCTION: Under this reading, the primary transfer is competence, not cost: procedural and adaptive knowledge moves from the ritual leader and the accumulated community practice to each new participant. There is no identified party who pays into this transfer at another's expense — the household leader's labor is reciprocated by receiving the same competence from prior generations, and no group is structurally worse off for the ritual's operation.
% ABSENT_VOICES: Members who find the ritual's demands (dietary restriction, time commitment, narrative repetition) burdensome relative to what they personally value from it are not separately represented as a payer class under this reading; if their objection is that no competence is actually being transmitted to them, that objection belongs to the sibling readings (mourning-practice or hybrid) rather than to this one, since this reading's claim is specifically about the transmission of adaptive capacity.
% DISAPPEARANCE_RATIONALE: Under this reading's own logic, if the ritual vanished overnight, the world would rearrange: a demonstrated, portable mechanism for reconstituting communal, legal, and religious life without central institutions would be lost, and successor generations would face institutional collapse scenarios without a rehearsed template. The sibling readings would instead say little rearranges, or that a different function (mourning, boundary-marking) is what's actually lost — hence the verdict is contested across readings even though this reading itself claims world_rearranges.
% FOUNDING_PROBLEM: A dispersed people needed to preserve the operational capacity to reconstitute law, worship, and social organization after the loss of centralized institutions (temple, land, political sovereignty), and to do so repeatably across unpredictable future ruptures rather than relying on any single institution surviving.
% FOUNDING_PROBLEM_CORROBORATION: Historians of diaspora institutional adaptation (outside the practicing community) point to the empirical fact that dispersed communities repeatedly reconstituted functioning legal and communal structures in radically different host environments, which they read as evidence the transmitted template did real adaptive work. Ritual theorists holding the sibling mourning-practice or hybrid readings dispute that survival-competence is the operative mechanism, attributing continuity instead to boundary-maintenance or textual/legal scholarship independent of the ritual; this reading's proponents are themselves largely inside the practicing tradition, so the corroboration from historians outside the community is partial, not decisive.
narrative_ontology:disappearance_verdict(catastrophe_memory_function__survival_competence_reading, contested).
narrative_ontology:founding_problem_status(catastrophe_memory_function__survival_competence_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_function__survival_competence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(catastrophe_memory_function__survival_competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_function__survival_competence_reading, 0.22, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_function__survival_competence_reading_tests).
:- end_tests(catastrophe_memory_function__survival_competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.22) and essentially flat across the interval because, under this reading specifically, the ritual's core transaction is competence transfer with no structural payer — the household leader's labor in one generation is repaid by having received the same competence in the prior generation, and no coalition is coerced or diminished by the ritual's persistence. Suppression (0.28) and resistance (0.30) are both modest: participation is broadly voluntary and socially reinforced rather than coercively enforced, though social expectation exerts some real pull. Theater ratio drifts mildly upward (0.22 to 0.30) reflecting that as institutional threats to any given diaspora community recede in stable eras, a slightly larger share of the ritual's activity becomes performative reaffirmation rather than urgent competence-rehearsal — this is a modest, honestly-authored drift, not a claim that the ritual has become primarily theatrical.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (household_ritual_leaders) and the beneficiary seats (successor_generations, diaspora_communities) are not structurally opposed under this reading — there is no payer class whose seat would compute a different type from the beneficiary seats. The genuine perspectival gap in this kernel is ACROSS readings, not across seats within this one: the same seder, read as mourning_practice, would locate a different transfer (memorial obligation, boundary maintenance) and might locate different payer classes (those who bear costs of maintained group boundaries); read as hybrid_transformation, both functions run concurrently. Within this reading alone, the structure is closer to a rope than a tangled_rope precisely because no victim group is authored.
 *
 * DIRECTIONALITY LOGIC:
 *   All named beneficiaries — household_ritual_leaders, successor_generations, diaspora_communities — receive net positive value under this reading's own terms: competence, portability, and continuity capacity. No victims are declared because this reading's specific claim is about adaptive-capacity transmission, which by its own logic has no structural loser. Directionality for all three sits toward the beneficiary end (low d); host_society_institutions and ritual_theorists are observers with analytical exit, outside the directionality calculus entirely.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem (preserving reconstitutive institutional capacity against future rupture) remains contested as live versus dead: for communities that have not faced institutional collapse in generations, the competence being rehearsed could be read as a solved problem persisting as habit — but the founding_problem_corroboration notes that dispersed communities have, in fact, repeatedly needed and used exactly this kind of adaptive reconstitution historically, which argues against mandatrophy even where the immediate crisis is absent, because the whole point of the D5 function is capacity held in reserve for unpredictable future rupture, not continuous active crisis. The classification of rope (rather than scaffold) reflects that this competence-transmission function is not framed within the tradition as transitional — it is treated as a standing capacity, not a bridge to a different steady state.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    competence_vs_commemoration_boundary,
    'Is the embodied rehearsal in the seder actually functioning to transmit generalizable institutional-adaptation competence (this reading), or is the appearance of ''competence transmission'' itself a retrospective interpretive gloss on what is structurally closer to memorial obligation (the mourning_practice_reading)?',
    'Comparative ethnography of communities that maintain the ritual with varying degrees of institutional literacy transfer (e.g., whether participants who only attend without engagement in the underlying legal/communal-organization content still successfully reconstitute institutions after later disruption) would help separate the causal contribution of ritual rehearsal from other transmission channels (formal education, textual scholarship, oral family history).',
    'If competence transmission is shown to be largely illusory and the real function is memorial/boundary-maintenance, this reading''s claimed_type and low ε would not survive — the constraint would collapse into the mourning_practice_reading''s structure instead, which authors a different ε and different beneficiary/victim set.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competence_vs_commemoration_boundary, conceptual, 'Whether the D5 (survival-competence) function is genuinely separable from the D1/D4 (mourning/boundary) function, or is a reading imposed after the fact.').

omega_variable(
    reading_isolability,
    'Can the survival-competence function be meaningfully isolated as its own constraint, or does it only exist embedded within the combined function the hybrid_transformation_reading describes — i.e., is this reading a genuine standalone constraint or an artifact of decomposition?',
    'Historical cases where a community''s mourning/boundary practices atrophied or were suppressed while an adaptive-capacity-transmission practice persisted independently (or vice versa) would provide evidence the functions are separable in practice, not just analytically.',
    'If no such decoupled case exists, the appropriate single constraint may be the hybrid_transformation_reading, and this reading and mourning_practice_reading would both be partial abstractions rather than independently instantiable constraints — this would not change this story''s authored ε but would weaken the claim that it captures a free-standing structural reality.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_isolability, conceptual, 'Whether D5 is separable from D1/D4 in any observed case, or only in analytic decomposition.').

omega_variable(
    beneficiary_completeness,
    'Are there participants who experience the ritual''s demands as net cost under this reading''s own terms — e.g., members for whom the competence transmitted is never actually usable or relevant to their lived institutional context — who should be authored as a payer class rather than assumed away by declaring no victims?',
    'Longitudinal interview data with participants who left observant communities, asking specifically whether they experienced the ritual as imparting usable competence versus imparting only obligation without corresponding capacity.',
    'If a substantial cohort experiences the transmission as cost without corresponding competence gain, the constraint would need a victims array and would likely reclassify toward tangled_rope even within this reading''s own framing.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_completeness, empirical, 'Whether declaring zero victims for the D5 reading is fully warranted or reflects incomplete stakeholder identification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_function__survival_competence_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_function__survival_competence_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_function__survival_competence_reading, theater_ratio, 20, 0.24).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_function__survival_competence_reading, theater_ratio, 40, 0.26).
narrative_ontology:measurement(cata_tr_t60, catastrophe_memory_function__survival_competence_reading, theater_ratio, 60, 0.28).
narrative_ontology:measurement(cata_tr_t80, catastrophe_memory_function__survival_competence_reading, theater_ratio, 80, 0.29).
narrative_ontology:measurement(cata_tr_t100, catastrophe_memory_function__survival_competence_reading, theater_ratio, 100, 0.3).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_function__survival_competence_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_function__survival_competence_reading, base_extractiveness, 20, 0.19).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_function__survival_competence_reading, base_extractiveness, 40, 0.2).
narrative_ontology:measurement(cata_be_t60, catastrophe_memory_function__survival_competence_reading, base_extractiveness, 60, 0.21).
narrative_ontology:measurement(cata_be_t80, catastrophe_memory_function__survival_competence_reading, base_extractiveness, 80, 0.21).
narrative_ontology:measurement(cata_be_t100, catastrophe_memory_function__survival_competence_reading, base_extractiveness, 100, 0.22).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(catastrophe_memory_function__survival_competence_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_function__survival_competence_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(catastrophe_memory_function__survival_competence_reading, 0.1).
narrative_ontology:affects_constraint(catastrophe_memory_function__survival_competence_reading, catastrophe_memory_function__mourning_practice_reading).
narrative_ontology:affects_constraint(catastrophe_memory_function__survival_competence_reading, catastrophe_memory_function__hybrid_transformation_reading).

% DUAL FORMULATION NOTE:
% Three sibling constraints decompose the colloquial 'Passover as memory-function' claim: mourning_practice_reading (D1/D4, memorial obligation and boundary maintenance), survival_competence_reading (D5, this story — adaptive institutional-competence transmission), and hybrid_transformation_reading (D1/D4+D5 combined). Each carries its own ε per the ε-invariance principle; this story's low, stable ε (0.22) reflects that under this reading alone the ritual's operation shows no identified victim class, in contrast to a mourning-practice reading which might locate boundary-maintenance costs on those who exit the tradition, or a hybrid reading which would need to reconcile both ε profiles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
