% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_preservation__survival_competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_preservation__survival_competence_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: catastrophe_memory_preservation__survival_competence_reading
 *   human_readable: Ritual Preservation of Survival Competence Across Generations
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   Religious and cultural communities facing recurring catastrophic threats
 *   (siege, famine, plague, persecution) develop ritual practices that
 *   commemorate historical disasters and rehearse collective response. This
 *   constraint models the operational reading: ritual preserves
 *   threat-recognition competence across generations so that when catastrophe
 *   recurs, the community can recognize early signs and coordinate mutual aid
 *   without centralized authority. The practice demands substantial
 *   participation from the present generation—time, emotional labor,
 *   opportunity costs—yet the benefits accrue primarily to future generations
 *   who will inherit the embodied knowledge. This reading claims the ritual
 *   functions as a tangled_rope: it coordinates genuine survival capacity
 *   (the coordination benefit that justifies enforcement) but extracts
 *   present-generation autonomy and labor (the victim cost that requires
 *   active suppression of exit). Alternative readings of the same kernel
 *   exist: a mourning_practice_reading argues the ritual primarily preserves
 *   symbolic continuity and group identity, not operational capacity; a
 *   hybrid_atrophy_reading argues the ritual once served survival function
 *   but has degraded to theater under modernity while enforcement persists
 *   theatrically. This constraint instantiates ONLY the
 *   survival_competence_reading.
 *
 * KEY AGENTS:
 *   - present_generation_participants: Enact and carry the ritual; bear time, emotional, and opportunity costs; locked into participation by identity fusion with the community.
 *   - ritual_authorities: Set and enforce the protocol; benefit from sustained institutional authority over meaning-making and community cohesion.
 *   - future_generations: Inherit embodied threat-recognition knowledge; cannot bargain with the constraint; are locked into receiving whatever practice is transmitted.
 *   - competing_memorialization_systems: Excluded alternative channels (secular training, written protocols, institutional disaster preparedness) that could transfer threat-recognition with lower present-generation cost.
 *   - modern_institutional_authority: Observe and measure whether ritual preservation produces better outcomes than formal institutional preparation.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_preservation__survival_competence_reading, 0.76).
domain_priors:suppression_score(catastrophe_memory_preservation__survival_competence_reading, 0.62).
domain_priors:theater_ratio(catastrophe_memory_preservation__survival_competence_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_preservation__survival_competence_reading, extractiveness, 0.76).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__survival_competence_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__survival_competence_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_preservation__survival_competence_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__survival_competence_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_preservation__survival_competence_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_preservation__survival_competence_reading, "Ritual Preservation of Survival Competence Across Generations").
narrative_ontology:topic_domain(catastrophe_memory_preservation__survival_competence_reading, "religious_studies/collective_memory/ritual_practice").

domain_priors:requires_active_enforcement(catastrophe_memory_preservation__survival_competence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_preservation__survival_competence_reading, '8cc12dc2-0cb7-4f7e-ab86-4e573615a1f9').
narrative_ontology:cs_kernel_codification('8cc12dc2-0cb7-4f7e-ab86-4e573615a1f9', fixed_text).
narrative_ontology:cs_authority_grounding('8cc12dc2-0cb7-4f7e-ab86-4e573615a1f9', lineage).
narrative_ontology:cs_interpretation_layer_present('8cc12dc2-0cb7-4f7e-ab86-4e573615a1f9').
narrative_ontology:cs_reading_relation('8cc12dc2-0cb7-4f7e-ab86-4e573615a1f9', catastrophe_memory_preservation__mourning_practice_reading, coexists_with).
narrative_ontology:cs_reading_relation('8cc12dc2-0cb7-4f7e-ab86-4e573615a1f9', catastrophe_memory_preservation__hybrid_atrophy_reading, influences).
narrative_ontology:cs_axiom('8cc12dc2-0cb7-4f7e-ab86-4e573615a1f9', foundational, embodied_practice_preserves_operational_knowledge).
narrative_ontology:cs_axiom_status(embodied_practice_preserves_operational_knowledge, holdable).
narrative_ontology:cs_axiom_grounding('8cc12dc2-0cb7-4f7e-ab86-4e573615a1f9', embodied_practice_preserves_operational_knowledge, empirically_contingent).
narrative_ontology:cs_axiom('8cc12dc2-0cb7-4f7e-ab86-4e573615a1f9', foundational, future_survival_benefit_justifies_present_generation_cost).
narrative_ontology:cs_axiom_status(future_survival_benefit_justifies_present_generation_cost, holdable).
narrative_ontology:cs_axiom_grounding('8cc12dc2-0cb7-4f7e-ab86-4e573615a1f9', future_survival_benefit_justifies_present_generation_cost, deontological).
narrative_ontology:cs_reference_frame('8cc12dc2-0cb7-4f7e-ab86-4e573615a1f9', knowledge_preservation_mandate).
narrative_ontology:cs_drift_state('8cc12dc2-0cb7-4f7e-ab86-4e573615a1f9', contemporary_institutional_replacement, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('8cc12dc2-0cb7-4f7e-ab86-4e573615a1f9', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_preservation__survival_competence_reading, catastrophe_memory_preservation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_preservation__survival_competence_reading, future_generations).
narrative_ontology:constraint_beneficiary(catastrophe_memory_preservation__survival_competence_reading, community_survival_capacity).
narrative_ontology:constraint_victim(catastrophe_memory_preservation__survival_competence_reading, present_generation_participants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Carry and enact ritual practices that commemorate historical catastrophes (starvation, massacre, siege, plague). They bear the direct costs: time spent in ritual performance and drill, emotional labor of grief-rehearsal, opportunity cost forgone income or leisure during commemorations. Their identity as community members is constituted through participation; exiting risks social marginalization and loss of group belonging.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__survival_competence_reading, present_generation_participants, payer,
    moderate, biographical, identity_locked, local).

% Set, maintain, and modify the ritual protocol. Typically religious specialists, elders, or formally designated keepers. They decide what forms the ritual takes, which components are non-negotiable, and how to respond when participation flags or practice degrades. They benefit from sustained authority over meaning-making and from the ritual's role in maintaining community cohesion under their stewardship.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__survival_competence_reading, ritual_authorities, agenda_setter,
    institutional, generational, mobile, local).

% Inherit embodied, practiced threat-recognition from the ritual tradition. They receive operational knowledge—how to organize during food shortage, how to read signs of violence, how to coordinate mutual aid—embedded in the ritual's repetitive enactment. They cannot bargain with the constraint (they do not yet exist); they are locked into receiving whatever practice is transmitted.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__survival_competence_reading, future_generations, beneficiary,
    powerless, generational, trapped, local).

% A non-agent beneficiary: the community's collective capacity to survive repeated catastrophe. The constraint vindicates the proposition that embodied repetition preserves operational knowledge across generations better than narrative alone.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__survival_competence_reading, community_survival_capacity, beneficiary,
    powerless, civilizational, trapped, local).
narrative_ontology:stakeholder_non_agent(catastrophe_memory_preservation__survival_competence_reading, community_survival_capacity).

% Alternative forms of threat-recognition transmission (secular education, written protocols, formal disaster preparedness training) exist but are kept subordinate or suppressed when the ritual system claims monopoly on legitimate threat-response knowledge. They would argue that operational capacity can be preserved through lower-cost transmission mechanisms; their voices are excluded from the authority structure.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__survival_competence_reading, competing_memorialization_systems, excluded,
    organized, biographical, constrained, local).

% State emergency-management systems, public-health institutions, and secular educational systems observe the constraint's operation. They measure whether ritual-preserved threat-recognition produces better survival outcomes than formal institutional preparation, or whether the constraint is theater that duplicates institutional infrastructure.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__survival_competence_reading, modern_institutional_authority, observer,
    institutional, biographical, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_preservation__survival_competence_reading, ritual_authorities).
narrative_ontology:fixing_cost_class(catastrophe_memory_preservation__survival_competence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of preserving operational knowledge about catastrophe response across generational turnover: how to coordinate resource distribution during scarcity, recognize early signs of collective threat, and execute mutual-aid protocols without centralized authority. The ritual embeds this knowledge in repeated practice so that bodies and habits carry the competence forward.
% TRANSFER_FUNCTION: Transfers the cost of knowledge preservation from future-generation emergency response (when catastrophe strikes) to present-generation ritual participation. Present generation bears time, emotion, and opportunity cost; future generations receive operational capacity they would otherwise have to re-learn during crisis.
% ABSENT_VOICES: Competing memorialization and threat-response systems (secular training, written protocols, institutional disaster preparedness) are structurally excluded from the authority setting. They would argue the ritual's operational claims are overstated, that secular mechanisms are more efficient, and that the present generation's identity-lock costs outweigh the future benefit. Their voices remain outside the ritual authority structure.
% DISAPPEARANCE_RATIONALE: If the ritual disappeared overnight, the community's threat-recognition capacity would degrade unless replaced by formal institutional preparation. Future generations would lose embodied knowledge; crisis response would require faster institutional mobilization or higher immediate costs. The community's vulnerability profile during the next catastrophe would shift—either institutional systems would absorb the preparation burden (rearranging resource allocation), or future generations would suffer higher mortality and disruption.
% FOUNDING_PROBLEM: Catastrophic events (siege, famine, pogrom, plague) recur across generations with unpredictable intervals. Each generation must learn to recognize early threat signs and organize collective response without centralized state authority. Without embedded practice, each generation rediscovers these competencies during crisis—at high cost. The ritual solves this by rehearsing response patterns so thoroughly that they become automatic and persist across the generational break.
% FOUNDING_PROBLEM_CORROBORATION: Historians and anthropologists document cases where communities that maintained ritual threat-recognition practices demonstrated faster, more coordinated response during subsequent catastrophes (e.g., Ashkenazi communities during pogroms with established mutual-aid protocols; siege-experienced populations with practiced rationing disciplines). Contemporary emergency-management research contests this: some evidence suggests formal training achieves faster threat recognition; other evidence suggests ritual-rehearsed communities show better sustained mutual aid under collapse of institutional authority. The founding problem—that catastrophe recurs and knowledge must transfer—is uncontested; whether ritual is the optimal or necessary mechanism is disputed by modern institutional authorities and by secular alternative systems.
narrative_ontology:disappearance_verdict(catastrophe_memory_preservation__survival_competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_preservation__survival_competence_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_preservation__survival_competence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(catastrophe_memory_preservation__survival_competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_preservation__survival_competence_reading, 0.76, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_preservation__survival_competence_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophe_memory_preservation__survival_competence_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(catastrophe_memory_preservation__survival_competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is measured at 0.76 across the interval (rising from 0.58 to 0.76 over 40 time points). This reflects the high present-generation cost burden: ritual participation is mandatory for identity maintenance, consumes significant time and emotional labor, and yields no direct benefit to those bearing the cost. The beneficiary (future generations) is temporally separated from the payer; this temporal asymmetry is the core structure of the tangled_rope. Suppression is moderate (0.62): the constraint persists through a combination of identity-lock (participants cannot exit without losing community membership) and authority enforcement (ritual authorities maintain the protocol and socially sanction non-participation). Theater_ratio is low-moderate (0.28): the ritual does perform genuine threat-recognition drilling, but an increasing share of enforcement effort is devoted to maintaining participation itself rather than optimizing the threat-recognition transfer. The measurement series show extractiveness rising as modernizing communities experience more acute tension between ritual demands and competing opportunities (higher opportunity cost over time), and suppression rising as ritual authorities must work harder to maintain participation against this tension. Theater_ratio rises as the justification-narrative emphasizes grief-processing and cultural identity rather than operational capacity—a gradual drift toward the mourning_practice_reading.
 *
 * PERSPECTIVAL GAP:
 *   The ritual_authorities and present_generation_participants should compute dramatically different classifications. The authorities see a genuine coordination problem and legitimate enforcement of a shared commitment; participants see mandatory participation in a practice that benefits others and constrains their own choices. The authorities have exit options (mobile) because they can redefine the protocol or step down; participants are identity_locked. The authorities' role (agenda_setter) gives them authority over how the constraint operates; participants have no such control. From the authorities' seat, high extractiveness reflects the cost of maintaining the practice; from the participants' seat, it reflects exploitation. The engine's per-seat computation should expose this perspectival gap through divergent type classifications even though both seats operate within the same constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Identity-lock is the key mechanism binding present_generation_participants to the constraint. They cannot exit without losing their community identity; the identity fusion is both structural (the community cannot function without their participation) and potentially internalized (participants internalize the identity such that exit feels psychologically impossible even if structural barriers were removed). This locks d toward 1.0 for participants regardless of how the broader community benefits. Ritual_authorities have mobile exit options because they can redefine the protocol or decline the role; this should keep their d lower despite their role as enforcer. The directionality derivation should amplify extraction for the participants and dampen it for the authorities, creating the per-seat divergence that reveals the constraint's structure.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem is that catastrophe recurs unpredictably and each generation must learn threat-recognition. The constraint's mandate is to preserve this knowledge across generational breaks. However, there is substantial empirical uncertainty (omega_1) about whether the ritual actually does this better than alternatives. If modern institutional disaster-preparedness systems prove superior at transferring operational knowledge, the ritual's function becomes marginal and the constraint's mandate has been superseded—it becomes a mandatrophy case where enforcement persists without functional justification. The rising theater_ratio in the measurements (0.12 → 0.28) and rising suppression_requirement suggest the constraint is experiencing this mandatrophy drift: enforcement effort must increase to maintain participation even as the functional justification (operational knowledge transfer) becomes less apparent. The measurement series are consistent with a tangled_rope undergoing mandatrophy—genuine coordination function persists but is increasingly obscured by enforcement overhead and theatrical justification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ritual_operational_transfer_mechanism,
    'Does the ritual actually transfer operational threat-recognition capacity to future generations, or does it primarily serve mourning and symbolic continuity?',
    'Controlled comparison: measure threat-recognition speed and mutual-aid coordination in communities with active ritual practice vs. communities where ritual has lapsed but institutional training exists. Document what competencies are actually practiced in the ritual vs. taught in secular settings.',
    'If the ritual primarily serves symbolic continuity (the mourning_practice_reading), then the constraint''s claimed function is misidentified and its extractiveness is pure overhead. If it demonstrably transfers operational competence faster than alternatives, the extraction is coordination cost rather than pure rent. This classification pivot would shift the constraint from tangled_rope toward rope or piton depending on the magnitude of the transfer benefit.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ritual_operational_transfer_mechanism, empirical, 'Whether ritual transmission of threat-recognition is functionally superior to institutional alternatives or primarily symbolic.').

omega_variable(
    reading_vs_sibling_kernel_contest,
    'This constraint instantiates ONE reading of the catastrophe_memory_preservation kernel. Sibling readings (mourning_practice_reading, hybrid_atrophy_reading) interpret the same ritual texts and practices differently. Which reading captures the kernel''s actual function?',
    'Historical analysis of communities facing actual catastrophe after long quiescent periods: did communities with active ritual practice demonstrate faster threat recognition and better coordination than those without? Ethnographic documentation of what knowledge is explicitly taught in ritual vs. incidental vs. lost.',
    'If the mourning_practice_reading is correct, this constraint mislabels the function; the beneficiary is symbolic continuity, not survival capacity, and extractiveness is higher (pure overhead). If the hybrid_atrophy_reading is correct, the constraint once served its claimed function but now operates theatrically; theater_ratio should be much higher and the constraint reclassifies toward piton. If this reading is correct, the constraint remains tangled_rope with genuine coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_vs_sibling_kernel_contest, conceptual, 'Which sibling reading of the catastrophe_memory_preservation kernel is structurally accurate.').

omega_variable(
    identity_lock_internalization_vs_structure,
    'Is the present generation''s identity-lock to ritual participation (exit_options: identity_locked) a structural feature of community identity, or an internalized suppression mechanism that would release if the ritual''s authority were challenged?',
    'Post-authority-challenge dynamics: when community authority structures weaken (migration, modernization, institutional replacement), do participants who exit the ritual experience persistent guilt/identity-deficit (internalized), or do they successfully reintegrate without suppression (structural lock dissolves)?',
    'If identity-lock is internalized suppression, the constraint''s effective suppression is higher than the 0.62 scalar suggests—participants carry the suppression beyond the ritual''s direct reach. If it is structural (community cannot function without participation), the scalar is accurate and suppression is lower. This affects the per-seat directionality computation for present_generation_participants.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_internalization_vs_structure, empirical, 'Whether identity-lock to ritual participation is structural or internalized suppression.').

omega_variable(
    future_generation_knowledge_retention,
    'How much of the ritual-embedded threat-recognition knowledge actually persists across the generational break? What fraction is lost, reinterpreted, or degraded in transmission?',
    'Ethnographic longitudinal study tracking specific threat-recognition protocols across 2-3 generational transitions. Document what competencies the older generation can articulate vs. what the younger generation actually practices; compare retention rates with written protocols or formal institutional training.',
    'If retention is low (<40%), the beneficiary (future_generations) receives substantially degraded benefit, and the constraint''s classification shifts downward—the high extractiveness from the present generation yields diminishing returns. If retention is high (>70%), the tangled_rope classification holds. Retention rate also feeds the mandatrophy analysis: if the founding problem (catastrophe-response knowledge loss) persists but the ritual fails to solve it, the constraint has lost its functional mandate.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(future_generation_knowledge_retention, empirical, 'What fraction of ritual-embedded threat-recognition knowledge persists across generational transmission.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_preservation__survival_competence_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_preservation__survival_competence_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(cata_tr_t0, observed).
narrative_ontology:measurement(cata_tr_t8, catastrophe_memory_preservation__survival_competence_reading, theater_ratio, 8, 0.16).
narrative_ontology:measurement_basis(cata_tr_t8, observed).
narrative_ontology:measurement(cata_tr_t16, catastrophe_memory_preservation__survival_competence_reading, theater_ratio, 16, 0.2).
narrative_ontology:measurement_basis(cata_tr_t16, observed).
narrative_ontology:measurement(cata_tr_t24, catastrophe_memory_preservation__survival_competence_reading, theater_ratio, 24, 0.24).
narrative_ontology:measurement_basis(cata_tr_t24, observed).
narrative_ontology:measurement(cata_tr_t32, catastrophe_memory_preservation__survival_competence_reading, theater_ratio, 32, 0.26).
narrative_ontology:measurement_basis(cata_tr_t32, observed).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_preservation__survival_competence_reading, theater_ratio, 40, 0.28).
narrative_ontology:measurement_basis(cata_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_preservation__survival_competence_reading, base_extractiveness, 0, 0.58).
narrative_ontology:measurement_basis(cata_be_t0, observed).
narrative_ontology:measurement(cata_be_t8, catastrophe_memory_preservation__survival_competence_reading, base_extractiveness, 8, 0.63).
narrative_ontology:measurement_basis(cata_be_t8, observed).
narrative_ontology:measurement(cata_be_t16, catastrophe_memory_preservation__survival_competence_reading, base_extractiveness, 16, 0.68).
narrative_ontology:measurement_basis(cata_be_t16, observed).
narrative_ontology:measurement(cata_be_t24, catastrophe_memory_preservation__survival_competence_reading, base_extractiveness, 24, 0.72).
narrative_ontology:measurement_basis(cata_be_t24, observed).
narrative_ontology:measurement(cata_be_t32, catastrophe_memory_preservation__survival_competence_reading, base_extractiveness, 32, 0.75).
narrative_ontology:measurement_basis(cata_be_t32, observed).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_preservation__survival_competence_reading, base_extractiveness, 40, 0.76).
narrative_ontology:measurement_basis(cata_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_preservation__survival_competence_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement_basis(cata_su_t0, observed).
narrative_ontology:measurement(cata_su_t8, catastrophe_memory_preservation__survival_competence_reading, suppression_requirement, 8, 0.52).
narrative_ontology:measurement_basis(cata_su_t8, observed).
narrative_ontology:measurement(cata_su_t16, catastrophe_memory_preservation__survival_competence_reading, suppression_requirement, 16, 0.56).
narrative_ontology:measurement_basis(cata_su_t16, observed).
narrative_ontology:measurement(cata_su_t24, catastrophe_memory_preservation__survival_competence_reading, suppression_requirement, 24, 0.59).
narrative_ontology:measurement_basis(cata_su_t24, observed).
narrative_ontology:measurement(cata_su_t32, catastrophe_memory_preservation__survival_competence_reading, suppression_requirement, 32, 0.61).
narrative_ontology:measurement_basis(cata_su_t32, observed).
narrative_ontology:measurement(cata_su_t40, catastrophe_memory_preservation__survival_competence_reading, suppression_requirement, 40, 0.62).
narrative_ontology:measurement_basis(cata_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_preservation__survival_competence_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(catastrophe_memory_preservation__survival_competence_reading, 0.12).
narrative_ontology:affects_constraint(catastrophe_memory_preservation__survival_competence_reading, catastrophe_memory_preservation__mourning_practice_reading).
narrative_ontology:affects_constraint(catastrophe_memory_preservation__survival_competence_reading, catastrophe_memory_preservation__hybrid_atrophy_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the catastrophe_memory_preservation kernel. All three share the kernel (the ritual practices and authority structures) but instantiate different constraints with different metrics, beneficiary/victim structures, and types. The survival_competence_reading (this file) claims high extractiveness, future-generation beneficiary, present-generation victim, tangled_rope type. The mourning_practice_reading claims lower extractiveness (present-generation identity benefit justifies costs), rope type. The hybrid_atrophy_reading claims high theater_ratio, piton type (function atrophied, enforcement persists theatrically). Linked by affects_constraints to signal kernel-reading family membership and to enable contamination analysis: if one reading's function degrades, the others' credibility is affected.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(catastrophe_memory_preservation__survival_competence_reading, organized, 0.48).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
