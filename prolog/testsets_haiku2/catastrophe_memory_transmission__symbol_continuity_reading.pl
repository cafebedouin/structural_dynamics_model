% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_transmission__symbol_continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   human_readable: Catastrophe Memory Transmission via Symbolic Form Continuity
 *   domain: religious/cultural/memorial
 *
 * SUMMARY:
 *   In communities marked by catastrophe—genocide, enslavement, forced
 *   diaspora, or mass death—ritual preserves identity through symbolic form
 *   continuity. The constraint holds that authentic identity transmission
 *   requires performing memorial and mourning rites in the canonical form
 *   transmitted from ancestors, with high fidelity to prescribed gesture,
 *   language, sequence, and emotional register. This reading frames
 *   symbol-form fidelity as THE survival mechanism: identity continuity
 *   depends on recognizing oneself in unbroken ritual practice. The reading
 *   coexists with two siblings: the operational_competence_reading, which
 *   holds that ritual transmits survival knowledge through pattern rehearsal;
 *   and the hybrid_embedded_reading, which claims the two (symbolic form and
 *   operational content) are inseparable. Each reading generates a different
 *   constraint with different beneficiaries, victims, and enforcement logic.
 *   This story instantiates the symbol_continuity reading alone.
 *
 * KEY AGENTS:
 *   - ritual_custodians: agenda-setters; enforce fidelity; identity-locked in their role as preservers
 *   - community_members_identity_invested: beneficiaries; their self-concept is constituted through recognized participation in the canonical form
 *   - adaptive_community_members: victims; bear the cost of fidelity constraints; excluded from setting ritual terms
 *   - innovation_practitioners: victims; blocked from transmitting adaptive memorial forms; pay reputational cost for proposing change
 *   - younger_generation_learners: dual-positioned; pay cost of fidelity (effort, alien language/gesture); benefit from identity guarantee
 *   - external_observers: analytical seat; document both fidelity function and its suppression of adaptive knowledge
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_transmission__symbol_continuity_reading, 0.68).
domain_priors:suppression_score(catastrophe_memory_transmission__symbol_continuity_reading, 0.71).
domain_priors:theater_ratio(catastrophe_memory_transmission__symbol_continuity_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_transmission__symbol_continuity_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__symbol_continuity_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__symbol_continuity_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_transmission__symbol_continuity_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__symbol_continuity_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_transmission__symbol_continuity_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_transmission__symbol_continuity_reading, "Catastrophe Memory Transmission via Symbolic Form Continuity").
narrative_ontology:topic_domain(catastrophe_memory_transmission__symbol_continuity_reading, "religious/cultural/memorial").

domain_priors:requires_active_enforcement(catastrophe_memory_transmission__symbol_continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_transmission__symbol_continuity_reading, 'aabc21ae-6880-4335-976a-3170e0a463bb').
narrative_ontology:cs_kernel_codification('aabc21ae-6880-4335-976a-3170e0a463bb', distributed).
narrative_ontology:cs_authority_grounding('aabc21ae-6880-4335-976a-3170e0a463bb', lineage).
narrative_ontology:cs_interpretation_layer_present('aabc21ae-6880-4335-976a-3170e0a463bb').
narrative_ontology:cs_reading_relation('aabc21ae-6880-4335-976a-3170e0a463bb', catastrophe_memory_transmission__operational_competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('aabc21ae-6880-4335-976a-3170e0a463bb', catastrophe_memory_transmission__hybrid_embedded_reading, coexists_with).
narrative_ontology:cs_axiom('aabc21ae-6880-4335-976a-3170e0a463bb', foundational, ritual_fidelity_constitutes_identity).
narrative_ontology:cs_axiom_status(ritual_fidelity_constitutes_identity, holdable).
narrative_ontology:cs_axiom_grounding('aabc21ae-6880-4335-976a-3170e0a463bb', ritual_fidelity_constitutes_identity, deontological).
narrative_ontology:cs_axiom('aabc21ae-6880-4335-976a-3170e0a463bb', foundational, symbolic_form_is_intrinsic_survival_good).
narrative_ontology:cs_axiom_status(symbolic_form_is_intrinsic_survival_good, holdable).
narrative_ontology:cs_axiom_grounding('aabc21ae-6880-4335-976a-3170e0a463bb', symbolic_form_is_intrinsic_survival_good, conventional).
narrative_ontology:cs_reference_frame('aabc21ae-6880-4335-976a-3170e0a463bb', unbroken_ritual_transmission_framework).
narrative_ontology:cs_drift_state('aabc21ae-6880-4335-976a-3170e0a463bb', diaspora_displacement_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('aabc21ae-6880-4335-976a-3170e0a463bb', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_transmission__symbol_continuity_reading, catastrophe_memory_transmission).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_transmission__symbol_continuity_reading, communal_identity_continuity).
narrative_ontology:constraint_victim(catastrophe_memory_transmission__symbol_continuity_reading, adaptive_community_members).
narrative_ontology:constraint_victim(catastrophe_memory_transmission__symbol_continuity_reading, innovation_practitioners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(catastrophe_memory_transmission__symbol_continuity_reading, community_members_identity_invested).
narrative_ontology:constraint_beneficiary(catastrophe_memory_transmission__symbol_continuity_reading, younger_generation_learners).
narrative_ontology:constraint_victim(catastrophe_memory_transmission__symbol_continuity_reading, younger_generation_learners).
narrative_ontology:constraint_vindicates(catastrophe_memory_transmission__symbol_continuity_reading, ritual_as_identity_carrier).
narrative_ontology:constraint_vindicates(catastrophe_memory_transmission__symbol_continuity_reading, symbol_fidelity_as_cultural_survival).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Elders, clergy, and designated preservers who maintain the canonical form of mourning and memorial rites. They set the fidelity standards, adjudicate deviations, and enforce transmission through teaching and public correction. Their legitimacy rests on unbroken continuity with prior custodians; deviation under their watch is framed as breach of sacred trust. They defend ritual form against innovation, adaptation, and streamlining.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__symbol_continuity_reading, ritual_custodians, agenda_setter,
    organized, generational, identity_locked, local).

% Participants whose self-concept, belonging, and continuity with ancestors are constituted through ritual participation. They benefit from the constraint's enforcement: ritual fidelity guarantees that their identity is legitimately grounded in unbroken transmission, that their mourning is recognized as properly performed, and that they belong to a continuous lineage. The constraint's persistence vindicates their identity frame.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__symbol_continuity_reading, community_members_identity_invested, beneficiary,
    moderate, generational, identity_locked, local).

% Individuals whose circumstances (disability, geographic dispersal, linguistic drift, life stage incompatibility, material constraints, or genuine incomprehension of archaic symbolic elements) would benefit from adaptive ritual forms that preserve meaning while altering performance. They are excluded from setting ritual terms, constrained in proposing modifications, and bear the cost of non-participation or marginalization when they cannot perform the canonical form. Their adaptive knowledge is not incorporated into transmitted practice.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__symbol_continuity_reading, adaptive_community_members, payer,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_transmission__symbol_continuity_reading, adaptive_community_members, excluded).

% Artists, scholars, liturgists, and creative practitioners who perceive living ritual functions that could be preserved through fresh symbolic forms or hybrid approaches. They propose experimental, contextual, or culturally-adapted memorial practices. They are blocked by fidelity constraints, face reputational cost for suggesting deviation, and their adaptive work is not transmitted as legitimate innovation but is treated as corruption of authentic form.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__symbol_continuity_reading, innovation_practitioners, payer,
    powerless, biographical, constrained, local).

% Those learning ritual forms in contexts where transmission conditions have changed (diaspora, secular upbringing, linguistic alienation, changed material base). They pay the cost of fidelity: hours memorizing archaic language they do not speak, performing gestures whose original contexts are opaque, carrying guilt for imperfect performance. They also benefit from the identity guarantee the constraint provides: learning the 'right' form certifies belonging.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__symbol_continuity_reading, younger_generation_learners, payer,
    powerless, biographical, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_transmission__symbol_continuity_reading, younger_generation_learners, beneficiary).

% Scholars, archivists, comparative ritualists studying how communities preserve catastrophe memory across generations. They document both the fidelity constraint and its costs: the adaptive knowledge that is suppressed, the individuals excluded or marginalized, and the fragility that results when ritual form is decoupled from living operational knowledge.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__symbol_continuity_reading, external_observers_anthropological, observer,
    analytical, generational, analytical, regional).

% Not a seated actor but the vindicated proposition this constraint's operation upholds: the idea that ritual fidelity IS the survival mechanism, that identity continuity depends on symbolic form preservation, and that innovation threatens both. This proposition is vindicated by the constraint's operation—it collects prestige and institutional reproduction but no material goods.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__symbol_continuity_reading, communal_identity_continuity, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(catastrophe_memory_transmission__symbol_continuity_reading, communal_identity_continuity).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_transmission__symbol_continuity_reading, ritual_custodians).
narrative_ontology:fixing_cost_class(catastrophe_memory_transmission__symbol_continuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves identity continuity across catastrophe and generational discontinuity: a community's self-recognition as 'the same community' despite diaspora, death, or secular displacement depends on recognizing mourning forms as unbroken with ancestral practice. Ritual fidelity encodes and transmits that continuity directly through symbolic form rather than through stated historical narrative or genealogy.
% TRANSFER_FUNCTION: Transfers the cost of fidelity (effort, time, constraint on adaptive innovation, marginalization of those who cannot perform canonically) from custodians and identity-invested participants to adaptive community members and innovation practitioners. Also transfers authority over legitimate memorial meaning from distributed community consensus to designated custodians.
% ABSENT_VOICES: Catastrophe survivors in conditions of radical displacement (refugee, diaspora, acute poverty, severe disability, linguistic alienation) whose adaptive practices are treated as degraded or inauthentic versions rather than as legitimate evolution of the same symbolic tradition. Also absent: the practitioners of hybrid or embedded readings of the same kernel (operational_competence_reading, hybrid_embedded_reading communities), whose legitimacy claims are structurally foreclosed by the symbol_continuity reading's framing.
% DISAPPEARANCE_RATIONALE: If symbolic-form fidelity constraints disappeared, memorial practices would rapidly adapt to changed circumstances: ritual forms would drift to reflect current language, performance capacity, and material contexts while preserving core meaning. Some communities would experience this as liberation (adaptive capacity restored); others would experience existential crisis (the identity-constituting continuity they depended on would be severed). The constraint's disappearance would dissolve the particular form of identity continuity it upholds, though identity and memory would reorganize around different anchors.
% FOUNDING_PROBLEM: After catastrophe (genocide, expulsion, slavery, mass death), a community is fragmented across geography, languages, and generations. The only guarantee that future members will recognize themselves as 'the same people' is if mourning and memorial practice is performed identically to how ancestors performed it — if the form itself IS the connection. Symbolic continuity becomes the survival mechanism because genealogy, land, and language are destroyed or scattered.
% FOUNDING_PROBLEM_CORROBORATION: Ritual custodians and identity-invested community members attest the founding problem is live and unsolved: discontinuity threatens dissolution. External observers and scholars of diaspora (non-benefiting parties) attest the founding problem originates in real catastrophe but that its continued necessity is contested—communities show resilience and belonging through multiple adaptive forms, while the fidelity constraint often masks exclusion and perpetuates trauma rehearsal as identity requirement. The corroboration diverges by structural position.
narrative_ontology:disappearance_verdict(catastrophe_memory_transmission__symbol_continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_transmission__symbol_continuity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_transmission__symbol_continuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(catastrophe_memory_transmission__symbol_continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_transmission__symbol_continuity_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is moderate-high (0.68) because the constraint transfers substantial cost to adaptive community members and innovation practitioners without material compensation—their excluded adaptive knowledge is treated as inauthentic. Suppression is correspondingly high (0.71): alternatives (adaptive forms, hybrid approaches, modernized symbolism) are actively excluded, and the framing treats deviation as identity threat rather than legitimate evolution. Theater ratio is high (0.58) because a growing share of the constraint's enforcement activity is performative: custodians spend effort policing symbolic correctness and emotional authenticity, demonstrating fidelity to ancestors more than transmitting actionable survival content. The measurement series tracks extraction rising early and plateauing (identity-locked participants have few exit options; the beneficiary frame is stable), while theater grows throughout (institutional maintenance of the form increasingly consumes effort relative to living memorial function). All metrics measured on a shared time grid spanning interval [0, 50] representing generational transmission across five decades of diaspora or displacement.
 *
 * PERSPECTIVAL GAP:
 *   From ritual_custodians' structural position: the constraint is cooperation (they defend identity continuity against dissolution; identity-invested participants consent to the fidelity standard because it guarantees their belonging). From adaptive_community_members and innovation_practitioners: the constraint is extraction (they are blocked from contributing adaptive knowledge, bear the cost of non-participation or marginalization, and are structurally excluded from setting terms). The engine computes these divergent types from the power/exit/beneficiary structure: custodians and identity-invested members have low directionality (beneficiary-proximate) while adaptive members have high directionality (target-proximate). The claimed type (tangled_rope) reflects the structural truth: genuine coordination function (identity continuity through form fidelity) coupled with asymmetric extraction (costs borne by those who cannot perform canonically).
 *
 * DIRECTIONALITY LOGIC:
 *   Ritual custodians: powerful/institutional, identity_locked exit, agenda-setter role → directionality ~0.2 (beneficiary end, low effective extraction). Community_members_identity_invested: moderate power, identity_locked exit, beneficiary role → directionality ~0.25 (near-beneficiary, they benefit from the fidelity guarantee despite some cost). Adaptive_community_members: moderate power, constrained exit (they cannot leave without identity loss), payer role → directionality ~0.75 (target end, high effective extraction). Innovation_practitioners: powerless, constrained exit, payer role → directionality ~0.80 (full target end; they are excluded and their work is not incorporated). Younger_generation_learners: powerless, identity_locked exit, dual payer+beneficiary role → directionality ~0.65 (asymmetric: they pay the fidelity cost but benefit from the identity continuity guarantee; no override needed because both payments and benefits are genuine). External observers: analytical power, analytical exit → directionality ~0.5 (symmetric observer position).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (identity continuity after catastrophe; how does a fragmented community recognize itself as the same lineage?) is LIVE for ritual custodians and identity-invested participants but CONTESTED for adaptive members and innovation practitioners. They attest the founding problem is partly solved by identity continuity no longer depending solely on geographic/genealogical continuity—it can be grounded in shared practice, shared narrative, or shared ethical framework. The fidelity constraint persists because it vindicates the identity-continuity frame (it collects prestige and institutional reproduction), not because the founding problem remains acute. This is the mandatrophy signal: the constraint's enforcement has shifted from solving the founding problem (enabling identity after catastrophe) to protecting the particular solution frame (fidelity-as-identity) against competing solutions (adaptive, hybrid, embedded forms). The theater_ratio rise (0.42→0.58) reflects this: early in diaspora, fidelity enforcement solves a real survival problem (community cohesion); later, it becomes performance maintenance (defending the fidelity frame against erosion).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression (0.71) structural (external barriers enforced by custodians) or internalized (identity-fusion such that adaptive members suppress their own proposals)?',
    'Post-exit suppression trajectory: interview members who have left the community or adopted hybrid practices and measure whether suppression persists (as internalized guilt/identity crisis) after external enforcement is removed.',
    'If suppression is highly internalized, the constraint''s effective suppression is higher than the structural measure—the target carries it forward. This would strengthen the Snare classification for adaptive members. If suppression is primarily structural, the classification holds at Tangled Rope (externally enforced extraction with real coordination function).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression in fidelity constraints').

omega_variable(
    reading_foreclosure_ambiguity,
    'Does the symbol_continuity reading logically foreclose the operational_competence and hybrid_embedded readings, or do they coexist as live positions held by different communities?',
    'Institutional history and theological analysis: examine whether custodians holding the symbol_continuity reading actively suppress competing readings or merely prioritize symbol fidelity while acknowledging operational competence might also matter. Also: survey diaspora communities to identify which readings they hold as primary.',
    'If symbol_continuity reading forecloses (logically rules out) the others, reclassify reading_relations from coexists_with to forecloses and mark the competing readings as foreclosed siblings. If coexistence is genuine, maintain coexists_with. The classification of sibling constraints depends on this resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_ambiguity, conceptual, 'Whether symbol_continuity logically eliminates or coexists with competing readings of catastrophe memory transmission').

omega_variable(
    adaptive_knowledge_loss_empirical,
    'What specific survival knowledge, adaptive capacity, or operational competence is being actively suppressed or excluded by the symbol-fidelity constraint?',
    'Comparative ethnography: document the adaptive practices proposed by innovation_practitioners and adaptive_community_members; catalog which are rejected and which are incorporated. Trace knowledge that was transmitted before the fidelity constraint hardened and is now absent from the canonical ritual form.',
    'High evidence of lost knowledge strengthens the Tangled Rope classification (real coordination function coupled with real extraction of adaptive capacity). Low evidence would shift toward pure Rope (coordination without substantial asymmetric loss). The theater_ratio rise would be reinterpreted: if lost knowledge is substantial, the rise reflects increasing performance maintenance; if lost knowledge is minimal, it reflects institutional overhead only.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adaptive_knowledge_loss_empirical, empirical, 'Quantity and significance of adaptive or operational knowledge excluded by fidelity constraints').

omega_variable(
    identity_locked_exit_mechanism,
    'For identity_locked participants (custodians, community_members_identity_invested), what specific identity-fusion mechanism locks them into the fidelity constraint?',
    'Ethnographic interview and psychological assessment: distinguish professional identity (career path dependence in custodial role), relational identity (self-concept constituted through community belonging), ideological identity (worldview that makes deviation unthinkable), and institutional identity (the organization HAS BECOME its function). Measure which predominates and whether breaking one mechanism would permit exit.',
    'Different fusion mechanisms have different structural implications for directionality. Career-locked custodians might have higher effective extraction (they cannot exit without professional cost). Identity-fused participants might have lower directionality (they actively choose the role because it constitutes their self). If most locking is relational/ideological, the constraint''s suppression is self-reinforcing (participants internalize it). If most is institutional/career, it is externally enforced. This affects the snare-vs-tangled-rope boundary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_locked_exit_mechanism, empirical, 'Which identity-fusion mechanisms lock participants into fidelity constraints and whether exit is possible if one mechanism breaks').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_transmission__symbol_continuity_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_transmission__symbol_continuity_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement_basis(cata_tr_t0, observed).
narrative_ontology:measurement(cata_tr_t8, catastrophe_memory_transmission__symbol_continuity_reading, theater_ratio, 8, 0.46).
narrative_ontology:measurement_basis(cata_tr_t8, observed).
narrative_ontology:measurement(cata_tr_t16, catastrophe_memory_transmission__symbol_continuity_reading, theater_ratio, 16, 0.5).
narrative_ontology:measurement_basis(cata_tr_t16, observed).
narrative_ontology:measurement(cata_tr_t24, catastrophe_memory_transmission__symbol_continuity_reading, theater_ratio, 24, 0.54).
narrative_ontology:measurement_basis(cata_tr_t24, observed).
narrative_ontology:measurement(cata_tr_t32, catastrophe_memory_transmission__symbol_continuity_reading, theater_ratio, 32, 0.56).
narrative_ontology:measurement_basis(cata_tr_t32, observed).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_transmission__symbol_continuity_reading, theater_ratio, 40, 0.57).
narrative_ontology:measurement_basis(cata_tr_t40, observed).
narrative_ontology:measurement(cata_tr_t50, catastrophe_memory_transmission__symbol_continuity_reading, theater_ratio, 50, 0.58).
narrative_ontology:measurement_basis(cata_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_transmission__symbol_continuity_reading, base_extractiveness, 0, 0.58).
narrative_ontology:measurement_basis(cata_be_t0, observed).
narrative_ontology:measurement(cata_be_t8, catastrophe_memory_transmission__symbol_continuity_reading, base_extractiveness, 8, 0.61).
narrative_ontology:measurement_basis(cata_be_t8, observed).
narrative_ontology:measurement(cata_be_t16, catastrophe_memory_transmission__symbol_continuity_reading, base_extractiveness, 16, 0.64).
narrative_ontology:measurement_basis(cata_be_t16, observed).
narrative_ontology:measurement(cata_be_t24, catastrophe_memory_transmission__symbol_continuity_reading, base_extractiveness, 24, 0.66).
narrative_ontology:measurement_basis(cata_be_t24, observed).
narrative_ontology:measurement(cata_be_t32, catastrophe_memory_transmission__symbol_continuity_reading, base_extractiveness, 32, 0.67).
narrative_ontology:measurement_basis(cata_be_t32, observed).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_transmission__symbol_continuity_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(cata_be_t40, observed).
narrative_ontology:measurement(cata_be_t50, catastrophe_memory_transmission__symbol_continuity_reading, base_extractiveness, 50, 0.68).
narrative_ontology:measurement_basis(cata_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_transmission__symbol_continuity_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement_basis(cata_su_t0, observed).
narrative_ontology:measurement(cata_su_t8, catastrophe_memory_transmission__symbol_continuity_reading, suppression_requirement, 8, 0.67).
narrative_ontology:measurement_basis(cata_su_t8, observed).
narrative_ontology:measurement(cata_su_t16, catastrophe_memory_transmission__symbol_continuity_reading, suppression_requirement, 16, 0.69).
narrative_ontology:measurement_basis(cata_su_t16, observed).
narrative_ontology:measurement(cata_su_t24, catastrophe_memory_transmission__symbol_continuity_reading, suppression_requirement, 24, 0.7).
narrative_ontology:measurement_basis(cata_su_t24, observed).
narrative_ontology:measurement(cata_su_t32, catastrophe_memory_transmission__symbol_continuity_reading, suppression_requirement, 32, 0.71).
narrative_ontology:measurement_basis(cata_su_t32, observed).
narrative_ontology:measurement(cata_su_t40, catastrophe_memory_transmission__symbol_continuity_reading, suppression_requirement, 40, 0.71).
narrative_ontology:measurement_basis(cata_su_t40, observed).
narrative_ontology:measurement(cata_su_t50, catastrophe_memory_transmission__symbol_continuity_reading, suppression_requirement, 50, 0.71).
narrative_ontology:measurement_basis(cata_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_transmission__symbol_continuity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(catastrophe_memory_transmission__symbol_continuity_reading, 0.12).
narrative_ontology:affects_constraint(catastrophe_memory_transmission__symbol_continuity_reading, catastrophe_memory_transmission__operational_competence_reading).
narrative_ontology:affects_constraint(catastrophe_memory_transmission__symbol_continuity_reading, catastrophe_memory_transmission__hybrid_embedded_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of a contested kernel (catastrophe_memory_transmission). Three structurally distinct constraints emerge from three competing readings of how communities preserve identity and knowledge after catastrophe. Symbol_continuity_reading (this story) emphasizes ritual fidelity as the identity survival mechanism and treats deviation as identity threat. Operational_competence_reading emphasizes pattern rehearsal and threat-assessment knowledge transmission through ritual. Hybrid_embedded_reading claims the two are inseparable—symbolic form carries operational content non-propositionally. Each reading generates a different epsilon, different victims, and different type. They are linked via network.affects_constraints to enable cross-reading contamination analysis. The sibling readings are separate constraint files (constraint_catastrophe_memory_transmission__operational_competence_reading.json and constraint_catastrophe_memory_transmission__hybrid_embedded_reading.json). Symbol_continuity_reading influences both siblings by establishing fidelity as the legitimacy criterion.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(catastrophe_memory_transmission__symbol_continuity_reading, moderate, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
