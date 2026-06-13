% ============================================================================
% CONSTRAINT STORY: feud_obligation_kernel__christianized_pacification_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_feud_obligation_kernel__christianized_pacification_reading, []).

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
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: feud_obligation_kernel__christianized_pacification_reading
 *   human_readable: Feud Obligation Kernel — Christianized Pacification Reading
 *   domain: legal_anthropology/medieval_history/comparative_political_systems
 *
 * SUMMARY:
 *   This constraint story instantiates ONE READING of a contested kernel: the
 *   feud-obligation system as understood through Christian pacification
 *   theology. In this reading, blood-feud participation violates divine law
 *   against vengeance; legitimate violence authority is monopolized by God
 *   and delegated exclusively to ecclesiastical and royal institutions. The
 *   reading reframes kinship-based feuding as both political impediment AND
 *   spiritual crime, creating a dual suppression mechanism (Church + Crown)
 *   targeting all feud participants as victims of sin and conditioning their
 *   absolution on accepting centralized dispute resolution. This reading
 *   coexists with two sibling readings: a stateless-coordination reading that
 *   sees feud as a self-enforcing justice mechanism in the absence of
 *   centralized authority, and an extraction-cycle reading that frames feud
 *   as economically destructive predation cycles. The three readings share a
 *   common kernel—the feud-obligation system itself—but instantiate radically
 *   different ε values and beneficiary/victim structures. This story
 *   authorizes the Christian reading alone; the siblings are separate
 *   constraint files linked via network.affects_constraints.
 *
 * KEY AGENTS:
 *   - feud_participants_all_parties: Bound by kinship duty to participate; under this reading, every participation incurs spiritual peril (sin, excommunication, damnation). Exit requires abandoning kinship identity—a prohibitive identity-lock cost.
 *   - clan_kinship_networks: Collective actors whose primary survival mechanism (mutual defense, autonomous justice) the reading declares sinful and illegitimate. Trapped in suppression cycles designed to subordinate clan autonomy to central authority.
 *   - ecclesiastical_authority: Expands jurisdictional reach by monopolizing moral legitimation of violence, absolution procedures, and penance. Operates the penitential machinery that conditions spiritual reconciliation on accepting Church override of kinship obligations.
 *   - royal_consolidation_machinery: Leverages Church's moral authority to suppress autonomous feud and consolidate centralized violence monopoly. Royal courts replace clan councils as locus of legitimate dispute resolution.
 *   - penitential_apparatus (non-agent): Confession, penance, excommunication—the machinery through which the reading operates suppression. Benefits operationally as feud is criminalized spiritually.
 *   - alternative_dispute_mechanisms (excluded): Non-ecclesiastical, non-royal resolution paths (clan arbitration, elder councils, compensation settlements) are structurally excluded or subordinated.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(feud_obligation_kernel__christianized_pacification_reading, 0.71).
domain_priors:suppression_score(feud_obligation_kernel__christianized_pacification_reading, 0.88).
domain_priors:theater_ratio(feud_obligation_kernel__christianized_pacification_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(feud_obligation_kernel__christianized_pacification_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(feud_obligation_kernel__christianized_pacification_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(feud_obligation_kernel__christianized_pacification_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(feud_obligation_kernel__christianized_pacification_reading, accessibility_collapse, 0.79).
narrative_ontology:constraint_metric(feud_obligation_kernel__christianized_pacification_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(feud_obligation_kernel__christianized_pacification_reading, tangled_rope).
narrative_ontology:human_readable(feud_obligation_kernel__christianized_pacification_reading, "Feud Obligation Kernel — Christianized Pacification Reading").
narrative_ontology:topic_domain(feud_obligation_kernel__christianized_pacification_reading, "legal_anthropology/medieval_history/comparative_political_systems").

domain_priors:requires_active_enforcement(feud_obligation_kernel__christianized_pacification_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(feud_obligation_kernel__christianized_pacification_reading, '7ab7bc7f-f790-4d99-9941-fa6311ee8a06').
narrative_ontology:cs_kernel_codification('7ab7bc7f-f790-4d99-9941-fa6311ee8a06', fixed_text).
narrative_ontology:cs_authority_grounding('7ab7bc7f-f790-4d99-9941-fa6311ee8a06', lineage).
narrative_ontology:cs_interpretation_layer_present('7ab7bc7f-f790-4d99-9941-fa6311ee8a06').
narrative_ontology:cs_reading_relation('7ab7bc7f-f790-4d99-9941-fa6311ee8a06', feud_obligation_kernel__stateless_coordination_reading, forecloses).
narrative_ontology:cs_reading_relation('7ab7bc7f-f790-4d99-9941-fa6311ee8a06', feud_obligation_kernel__extraction_cycle_reading, coexists_with).
narrative_ontology:cs_axiom('7ab7bc7f-f790-4d99-9941-fa6311ee8a06', foundational, divine_monopoly_on_vengeance).
narrative_ontology:cs_axiom_status(divine_monopoly_on_vengeance, holdable).
narrative_ontology:cs_axiom_grounding('7ab7bc7f-f790-4d99-9941-fa6311ee8a06', divine_monopoly_on_vengeance, deontological).
narrative_ontology:cs_axiom('7ab7bc7f-f790-4d99-9941-fa6311ee8a06', foundational, ecclesiastical_interpretive_authority_on_legitimate_violence).
narrative_ontology:cs_axiom_status(ecclesiastical_interpretive_authority_on_legitimate_violence, holdable).
narrative_ontology:cs_axiom_grounding('7ab7bc7f-f790-4d99-9941-fa6311ee8a06', ecclesiastical_interpretive_authority_on_legitimate_violence, conventional).
narrative_ontology:cs_reference_frame('7ab7bc7f-f790-4d99-9941-fa6311ee8a06', divine_law_pacification_authority_structure).
narrative_ontology:cs_drift_state('7ab7bc7f-f790-4d99-9941-fa6311ee8a06', late_medieval_consolidation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7ab7bc7f-f790-4d99-9941-fa6311ee8a06', '').
narrative_ontology:cs_kernel_id(feud_obligation_kernel__christianized_pacification_reading, feud_obligation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(feud_obligation_kernel__christianized_pacification_reading, ecclesiastical_authority).
narrative_ontology:constraint_beneficiary(feud_obligation_kernel__christianized_pacification_reading, royal_consolidation_machinery).
narrative_ontology:constraint_victim(feud_obligation_kernel__christianized_pacification_reading, feud_participants_all_parties).
narrative_ontology:constraint_victim(feud_obligation_kernel__christianized_pacification_reading, clan_kinship_networks).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(feud_obligation_kernel__christianized_pacification_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(feud_obligation_kernel__christianized_pacification_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(feud_obligation_kernel__christianized_pacification_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(feud_obligation_kernel__christianized_pacification_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(feud_obligation_kernel__christianized_pacification_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.71 at interval end) because the constraint transfers legitimate violence authority and jurisdictional control from kinship networks to centralized institutions. All feud participants, regardless of faction, are reframed as victims of sin requiring redemption through ecclesiastical channels—this universal victimhood is the reading's distinctive structural move. Suppression is very high (0.88) because the constraint's persistence depends on active enforcement of the Church/Crown monopoly on legitimate violence legitimation; kinship-based alternatives must be continuously delegitimized and suppressed. Theater is moderate (0.42) because genuine coordination work occurs (the reading does organize a transition from autonomous cycles to centralized authority), but a growing share of enforcement activity (especially at organizational level, t=300, suppression=0.91) is theater: performing the threat of spiritual peril, excommunication rituals, public penance ceremonies that reinforce the monopoly without necessarily preventing covert feuding. The measurement series trace enforcement intensification over 300 time units (representing the medieval consolidation period): suppression_requirement rises from 0.55 to 0.88 as the machinery hardens and kinship resistance erodes (resistance falls from 0.72 to 0.35 individual-level). Accessibility collapse rises from 0.62 to 0.81 individual-level as feud participants exhaust alternatives: kinship networks are suppressed, alternative dispute mechanisms are delegitimized, and the only path to social reintegration is ecclesiastical absolution. Stakes inflation parallels accessibility collapse because the constraint adds a new dimension of penalty (spiritual peril, not just kinship obligation or political cost) that makes remaining in feud cycles increasingly costly.
 *
 * PERSPECTIVAL GAP:
 *   This reading will compute very differently across seats. From the ecclesiastical and royal institutional seats (beneficiaries, agenda-setters), the constraint should compute as tangled-rope: genuine coordination (replacing destructive cycles with centralized authority), disciplined by active enforcement and benefiting concentrated actors. From the feud-participant seats (payers, victims, identity-locked), the constraint should compute as snare or worse: coercive suppression of kinship autonomy, offering only spiritual extortion (absolution contingent on submission) as exit. The structural asymmetry is stark: beneficiaries operate at institutional scale with arbitrage-grade exit (the Church can abandon the reading if it loses utility), while payers operate at local/kinship scale with identity-locked exit (kinship identity is constitutive; exiting feud means existential abandonment). The engine computes this divergence automatically from power atoms, exit options, and beneficiary/victim declarations—no override needed.
 *
 * DIRECTIONALITY LOGIC:
 *   All feud participants (regardless of faction affiliation) are victims in this reading because the constraint reframes their entire kinship obligation system as sinful. Beneficiaries are the institutional actors (Church and Crown) who consolidate authority by monopolizing violence legitimation. The beneficiary identity of the Church is not accidental: it benefits from expanded jurisdiction, confession reach, penitential machinery operation, and interpretive monopoly on divine law. The beneficiary identity of royal consolidation is explicit: the constraint enables suppression of kinship-based resistance to centralization. Victims bear identity-lock suppression (kinship bonds are inescapable; exit requires abandoning social identity) and spiritual threat (sin, damnation, excommunication). Payer status tracks with victim status here: feud participants pay the spiritual and social cost of participating in what the reading declares illegitimate activity.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint exhibits strong mandatrophy indicators: the founding problem (autonomous feud cycles preventing centralization) is foundational to the constraint's existence, but the reading itself contains the seeds of mandatrophy obsolescence. Once centralized authority is established and kinship-based feuding is sufficiently suppressed (which the measurements show occurring by t=250-300), the constraint's coordination function is exhausted. The penitential machinery remains (theater_ratio holding at 0.42) but now mainly performs authority maintenance rather than solving the original coordination problem. The declared status (founding_problem_status: contested) reflects this: by t=300, some parties attest the founding problem is solved (centralization is secure), while others attest it persists (suppressed kinship resistance continues in covert forms). The measurement trajectory shows the classic mandatrophy pattern: extractiveness plateaus at t=200-300, theater rises (0.40-0.42), and resistance collapses (0.72→0.35 individual), indicating the constraint has shifted from solving coordination problems (early interval) to maintaining institutional power through ritualized suppression (late interval). The tangled-rope claim reflects the early-interval structure; by t=300, the constraint approaches piton classification (theater-maintained, function-attenuated, institutional actors benefiting while no genuine coordination work remains).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ecclesiastical_capture_vs_genuine_coordination,
    'Does the Church''s expansion of jurisdictional authority through the pacification reading represent genuine solution to coordination failures (replacing destructive feuding with functional centralized authority), or does it primarily represent institutional capture of a natural social transition toward centralization?',
    'Comparative historical analysis: if societies transitioned from feuding to centralized authority WITH Church institutional involvement at similar rates and patterns as WITHOUT Church involvement, the Church''s role is facilitative rather than causally central. If societies that rejected Church authority experienced substantially slower or failed centralization transitions, the Church''s coordination role was causal.',
    'If capture, extractiveness ε should be higher (0.75+, snare range); if genuine coordination, ε of 0.71 tangled_rope is defensible. The classification hinges on whether the centralization would have occurred without ecclesiastical suppression machinery.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ecclesiastical_capture_vs_genuine_coordination, empirical, 'Whether Church authority expansion is causal to centralization or parasitic on it.').

omega_variable(
    reading_kernel_foreclosure,
    'Does the Christianized pacification reading logically foreclose the stateless-coordination reading within a single moral/legal framework, or do the two readings coexist as incommensurable but held-simultaneously commitments?',
    'Doctrinal analysis: if Church theologians explicitly rule out the coordination reading as theologically incoherent or morally foreclosed (vengeance is never justifiable, kinship-based authority is never legitimate), the readings foreclose. If both readings persist in theological and legal discourse without explicit foreclosure (dueling schools of thought, regional variation in doctrine application), they coexist.',
    'If foreclosure, the reading_relations entry for stateless_coordination should be ''forecloses''; if coexist, ''coexists_with''. This is a conceptual rather than empirical question about framework architecture, not about the world.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_kernel_foreclosure, conceptual, 'Logical relationship between this reading and the stateless coordination reading within Church doctrine.').

omega_variable(
    identity_lock_mechanism_durability,
    'Is the identity-lock exit barrier sustained by internal psychological fusion with kinship identity (feud participation is constitutive of who I am), structural kinship-network enforcement (abandoning feud means clan expulsion), or both equally?',
    'Post-exit trajectory analysis: when individuals exit feud participation (through conversion, migration, or dramatic role change), do they report persistent internalized identity conflict (identity-lock persists post-exit), or do they report freedom and reintegration into non-feud communities (identity-lock was structurally enforced, not internalized)? Historical records of converts, religious refugees, and institutional immigrants provide evidence.',
    'If internalized, suppression is carried by the exiting agent—the constraint''s effective suppression is higher than 0.88 structural measure suggests. If structural, suppression collapses once institutional enforcement machinery (clan expulsion threat, kinship sanction) is removed. Affects long-term classification: identity-internalized suppression supports snare; structure-external supports tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_durability, empirical, 'Whether identity-lock is internalized identity fusion or external kinship-network enforcement.').

omega_variable(
    ecclesiastical_sincere_doctrine_vs_instrumental_authority_expansion,
    'Do Church theologians articulate the pacification reading as a sincere divine-law interpretation, or as instrumental justification for institutional authority expansion? How much of the reading''s force depends on the Church''s good-faith belief in the theology versus institutional incentive to suppress rivals?',
    'Textual and institutional analysis: examine Church councils, papal letters, and theological treatises for consistency with other theological commitments, internal debates over the reading, and evidence of resistance from theologians skeptical of the framework. Evidence of theological coherence and good-faith debate supports sincere doctrine; evidence of instrumental post-hoc justification (reading articulated only after authority expansion decisions are made, internal dissent suppressed, contradictions unaddressed) suggests instrumentalism.',
    'If sincere doctrine, the constraint''s operation is more aligned with the stated coordination function; if instrumental, the constraint is closer to pure extraction with coordination as cover (snare). Affects long-term classification and false-summit risk (a false mountain pretending to be coordinated rope).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ecclesiastical_sincere_doctrine_vs_instrumental_authority_expansion, preference, 'Epistemic status of Church theological commitment to pacification doctrine.').

omega_variable(
    kernel_reading_count_and_scope,
    'Are there additional sibling readings of the feud-obligation kernel beyond the three identified (stateless-coordination, extraction-cycle, christianized-pacification)?',
    'Systematic historical and anthropological literature review across cultures and periods: examine how different societies and intellectual traditions read the feud-obligation kernel. Indigenous justice readings, feminist/gender-critical readings, economically-focused readings, and post-colonial readings may instantiate distinct constraints not yet documented.',
    'If additional readings exist with substantially different ε and beneficiary structures, the constraint family is incompletely specified. The network.affects_constraints links may be incomplete. Affects confidence in the three-reading scope and the family analysis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_count_and_scope, empirical, 'Whether the feud-obligation kernel has more than three structural readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(feud_obligation_kernel__christianized_pacification_reading, 0, 300).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(feud_tr_t0, feud_obligation_kernel__christianized_pacification_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(feud_tr_t50, feud_obligation_kernel__christianized_pacification_reading, theater_ratio, 50, 0.3).
narrative_ontology:measurement(feud_tr_t100, feud_obligation_kernel__christianized_pacification_reading, theater_ratio, 100, 0.35).
narrative_ontology:measurement(feud_tr_t150, feud_obligation_kernel__christianized_pacification_reading, theater_ratio, 150, 0.38).
narrative_ontology:measurement(feud_tr_t200, feud_obligation_kernel__christianized_pacification_reading, theater_ratio, 200, 0.4).
narrative_ontology:measurement(feud_tr_t250, feud_obligation_kernel__christianized_pacification_reading, theater_ratio, 250, 0.41).
narrative_ontology:measurement(feud_tr_t300, feud_obligation_kernel__christianized_pacification_reading, theater_ratio, 300, 0.42).

% Extraction over time
narrative_ontology:measurement(feud_be_t0, feud_obligation_kernel__christianized_pacification_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(feud_be_t50, feud_obligation_kernel__christianized_pacification_reading, base_extractiveness, 50, 0.48).
narrative_ontology:measurement(feud_be_t100, feud_obligation_kernel__christianized_pacification_reading, base_extractiveness, 100, 0.58).
narrative_ontology:measurement(feud_be_t150, feud_obligation_kernel__christianized_pacification_reading, base_extractiveness, 150, 0.65).
narrative_ontology:measurement(feud_be_t200, feud_obligation_kernel__christianized_pacification_reading, base_extractiveness, 200, 0.69).
narrative_ontology:measurement(feud_be_t250, feud_obligation_kernel__christianized_pacification_reading, base_extractiveness, 250, 0.71).
narrative_ontology:measurement(feud_be_t300, feud_obligation_kernel__christianized_pacification_reading, base_extractiveness, 300, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(feud_su_t0, feud_obligation_kernel__christianized_pacification_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(feud_su_t50, feud_obligation_kernel__christianized_pacification_reading, suppression_requirement, 50, 0.68).
narrative_ontology:measurement(feud_su_t100, feud_obligation_kernel__christianized_pacification_reading, suppression_requirement, 100, 0.75).
narrative_ontology:measurement(feud_su_t150, feud_obligation_kernel__christianized_pacification_reading, suppression_requirement, 150, 0.82).
narrative_ontology:measurement(feud_su_t200, feud_obligation_kernel__christianized_pacification_reading, suppression_requirement, 200, 0.85).
narrative_ontology:measurement(feud_su_t250, feud_obligation_kernel__christianized_pacification_reading, suppression_requirement, 250, 0.87).
narrative_ontology:measurement(feud_su_t300, feud_obligation_kernel__christianized_pacification_reading, suppression_requirement, 300, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(feud_obligation_kernel__christianized_pacification_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(feud_obligation_kernel__christianized_pacification_reading, feud_obligation_kernel__stateless_coordination_reading).
narrative_ontology:affects_constraint(feud_obligation_kernel__christianized_pacification_reading, feud_obligation_kernel__extraction_cycle_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the feud-obligation kernel. The stateless-coordination and extraction-cycle readings are sibling constraints in the same family, sharing the kernel but instantiating different ε values and victim/beneficiary structures. The Christianized pacification reading (this file) declares feuding spiritually illegitimate and frames centralized Church/Crown authority as the sole legitimate violence monopoly; ε=0.71, victims=all feud participants, beneficiaries=ecclesiastical+royal institutions. The stateless-coordination reading frames feuding as functional justice mechanism in absence of centralized authority; lower ε, kinship collectives as beneficiaries. The extraction-cycle reading frames feuding as predatory destructive cycles depleting productive capacity; ε likely 0.75+, powerful clans as beneficiaries/victimizers. All three instantiate genuine constraints with distinct structural properties; the family decomposition is necessary to avoid ε conflation (DP-001 ε-invariance principle).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(feud_obligation_kernel__christianized_pacification_reading, moderate, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
