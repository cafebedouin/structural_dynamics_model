% ============================================================================
% CONSTRAINT STORY: maat_order_principle__divine_mandate_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_maat_order_principle__divine_mandate_reading, []).

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
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: maat_order_principle__divine_mandate_reading
 *   human_readable: Ma'at as Divine Mandate Flowing Through Pharaoh (Source, Not Subject)
 *   domain: religious/political
 *
 * SUMMARY:
 *   In royal and temple inscriptions, Pharaoh does not merely uphold Ma'at —
 *   he IS Ma'at made manifest, the point at which cosmic order enters the
 *   human world. Because the constraint's authority derives entirely from the
 *   ruler's person, no external standard exists by which a royal act could be
 *   judged a violation: whatever the Pharaoh does to hold back isfet (chaos)
 *   is, by the logic of this reading, Ma'at in operation. This licenses
 *   extraction of labor, grain, and tribute as cosmic necessity rather than
 *   policy, and forecloses appeal by officials, peasants, or conquered
 *   populations. Temple priesthoods co-author and benefit from this framing
 *   in exchange for land and tax privileges.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(maat_order_principle__divine_mandate_reading, 0.81).
domain_priors:suppression_score(maat_order_principle__divine_mandate_reading, 0.87).
domain_priors:theater_ratio(maat_order_principle__divine_mandate_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(maat_order_principle__divine_mandate_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(maat_order_principle__divine_mandate_reading, suppression_requirement, 0.87).
narrative_ontology:constraint_metric(maat_order_principle__divine_mandate_reading, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(maat_order_principle__divine_mandate_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(maat_order_principle__divine_mandate_reading, resistance, 0.28).

% --- Constraint claim ---
narrative_ontology:constraint_claim(maat_order_principle__divine_mandate_reading, tangled_rope).
narrative_ontology:human_readable(maat_order_principle__divine_mandate_reading, "Ma'at as Divine Mandate Flowing Through Pharaoh (Source, Not Subject)").
narrative_ontology:topic_domain(maat_order_principle__divine_mandate_reading, "religious/political").

domain_priors:requires_active_enforcement(maat_order_principle__divine_mandate_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(maat_order_principle__divine_mandate_reading, '78460ec1-38cd-4572-b87c-6aebe8b909a1').
narrative_ontology:cs_kernel_codification('78460ec1-38cd-4572-b87c-6aebe8b909a1', distributed).
narrative_ontology:cs_authority_grounding('78460ec1-38cd-4572-b87c-6aebe8b909a1', extraction).
narrative_ontology:cs_interpretation_layer_present('78460ec1-38cd-4572-b87c-6aebe8b909a1').
narrative_ontology:cs_reading_relation('78460ec1-38cd-4572-b87c-6aebe8b909a1', maat_order_principle__reciprocity_reading, forecloses).
narrative_ontology:cs_reading_relation('78460ec1-38cd-4572-b87c-6aebe8b909a1', maat_order_principle__distributed_maintenance_reading, coexists_with).
narrative_ontology:cs_axiom('78460ec1-38cd-4572-b87c-6aebe8b909a1', foundational, ruler_constitutes_rather_than_obeys_maat).
narrative_ontology:cs_axiom_status(ruler_constitutes_rather_than_obeys_maat, holdable).
narrative_ontology:cs_axiom_grounding('78460ec1-38cd-4572-b87c-6aebe8b909a1', ruler_constitutes_rather_than_obeys_maat, theological).
narrative_ontology:cs_axiom('78460ec1-38cd-4572-b87c-6aebe8b909a1', secondary, royal_extraction_is_cosmic_necessity_not_policy).
narrative_ontology:cs_axiom_status(royal_extraction_is_cosmic_necessity_not_policy, holdable).
narrative_ontology:cs_axiom_grounding('78460ec1-38cd-4572-b87c-6aebe8b909a1', royal_extraction_is_cosmic_necessity_not_policy, instrumental).
narrative_ontology:cs_reference_frame('78460ec1-38cd-4572-b87c-6aebe8b909a1', primeval_cosmic_order_at_creation).
narrative_ontology:cs_drift_state('78460ec1-38cd-4572-b87c-6aebe8b909a1', late_new_kingdom_administrative_strain, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('78460ec1-38cd-4572-b87c-6aebe8b909a1', '').
narrative_ontology:cs_kernel_id(maat_order_principle__divine_mandate_reading, maat_order_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(maat_order_principle__divine_mandate_reading, pharaoh_and_royal_house).
narrative_ontology:constraint_beneficiary(maat_order_principle__divine_mandate_reading, temple_priesthood).
narrative_ontology:constraint_beneficiary(maat_order_principle__divine_mandate_reading, central_administration_elite).
narrative_ontology:constraint_victim(maat_order_principle__divine_mandate_reading, peasant_laborers).
narrative_ontology:constraint_victim(maat_order_principle__divine_mandate_reading, corvee_conscripts).
narrative_ontology:constraint_victim(maat_order_principle__divine_mandate_reading, provincial_officials_under_royal_audit).
narrative_ontology:constraint_victim(maat_order_principle__divine_mandate_reading, foreign_tributary_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(maat_order_principle__divine_mandate_reading, provincial_officials_under_royal_audit).
narrative_ontology:constraint_vindicates(maat_order_principle__divine_mandate_reading, cosmic_order_doctrine).
narrative_ontology:constraint_vindicates(maat_order_principle__divine_mandate_reading, isfet_chaos_opposition_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Declared the living embodiment of Ma'at itself, not a party bound by it. Issues decrees, commands corvee labor, conscripts armies, and redistributes tribute, all justified as sustaining cosmic order rather than as policy choices that could be wrong. Because the ruler IS Ma'at under this reading, no priestly college, official, or subject can invoke Ma'at against a royal act — the constraint has no purchase on its own source. Faces essentially no institutional exit cost: any failure (famine, military defeat, court intrigue) is narratively absorbed as isfet intruding from outside, never as royal violation of Ma'at.
narrative_ontology:constraint_stakeholder(maat_order_principle__divine_mandate_reading, pharaoh_and_royal_house, agenda_setter,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(maat_order_principle__divine_mandate_reading, pharaoh_and_royal_house, beneficiary).

% Administers temple estates, controls a large share of grain and land, and produces the ritual and textual apparatus that certifies the Pharaoh as Ma'at's embodiment. In exchange, temples receive land grants, tax exemption, and protected status. Their exit option is real (temples could theoretically withdraw ritual cooperation) but rarely exercised, because the arrangement is also their primary revenue base.
narrative_ontology:constraint_stakeholder(maat_order_principle__divine_mandate_reading, temple_priesthood, beneficiary,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(maat_order_principle__divine_mandate_reading, temple_priesthood, agenda_setter).

% Viziers, treasury officials, and nomarchs who administer the extraction of surplus grain, labor, and tribute in the Pharaoh's name. They benefit from proximity to royal authority and from the legitimacy the divine-mandate framing lends to their own local enforcement, but they are also subject to arbitrary royal audit and can be destroyed by a shift in royal favor — their security depends entirely on staying aligned with the source, not on any Ma'at claim they could assert independently.
narrative_ontology:constraint_stakeholder(maat_order_principle__divine_mandate_reading, central_administration_elite, beneficiary,
    powerful, biographical, constrained, regional).

% Owe corvee labor on royal and temple building projects and surrender grain surplus as a matter of cosmic obligation rather than negotiated tax. Cannot appeal a harsh harvest levy or a conscription order as a violation of Ma'at, because under this reading the ruler's acts define what Ma'at is; there is no external standard to invoke against him. Geographic and social mobility is essentially nonexistent within the interval.
narrative_ontology:constraint_stakeholder(maat_order_principle__divine_mandate_reading, peasant_laborers, payer,
    powerless, biographical, trapped, local).

% Conscripted seasonally for monument construction, canal work, and military campaigns framed as necessary to hold back cosmic chaos (isfet). Bear direct physical and mortality risk. Desertion is treated as a cosmic-order offense, not merely an administrative one, which forecloses the exit route entirely.
narrative_ontology:constraint_stakeholder(maat_order_principle__divine_mandate_reading, corvee_conscripts, payer,
    powerless, immediate, trapped, regional).

% Local administrators who benefit from delegated authority but are periodically punished, demoted, or executed when royal audits find shortfalls, framed as personal failures to uphold Ma'at rather than as disputes over royal demands. They cannot contest an unreasonable quota because the quota, having issued from the embodiment of Ma'at, is definitionally not unreasonable.
narrative_ontology:constraint_stakeholder(maat_order_principle__divine_mandate_reading, provincial_officials_under_royal_audit, payer,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(maat_order_principle__divine_mandate_reading, provincial_officials_under_royal_audit, beneficiary).

% Conquered or client populations required to render tribute justified as restoring cosmic order to lands that would otherwise be chaotic (isfet). Their own political and religious frameworks are treated as irrelevant to the question of whether the tribute is legitimate.
narrative_ontology:constraint_stakeholder(maat_order_principle__divine_mandate_reading, foreign_tributary_populations, payer,
    powerless, generational, trapped, continental).

% Local or foreign religious traditions with their own accounts of order and legitimacy are structurally excluded from the discourse: the divine-mandate reading treats non-Egyptian political orders as chaos requiring correction, leaving no room for their self-understanding to be heard as an alternative account of order.
narrative_ontology:constraint_stakeholder(maat_order_principle__divine_mandate_reading, rival_cosmological_traditions, excluded,
    moderate, generational, trapped, regional).

% Reconstruct the divine-mandate reading from royal inscriptions, temple texts, and administrative records, largely produced by or for the beneficiary classes, and weigh it against wisdom-literature and tomb-biography evidence suggesting reciprocal and distributed readings coexisted or contended with it.
narrative_ontology:constraint_stakeholder(maat_order_principle__divine_mandate_reading, modern_egyptologists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(maat_order_principle__divine_mandate_reading, pharaoh_and_royal_house).
narrative_ontology:fixing_cost_class(maat_order_principle__divine_mandate_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, unambiguous source of legitimate authority that in principle ends disputes over succession, jurisdiction, and the boundary between legitimate rule and usurpation, by locating the standard of order in the ruler's person rather than in a contestable external code.
% TRANSFER_FUNCTION: Moves labor (corvee), agricultural surplus, and tribute from peasants, conscripts, provincial populations, and tributary states upward to the royal house, temple estates, and administrative elite, under the justification that the transfer sustains cosmic order rather than serving particular interests.
% ABSENT_VOICES: Peasant laborers, corvee conscripts, and tributary populations have no institutional channel through which to argue that a specific royal demand is excessive or unjust, because the reading defines the ruler's acts as constitutive of Ma'at rather than as answerable to it; rival cosmological traditions are excluded by definition rather than engaged.
% DISAPPEARANCE_RATIONALE: If the divine-mandate reading collapsed, the legitimacy chain binding corvee labor, temple land grants, and tribute extraction to an unchallengeable cosmic source would break; officials would need to justify demands on grounds open to contest (custom, negotiated obligation, demonstrated need), and reciprocity or distributed-maintenance framings — already latent in wisdom literature and provincial practice — would have room to become the operative standard against which royal conduct could actually be judged.
% FOUNDING_PROBLEM: Early Egyptian state formation needed to end recurring succession disputes and provincial fragmentation by establishing a single, non-negotiable source of political and cosmic legitimacy that could not be relitigated by rival claimants or local power centers.
% FOUNDING_PROBLEM_CORROBORATION: Royal and temple inscriptions (produced by the beneficiary parties) attest the founding problem as permanently live — chaos is ever-present and only the king holds it back. Independent corroboration is thin: wisdom literature (e.g. Instructions attributed to non-royal officials) and tomb autobiographies of provincial nomarchs describe rulers failing to maintain order and being judged for it in practice, which is inconsistent with a reading in which royal acts cannot fail Ma'at by definition — suggesting the strong source-not-subject claim is itself a royal/temple assertion rather than a broadly corroborated one.
narrative_ontology:disappearance_verdict(maat_order_principle__divine_mandate_reading, world_rearranges).
narrative_ontology:founding_problem_status(maat_order_principle__divine_mandate_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(maat_order_principle__divine_mandate_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(maat_order_principle__divine_mandate_reading, 'none', 1).
narrative_ontology:epsilon_provenance(maat_order_principle__divine_mandate_reading, 0.81, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(maat_order_principle__divine_mandate_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(maat_order_principle__divine_mandate_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(maat_order_principle__divine_mandate_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.81) and rising over the measured interval because a legitimacy structure that places the ruler outside the constraint he administers has no internal brake on the scale of extraction it can justify — every increase in corvee demand or tribute is recast as a proportionate response to increased cosmic threat. Suppression is very high (0.87) because the reading's core move (source, not subject) structurally forecloses the reciprocity and distributed-maintenance framings as live alternatives for anyone within the system; dissent is not merely discouraged but rendered incoherent (you cannot accuse Ma'at's embodiment of violating Ma'at). Theater ratio rises over time (0.35 to 0.62) as the coordination function (ending succession disputes, providing a stable legitimacy anchor) is real but increasingly serves as backdrop for consolidating extraction rather than doing independent coordination work — later dynasties layered increasingly elaborate ritual affirmation onto a legitimacy claim that faced growing practical strain (administrative failures, foreign incursion) it could not narratively acknowledge as royal failure.
 *
 * DIRECTIONALITY LOGIC:
 *   Pharaoh and the royal house sit at the extreme beneficiary end with effectively unconstrained exit (arbitrage) because the constraint's authority originates in them; they cannot be a target of their own source-claim. Temple priesthood and central administrative elite are secondary beneficiaries whose cooperation is purchased with land, tax exemption, and delegated authority, giving them real but underused exit options. Peasant laborers, corvee conscripts, and foreign tributary populations are the structural targets: trapped, powerless, and given no channel to contest demands because the reading defines the ruler's acts as constitutively legitimate. Provincial officials occupy an intermediate position, benefiting from delegated authority while remaining exposed to arbitrary royal punishment, which the reading also frames as Ma'at-restoring rather than as a power dispute.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — ending succession disputes and providing a single legitimacy anchor — was arguably live during early state consolidation. Under this reading, however, the anchor has no mechanism for detecting when its own operation has become extractive rather than order-preserving, because the ruler's acts are constitutively Ma'at. The rising extraction and suppression trajectory alongside a growing theater ratio is exactly the divergence the classification exists to register: a constraint claimed by its own tradition as timeless cosmic order, but whose measured operation shows extraction accumulating in a way a genuine natural-law framing could not produce. Corroboration for the founding problem's continued relevance comes only from the beneficiary parties (royal and temple inscriptions); non-royal wisdom literature suggests the practical standard for judging rulers was closer to reciprocity than to the source-not-subject claim this reading makes, which supports the tangled_rope reading over an unqualified mountain reading of the divine mandate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    source_versus_subject_framing,
    'Is the divine-mandate reading (Pharaoh as source of Ma''at, unconstrained by it) an accurate account of how Egyptians across social strata actually understood royal legitimacy, or is it primarily the self-presentation of the royal and temple institutions that benefited from it?',
    'Comparative analysis of royal/temple inscriptions against non-royal wisdom literature, tomb autobiographies of provincial officials, and administrative complaint records, weighing how often rulers are in practice described as failing to maintain Ma''at (which would be incoherent under a strict source-not-subject reading).',
    'If non-royal sources routinely judge rulers against a Ma''at standard, this reading is best understood as royal ideology rather than lived structural reality, and the reciprocity_reading or distributed_maintenance_reading would better describe the operative constraint for most of the population — this reading would then only be true from the royal/temple seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(source_versus_subject_framing, conceptual, 'Whether source-not-subject is a genuine cosmological premise or a self-serving royal framing.').

omega_variable(
    extraction_versus_cosmic_necessity,
    'Was the scale of corvee labor and tribute extraction under this reading proportionate to genuine state-maintenance needs (irrigation, defense, monument-based legitimacy maintenance), or did it substantially exceed functional necessity once the cosmic-necessity framing removed any internal check?',
    'Archaeological and administrative-record analysis comparing labor and grain extraction levels against documented state functional requirements across dynasties, and against periods of administrative breakdown (e.g. First and Second Intermediate Periods) to see whether extraction fell when the divine-mandate framing weakened.',
    'If extraction tracked functional necessity, part of the measured extractiveness reflects genuine coordination cost rather than rent; if extraction diverged upward independent of functional need, it supports the tangled_rope classification over a more coordination-favorable reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extraction_versus_cosmic_necessity, empirical, 'Whether extraction levels tracked functional state needs or exceeded them once cosmic justification removed internal limits.').

omega_variable(
    kernel_framing_underdetermination,
    'Given that royal, temple, and non-royal sources do not agree on where Ma''at''s binding force is located, is the divine-mandate reading a coherent single framing at all, or does it only appear coherent because it is reconstructed almost entirely from royal/temple sources that had reason to assert it?',
    'Cross-reference royal inscriptional claims against the frequency and content of non-elite or provincial-official invocations of Ma''at to test whether the source-not-subject premise was operative outside court/temple contexts, versus the reciprocity or distributed-maintenance framings being operative there instead.',
    'If the divine-mandate reading only holds within court and temple contexts, its scope of applicability (and thus the seats over which its high extraction/suppression profile is accurate) is narrower than a story treating it as the general Egyptian understanding would suggest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether the divine-mandate reading is broadly attested or reconstructable mainly from the seats that benefited from asserting it.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(maat_order_principle__divine_mandate_reading, 0, 400).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(maat_tr_t0, maat_order_principle__divine_mandate_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(maat_tr_t80, maat_order_principle__divine_mandate_reading, theater_ratio, 80, 0.42).
narrative_ontology:measurement(maat_tr_t160, maat_order_principle__divine_mandate_reading, theater_ratio, 160, 0.48).
narrative_ontology:measurement(maat_tr_t240, maat_order_principle__divine_mandate_reading, theater_ratio, 240, 0.53).
narrative_ontology:measurement(maat_tr_t320, maat_order_principle__divine_mandate_reading, theater_ratio, 320, 0.58).
narrative_ontology:measurement(maat_tr_t400, maat_order_principle__divine_mandate_reading, theater_ratio, 400, 0.62).

% Extraction over time
narrative_ontology:measurement(maat_be_t0, maat_order_principle__divine_mandate_reading, base_extractiveness, 0, 0.58).
narrative_ontology:measurement(maat_be_t80, maat_order_principle__divine_mandate_reading, base_extractiveness, 80, 0.65).
narrative_ontology:measurement(maat_be_t160, maat_order_principle__divine_mandate_reading, base_extractiveness, 160, 0.71).
narrative_ontology:measurement(maat_be_t240, maat_order_principle__divine_mandate_reading, base_extractiveness, 240, 0.76).
narrative_ontology:measurement(maat_be_t320, maat_order_principle__divine_mandate_reading, base_extractiveness, 320, 0.79).
narrative_ontology:measurement(maat_be_t400, maat_order_principle__divine_mandate_reading, base_extractiveness, 400, 0.81).

% Suppression requirement over time
narrative_ontology:measurement(maat_su_t0, maat_order_principle__divine_mandate_reading, suppression_requirement, 0, 0.68).
narrative_ontology:measurement(maat_su_t80, maat_order_principle__divine_mandate_reading, suppression_requirement, 80, 0.74).
narrative_ontology:measurement(maat_su_t160, maat_order_principle__divine_mandate_reading, suppression_requirement, 160, 0.79).
narrative_ontology:measurement(maat_su_t240, maat_order_principle__divine_mandate_reading, suppression_requirement, 240, 0.82).
narrative_ontology:measurement(maat_su_t320, maat_order_principle__divine_mandate_reading, suppression_requirement, 320, 0.85).
narrative_ontology:measurement(maat_su_t400, maat_order_principle__divine_mandate_reading, suppression_requirement, 400, 0.87).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(maat_order_principle__divine_mandate_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(maat_order_principle__divine_mandate_reading, reciprocity_reading).
narrative_ontology:affects_constraint(maat_order_principle__divine_mandate_reading, distributed_maintenance_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the maat_order_principle kernel. divine_mandate_reading (this file) places Pharaoh outside the constraint as its source, producing high extraction and high suppression of alternative framings. reciprocity_reading treats Pharaoh as bound by mutual obligation, producing a tangled_rope with lower suppression and an active check on royal extraction. distributed_maintenance_reading spreads the maintenance obligation across all social strata, producing a coordination-heavy profile closer to rope. The three share royal, temple, and provincial source material but author distinct ε values because each reading locates the constraint's binding force differently relative to the ruler.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
