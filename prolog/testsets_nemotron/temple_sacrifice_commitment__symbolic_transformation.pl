% ============================================================================
% CONSTRAINT STORY: temple_sacrifice_commitment__symbolic_transformation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_temple_sacrifice_commitment__symbolic_transformation, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: temple_sacrifice_commitment__symbolic_transformation
 *   human_readable: Authorized Symbolic Transformation of Temple Sacrifice Commitment
 *   domain: religious_law/halakhic_tradition/commitment_system
 *
 * SUMMARY:
 *   This constraint story models the 'symbolic transformation' reading of the
 *   temple_sacrifice_commitment kernel — the claim that prayer and study are
 *   not emergency substitutes but the *authorized, permanent
 *   re-instantiation* of the divine command for sacrifice. The authority
 *   structure (halakhic decisors across generations) asserts the power to
 *   redefine the material form of a divine command while preserving its
 *   obligation-force. This reading carries high extractiveness (0.62) because
 *   it requires the material-performance adherents to accept a rewrite of
 *   what they hold as non-negotiable divine law, and it sustains this through
 *   active enforcement (exclusion from communal legitimacy, doctrinal
 *   policing). Suppression is moderate (0.45) because exit is possible but
 *   identity-costly. Theater ratio (0.38) reflects the gap between the
 *   liturgy's persistent sacrificial language ("restore our judges as of old,
 *   rebuild Jerusalem, restore the service to Your sanctuary") and the
 *   doctrinal claim that the service *has been* restored in prayer.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(temple_sacrifice_commitment__symbolic_transformation, 0.62).
domain_priors:suppression_score(temple_sacrifice_commitment__symbolic_transformation, 0.45).
domain_priors:theater_ratio(temple_sacrifice_commitment__symbolic_transformation, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(temple_sacrifice_commitment__symbolic_transformation, extractiveness, 0.62).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__symbolic_transformation, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__symbolic_transformation, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(temple_sacrifice_commitment__symbolic_transformation, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__symbolic_transformation, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(temple_sacrifice_commitment__symbolic_transformation, tangled_rope).
narrative_ontology:human_readable(temple_sacrifice_commitment__symbolic_transformation, "Authorized Symbolic Transformation of Temple Sacrifice Commitment").
narrative_ontology:topic_domain(temple_sacrifice_commitment__symbolic_transformation, "religious_law/halakhic_tradition/commitment_system").

domain_priors:requires_active_enforcement(temple_sacrifice_commitment__symbolic_transformation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(temple_sacrifice_commitment__symbolic_transformation, 'c00ef990-eac7-4a81-b309-387434e7145b').
narrative_ontology:cs_kernel_codification('c00ef990-eac7-4a81-b309-387434e7145b', formalized).
narrative_ontology:cs_authority_grounding('c00ef990-eac7-4a81-b309-387434e7145b', lineage).
narrative_ontology:cs_interpretation_layer_present('c00ef990-eac7-4a81-b309-387434e7145b').
narrative_ontology:cs_reading_relation('c00ef990-eac7-4a81-b309-387434e7145b', temple_sacrifice_commitment__hybrid_preparatory, influences).
narrative_ontology:cs_reading_relation('c00ef990-eac7-4a81-b309-387434e7145b', temple_sacrifice_commitment__performance_only, forecloses).
narrative_ontology:cs_reading_relation('c00ef990-eac7-4a81-b309-387434e7145b', temple_sacrifice_commitment__study_as_exercise, coexists_with).
narrative_ontology:cs_axiom('c00ef990-eac7-4a81-b309-387434e7145b', foundational, authorized_interpretation_can_permanently_redefine_divine_command_form).
narrative_ontology:cs_axiom_status(authorized_interpretation_can_permanently_redefine_divine_command_form, holdable).
narrative_ontology:cs_axiom_grounding('c00ef990-eac7-4a81-b309-387434e7145b', authorized_interpretation_can_permanently_redefine_divine_command_form, conventional).
narrative_ontology:cs_axiom('c00ef990-eac7-4a81-b309-387434e7145b', foundational, prayer_and_study_are_the_fulfillment_not_substitute_of_sacrifice_obligation).
narrative_ontology:cs_axiom_status(prayer_and_study_are_the_fulfillment_not_substitute_of_sacrifice_obligation, holdable).
narrative_ontology:cs_axiom_grounding('c00ef990-eac7-4a81-b309-387434e7145b', prayer_and_study_are_the_fulfillment_not_substitute_of_sacrifice_obligation, conventional).
narrative_ontology:cs_reference_frame('c00ef990-eac7-4a81-b309-387434e7145b', sinai_sacrificial_cult).
narrative_ontology:cs_drift_state('c00ef990-eac7-4a81-b309-387434e7145b', post_temple_destruction_rabbinic_consolidation, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('c00ef990-eac7-4a81-b309-387434e7145b', '').
narrative_ontology:cs_kernel_id(temple_sacrifice_commitment__symbolic_transformation, temple_sacrifice_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__symbolic_transformation, halakhic_authorities).
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__symbolic_transformation, liturgical_institutions).
narrative_ontology:constraint_victim(temple_sacrifice_commitment__symbolic_transformation, material_performance_adherents).
narrative_ontology:constraint_victim(temple_sacrifice_commitment__symbolic_transformation, restorationist_factions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__symbolic_transformation, general_observant_population).
narrative_ontology:constraint_victim(temple_sacrifice_commitment__symbolic_transformation, general_observant_population).
narrative_ontology:constraint_vindicates(temple_sacrifice_commitment__symbolic_transformation, divine_will_accommodates_historical_conditions).
narrative_ontology:constraint_vindicates(temple_sacrifice_commitment__symbolic_transformation, authoritative_interpretation_binds_ritual_obligation).
narrative_ontology:constraint_vindicates(temple_sacrifice_commitment__symbolic_transformation, prayer_as_sacrifice_equivalence_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Authoritative rabbinic bodies that issue rulings declaring prayer and study as the authorized instantiation of the sacrifice commitment. They define the interpretive framework, legitimize the transformation, and accrue institutional authority from maintaining continuity of the tradition under changed conditions. Their exit options are maximal — they control the hermeneutic and face no structural penalty for their position.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__symbolic_transformation, halakhic_authorities, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(temple_sacrifice_commitment__symbolic_transformation, halakhic_authorities, beneficiary).

% Synagogues, yeshivas, and communal organizations that operationalize the transformed practice — daily prayer services replace sacrificial rites, study of sacrificial law constitutes curricular core. They benefit from a sustainable, scalable ritual economy that requires no Temple, priesthood, or altar. Their exit is mobile: they could adopt alternative liturgies but the current structure serves their institutional continuity.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__symbolic_transformation, liturgical_institutions, beneficiary,
    organized, generational, mobile, global).

% Groups and individuals who hold that the divine command for material sacrifice — animals, grain, incense, priestly service on the Temple Mount — remains binding and unaltered. They experience the symbolic transformation as an unauthorized rewrite of divine law. Their identity is fused to the belief that the covenant requires concrete performance; exit means abandoning a core theological self-understanding. They bear the cost of marginalization within mainstream Jewish life and the cognitive burden of holding a 'deviant' position.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__symbolic_transformation, material_performance_adherents, payer,
    moderate, biographical, identity_locked, global).

% Activist movements (e.g., Temple Institute, Mount Faithful) preparing for literal restoration of sacrificial worship. They treat the symbolic transformation as a temporary, emergency measure — not an authorized permanent redefinition. They pay in political friction, state surveillance, intra-communal hostility, and resource diversion toward a goal the authority structure declares premature or forbidden. Exit is constrained: they can moderate but cannot abandon the restoration goal without surrendering their raison d'être.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__symbolic_transformation, restorationist_factions, payer,
    moderate, generational, constrained, regional).

% The broad community of halakhically observant Jews who participate in prayer and study as the normative substitute. They benefit from a coherent, accessible, portable practice that fits diaspora life. They also bear diffuse costs: the liturgy's sacrificial language creates a persistent tension between what they say and what they do; the theological claim that prayer 'replaces' sacrifice requires sustained cognitive maintenance. Exit is constrained by communal belonging and identity.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__symbolic_transformation, general_observant_population, beneficiary,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(temple_sacrifice_commitment__symbolic_transformation, general_observant_population, payer).

% Scholars of religion, law, and anthropology who analyze the transformation as a case study in ritual adaptation, authority legitimation, and symbolic substitution. They neither collect nor pay; they map the structural dynamics.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__symbolic_transformation, secular_academic_observers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains communal cohesion and covenantal continuity across two millennia of Temple absence by providing a portable, authoritative, and internally coherent substitute for the central rite of biblical religion. Solves the coordination problem of 'how to be the priestly people without the priestly cult.'
% TRANSFER_FUNCTION: Moves interpretive authority and ritual legitimacy from the material performance of sacrifice (which requires Temple, priesthood, sovereignty) to the textual-intellectual performance of prayer and study (which requires only community, text, and authorized interpretation). The transfer is from a geography-bound, lineage-gated, resource-intensive rite to a diaspora-capable, text-mediated, interpretation-gated practice.
% ABSENT_VOICES: The priestly lineages (kohanim) who would have performed the sacrifices — their voices are archived in texts but structurally excluded from the decision to transform the rite. The prophetic tradition that criticized sacrificial formalism without offering a permanent symbolic substitute. The historical Sadducean position that rejected oral interpretation's authority to rewrite written command.
% DISAPPEARANCE_RATIONALE: If the authorized transformation ruling vanished overnight, the halakhic system would face a legitimacy crisis: either the sacrifice obligation reasserts as binding-but-impossible (creating systemic incoherence), or the prayer liturgy loses its sacrificial theology and must be radically rewritten, or a new authority must declare a new substitute. The entire liturgical, educational, and theological architecture of rabbinic Judaism is built on this transformation.
% FOUNDING_PROBLEM: The destruction of the Second Temple (70 CE) eliminated the physical, institutional, and legal conditions for fulfilling the Torah's central positive commandments — the daily and festival sacrifices that structured the covenantal relationship. The founding problem was: how does a people defined by a Temple-centered cult survive the Temple's destruction without dissolving the covenant?
% FOUNDING_PROBLEM_CORROBORATION: The halakhic authorities attest the problem remains live (exile continues, Temple not rebuilt). Material performance adherents attest the problem is misframed — the obligation never lapsed, only the capacity; the transformation is the problem. Academic historians corroborate the historical founding conditions but note the transformation's scope expanded far beyond the original emergency (prayer fixed as permanent substitute, not temporary measure).
narrative_ontology:disappearance_verdict(temple_sacrifice_commitment__symbolic_transformation, world_rearranges).
narrative_ontology:founding_problem_status(temple_sacrifice_commitment__symbolic_transformation, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(temple_sacrifice_commitment__symbolic_transformation, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron+rescue1', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(temple_sacrifice_commitment__symbolic_transformation, 'none', 1).
narrative_ontology:epsilon_provenance(temple_sacrifice_commitment__symbolic_transformation, 0.62, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(temple_sacrifice_commitment__symbolic_transformation_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(temple_sacrifice_commitment__symbolic_transformation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(temple_sacrifice_commitment__symbolic_transformation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The claimed type is tangled_rope because the constraint performs genuine coordination (sustaining a coherent, portable, intergenerational practice for millions across millennia) AND asymmetric extraction (those who cannot accept the transformation are marginalized, their position delegitimized, their identity-costs externalized). Requires active enforcement: the authority structure must continuously police the boundary between 'authorized transformation' and 'unauthorized innovation' — a boundary that itself requires constant interpretive labor. The metrics are authored from the reading's own lights: extractiveness measures the cost imposed on those who hold the original material performance as binding; suppression measures the social and institutional pressure to accept the transformation; theater measures the performative maintenance of sacrificial language in a system that declares sacrifice obsolete.
 *
 * DIRECTIONALITY LOGIC:
 *   Halakhic authorities are structural beneficiaries (d ≈ 0.15) — they control the interpretive frame, gain institutional authority from managing the transformation, face no exit cost. Liturgical institutions are beneficiaries (d ≈ 0.25) — they receive a sustainable ritual economy. General observant population is near-symmetric (d ≈ 0.5) — genuine coordination benefit, diffuse identity-maintenance cost. Material performance adherents are full targets (d ≈ 0.95) — identity-locked, bear the full cost of the rewrite, cannot exit without self-annihilation. Restorationist factions are high targets (d ≈ 0.85) — constrained exit, bear political and communal costs for holding the 'premature' position. The engine computes χ from these structural positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The transformation originated as emergency response to Temple destruction (scaffold-like). Over two millennia it hardened into permanent doctrine with no sunset clause — the 'temporary' substitute became the only legitimate form. This is mandatrophy: the emergency coordination function (survival without Temple) was succeeded by a permanent authority claim (we decide what the command *is*). The classification as tangled_rope captures this: the coordination is real (the practice works, the community persists) but the extraction is real (the authority structure claims power to redefine divine command, and those who dissent pay). A pure rope reading would deny the extraction; a pure snare reading would deny the coordination. The metrics and structural data force the hybrid.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    transformation_authorization_source,
    'Does the authority structure''s claim to authorize permanent transformation of a divine command derive from the command itself (internal authorization) or from the authority structure''s own power (external usurpation)?',
    'Internal textual evidence: does the Torah or prophetic tradition contain a provision for authorized symbolic substitution of sacrifices? External historical evidence: when and by whom was the ''prayer replaces sacrifice'' doctrine first articulated as permanent rather than provisional?',
    'If internal authorization exists, the transformation is a genuine coordination with modest extraction (rope-like). If external usurpation, the extraction is foundational — the authority structure extracts legitimacy by rewriting the kernel it claims to serve (snare-like). This omega directly controls whether ε = 0.62 is coordination-cost or extraction-rent.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(transformation_authorization_source, conceptual, 'Whether the transformation''s authorization is endogenous to the kernel or exogenous imposition by the authority structure.').

omega_variable(
    material_performance_viability,
    'If the Temple were rebuilt tomorrow, would the symbolic_transformation reading declare the transformation reversible (prayer was always provisional) or irreversible (prayer is now the permanent form)?',
    'Track halakhic responsa and institutional statements on hypothetical restoration. The Temple Institute''s program and mainstream rabbinic responses to it are the live test.',
    'If reversible, the current high extractiveness is temporary emergency measure (scaffold-like). If irreversible, the authority structure has permanently rewritten the command — the extraction from material-performance adherents is structural and permanent (tangled_rope or snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(material_performance_viability, empirical, 'Whether the transformation claims permanence or provisionality determines its mandatrophy status.').

omega_variable(
    cs_framing_under_determination,
    'Does the commitment-system kernel reside in the Written Torah''s sacrificial legislation (fixed_text), the Oral Torah''s interpretive authority (lineage), or the living practice of the observant community (practice)?',
    'Compare how each reading locates the kernel: symbolic_transformation grounds in Oral Torah''s interpretive authority; performance_only grounds in Written Torah''s fixed text; hybrid_preparatory grounds in messianic teleology; study_as_exercise grounds in the intellectual practice itself. The framing choice determines which authority_grounding value is correct and whether interpretation_layer_present applies.',
    'If kernel = fixed_text, the transformation is axiom_overriding drift (high extraction). If kernel = lineage/practice, the transformation is authorized interpretation (lower extraction). The cs_structure classification depends on this framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_framing_under_determination, conceptual, 'Alternative CS framings of the same kernel produce different authority_grounding and drift_state classifications.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(temple_sacrifice_commitment__symbolic_transformation, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tsc_sym_tr_t0, temple_sacrifice_commitment__symbolic_transformation, theater_ratio, 0, 0.05).
narrative_ontology:measurement(tsc_sym_tr_t500, temple_sacrifice_commitment__symbolic_transformation, theater_ratio, 500, 0.12).
narrative_ontology:measurement(tsc_sym_tr_t1000, temple_sacrifice_commitment__symbolic_transformation, theater_ratio, 1000, 0.22).
narrative_ontology:measurement(tsc_sym_tr_t1500, temple_sacrifice_commitment__symbolic_transformation, theater_ratio, 1500, 0.31).
narrative_ontology:measurement(tsc_sym_tr_t2000, temple_sacrifice_commitment__symbolic_transformation, theater_ratio, 2000, 0.38).

% Extraction over time
narrative_ontology:measurement(tsc_sym_be_t0, temple_sacrifice_commitment__symbolic_transformation, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(tsc_sym_be_t500, temple_sacrifice_commitment__symbolic_transformation, base_extractiveness, 500, 0.28).
narrative_ontology:measurement(tsc_sym_be_t1000, temple_sacrifice_commitment__symbolic_transformation, base_extractiveness, 1000, 0.42).
narrative_ontology:measurement(tsc_sym_be_t1500, temple_sacrifice_commitment__symbolic_transformation, base_extractiveness, 1500, 0.55).
narrative_ontology:measurement(tsc_sym_be_t2000, temple_sacrifice_commitment__symbolic_transformation, base_extractiveness, 2000, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(tsc_sym_su_t0, temple_sacrifice_commitment__symbolic_transformation, suppression_requirement, 0, 0.08).
narrative_ontology:measurement(tsc_sym_su_t500, temple_sacrifice_commitment__symbolic_transformation, suppression_requirement, 500, 0.18).
narrative_ontology:measurement(tsc_sym_su_t1000, temple_sacrifice_commitment__symbolic_transformation, suppression_requirement, 1000, 0.32).
narrative_ontology:measurement(tsc_sym_su_t1500, temple_sacrifice_commitment__symbolic_transformation, suppression_requirement, 1500, 0.41).
narrative_ontology:measurement(tsc_sym_su_t2000, temple_sacrifice_commitment__symbolic_transformation, suppression_requirement, 2000, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(temple_sacrifice_commitment__symbolic_transformation, identity_coordination).
narrative_ontology:boltzmann_floor_override(temple_sacrifice_commitment__symbolic_transformation, 0.08).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__symbolic_transformation, temple_sacrifice_commitment__hybrid_preparatory).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__symbolic_transformation, temple_sacrifice_commitment__performance_only).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__symbolic_transformation, temple_sacrifice_commitment__study_as_exercise).

% DUAL FORMULATION NOTE:
% This constraint is one of four readings of the temple_sacrifice_commitment kernel. The ε-invariance principle requires separate stories because the symbolic_transformation reading claims authorized permanent redefinition (high ε for dissenters), while hybrid_preparatory claims provisional preparation (lower ε), performance_only denies transformation validity (ε near zero for adherents, high for authority), and study_as_exercise claims intellectual performance satisfies the command (moderate ε). Each reading has distinct beneficiary/victim structures and distinct coordination/extraction profiles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(temple_sacrifice_commitment__symbolic_transformation, moderate, 0.95).
constraint_indexing:directionality_override(temple_sacrifice_commitment__symbolic_transformation, organized, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
