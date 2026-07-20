% ============================================================================
% CONSTRAINT STORY: hebrew_vitality__liturgical_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_vitality__liturgical_reading, []).

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
 *   constraint_id: hebrew_vitality__liturgical_reading
 *   human_readable: Hebrew Vitality as Liturgical Continuity
 *   domain: sociolinguistic/religious
 *
 * SUMMARY:
 *   This constraint instantiates the liturgical reading of the contested
 *   hebrew_vitality kernel. It claims that ritual preservationâunbroken
 *   liturgical use across diasporic communitiesâconstitutes the entirety of
 *   Hebrew linguistic vitality, and that this liturgical practice occupies
 *   the kernel of the concept. The reading is historically dominant in
 *   rabbinic and traditionalist circles. It generates low extractiveness
 *   because it does not materially tax non-adherents; its primary effect is
 *   definitional and status-conferring. The beneficiary set is concentrated
 *   among rabbinic authorities whose institutional role is validated by the
 *   claim. No victim set is declared because the preservation mechanism
 *   imposes no direct cost on non-participants; vernacular Hebrew users are
 *   not extracted from, only backgrounded in the definitional framework.
 *
 * KEY AGENTS:
 *   - rabbinic_authorities (institutional/identity_locked): Agenda-setters who maintain the liturgical framework and derive institutional legitimacy from its definitional centrality.
 *   - traditional_liturgical_communities (organized/identity_locked): Beneficiaries whose linguistic practice is axiomatically defined as the locus of vitality.
 *   - vernacular_revivalists (moderate/mobile): Excluded voices who hold the native-daily reading; they contest the kernel occupancy but are not victims of extraction.
 *   - linguistic_anthropologists (analytical/analytical): Observers who document the contest without adjudicating it from within.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_vitality__liturgical_reading, 0.12).
domain_priors:suppression_score(hebrew_vitality__liturgical_reading, 0.15).
domain_priors:theater_ratio(hebrew_vitality__liturgical_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_vitality__liturgical_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(hebrew_vitality__liturgical_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(hebrew_vitality__liturgical_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_vitality__liturgical_reading, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(hebrew_vitality__liturgical_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_vitality__liturgical_reading, rope).
narrative_ontology:human_readable(hebrew_vitality__liturgical_reading, "Hebrew Vitality as Liturgical Continuity").
narrative_ontology:topic_domain(hebrew_vitality__liturgical_reading, "sociolinguistic/religious").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_vitality__liturgical_reading, 'd151dad6-d095-4ac1-af84-848597f9bd32').
narrative_ontology:cs_kernel_codification('d151dad6-d095-4ac1-af84-848597f9bd32', fixed_text).
narrative_ontology:cs_authority_grounding('d151dad6-d095-4ac1-af84-848597f9bd32', lineage).
narrative_ontology:cs_interpretation_layer_present('d151dad6-d095-4ac1-af84-848597f9bd32').
narrative_ontology:cs_reading_relation('d151dad6-d095-4ac1-af84-848597f9bd32', hebrew_vitality__native_daily_reading, forecloses).
narrative_ontology:cs_reading_relation('d151dad6-d095-4ac1-af84-848597f9bd32', hebrew_vitality__hybrid_continuity_reading, forecloses).
narrative_ontology:cs_axiom('d151dad6-d095-4ac1-af84-848597f9bd32', foundational, ritual_presence_constitutes_linguistic_life).
narrative_ontology:cs_axiom_status(ritual_presence_constitutes_linguistic_life, holdable).
narrative_ontology:cs_axiom_grounding('d151dad6-d095-4ac1-af84-848597f9bd32', ritual_presence_constitutes_linguistic_life, theological).
narrative_ontology:cs_reference_frame('d151dad6-d095-4ac1-af84-848597f9bd32', rabbinic_liturgical_continuity).
narrative_ontology:cs_drift_state('d151dad6-d095-4ac1-af84-848597f9bd32', contemporary_vernacular_challenge, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('d151dad6-d095-4ac1-af84-848597f9bd32', '').
narrative_ontology:cs_kernel_id(hebrew_vitality__liturgical_reading, hebrew_vitality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_vitality__liturgical_reading, rabbinic_authorities).
narrative_ontology:constraint_beneficiary(hebrew_vitality__liturgical_reading, traditional_liturgical_communities).
narrative_ontology:constraint_vindicates(hebrew_vitality__liturgical_reading, liturgical_use_as_vitality).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain and transmit the halakhic and liturgical frameworks that define correct Hebrew usage in ritual contexts. Their institutional authority derives from continuity with rabbinic lineage and the claim that unbroken liturgical practice constitutes the living core of the language.
narrative_ontology:constraint_stakeholder(hebrew_vitality__liturgical_reading, rabbinic_authorities, agenda_setter,
    institutional, civilizational, identity_locked, global).

% Communities for whom Hebrew liturgical proficiency is the primary mode of language engagement. They benefit from a definition of vitality that centers their daily and weekly practice, validating their linguistic identity without requiring vernacular fluency.
narrative_ontology:constraint_stakeholder(hebrew_vitality__liturgical_reading, traditional_liturgical_communities, beneficiary,
    organized, generational, identity_locked, global).

% Advocates for native daily Hebrew use as the sole criterion of vitality. They are structurally backgrounded by the liturgical reading's definitional centrality, though not materially extracted from; their voice is present in the broader contest but excluded from the authority structure of this reading.
narrative_ontology:constraint_stakeholder(hebrew_vitality__liturgical_reading, vernacular_revivalists, excluded,
    moderate, biographical, mobile, national).

% Observe and document the competing definitions of language vitality. They note that the liturgical reading solves a specific coordination problem for diasporic continuity but do not adjudicate the vitality question from within the rabbinic framework.
narrative_ontology:constraint_stakeholder(hebrew_vitality__liturgical_reading, linguistic_anthropologists, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates diasporic Jewish communal continuity across spatial dispersion by anchoring Hebrew identity in a non-territorial, non-vernacular practice that can be transmitted without native speaker communities.
% TRANSFER_FUNCTION: Moves definitional authority over what counts as living Hebrew from vernacular and nativist frameworks to the liturgical-rabbinic domain, conferring institutional centrality and legitimacy on rabbinic authorities and traditional communities.
% ABSENT_VOICES: Zionist sociolinguists, secular Hebrew poets, and Israeli educational planners who define vitality through native acquisition and daily communicative use; their absence from the rabbinic authority structure is structural, not incidental.
% DISAPPEARANCE_RATIONALE: If the liturgical reading vanished, rabbinic claims to exclusive guardianship of Hebrew vitality would lose their definitional foundation; traditional communities would need to renegotiate their linguistic identity without the axiomatic centrality of liturgical use, and the resource flows of status, students, and institutional priority that follow from that centrality would shift toward vernacular frameworks.
% FOUNDING_PROBLEM: The dispersal of Jews across multiple vernacular environments and the loss of Hebrew as a daily mother tongue created a crisis of linguistic continuity; without a shared non-vernacular anchor, Hebrew risked dissolving into purely historical memory.
% FOUNDING_PROBLEM_CORROBORATION: Secular historians of Jewish languages corroborate the historical crisis of dispersion and loss of native Hebrew. However, Zionist sociolinguists and Israeli state institutions attest that the founding problem was solved by the twentieth-century vernacular revival, rendering the liturgical-only framing obsolete; this corroboration comes from outside the rabbinic beneficiary set.
narrative_ontology:disappearance_verdict(hebrew_vitality__liturgical_reading, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_vitality__liturgical_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_vitality__liturgical_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(hebrew_vitality__liturgical_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_vitality__liturgical_reading, 0.12, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_vitality__liturgical_reading_tests).
:- end_tests(hebrew_vitality__liturgical_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Metrics are authored low across the board: extractiveness at 0.12 because the constraint moves status and definitional authority, not material resources; suppression at 0.15 because the reading does not actively suppress sibling readings but achieves discursive dominance through institutional inertia; theater_ratio at 0.10 because liturgical use remains functionally central to the communities that practice it, not a performed relic. Accessibility_collapse is low (0.20) because alternatives (the sibling readings) are clearly visible and institutionally active, especially in Israel. Resistance is moderate (0.35) because the native-daily and hybrid readings mount a persistent counter-definition. The claimed type is rope: a definitional coordination mechanism that solves the collective-action problem of diasporic continuity without asymmetric extraction.
 *
 * PERSPECTIVAL GAP:
 *   The rabbinic agenda-setter seat experiences the constraint as rope: it coordinates communal identity and sustains a sacred practice across dispersion. The vernacular-revivalist excluded seat experiences the same definitional structure as a discursive erasure of their lived linguistic reality, though not as material extraction. The engine computes this divergence from structural position rather than authored claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Rabbinic authorities and traditional liturgical communities are declared beneficiaries (low d, validation and status flow to them). Vernacular revivalists are not declared victims because no structural extraction is directed at them; they are excluded from the authority structure but not taxed by it. No override is needed because the structural derivation matches the actual relationship: benefits flow to the liturgical establishment, costs are negligible.
 *
 * MANDATROPHY ANALYSIS:
 *   The liturgical reading risks mandatrophy if the founding problem (dispersion without vernacular anchor) is treated as still live when the vernacular revival has solved it for millions. However, the reading does not harden into a piton because its theater remains low and its institutional carriers genuinely practice the liturgy. The classification as rope is protected by the absence of a victim set: without identifiable payers, the constraint cannot be a snare or tangled rope. If a victim set were manufactured (e.g., secular Israelis forced to subsidize religious academies on vitality grounds), the type would shift toward tangled rope; no such mechanism is present here.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Is the liturgical reading''s occupancy of the kernel a definitional truth within a theological framework, or a factional position in a sociolinguistic dispute?',
    'Comparative analysis of authority structures across Jewish denominations; if only lineage-based rabbinic authorities hold the claim while secular and Zionist authorities reject it, the reading is a factional position.',
    'If factional, the rope classification holds but with higher resistance and potential for type drift if the authority structure weakens; if definitional truth within a closed theological framework, the low resistance and low extraction metrics are stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Whether the liturgical kernel occupancy is theological necessity or factional claim.').

omega_variable(
    vitality_incommensurability,
    'Does the coexistence of millions of native Hebrew speakers falsify the liturgical reading''s sufficiency claim, or does the liturgical reading define a distinct vitality (sacred versus secular) that is incommensurable with vernacular vitality?',
    'Ethnographic study of how rabbinic authorities conceptualize the relationship between Ivrit and Lashon Kodesh; look for explicit boundary-work that treats the categories as non-competing.',
    'If incommensurable, the sibling readings are not competing for the same kernel but naming different constraints, which would require decomposition per epsilon-invariance; if commensurable, the readings are genuine rivals and the current family link is correct.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(vitality_incommensurability, conceptual, 'Whether liturgical and vernacular vitality are commensurable or distinct constraints.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_vitality__liturgical_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebrew_vitality_liturgical_tr_t0, hebrew_vitality__liturgical_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(hebrew_vitality_liturgical_tr_t25, hebrew_vitality__liturgical_reading, theater_ratio, 25, 0.06).
narrative_ontology:measurement(hebrew_vitality_liturgical_tr_t50, hebrew_vitality__liturgical_reading, theater_ratio, 50, 0.07).
narrative_ontology:measurement(hebrew_vitality_liturgical_tr_t75, hebrew_vitality__liturgical_reading, theater_ratio, 75, 0.08).
narrative_ontology:measurement(hebrew_vitality_liturgical_tr_t100, hebrew_vitality__liturgical_reading, theater_ratio, 100, 0.1).

% Extraction over time
narrative_ontology:measurement(hebrew_vitality_liturgical_be_t0, hebrew_vitality__liturgical_reading, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(hebrew_vitality_liturgical_be_t25, hebrew_vitality__liturgical_reading, base_extractiveness, 25, 0.09).
narrative_ontology:measurement(hebrew_vitality_liturgical_be_t50, hebrew_vitality__liturgical_reading, base_extractiveness, 50, 0.1).
narrative_ontology:measurement(hebrew_vitality_liturgical_be_t75, hebrew_vitality__liturgical_reading, base_extractiveness, 75, 0.11).
narrative_ontology:measurement(hebrew_vitality_liturgical_be_t100, hebrew_vitality__liturgical_reading, base_extractiveness, 100, 0.12).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(hebrew_vitality__liturgical_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_vitality__liturgical_reading, identity_coordination).
narrative_ontology:affects_constraint(hebrew_vitality__liturgical_reading, native_daily_reading).
narrative_ontology:affects_constraint(hebrew_vitality__liturgical_reading, hybrid_continuity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the hebrew_vitality kernel. It is distinguished by its claim that unbroken liturgical use alone constitutes Hebrew vitality and occupies the kernel. Sibling readings decompose the kernel differently: native_daily_reading restricts vitality to native generation, while hybrid_continuity_reading requires both liturgical substrate and vernacular reconstruction. These are structurally distinct constraints linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
