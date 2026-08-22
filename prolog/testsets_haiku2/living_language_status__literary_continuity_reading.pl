% ============================================================================
% CONSTRAINT STORY: living_language_status__literary_continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_living_language_status__literary_continuity_reading, []).

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
 *   constraint_id: living_language_status__literary_continuity_reading
 *   human_readable: Living Language Status via Literary Continuity (Haskalah Reading)
 *   domain: sociolinguistics/cultural_authority/nationalism
 *
 * SUMMARY:
 *   This constraint instantiates ONE reading of the contested kernel
 *   'living_language_status' — specifically, the literary_continuity_reading.
 *   It defines language vitality through secular intellectual and literary
 *   production (Haskalah periodicals, modern Hebrew poetry and prose) rather
 *   than through liturgical transmission or native-speaker demographics. This
 *   reading benefits maskilim and secular intellectuals by granting them
 *   cultural authority to define vitality; it excludes illiterate speakers,
 *   religious communities, and native-speaker families whose fluency does not
 *   manifest in literary form. The constraint is CLAIMED as rope (genuine
 *   coordination for a shared definition) while the authored metrics show
 *   substantial extraction (authority asymmetry, structural exclusion,
 *   suppression of alternative readings). The claim/metric gap is deliberate
 *   and measures the core analytical question: does this reading coordinate
 *   or extract?
 *
 * KEY AGENTS:
 *   - maskilim_and_secular_intellectuals: beneficiaries of the definition; their literary output constitutes the evidence for vitality
 *   - illiterate_and_non_literary_speakers: excluded and rendered invisible; their fluency counts as zero under the definition
 *   - religious_authorities_and_liturgical_communities: excluded by the reading's filter; their work is rendered invisible
 *   - native_speaker_families: historically present but excluded because their domestic transmission is non-literary
 *   - colonial_and_nationalist_authorities: agenda-setter; adopt the reading as state policy enabling national revival
 *   - linguistic_observers: analytical seat; dispute whether literary production vindicates vitality in linguistically coherent terms
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(living_language_status__literary_continuity_reading, 0.42).
domain_priors:suppression_score(living_language_status__literary_continuity_reading, 0.38).
domain_priors:theater_ratio(living_language_status__literary_continuity_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(living_language_status__literary_continuity_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(living_language_status__literary_continuity_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(living_language_status__literary_continuity_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(living_language_status__literary_continuity_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(living_language_status__literary_continuity_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(living_language_status__literary_continuity_reading, rope).
narrative_ontology:human_readable(living_language_status__literary_continuity_reading, "Living Language Status via Literary Continuity (Haskalah Reading)").
narrative_ontology:topic_domain(living_language_status__literary_continuity_reading, "sociolinguistics/cultural_authority/nationalism").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(living_language_status__literary_continuity_reading, 'fdbfe552-d476-4e2b-9cf9-1888976e0ab4').
narrative_ontology:cs_kernel_codification('fdbfe552-d476-4e2b-9cf9-1888976e0ab4', distributed).
narrative_ontology:cs_authority_grounding('fdbfe552-d476-4e2b-9cf9-1888976e0ab4', extraction).
narrative_ontology:cs_interpretation_layer_present('fdbfe552-d476-4e2b-9cf9-1888976e0ab4').
narrative_ontology:cs_reading_relation('fdbfe552-d476-4e2b-9cf9-1888976e0ab4', living_language_status__liturgical_preservation_reading, coexists_with).
narrative_ontology:cs_reading_relation('fdbfe552-d476-4e2b-9cf9-1888976e0ab4', living_language_status__native_generation_reading, coexists_with).
narrative_ontology:cs_axiom('fdbfe552-d476-4e2b-9cf9-1888976e0ab4', foundational, literary_production_constitutes_vitality).
narrative_ontology:cs_axiom_status(literary_production_constitutes_vitality, holdable).
narrative_ontology:cs_axiom_grounding('fdbfe552-d476-4e2b-9cf9-1888976e0ab4', literary_production_constitutes_vitality, empirically_contingent).
narrative_ontology:cs_axiom('fdbfe552-d476-4e2b-9cf9-1888976e0ab4', foundational, native_speaker_status_not_required).
narrative_ontology:cs_axiom_status(native_speaker_status_not_required, holdable).
narrative_ontology:cs_axiom_grounding('fdbfe552-d476-4e2b-9cf9-1888976e0ab4', native_speaker_status_not_required, empirically_contingent).
narrative_ontology:cs_reference_frame('fdbfe552-d476-4e2b-9cf9-1888976e0ab4', hebrew_as_literary_medium_revival).
narrative_ontology:cs_drift_state('fdbfe552-d476-4e2b-9cf9-1888976e0ab4', post_1948_israeli_state_adoption, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('fdbfe552-d476-4e2b-9cf9-1888976e0ab4', '2026-06-12T14:23:45Z').
narrative_ontology:cs_kernel_id(living_language_status__literary_continuity_reading, living_language_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(living_language_status__literary_continuity_reading, maskilim_and_secular_intellectuals).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(living_language_status__literary_continuity_reading, illiterate_and_non_literary_speakers).
narrative_ontology:constraint_vindicates(living_language_status__literary_continuity_reading, literary_vitality_doctrine).
narrative_ontology:constraint_vindicates(living_language_status__literary_continuity_reading, secular_cultural_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Claim cultural authority to define what counts as language 'vitality' through literary and intellectual production. They produce the Haskalah periodicals, secular literature, and critical scholarship in Hebrew that constitute the evidence for the constraint's definition. Their authority rests on this definition's acceptance; if language vitality required mass native speakers instead, their authority would collapse.
narrative_ontology:constraint_stakeholder(living_language_status__literary_continuity_reading, maskilim_and_secular_intellectuals, beneficiary,
    organized, generational, mobile, regional).

% Are structurally excluded from the definition of language vitality because they do not produce or consume literary work. Their fluency in the language counts as nothing under this constraint; they bear the cost of being invisible to the vitality measure while intellectuals' literary output carries full weight.
narrative_ontology:constraint_stakeholder(living_language_status__literary_continuity_reading, illiterate_and_non_literary_speakers, payer,
    powerless, biographical, trapped, regional).

% Maintain Hebrew through continuous liturgical transmission and sacred-text study, which would constitute language vitality under the sibling liturgical_preservation_reading. Under the literary_continuity_reading, their work is rendered invisible; their exclusion is structural — the constraint does not measure or credit liturgical vitality, only secular literary vitality.
narrative_ontology:constraint_stakeholder(living_language_status__literary_continuity_reading, religious_authorities_and_liturgical_communities, excluded,
    organized, civilizational, constrained, regional).

% Speak Hebrew as a mother tongue in daily life (or did historically), but their transmission is invisible to the constraint because it is neither literary nor intellectualized. Under the native_generation_reading, their work would constitute vitality; under this reading, they are excluded because 'productive medium for new literary work' filters out domestic, non-intellectual use.
narrative_ontology:constraint_stakeholder(living_language_status__literary_continuity_reading, native_speaker_families_and_rural_communities, excluded,
    powerless, generational, trapped, local).

% Adopt and enforce the literary_continuity_reading as the official standard for language vitality, enabling them to declare Hebrew 'alive' and worthy of national revival even without mass native speakers. The reading solves a nationalist problem: proving a language can be revived through intellectual and literary work rather than requiring demographic continuity.
narrative_ontology:constraint_stakeholder(living_language_status__literary_continuity_reading, colonial_and_nationalist_authorities, agenda_setter,
    institutional, generational, analytical, regional).

% Examine whether this definition captures what linguists mean by 'language vitality' or whether it privileges a particular reading (the elite literary reading) over demographic measures. They note that the definition's adoption depends on accepting the Haskalah as vindicating evidence, which requires first accepting the reading itself.
narrative_ontology:constraint_stakeholder(living_language_status__literary_continuity_reading, linguistic_observers_and_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(living_language_status__literary_continuity_reading, maskilim_and_secular_intellectuals).
narrative_ontology:fixing_cost_class(living_language_status__literary_continuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables secular intellectuals, nationalist authorities, and revival movements to coordinate around a definition of language vitality that decouples it from native-speaker demographics. The Haskalah periodicals and modern Hebrew literature serve as the coordinating evidence: if literary production counts as vitality, Hebrew is alive; if mass native speakers are required, the coordination claim fails.
% TRANSFER_FUNCTION: Transfers cultural authority from religious authorities (who controlled vitality through liturgical transmission) to secular intellectuals (who control it through literary production). It also transfers the definition of what counts as 'speaking the language' from vernacular daily use to intellectual and literary use.
% ABSENT_VOICES: Illiterate and non-literary speakers are structurally absent from the coordination — their fluency is not counted or credited. Religious authorities are excluded because their liturgical work is rendered invisible by the reading. Native-speaker families whose transmission is non-literary are also excluded. These constituencies would object that vitality requires demographic reality, not just elite literary production.
% DISAPPEARANCE_RATIONALE: If this constraint disappeared and language vitality reverted to demographic measures (native-speaker transmission in daily life), the entire Haskalah revival movement's legitimacy claim would collapse. Hebrew would be declared 'dead' because it lacked mass native speakers. The constraint's existence is what enables the revival narrative; its absence would reorganize the entire classification of Hebrew language status.
% FOUNDING_PROBLEM: In the late 18th and 19th centuries, Hebrew had no community of native speakers using it in daily domestic life, yet Jewish intellectuals and nationalists wanted to claim it remained a 'living' language capable of carrying modern intellectual and literary work. The Haskalah periodicals and secular literature emerged as evidence that Hebrew could be a productive medium for new thought and creativity without native-speaker communities.
% FOUNDING_PROBLEM_CORROBORATION: Historians and literary scholars document the Haskalah periodicals and the emergence of modern Hebrew literature as factual developments. However, they dispute whether these developments prove language 'vitality' in a linguistically coherent sense. Linguists outside the nationalist tradition argue vitality requires demographic transmission; historians inside the Zionist tradition credit literary production as sufficient evidence. The founding problem is real but its resolution is readings-dependent. No external corroboration from non-Zionist linguistic authorities exists for the claim that literary production alone constitutes vitality.
narrative_ontology:disappearance_verdict(living_language_status__literary_continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(living_language_status__literary_continuity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(living_language_status__literary_continuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(living_language_status__literary_continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(living_language_status__literary_continuity_reading, 0.42, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(living_language_status__literary_continuity_reading_tests).
:- end_tests(living_language_status__literary_continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-low (0.42 at 1950) rather than high because the constraint is not sustained by coercive force — no government explicitly suppresses other definitions of vitality, and the literary reading emerged from genuine intellectual work rather than decree. However, extractiveness rises over the interval (0.25→0.42) because the reading's adoption as state policy (Zionism, Israeli language policy) increasingly privileges the literary definition and marginalizes the alternatives. Suppression is correspondingly low-moderate (0.38): the constraint persists through cultural authority and institutional policy, not through active coercion, but it does suppress competing definitions (the native_generation_reading and liturgical_preservation_reading are de-legitimized even though they remain live intellectually). Theater rises (0.15→0.31 by 1920) as the reading becomes institutionalized and performative maintenance (Hebrew language academies, canonical status for specific literary works) accumulates, then slightly declines (0.31→0.28 by 1950) as it normalizes into actual policy. The measurement series use one time grid so every metric is authored at every examined point.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seat (maskilim) and the excluded seats (illiterate speakers, religious communities, native-speaker families) should compute different types: from the intellectuals' position this is genuine rope — they coordinate around a shared definition that enables collective action (producing literary work that vindicates language revival). From the excluded seats' position it is snare-flavored — they are locked out of the definition and cannot exit by producing vernacular speech (the constraint filters it out as not-literary). The engine computes this seat divergence from the structural data: beneficiaries get low d (subsidy flowing to their authority), victims and excluded get high d (their fluency is counted as zero). The authored claim (rope) reflects the beneficiary reading; the authored metrics show extraction visible only from the excluded seats. This is exactly how institutional capture of definitional authority appears: one seat sees coordination, the other sees exclusion.
 *
 * DIRECTIONALITY LOGIC:
 *   Maskilim_and_secular_intellectuals are the structural beneficiary: the constraint grants them cultural authority to define what counts as language vitality and reserves that authority to themselves (literary producers). They are mobile (they could exit to other languages or other definitions) but choose not to because the reading gives them power. Their directionality d is low (~0.15), approaching full beneficiary. Illiterate_and_non_literary_speakers are structurally victimized: their fluency is rendered invisible and worthless by the constraint's definition. They are trapped (they cannot produce literary work to become visible, and they cannot exit the language to protest the definition). Their d is high (~0.85), approaching full target. Religious_authorities are excluded rather than extracted from: they maintain liturgical vitality but are not credited by the reading. Their d sits high (~0.75) because they are locked out, but they have some exit (their communities can continue liturgical transmission even if unrecognized). Colonial_and_nationalist_authorities set the agenda and benefit from the reading's adoption as policy, so they sit near beneficiary (d~0.20), though their power is institutional rather than cultural. The divergence in d values across seats is the source of per-seat type divergence: a beneficiary seat computes rope, a victim seat computes snare, an excluded seat computes piton (the constraint persists for the authority but delivers nothing to them).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding problem (proving Hebrew could carry modern intellectual work without native speakers) remains live and contested. The literary_continuity_reading was built to solve it by establishing that literary production = vitality. However, once the reading became institutionalized as Israeli state policy (by 1920–1950), the original mandate shifted: the reading now functions to legitimize the state's language policy and to de-legitimize the liturgical_preservation_reading and native_generation_reading as 'inadequate' definitions of vitality. This is mandate drift: the reading was originally an analytical claim about what vitality means; it became a political tool for suppressing competing claims. The theater_ratio rise (0.15→0.31) and suppression_requirement rise (0.22→0.42) capture this drift. At the 1950 endpoint, the founding problem is less 'is Hebrew vitality possible without native speakers' (answered: yes, the Haskalah proved it) and more 'which reading of vitality legitimizes our state?' The constraint's persistence depends increasingly on institutional policy and cultural authority rather than on the original intellectual argument. This is not yet full mandatrophy (the founding problem is not dead, only transformed into a tool), but the pattern is visible in the metrics.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    definition_vs_discovery,
    'Does the literary_continuity_reading discover what linguists mean by ''language vitality,'' or does it constitute a novel definition that privileges literary production over demographic transmission?',
    'Compare the reading''s definition of vitality with peer-reviewed linguistic definitions of language vitality from linguists outside the nationalist tradition. If linguistic consensus requires native-speaker transmission (as in Ethnologue, UNESCO vitality scales), the reading is definitional rather than descriptive.',
    'If the reading is definitional (not descriptive), it is an extracted definition that grants cultural authority to the maskilim; the constraint becomes snare-flavored across all seats, not just excluded ones. If it is consistent with linguistic consensus, the constraint is genuine rope with a shared definition.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(definition_vs_discovery, empirical, 'Whether the reading''s definition of vitality aligns with or departs from linguistic consensus.').

omega_variable(
    coercion_vs_coordination,
    'Is the literary_continuity_reading sustained by coercive institutional enforcement (state language policy, educational mandates) or by genuine intellectual coordination among the maskilim?',
    'Historical analysis of how the reading was adopted: was it mandated by state authority and suppressed alternatives, or did it emerge from voluntary intellectual collaboration and gradually become institutionalized?',
    'If primarily coercive, suppression is higher and the constraint is snare-flavored. If primarily coordinated, the constraint is genuine rope. The interval measurement series (suppression_requirement rising from 0.22 to 0.42) suggests coercive institutional adoption; high resolution would clarify the mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coercion_vs_coordination, empirical, 'Whether institutional enforcement or intellectual coordination drives the reading''s persistence.').

omega_variable(
    reading_foreclosure_ambiguity,
    'Does the literary_continuity_reading logically foreclose the native_generation_reading, or do they coexist as different valid readings of what language vitality means?',
    'Analytic inquiry: can a single framework (e.g., an academic definition of language vitality) hold both readings simultaneously? If yes, they coexist; if no, one forecloses the other.',
    'If native_generation forecloses literary_continuity (because demographics are empirically necessary for real vitality), this reading is a false natural law vulnerable to falsification. If they coexist (different definitions, neither logically excludes the other), the constraint is a reading that must remain contested. If literary_continuity forecloses native_generation (literary work alone is sufficient), the foreclosure depends on accepting the definition first — which creates circularity (the reading assumes what it claims to prove).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_foreclosure_ambiguity, conceptual, 'The logical structure of the kernel contest: do the readings foreclose or coexist?').

omega_variable(
    suppression_mechanism_obscured,
    'Is suppression of alternative readings (liturgical_preservation, native_generation) structural (the literary reading''s adoption de-legitimizes the alternatives by institutional policy) or internalized (intellectuals who adopt the reading come to see the alternatives as inadequate)?',
    'Post-adoption trajectory: if suppression persists after institutional enforcement declines, the mechanism is internalized (the reading has been cognitively absorbed); if suppression depends on ongoing policy enforcement, the mechanism is structural.',
    'If structural, the constraint''s suppression is externally imposed and could be lifted by policy change. If internalized, the suppression persists even if policy shifts, because the maskilim and their intellectual heirs believe the reading is correct. The rising theater_ratio (0.15→0.31) suggests increasing theatricality, which could indicate either mechanism — institutional performance (structural) or naturalization (internalized).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_obscured, empirical, 'Structural vs. internalized mechanism for suppression of competing readings.').

omega_variable(
    sibling_reading_kernel_contest,
    'This constraint is one reading of the kernel ''living_language_status''; the sibling readings (liturgical_preservation, native_generation) are alternate framings of what vitality means. Which sibling readings does this reading foreclose, coexist with, or influence?',
    'Analytic inquiry: logical analysis of premise conflicts. Empirical resolution: historical record of how the readings competed and whether they can be held simultaneously in a single intellectual tradition.',
    'Foreclosure relations determine kernel stability. If literary_continuity forecloses both siblings, the kernel is resolved and only this reading survives (native_generation and liturgical_preservation are proven false). If all three coexist, the kernel remains contested and open across the three readings. If literary_continuity influences but does not foreclose (it changes the conditions under which the others operate), the kernel shows a hierarchical structure (one reading dominant, others subordinate).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_kernel_contest, conceptual, 'Logical relationship of this reading to sibling readings: foreclosure, coexistence, or influence structure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(living_language_status__literary_continuity_reading, 1775, 1950).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(livi_tr_t1775, living_language_status__literary_continuity_reading, theater_ratio, 1775, 0.15).
narrative_ontology:measurement_basis(livi_tr_t1775, observed).
narrative_ontology:measurement(livi_tr_t1820, living_language_status__literary_continuity_reading, theater_ratio, 1820, 0.18).
narrative_ontology:measurement_basis(livi_tr_t1820, observed).
narrative_ontology:measurement(livi_tr_t1870, living_language_status__literary_continuity_reading, theater_ratio, 1870, 0.24).
narrative_ontology:measurement_basis(livi_tr_t1870, observed).
narrative_ontology:measurement(livi_tr_t1920, living_language_status__literary_continuity_reading, theater_ratio, 1920, 0.31).
narrative_ontology:measurement_basis(livi_tr_t1920, observed).
narrative_ontology:measurement(livi_tr_t1950, living_language_status__literary_continuity_reading, theater_ratio, 1950, 0.28).
narrative_ontology:measurement_basis(livi_tr_t1950, observed).

% Extraction over time
narrative_ontology:measurement(livi_be_t1775, living_language_status__literary_continuity_reading, base_extractiveness, 1775, 0.25).
narrative_ontology:measurement_basis(livi_be_t1775, observed).
narrative_ontology:measurement(livi_be_t1820, living_language_status__literary_continuity_reading, base_extractiveness, 1820, 0.35).
narrative_ontology:measurement_basis(livi_be_t1820, observed).
narrative_ontology:measurement(livi_be_t1870, living_language_status__literary_continuity_reading, base_extractiveness, 1870, 0.42).
narrative_ontology:measurement_basis(livi_be_t1870, observed).
narrative_ontology:measurement(livi_be_t1920, living_language_status__literary_continuity_reading, base_extractiveness, 1920, 0.48).
narrative_ontology:measurement_basis(livi_be_t1920, observed).
narrative_ontology:measurement(livi_be_t1950, living_language_status__literary_continuity_reading, base_extractiveness, 1950, 0.42).
narrative_ontology:measurement_basis(livi_be_t1950, observed).

% Suppression requirement over time
narrative_ontology:measurement(livi_su_t1775, living_language_status__literary_continuity_reading, suppression_requirement, 1775, 0.22).
narrative_ontology:measurement_basis(livi_su_t1775, observed).
narrative_ontology:measurement(livi_su_t1820, living_language_status__literary_continuity_reading, suppression_requirement, 1820, 0.28).
narrative_ontology:measurement_basis(livi_su_t1820, observed).
narrative_ontology:measurement(livi_su_t1870, living_language_status__literary_continuity_reading, suppression_requirement, 1870, 0.36).
narrative_ontology:measurement_basis(livi_su_t1870, observed).
narrative_ontology:measurement(livi_su_t1920, living_language_status__literary_continuity_reading, suppression_requirement, 1920, 0.42).
narrative_ontology:measurement_basis(livi_su_t1920, observed).
narrative_ontology:measurement(livi_su_t1950, living_language_status__literary_continuity_reading, suppression_requirement, 1950, 0.38).
narrative_ontology:measurement_basis(livi_su_t1950, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(living_language_status__literary_continuity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(living_language_status__literary_continuity_reading, 0.12).
narrative_ontology:affects_constraint(living_language_status__literary_continuity_reading, living_language_status__liturgical_preservation_reading).
narrative_ontology:affects_constraint(living_language_status__literary_continuity_reading, living_language_status__native_generation_reading).

% DUAL FORMULATION NOTE:
% The constraint 'living_language_status' decomposes into three structurally distinct constraints, one per reading: (1) literary_continuity_reading (this story) — defines vitality through secular intellectual and literary production; (2) liturgical_preservation_reading — defines vitality through continuous liturgical transmission and sacred-text study; (3) native_generation_reading — defines vitality through native-speaker transmission in daily life. Each reading instantiates a different constraint with different beneficiaries, victims, extractiveness measures, and classifications. The three stories form a constraint family linked by the shared kernel 'living_language_status'. Each reading has different ε: literary_continuity is lower-extraction (elite coordination around literary authority, lower coercive cost); liturgical_preservation is low-extraction (genuine coordination within religious communities); native_generation is contestable (high extraction if it is used to de-legitimize the other readings, or genuine rope if demographically objective). The sibling readings are SEPARATE constraint stories, not variants of this one. This story authors only the literary_continuity reading as a clean, ε-invariant constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(living_language_status__literary_continuity_reading, powerless, 0.85).
constraint_indexing:directionality_override(living_language_status__literary_continuity_reading, organized, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
