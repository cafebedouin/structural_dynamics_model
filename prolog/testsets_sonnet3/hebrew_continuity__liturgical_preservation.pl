% ============================================================================
% CONSTRAINT STORY: hebrew_continuity__liturgical_preservation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_continuity__liturgical_preservation, []).

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
 *   constraint_id: hebrew_continuity__liturgical_preservation
 *   human_readable: Hebrew Continuity via Liturgical Preservation and Textual Transmission
 *   domain: sociolinguistics/religious institutions
 *
 * SUMMARY:
 *   This constraint instantiates one reading of the contested 'Hebrew
 *   continuity' kernel: the claim that Hebrew persists as a living linguistic
 *   entity through preserved ritual recitation and textual transmission,
 *   requiring zero native speakers. Under this reading, diaspora institutions
 *   (rabbinic authorities, seminaries, textual scholarship) maintain a
 *   specialized liturgical register whose fidelity to received tradition, not
 *   generative daily use, constitutes the language's continuity. The rise of
 *   theater ratio over the measured interval reflects growing performative
 *   recitation (bar/bat mitzvah Hebrew reading, ceremonial fluency)
 *   increasingly decoupled from comprehension, even as institutional
 *   insistence on the register's necessity persists. Suppression declines
 *   slightly over time as translation technology and transliteration aids
 *   reduce the practical cost of non-fluency, even while the institutional
 *   requirement to demonstrate liturgical competence for full communal
 *   participation remains largely intact.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_continuity__liturgical_preservation, 0.38).
domain_priors:suppression_score(hebrew_continuity__liturgical_preservation, 0.42).
domain_priors:theater_ratio(hebrew_continuity__liturgical_preservation, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_continuity__liturgical_preservation, extractiveness, 0.38).
narrative_ontology:constraint_metric(hebrew_continuity__liturgical_preservation, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(hebrew_continuity__liturgical_preservation, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_continuity__liturgical_preservation, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(hebrew_continuity__liturgical_preservation, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_continuity__liturgical_preservation, tangled_rope).
narrative_ontology:human_readable(hebrew_continuity__liturgical_preservation, "Hebrew Continuity via Liturgical Preservation and Textual Transmission").
narrative_ontology:topic_domain(hebrew_continuity__liturgical_preservation, "sociolinguistics/religious institutions").

domain_priors:requires_active_enforcement(hebrew_continuity__liturgical_preservation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_continuity__liturgical_preservation, '3b3a47f9-0937-47aa-9b82-df84dd67c083').
narrative_ontology:cs_kernel_codification('3b3a47f9-0937-47aa-9b82-df84dd67c083', fixed_text).
narrative_ontology:cs_authority_grounding('3b3a47f9-0937-47aa-9b82-df84dd67c083', lineage).
narrative_ontology:cs_interpretation_layer_present('3b3a47f9-0937-47aa-9b82-df84dd67c083').
narrative_ontology:cs_reading_relation('3b3a47f9-0937-47aa-9b82-df84dd67c083', hebrew_continuity__native_generative, coexists_with).
narrative_ontology:cs_reading_relation('3b3a47f9-0937-47aa-9b82-df84dd67c083', hebrew_continuity__bridge_pidginized, influences).
narrative_ontology:cs_axiom('3b3a47f9-0937-47aa-9b82-df84dd67c083', foundational, textual_fidelity_constitutes_linguistic_life).
narrative_ontology:cs_axiom_status(textual_fidelity_constitutes_linguistic_life, holdable).
narrative_ontology:cs_axiom_grounding('3b3a47f9-0937-47aa-9b82-df84dd67c083', textual_fidelity_constitutes_linguistic_life, conventional).
narrative_ontology:cs_axiom('3b3a47f9-0937-47aa-9b82-df84dd67c083', foundational, native_speaker_generativity_not_required_for_continuity).
narrative_ontology:cs_axiom_status(native_speaker_generativity_not_required_for_continuity, holdable).
narrative_ontology:cs_axiom_grounding('3b3a47f9-0937-47aa-9b82-df84dd67c083', native_speaker_generativity_not_required_for_continuity, conventional).
narrative_ontology:cs_reference_frame('3b3a47f9-0937-47aa-9b82-df84dd67c083', post_temple_diaspora_textual_register).
narrative_ontology:cs_drift_state('3b3a47f9-0937-47aa-9b82-df84dd67c083', post_israeli_revival_contemporary, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('3b3a47f9-0937-47aa-9b82-df84dd67c083', '').
narrative_ontology:cs_kernel_id(hebrew_continuity__liturgical_preservation, hebrew_continuity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_continuity__liturgical_preservation, rabbinic_authorities).
narrative_ontology:constraint_beneficiary(hebrew_continuity__liturgical_preservation, religious_educational_institutions).
narrative_ontology:constraint_beneficiary(hebrew_continuity__liturgical_preservation, textual_scholarship_class).
narrative_ontology:constraint_victim(hebrew_continuity__liturgical_preservation, secularizing_diaspora_communities).
narrative_ontology:constraint_victim(hebrew_continuity__liturgical_preservation, non_liturgically_fluent_congregants).
narrative_ontology:constraint_victim(hebrew_continuity__liturgical_preservation, vernacular_hebrew_revivalists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set and enforce the standard for what counts as correct liturgical Hebrew recitation, control ordination and interpretive authority, and adjudicate which textual variants and pronunciations are legitimate. Their institutional standing depends on Hebrew remaining a specialized, textually-anchored register that only the trained can properly transmit.
narrative_ontology:constraint_stakeholder(hebrew_continuity__liturgical_preservation, rabbinic_authorities, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Yeshivot, day schools, and seminaries build curricula, credentials, and revenue around teaching liturgical Hebrew literacy. Their continued relevance depends on Hebrew being preserved as a register requiring years of formal instruction rather than something acquired informally.
narrative_ontology:constraint_stakeholder(hebrew_continuity__liturgical_preservation, religious_educational_institutions, beneficiary,
    organized, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(hebrew_continuity__liturgical_preservation, religious_educational_institutions, agenda_setter).

% Academics, commentators, and liturgical scholars derive professional standing and institutional funding from Hebrew's textual continuity. They collect prestige and employment from maintaining and interpreting the corpus; their expertise is devalued if the language migrates toward vernacular generativity that bypasses textual mediation.
narrative_ontology:constraint_stakeholder(hebrew_continuity__liturgical_preservation, textual_scholarship_class, beneficiary,
    organized, civilizational, mobile, global).

% Diaspora Jews with declining ritual observance still encounter Hebrew as a gatekeeping requirement for full participation in communal life-cycle events, prayer, and text study. They bear the cost of maintaining fluency in a register they rarely use generatively, or else face exclusion from full ritual participation; opting out entirely often means leaving the institutional community altogether.
narrative_ontology:constraint_stakeholder(hebrew_continuity__liturgical_preservation, secularizing_diaspora_communities, payer,
    moderate, biographical, constrained, global).

% Congregants who never acquired fluent liturgical Hebrew sit through services they cannot fully follow, relying on transliteration and translation aids. They pay socially and spiritually — reduced participation, dependency on intermediaries — without practical means to close the gap short of years of study they may not have access to or time for.
narrative_ontology:constraint_stakeholder(hebrew_continuity__liturgical_preservation, non_liturgically_fluent_congregants, payer,
    powerless, biographical, trapped, local).

% Advocates of spoken, generative Hebrew (largely aligned with the Israeli native-speaker reading of the kernel) argue liturgical preservation ossifies the language into a museum piece disconnected from living use. They are structurally sidelined in diaspora religious institutions, which privilege textual correctness over generative competence; their claim that the language should be judged by daily creative use rather than recitation fidelity gets little institutional hearing in synagogue contexts.
narrative_ontology:constraint_stakeholder(hebrew_continuity__liturgical_preservation, vernacular_hebrew_revivalists, excluded,
    moderate, generational, constrained, national).

% Linguists and historians of Jewish languages document how liturgical Hebrew persisted for nearly two millennia without native speakers, evaluating whether this constitutes genuine language continuity or a distinct phenomenon (textual/ritual preservation) that only later fused with the revived vernacular in modern Israel.
narrative_ontology:constraint_stakeholder(hebrew_continuity__liturgical_preservation, sociolinguistic_observers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hebrew_continuity__liturgical_preservation, diffuse).
narrative_ontology:fixing_cost_class(hebrew_continuity__liturgical_preservation, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides diaspora Jewish communities across radically different vernacular environments with a stable, mutually intelligible liturgical register — anyone trained in the tradition anywhere in the world can participate in shared prayer, textual study, and ritual regardless of local spoken language.
% TRANSFER_FUNCTION: Moves interpretive authority, ordination gatekeeping power, and institutional revenue (tuition, communal dues tied to religious education) from ordinary community members toward rabbinic authorities, educational institutions, and the scholarly class that certifies correct transmission.
% ABSENT_VOICES: Vernacular Hebrew revivalists who hold that a language lives through generative daily use, not textual recitation, are largely absent from diaspora liturgical governance — their framework would judge liturgical Hebrew's continuity claim as a category error, but they have no seat in synagogue ritual committees or seminary curricula.
% DISAPPEARANCE_RATIONALE: If liturgical preservation collapsed — if textual transmission and recitation training ceased — the institutional architecture built around Hebrew literacy (seminaries, ordination pathways, textual scholarship careers, communal gatekeeping around ritual competence) would lose its object and would have to reorganize around either full vernacularization or translation-based practice, materially changing who holds authority in Jewish communal life.
% FOUNDING_PROBLEM: After the loss of Hebrew as a widely spoken vernacular in antiquity and the dispersal of Jewish communities across incompatible host languages, a shared textual/liturgical Hebrew register was maintained to preserve access to scripture, prayer, and legal texts across geographically and linguistically fragmented diaspora communities.
% FOUNDING_PROBLEM_CORROBORATION: Historians of Jewish diaspora languages (a source outside the beneficiary class) corroborate that liturgical Hebrew solved a real coordination problem for over a thousand years when communities had no other shared tongue. However, since the emergence of a robust native-speaking Hebrew population in Israel and near-universal availability of vernacular translations, independent sociolinguists note the original coordination problem (enabling any diaspora Jew to communicate in a shared tongue) is largely solved by other means — modern translation technology and the existence of a living vernacular elsewhere — while the liturgical apparatus persists institutionally regardless.
narrative_ontology:disappearance_verdict(hebrew_continuity__liturgical_preservation, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_continuity__liturgical_preservation, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_continuity__liturgical_preservation, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(hebrew_continuity__liturgical_preservation, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_continuity__liturgical_preservation, 0.38, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_continuity__liturgical_preservation_tests).
:- end_tests(hebrew_continuity__liturgical_preservation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.38 by interval end) — this is not primarily a rent-extraction structure but a credentialing and gatekeeping structure: institutions capture tuition, deference, and interpretive authority from a population that mostly cannot verify whether textual fidelity actually requires the institutional apparatus maintaining it. Suppression is moderate-declining (0.42) because exit is genuinely possible (translation, transliteration, secular practice) but socially costly within observant communities. Theater ratio rises over time (0.10 to 0.30) as recitation competence increasingly substitutes for comprehension — a Goodhart-style drift where 'can recite the blessing correctly' replaces 'understands and generates the language,' which is exactly the phenomenon the sibling native_generative reading would flag as counterfeit continuity.
 *
 * DIRECTIONALITY LOGIC:
 *   Rabbinic authorities and educational institutions sit at the beneficiary end: they set the standard for correctness, control credentialing, and collect tuition/deference contingent on the register's continued specialization. Ordinary congregants and secularizing communities sit toward the target end: they bear the social and educational cost of maintaining or demonstrating liturgical competence without proportional benefit, and their exit options are constrained by communal belonging rather than truly free. Vernacular revivalists are excluded outright — their competing account of what 'living language' means gets no institutional hearing in this reading's governing bodies.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (a shared register for a linguistically fragmented diaspora) is genuinely contested as live vs. dead: it was clearly live for over a millennium and is substantially weaker now that translation tools and a living vernacular elsewhere exist, yet the institutional apparatus (seminaries, ordination, credentialing) persists at full strength regardless of the founding problem's current status. This is the tangled_rope signature exactly: real coordination function historically, real asymmetric extraction now (tuition, deference, gatekeeping) riding on the same structure, requiring active enforcement (ordination standards, ritual-competence requirements for full communal participation) to hold. Classifying this as pure snare would erase the genuine historical coordination value; classifying it as pure rope would erase the documented capture by credentialing institutions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    liturgical_vs_native_kernel_occupancy,
    'Does the kernel ''Hebrew continuity'' admit multiple simultaneous valid occupants (liturgical AND native-generative), or does the emergence of a robust native-speaking population in Israel retroactively demote the liturgical-only claim to a weaker, derivative status?',
    'Track institutional and scholarly consensus in sociolinguistics: does the field treat pre-revival liturgical Hebrew and post-revival Israeli Hebrew as the same language''s continuous life, or as two structurally distinct phenomena (preserved textual register vs. revived vernacular) that happen to share a name and corpus?',
    'If the field treats them as the same continuous entity, this reading''s claim to constitute ''Hebrew being alive'' independently is weakened — it becomes parasitic on the native_generative reading''s success. If treated as genuinely distinct phenomena, this reading stands on its own historical footing regardless of the 20th-century revival.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(liturgical_vs_native_kernel_occupancy, conceptual, 'Whether liturgical preservation is an independent kernel occupant or derivative of native revival.').

omega_variable(
    credentialing_capture_vs_genuine_transmission_need,
    'Is the tuition, deference, and gatekeeping captured by rabbinic and educational institutions a necessary cost of transmitting a genuinely difficult textual tradition, or does it substantially exceed what transmission requires?',
    'Compare communities/movements with lighter-weight, less credentialed Hebrew literacy instruction (e.g., some Reform or havurah-movement approaches) against traditional yeshiva-track credentialing on measured textual competence outcomes per unit of cost/time invested.',
    'If lighter-weight approaches achieve comparable competence, the additional institutional overhead in traditional credentialing structures is better characterized as extraction; if not, it supports genuine coordination-cost necessity, shifting classification weight from tangled_rope toward rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(credentialing_capture_vs_genuine_transmission_need, empirical, 'Whether institutional credentialing overhead is necessary transmission cost or captured rent.').

omega_variable(
    secularization_as_victim_or_natural_attrition,
    'Is declining diaspora Hebrew liturgical fluency better modeled as this constraint''s victims bearing an unjust cost (as authored), or as natural secular drift that the liturgical apparatus is simply failing to prevent (making ''secularizing forces'' the threat, not the beneficiary)?',
    'Distinguish cases where individuals actively want liturgical participation but are excluded by competence barriers (supports victim framing) from cases where individuals have no interest in liturgical participation at all (supports natural-attrition framing, no victim relationship to this constraint).',
    'If most non-fluent congregants fall into the second category, the victim declaration for non_liturgically_fluent_congregants should be narrowed or the extractiveness score revised downward, since the constraint would not be actively extracting from people who were never seeking to pay its cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(secularization_as_victim_or_natural_attrition, conceptual, 'Whether secularizing congregants are victims of the constraint or unrelated to it.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_continuity__liturgical_preservation, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t0, hebrew_continuity__liturgical_preservation, theater_ratio, 0, 0.1).
narrative_ontology:measurement(hebr_tr_t20, hebrew_continuity__liturgical_preservation, theater_ratio, 20, 0.14).
narrative_ontology:measurement(hebr_tr_t40, hebrew_continuity__liturgical_preservation, theater_ratio, 40, 0.18).
narrative_ontology:measurement(hebr_tr_t60, hebrew_continuity__liturgical_preservation, theater_ratio, 60, 0.22).
narrative_ontology:measurement(hebr_tr_t80, hebrew_continuity__liturgical_preservation, theater_ratio, 80, 0.26).
narrative_ontology:measurement(hebr_tr_t100, hebrew_continuity__liturgical_preservation, theater_ratio, 100, 0.3).

% Extraction over time
narrative_ontology:measurement(hebr_be_t0, hebrew_continuity__liturgical_preservation, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(hebr_be_t20, hebrew_continuity__liturgical_preservation, base_extractiveness, 20, 0.26).
narrative_ontology:measurement(hebr_be_t40, hebrew_continuity__liturgical_preservation, base_extractiveness, 40, 0.3).
narrative_ontology:measurement(hebr_be_t60, hebrew_continuity__liturgical_preservation, base_extractiveness, 60, 0.33).
narrative_ontology:measurement(hebr_be_t80, hebrew_continuity__liturgical_preservation, base_extractiveness, 80, 0.36).
narrative_ontology:measurement(hebr_be_t100, hebrew_continuity__liturgical_preservation, base_extractiveness, 100, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t0, hebrew_continuity__liturgical_preservation, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(hebr_su_t20, hebrew_continuity__liturgical_preservation, suppression_requirement, 20, 0.48).
narrative_ontology:measurement(hebr_su_t40, hebrew_continuity__liturgical_preservation, suppression_requirement, 40, 0.46).
narrative_ontology:measurement(hebr_su_t60, hebrew_continuity__liturgical_preservation, suppression_requirement, 60, 0.44).
narrative_ontology:measurement(hebr_su_t80, hebrew_continuity__liturgical_preservation, suppression_requirement, 80, 0.43).
narrative_ontology:measurement(hebr_su_t100, hebrew_continuity__liturgical_preservation, suppression_requirement, 100, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_continuity__liturgical_preservation, identity_coordination).
narrative_ontology:boltzmann_floor_override(hebrew_continuity__liturgical_preservation, 0.1).
narrative_ontology:affects_constraint(hebrew_continuity__liturgical_preservation, hebrew_continuity__native_generative).
narrative_ontology:affects_constraint(hebrew_continuity__liturgical_preservation, hebrew_continuity__bridge_pidginized).

% DUAL FORMULATION NOTE:
% This story is one of three sibling constraints occupying the contested hebrew_continuity kernel. liturgical_preservation (this story) locates Hebrew's continuity in textual/ritual fidelity and authors a tangled_rope structure with rabbinic/educational beneficiaries and secularizing-community victims. native_generative locates continuity in daily generative native use and would author a different structure entirely (likely rope or mountain-adjacent, given Israeli Hebrew's status as a genuinely living vernacular with minimal identifiable victims). bridge_pidginized occupies a middle position as a contact-language reading. Each carries its own ε, beneficiary/victim sets, and claimed_type; they are linked here rather than merged because the underlying claims about what makes a language 'alive' are structurally incompatible measurement bases, not three views of one fact.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
