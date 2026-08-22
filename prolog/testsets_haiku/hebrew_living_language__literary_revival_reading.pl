% ============================================================================
% CONSTRAINT STORY: hebrew_living_language__literary_revival_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_living_language__literary_revival_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: hebrew_living_language__literary_revival_reading
 *   human_readable: Hebrew Language Continuity via Haskalah Literary Production
 *   domain: linguistic/cultural/historical
 *
 * SUMMARY:
 *   The Haskalah movement (Jewish Enlightenment, 18th–19th centuries)
 *   constitutes a reading of 'Hebrew living language' where vitality is
 *   instantiated through written intellectual production and generative
 *   literary competence, not through native daily speech. Haskalah writers
 *   like Mendelssohn, Wessely, Krochmal, and others produced philosophy,
 *   poetry, essays, and pedagogy in Hebrew despite speaking Yiddish, German,
 *   or Russian in daily life. This reading treats the constraint as a
 *   coordination mechanism that solves the problem of how dispersed diaspora
 *   intellectuals can participate in European modernity while maintaining
 *   Hebrew as a carrier of textual continuity. No extraction occurs: the
 *   writers labor unpaid; no one is coerced; the alternatives (using only
 *   Hebrew for liturgy, or using only vernacular languages) remain available.
 *   The constraint persists through voluntary participation and cultural
 *   affinity, not enforcement.
 *
 * KEY AGENTS:
 *   - Haskalah writers (agenda-setters): produce original philosophical and literary work in Hebrew despite non-native competence; extend vocabulary and argumentative forms to accommodate modern thought
 *   - Hebrew scholarly community (beneficiaries): read and transmit Haskalah texts; gain intellectual coherence through unbroken written tradition
 *   - Jewish diaspora intellectual class (beneficiaries): access modern intellectual resources in Hebrew; maintain connection to ancestral textual authority
 *   - Liturgical authorities (excluded): would contest the separation of language vitality from religious practice and vernacular mastery
 *   - Native-generation advocates (excluded, later): would argue literary production without native speakers is incomplete revitalization
 *   - European enlightenment institutions (observers): provided publishing and legitimacy infrastructure that made written Hebrew viable as intellectual practice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_living_language__literary_revival_reading, 0.12).
domain_priors:suppression_score(hebrew_living_language__literary_revival_reading, 0.05).
domain_priors:theater_ratio(hebrew_living_language__literary_revival_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_living_language__literary_revival_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(hebrew_living_language__literary_revival_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(hebrew_living_language__literary_revival_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_living_language__literary_revival_reading, accessibility_collapse, 0.15).
narrative_ontology:constraint_metric(hebrew_living_language__literary_revival_reading, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_living_language__literary_revival_reading, rope).
narrative_ontology:human_readable(hebrew_living_language__literary_revival_reading, "Hebrew Language Continuity via Haskalah Literary Production").
narrative_ontology:topic_domain(hebrew_living_language__literary_revival_reading, "linguistic/cultural/historical").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_living_language__literary_revival_reading, '7e5a9e49-4215-4af1-8abc-cc86d7604e93').
narrative_ontology:cs_kernel_codification('7e5a9e49-4215-4af1-8abc-cc86d7604e93', distributed).
narrative_ontology:cs_authority_grounding('7e5a9e49-4215-4af1-8abc-cc86d7604e93', distributed).
narrative_ontology:cs_reading_relation('7e5a9e49-4215-4af1-8abc-cc86d7604e93', hebrew_living_language__liturgical_continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('7e5a9e49-4215-4af1-8abc-cc86d7604e93', hebrew_living_language__native_generation_reading, coexists_with).
narrative_ontology:cs_axiom('7e5a9e49-4215-4af1-8abc-cc86d7604e93', foundational, written_generative_competence_constitutes_vitality).
narrative_ontology:cs_axiom_status(written_generative_competence_constitutes_vitality, holdable).
narrative_ontology:cs_axiom_grounding('7e5a9e49-4215-4af1-8abc-cc86d7604e93', written_generative_competence_constitutes_vitality, conventional).
narrative_ontology:cs_axiom('7e5a9e49-4215-4af1-8abc-cc86d7604e93', secondary, diaspora_dispersal_requires_textual_mediation).
narrative_ontology:cs_axiom_status(diaspora_dispersal_requires_textual_mediation, holdable).
narrative_ontology:cs_axiom_grounding('7e5a9e49-4215-4af1-8abc-cc86d7604e93', diaspora_dispersal_requires_textual_mediation, empirically_contingent).
narrative_ontology:cs_reference_frame('7e5a9e49-4215-4af1-8abc-cc86d7604e93', hebrew_intellectual_culture_through_written_continuity).
narrative_ontology:cs_drift_state('7e5a9e49-4215-4af1-8abc-cc86d7604e93', early_twentieth_century_native_revival_emergence, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('7e5a9e49-4215-4af1-8abc-cc86d7604e93', '2026-06-12T14:23:47Z').
narrative_ontology:cs_kernel_id(hebrew_living_language__literary_revival_reading, hebrew_living_language).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_living_language__literary_revival_reading, haskalah_writers).
narrative_ontology:constraint_beneficiary(hebrew_living_language__literary_revival_reading, hebrew_scholarly_community).
narrative_ontology:constraint_beneficiary(hebrew_living_language__literary_revival_reading, jewish_diaspora_intellectual_class).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Produce literary works, philosophical essays, poetry, and pedagogical texts in Hebrew during the 18th and 19th centuries. They actively choose Hebrew as the medium for intellectual work despite living primarily in vernacular languages (Yiddish, German, Russian, etc.). They maintain and extend Hebrew's written literary register, creating new vocabulary and argumentative forms. They do not speak Hebrew natively in daily life.
narrative_ontology:constraint_stakeholder(hebrew_living_language__literary_revival_reading, haskalah_writers, agenda_setter,
    organized, generational, mobile, global).

% Read, interpret, and transmit Haskalah texts. They benefit from the continuation of Hebrew as a written medium for intellectual exchange and from the extension of its literary range. They gain legitimacy and coherence as a cultural group through the unbroken chain of Hebrew textual production.
narrative_ontology:constraint_stakeholder(hebrew_living_language__literary_revival_reading, hebrew_scholarly_community, beneficiary,
    organized, generational, mobile, global).

% Access a centuries-long tradition of Hebrew intellectual work that connects them to ancestral textual authority (the Hebrew Bible, Talmud, medieval philosophy) while also enabling modern thought in Hebrew. The constraint allows them to participate in modernity without abandoning the Hebrew linguistic-cultural carrier.
narrative_ontology:constraint_stakeholder(hebrew_living_language__literary_revival_reading, jewish_diaspora_intellectual_class, beneficiary,
    moderate, generational, constrained, global).

% Traditional rabbinic and religious authorities who might contest whether written literary production constitutes 'living Hebrew' if unmoored from liturgical and religious practice. They are excluded from this reading's framework because the literary revival reading treats language vitality as independent of religious function.
narrative_ontology:constraint_stakeholder(hebrew_living_language__literary_revival_reading, jewish_liturgical_authorities, excluded,
    institutional, civilizational, trapped, global).

% Later language-revitalization activists (20th century onward) who would argue that living language requires native speakers producing generative daily speech. They view literary production without native speakers as an incomplete or preparatory stage, not a complete living language.
narrative_ontology:constraint_stakeholder(hebrew_living_language__literary_revival_reading, native_generation_advocates, excluded,
    moderate, biographical, mobile, global).

% Universities, publishing houses, journals, and intellectual societies that hosted, published, or engaged with Haskalah texts. They provided legitimacy infrastructure and distribution channels that made written Hebrew production viable as an intellectual practice.
narrative_ontology:constraint_stakeholder(hebrew_living_language__literary_revival_reading, european_enlightenment_institutions, observer,
    institutional, generational, analytical, continental).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains Hebrew as a functional written medium for intellectual exchange, philosophical argumentation, and literary expression across a dispersed diaspora community. Solves the coordination problem of how a geographically scattered, linguistically fragmented Jewish intellectual class can participate in shared textual culture without a common native language.
% TRANSFER_FUNCTION: Transfers cultural authority and continuity from the medieval Hebrew canon (Bible, Talmud, medieval philosophy) into the modern period via the labor of Haskalah writers who extend the literary register and vocabulary to accommodate contemporary ideas. The beneficiaries receive intellectual coherence and historical continuity; the payers (to the extent there is a payment structure) are the writers who invest generative effort in an unpaid or minimally compensated literary project.
% ABSENT_VOICES: Vernacular-only speakers and writers (masses of diaspora Jewish communities whose primary linguistic competence is Yiddish, Ladino, German, Russian) have no seat at this reading because the constraint operates at the elite literary level. Their absence is structural to the reading: the constraint is about written generative competence, not spoken competence or mass participation.
% DISAPPEARANCE_RATIONALE: If Haskalah literary production had not occurred, Hebrew would have persisted through liturgical recitation and scholarly study, but its written register would have ossified around medieval and Talmudic forms without modern philosophical, scientific, or poetic vocabulary. Whether that absence constitutes a loss of 'living Hebrew' or merely a pause in one reading of Hebrew's vitality is precisely the contest embedded in the kernel.
% FOUNDING_PROBLEM: European Jewish intellectual communities in the 18th century faced a legitimacy and coherence crisis: they were being integrated into European intellectual culture and Enlightenment thought, but the Hebrew textual tradition — which grounded their cultural identity and access to religious authority — belonged to medieval and ancient registers. How could they be modern thinkers while maintaining Hebrew as a carrier of their tradition?
% FOUNDING_PROBLEM_CORROBORATION: The Haskalah writers themselves (Mendelssohn, Wessely, and others) explicitly attest this founding problem in their prefaces and programmatic essays: they present Hebrew literary revival as enabling Jews to 'enter European culture while keeping the treasures of our language.' Later historians of Hebrew language and Jewish intellectual history (not themselves Haskalah beneficiaries, e.g., Fishman, Rabin, Harshav writing in 20th-century academic contexts) corroborate that the problem was real and acute. However, liturgical-continuity advocates and later native-generation advocates contest that the founding problem required literary revival rather than renewed emphasis on liturgical mastery or eventually native speech.
narrative_ontology:disappearance_verdict(hebrew_living_language__literary_revival_reading, contested).
narrative_ontology:founding_problem_status(hebrew_living_language__literary_revival_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_living_language__literary_revival_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(hebrew_living_language__literary_revival_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_living_language__literary_revival_reading, 0.12, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_living_language__literary_revival_reading_tests).
:- end_tests(hebrew_living_language__literary_revival_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very low (0.12) because the constraint operates through voluntary participation and cultural commitment, not leverage or coercion. Writers choose Hebrew; no one is forced to read or write it; economic benefit is negligible (literary publication in Hebrew was subsistence or volunteer work). Suppression is minimal (0.05) because there is no need to prevent exit or suppress alternatives — the constraint's persistence depends on continuing desire for Hebrew literary culture, not on blocking competing languages. Theater is moderate (0.22) because the written literary production serves both genuine coordination (maintaining Hebrew as shared intellectual medium) and performative cultural signaling (demonstrating that modern Jews could be sophisticated European intellectuals while keeping Hebrew). Accessibility collapse is very low (0.15) because alternatives persist visibly throughout the interval: Yiddish, German, Russian, French all remain available as intellectual media; choosing Hebrew is not forced by structural collapse of alternatives. Resistance is low (0.08) because the constraint meets little organized opposition — it is celebrated by its participants as a cultural achievement, even if contested by liturgical authorities or later native-generation advocates. The measurement series shows very slight drift over 160 years: extractiveness and theater ratio rise marginally as the Haskalah becomes more ambitious and more conscious of its cultural-political role, but the core structure remains stable and non-extractive throughout.
 *
 * PERSPECTIVAL GAP:
 *   The perspective divergence in this constraint is between the reading itself (literary production = living language) and the sibling readings (liturgical recitation = living language, or native daily speech = living language). From the literary-revival seat, the Haskalah constraint is a coordination success: it kept Hebrew intellectually viable during the modern transition. From the liturgical-continuity seat, Haskalah is a partial reading that treats writing as the whole of vitality while ignoring the unbroken chain of daily liturgical use. From the native-generation seat, Haskalah is a necessary but incomplete stage — preparation for true revival only when speakers become native. These are not computational divergences within a single seat; they are alternative framings of the kernel itself. The engine does not compute seat divergence here because the constraint structure does not support multiple seats at different power levels; it is a single-layer elite intellectual practice.
 *
 * DIRECTIONALITY LOGIC:
 *   There is no extractiveness-driven directionality in this constraint because no one is targetable as a net victim. Haskalah writers benefit (cultural coherence, intellectual platform) but also labor without compensation; they cannot easily be classified as purely beneficiary or purely victimized. The scholarly community and diaspora intellectuals benefit without bearing costs (they inherit the literary tradition without having to produce it). Excluded parties (liturgical authorities, vernacular speakers) do not participate, so directionality does not apply to them. This constraint lacks the asymmetry that drives power-indexed directionality. It is organized around shared commitment to a cultural practice, not around extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is NOT in mandatrophy. The founding problem (How can modern Jews engage European thought while maintaining Hebrew linguistic-cultural identity?) remains live throughout the Haskalah period and beyond. Writers continue to produce Hebrew literature at the interval's end because the problem persists and the solution (literary production) remains valued. There is no zombie-constraint effect where enforcement persists after the function atrophies. The later native-generation reading does eventually supersede the literary-revival reading (by the early 20th century, Hebrew-speaking communities exist and native speech becomes possible), but that supersession is a historical transition, not mandatrophy within this constraint. Mandatrophy would require that the literary-revival constraint persist into a period when no one cares about Hebrew literary continuity through Haskalah texts — and that did not occur.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    literacy_as_generativity_boundary,
    'Does generative competence demonstrated through written literary production constitute the same kind of language vitality as native daily speech?',
    'Linguistic analysis of Haskalah Hebrew: does it show productive morphosyntactic innovation and semantic extension, or mostly recombination of canonical forms? Comparative analysis with native-speaker innovations in later 20th-century Hebrew.',
    'If Haskalah Hebrew shows high generative innovation, the literary-revival reading''s claim to language vitality is strengthened. If it shows mostly canonical recombination, the reading becomes more about cultural continuity than linguistic creativity, and native-generation advocates gain ground.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(literacy_as_generativity_boundary, empirical, 'Whether Haskalah literary production demonstrates genuine linguistic generativity or primarily canonical recombination.').

omega_variable(
    kernel_identity_across_readings,
    'Do all three readings (literary, liturgical, native-generation) refer to the same ''living language'' phenomenon, or are they measuring different things that merely share a label?',
    'Definitional archaeology: trace how ''living language'' is defined in each reading''s source texts (Haskalah prefaces, liturgical rabbinic literature, native-generation manifestos). Do they dispute the same question or ask different questions?',
    'If they ask different questions, the kernel does not genuinely pit the readings against each other; they are three separate constraints given the same name (ε-invariance problem). If they dispute the same question, the readings are genuine alternatives and the contest is real.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_identity_across_readings, conceptual, 'Whether the kernel genuinely contains three competing readings of one phenomenon, or three separate phenomena accidentally named the same.').

omega_variable(
    performance_vs_naturalization_in_reading_choice,
    'Is the literary-revival reading an accurate description of how Hebrew remained viable during the Haskalah, or a post-hoc narrative construction by scholars (and the reading''s own participants) that naturalizes cultural choices as linguistic necessity?',
    'Historical investigation of Haskalah writers'' own stated motivations: do they frame Hebrew literary work as necessary for language survival, or as cultural/political signaling? Do they exhibit anxiety about Hebrew vitality, or confidence in its continuation through other means (liturgy, study)?',
    'If writers framed their work as language survival, the reading is self-aware and defensible. If they did not, the reading may be a later naturalization that ascribes more linguistic function to literary production than was originally intended — shifting it toward performance rather than genuine coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(performance_vs_naturalization_in_reading_choice, empirical, 'Whether the literary-revival reading accurately represents historical actors'' own understanding, or imposes a later linguistic logic on cultural choices.').

omega_variable(
    sibling_reading_foreclosure_possibility,
    'Given that native Hebrew speakers eventually emerge (early 20th century), does the existence of the native_generation_reading logically foreclose the literary_revival_reading, or can both readings remain coherent for different historical periods?',
    'Definitional: if native speech exists, can we still say Hebrew ''lives'' through literary production, or does it now live through native speech (in which case literary production becomes secondary)? Does the kernel accommodate temporal phases where different readings dominate?',
    'If readings are period-specific and not universally asserting a single truth about language vitality, they coexist_with. If the native-generation reading asserts universal priority once native speakers exist, it may foreclose the literary reading retroactively (a form of conceptual foreclosure rather than logical foreclosure). This affects how the constraint is classified as native-speaker Hebrew revitalization proceeds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure_possibility, conceptual, 'Whether the native-generation reading logically forecloses the literary-revival reading once native speakers exist, or whether both can remain valid for different purposes/periods.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_living_language__literary_revival_reading, 1740, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t1740, hebrew_living_language__literary_revival_reading, theater_ratio, 1740, 0.18).
narrative_ontology:measurement_basis(hebr_tr_t1740, projected).
narrative_ontology:measurement(hebr_tr_t1780, hebrew_living_language__literary_revival_reading, theater_ratio, 1780, 0.19).
narrative_ontology:measurement_basis(hebr_tr_t1780, observed).
narrative_ontology:measurement(hebr_tr_t1820, hebrew_living_language__literary_revival_reading, theater_ratio, 1820, 0.21).
narrative_ontology:measurement_basis(hebr_tr_t1820, observed).
narrative_ontology:measurement(hebr_tr_t1860, hebrew_living_language__literary_revival_reading, theater_ratio, 1860, 0.24).
narrative_ontology:measurement_basis(hebr_tr_t1860, observed).
narrative_ontology:measurement(hebr_tr_t1880, hebrew_living_language__literary_revival_reading, theater_ratio, 1880, 0.23).
narrative_ontology:measurement_basis(hebr_tr_t1880, observed).
narrative_ontology:measurement(hebr_tr_t1900, hebrew_living_language__literary_revival_reading, theater_ratio, 1900, 0.22).
narrative_ontology:measurement_basis(hebr_tr_t1900, observed).

% Extraction over time
narrative_ontology:measurement(hebr_be_t1740, hebrew_living_language__literary_revival_reading, base_extractiveness, 1740, 0.08).
narrative_ontology:measurement_basis(hebr_be_t1740, projected).
narrative_ontology:measurement(hebr_be_t1780, hebrew_living_language__literary_revival_reading, base_extractiveness, 1780, 0.1).
narrative_ontology:measurement_basis(hebr_be_t1780, observed).
narrative_ontology:measurement(hebr_be_t1820, hebrew_living_language__literary_revival_reading, base_extractiveness, 1820, 0.12).
narrative_ontology:measurement_basis(hebr_be_t1820, observed).
narrative_ontology:measurement(hebr_be_t1860, hebrew_living_language__literary_revival_reading, base_extractiveness, 1860, 0.13).
narrative_ontology:measurement_basis(hebr_be_t1860, observed).
narrative_ontology:measurement(hebr_be_t1880, hebrew_living_language__literary_revival_reading, base_extractiveness, 1880, 0.12).
narrative_ontology:measurement_basis(hebr_be_t1880, observed).
narrative_ontology:measurement(hebr_be_t1900, hebrew_living_language__literary_revival_reading, base_extractiveness, 1900, 0.12).
narrative_ontology:measurement_basis(hebr_be_t1900, observed).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t1740, hebrew_living_language__literary_revival_reading, suppression_requirement, 1740, 0.04).
narrative_ontology:measurement_basis(hebr_su_t1740, projected).
narrative_ontology:measurement(hebr_su_t1780, hebrew_living_language__literary_revival_reading, suppression_requirement, 1780, 0.04).
narrative_ontology:measurement_basis(hebr_su_t1780, observed).
narrative_ontology:measurement(hebr_su_t1820, hebrew_living_language__literary_revival_reading, suppression_requirement, 1820, 0.05).
narrative_ontology:measurement_basis(hebr_su_t1820, observed).
narrative_ontology:measurement(hebr_su_t1860, hebrew_living_language__literary_revival_reading, suppression_requirement, 1860, 0.06).
narrative_ontology:measurement_basis(hebr_su_t1860, observed).
narrative_ontology:measurement(hebr_su_t1880, hebrew_living_language__literary_revival_reading, suppression_requirement, 1880, 0.05).
narrative_ontology:measurement_basis(hebr_su_t1880, observed).
narrative_ontology:measurement(hebr_su_t1900, hebrew_living_language__literary_revival_reading, suppression_requirement, 1900, 0.05).
narrative_ontology:measurement_basis(hebr_su_t1900, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_living_language__literary_revival_reading, information_standard).
narrative_ontology:boltzmann_floor_override(hebrew_living_language__literary_revival_reading, 0.05).
narrative_ontology:affects_constraint(hebrew_living_language__literary_revival_reading, hebrew_living_language__liturgical_continuity_reading).
narrative_ontology:affects_constraint(hebrew_living_language__literary_revival_reading, hebrew_living_language__native_generation_reading).

% DUAL FORMULATION NOTE:
% The hebrew_living_language kernel constrains three distinct readings of what constitutes Hebrew remaining a living language during diaspora. The literary_revival_reading (this constraint) treats vitality as written generative intellectual production; the liturgical_continuity_reading treats it as unbroken daily liturgical recitation and study; the native_generation_reading treats it as generative daily native speech. These readings are not alternative observations of one constraint; they are three separate constraints on the same kernel. Each has its own epsilon, beneficiary/victim structure, and measured type. They are linked via affects_constraints to register their mutual influence on the kernel's interpretation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
