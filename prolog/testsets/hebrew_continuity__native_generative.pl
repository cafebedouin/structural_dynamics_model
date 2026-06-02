% ============================================================================
% CONSTRAINT STORY: hebrew_continuity__native_generative
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_continuity__native_generative, []).

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
 *   constraint_id: hebrew_continuity__native_generative
 *   human_readable: Hebrew Continuity Through Native Speaker Generative Use
 *   domain: sociolinguistics/language_revitalization/commitment_systems
 *
 * SUMMARY:
 *   Hebrew's return from liturgical-only use to a living native-speaker
 *   language represents a unique case of language revitalization grounded in
 *   nationalist, religious, and cultural commitment systems. The
 *   native-generative reading defines Hebrew continuity as requiring native
 *   childhood acquisition and daily generative use — the properties of
 *   'living languages.' This constraint exhibits a core tension between
 *   multiple legitimate continuity modes: liturgical preservation (textual,
 *   ritual, scholarly transmission), bridge-pidginized (simplified
 *   intermediate forms enabling adult learner acquisition), and
 *   native-generative (full revitalization through immersion in native
 *   communities). The native-generative reading benefits from 20th-century
 *   Israeli state power, institutional authority (Hebrew Language Academy),
 *   and the success of the Hebrew revival in creating native speaker
 *   communities. It extracts from those who invested in alternative
 *   continuity modes (diaspora scholars, liturgical communities,
 *   second-language learners) by rendering their Hebrew 'not really Hebrew' —
 *   insufficiently native, insufficiently generative, insufficiently
 *   authentic. Extractiveness has increased over the 50-year interval as the
 *   native-generative definition has become institutional orthodoxy;
 *   suppression has intensified as the authority structure has more firmly
 *   excluded alternatives; theater ratio has decreased as the constraint has
 *   moved from performative ideology toward institutional enforcement through
 *   education, media, and cultural gatekeeping.
 *
 * KEY AGENTS:
 *   - Hebrew Language Academy: Institutional beneficiary (institutional/arbitrage) — custodian of 'authentic' Hebrew; derives authority from native-generative definition; can standardize, expand lexicon, adjudicate legitimacy
 *   - Native Hebrew Speaker Communities (Israel-centered): Primary beneficiary (powerful/arbitrage) — their daily generative use becomes linguistic standard; their intuition defines legitimacy; highest arbitrage options
 *   - Diaspora Hebrew Scholars: Primary victim (powerless/trapped) — scholarly expertise in textual, medieval, biblical Hebrew rendered structurally invisible; cannot exit; defined as non-native by default
 *   - Liturgical-Only Hebrew Communities: Primary victim (powerless/trapped) — intergenerational transmission of liturgical/ritual Hebrew delegitimized as insufficient; structurally redefined as 'not really Hebrew speakers'
 *   - Second-Language Learners and Diaspora Communities: Mixed position (moderate/constrained) — benefit from expanded lexicon and institutional support; constrained by psychological burden of 'acquired' vs. 'native' hierarchy; cannot access full native-speaker status
 *   - Jewish Cultural Pluralists and Diasporic Traditionalists: Organized resistance (organized/constrained) — maintain plural continuity modes; marginalized by hegemonic native-generative definition; constrained from creating alternative institutional authority
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_continuity__native_generative, 0.58).
domain_priors:suppression_score(hebrew_continuity__native_generative, 0.72).
domain_priors:theater_ratio(hebrew_continuity__native_generative, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_continuity__native_generative, extractiveness, 0.58).
narrative_ontology:constraint_metric(hebrew_continuity__native_generative, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(hebrew_continuity__native_generative, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_continuity__native_generative, tangled_rope).
narrative_ontology:human_readable(hebrew_continuity__native_generative, "Hebrew Continuity Through Native Speaker Generative Use").
narrative_ontology:topic_domain(hebrew_continuity__native_generative, "sociolinguistics/language_revitalization/commitment_systems").

domain_priors:requires_active_enforcement(hebrew_continuity__native_generative).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_continuity__native_generative, '974a7e0d-f695-47aa-842f-abb5e029c640').
narrative_ontology:cs_kernel_codification('974a7e0d-f695-47aa-842f-abb5e029c640', formalized).
narrative_ontology:cs_authority_grounding('974a7e0d-f695-47aa-842f-abb5e029c640', extraction).
narrative_ontology:cs_interpretation_layer_present('974a7e0d-f695-47aa-842f-abb5e029c640').
narrative_ontology:cs_reading_relation('974a7e0d-f695-47aa-842f-abb5e029c640', hebrew_continuity__liturgical_preservation, forecloses).
narrative_ontology:cs_reading_relation('974a7e0d-f695-47aa-842f-abb5e029c640', hebrew_continuity__bridge_pidginized, influences).
narrative_ontology:cs_axiom('974a7e0d-f695-47aa-842f-abb5e029c640', foundational, hebrew_requires_native_speakers).
narrative_ontology:cs_axiom_status(hebrew_requires_native_speakers, holdable).
narrative_ontology:cs_axiom_grounding('974a7e0d-f695-47aa-842f-abb5e029c640', hebrew_requires_native_speakers, empirically_contingent).
narrative_ontology:cs_axiom('974a7e0d-f695-47aa-842f-abb5e029c640', foundational, authenticity_through_nativeness).
narrative_ontology:cs_axiom_status(authenticity_through_nativeness, holdable).
narrative_ontology:cs_axiom_grounding('974a7e0d-f695-47aa-842f-abb5e029c640', authenticity_through_nativeness, deontological).
narrative_ontology:cs_reference_frame('974a7e0d-f695-47aa-842f-abb5e029c640', native_speaker_linguistic_revival).
narrative_ontology:cs_drift_state('974a7e0d-f695-47aa-842f-abb5e029c640', contemporary_diaspora_pluralism, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('974a7e0d-f695-47aa-842f-abb5e029c640', '').
narrative_ontology:cs_kernel_id(hebrew_continuity__native_generative, hebrew_continuity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_continuity__native_generative, hebrew_cultural_nationalists).
narrative_ontology:constraint_beneficiary(hebrew_continuity__native_generative, language_academy_authority).
narrative_ontology:constraint_beneficiary(hebrew_continuity__native_generative, native_speaker_communities).
narrative_ontology:constraint_victim(hebrew_continuity__native_generative, liturgical_only_communities).
narrative_ontology:constraint_victim(hebrew_continuity__native_generative, diaspora_hebrew_scholars).
narrative_ontology:constraint_victim(hebrew_continuity__native_generative, second_language_learners).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DIASPORA HEBREW SCHOLAR (SNARE) — Trapped by the definitional collapse of 'real Hebrew' into native-speaker generative use. Scholarly expertise in textual Hebrew, medieval traditions, and liturgical transmission is rendered structurally invisible. Cannot exit — the constraint redefines what counts as legitimate knowledge. Experiences maximum extraction: their professional identity and scholarly work are devalued as the constraint tightens.
constraint_indexing:constraint_classification(hebrew_continuity__native_generative, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: LITURGICAL-ONLY HEBREW COMMUNITIES (SNARE) — Defined structurally as 'not really Hebrew speakers' because their Hebrew is not natively generative daily speech. Their intergenerational transmission of liturgical, textual, and ritual Hebrew is rendered insufficient. Trapped by the redefinition of the kernel: the native-generative reading treats liturgical-only practice as a failed precursor, not a legitimate continuity. Maximum extraction through delegitimation.
constraint_indexing:constraint_classification(hebrew_continuity__native_generative, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: SECOND-LANGUAGE LEARNERS / DIASPORA HEBREW LEARNERS (TANGLED ROPE) — Constrained by resource requirements, access barriers, and the psychological burden of learning a language defined as 'native only.' Also benefit from the expanded lexicon, standardized pronunciation, and institutional support (schools, media, literature) that the native-generative reading produces. Real coordination function (Hebrew as living language enables connection to Israeli culture and Jewish identity) coexists with asymmetric extraction (their Hebrew is always 'acquired,' never native).
constraint_indexing:constraint_classification(hebrew_continuity__native_generative, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: HEBREW LANGUAGE ACADEMY / CULTURAL AUTHORITY (ROPE) — Primary beneficiary. The native-generative reading grants them authority as custodians of 'authentic' Hebrew: they can standardize pronunciation, expand the lexicon, and adjudicate what counts as legitimate Hebrew practice. Experiences the constraint as coordination: defining native speech enables them to guide linguistic evolution. Net beneficiary with arbitrage options — they can adjust standards, influence curriculum, and shape institutional Hebrew norms.
constraint_indexing:constraint_classification(hebrew_continuity__native_generative, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: NATIVE HEBREW SPEAKER COMMUNITIES (ISRAEL-CENTERED) (ROPE) — Primary beneficiary. The native-generative reading grants them implicit authority as the 'authentic' speakers whose intuition defines legitimate Hebrew. Their daily generative use becomes the standard; their linguistic innovations are linguistic evolution, not corruption. Experiences the constraint as coordination: they are solving the problem of maintaining Hebrew as a living language through intergenerational transmission. Net beneficiary with high arbitrage.
constraint_indexing:constraint_classification(hebrew_continuity__native_generative, rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: JEWISH CULTURAL PLURALISTS / DIASPORIC TRADITIONALISTS (ORGANIZED) (TANGLED ROPE) — Organized resistance to the exclusive native-generative framing. They maintain that Hebrew continuity can be plural: liturgical, textual, scholarly, and native-generative modes are all legitimate. The constraint extracts from their position by marginalizing alternatives, but they also benefit from the revitalization's expanded Hebrew ecosystem (media, literature, institutional support). Their exit option is constrained — they can organize alternative communities but not escape the hegemonic definition within mainstream Israeli and diaspora institutions.
constraint_indexing:constraint_classification(hebrew_continuity__native_generative, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURALIZED NECESSITY VIEW (MOUNTAIN) — From a civilizational view, the native-generative requirement appears as a natural linguistic law: all living languages depend on native speaker intuition and daily use for survival. Dead languages lack these properties; living languages possess them. This perspective treats the native-generative boundary as an immutable property of linguistic vitality itself. However, the structural data contradicts this classification — the engine will compute false-summit status, revealing that 'living language' is defined by the native-generative reading, not discovered as a natural property.
constraint_indexing:constraint_classification(hebrew_continuity__native_generative, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_continuity__native_generative_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(hebrew_continuity__native_generative, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hebrew_continuity__native_generative, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(hebrew_continuity__native_generative, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(hebrew_continuity__native_generative_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The native-generative reading extracts through redefinition of legitimacy: diaspora scholars and liturgical communities lose professional/cultural standing, not through overt coercion but through definitional collapse (their Hebrew ceases to count as 'real'). The extraction increased from 0.32 at t=0 (1920s Hebrew revival) to 0.58 at t=50 (contemporary) as institutional authority solidified. Suppression (0.72): High. The suppression has intensified over time as multiple barriers reinforced the native-generative requirement: state educational policy (Hebrew immersion in schools), institutional gatekeeping (Hebrew Language Academy authority), media dominance (Hebrew media assumes native competence), and cultural prestige (native speakers gain status; learners are subordinated). Barriers include linguistic complexity (Hebrew morphology and phonology require native acquisition to master fully), identity fusion (Hebrew 'nativeness' becomes marker of Israeli identity and authenticity), and the psychological burden of permanent learner status. Theater ratio (0.48): Moderate. The constraint has shifted from performative ideology (t=0: 0.62, when revival was aspirational rhetoric) toward functional enforcement (t=50: 0.48). The theatricality has decreased because the native-generative model has succeeded institutionally — the performance of 'authentic native Hebrew' is now backed by state power, educational systems, and cultural gatekeeping. Lower theater indicates higher real enforcement, not symbolic performance.
 *
 * PERSPECTIVAL GAP:
 *   This constraint produces maximum perspectival divergence. Native speaker communities see Rope (coordination enabling linguistic vitality). Institutional authority sees Rope (power to standardize and guide evolution). Diaspora scholars and liturgical communities see Snare (trapped by redefinition, no exit). Second-language learners see Tangled Rope (real benefits from expansion, real extraction from permanent non-native status). Organized pluralists see Tangled Rope (marginalized but still producing Hebrew, benefiting from revitalization infrastructure while resisting hegemonic definition). The analytical observer risks seeing Mountain (native-generative requirement as immutable linguistic law), but structural data reveals false summit: the requirement is a reading of the kernel, not a discovered natural property.
 *
 * DIRECTIONALITY LOGIC:
 *   The native-generative reading creates asymmetric structural positions: native speakers (high arbitrage, low d) experience low extraction chi; diaspora scholars and liturgical communities (trapped, high d) experience maximum extraction; second-language learners (constrained, moderate d) experience moderate extraction. The institutional authority (Hebrew Language Academy) operates at low d (beneficiary status with arbitrage options), deriving power from defining nativeness. The reading's directionality differs from the liturgical_preservation reading: where liturgical preservation values textual transmission and scholarly expertise, native-generative values childhood acquisition and daily use. These are not just different emphases — they define different victim sets and different structural positions for scholarly authority.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint's mandatrophy resolves by clarifying that the native-generative classification (Tangled Rope at base, Rope from beneficiary perspectives, Snare from victim perspectives) is correct for THIS reading of the kernel. The mandatrophy would arise if we tried to force one classification across all readings of Hebrew continuity. But the framework correctly shows that each reading instantiates different structural properties: native-generative has asymmetric extraction (victims trapped by redefinition); liturgical-preservation would show different extraction vectors (victims would be those seeking revitalization, not those maintaining tradition); bridge-pidginized would show different beneficiary/victim alignments. The constraint's classification is stable within its reading; the appearance of mandatrophy dissolves when we recognize we are dealing with three structurally distinct constraints, not one constraint viewed three ways.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    liturgical_transmission_sufficiency,
    'Is intergenerational transmission of liturgical, textual, and ritual Hebrew sufficient to sustain Hebrew as a meaningful continuity, or does native-speaker daily generative use constitute a structurally distinct requirement?',
    'Historical comparison: medieval liturgical Hebrew communities (Yemen, Morocco, Baghdad) vs. contemporary Hebrew revival; analysis of whether liturgical-transmission communities maintained sufficient Hebrew vitality for cultural/religious continuity without native-generative daily use',
    'If sufficient: native-generative reading is one valid continuity mode among several, not the exclusive definition of ''living Hebrew.'' Reclassifies to Rope from multiple perspectives. If not sufficient: native-generative reading is correct that liturgical-only transmission fails to sustain Hebrew; Mountain from analytical perspective is closer to truth.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(liturgical_transmission_sufficiency, empirical, 'Whether liturgical transmission alone can sustain Hebrew continuity').

omega_variable(
    native_speaker_definition_boundary,
    'What defines a ''native speaker'' of Hebrew in the revitalization context? Is it biological acquisition (first language learned in childhood), or functional competence (ability to conduct daily life generatively in Hebrew), or both?',
    'Linguistic anthropology of Hebrew acquisition: What percentage of ''native speakers'' in Israel acquired Hebrew as L1 vs. L2? What acquisition patterns characterize second-generation Israelis with mixed-language home backgrounds? How do communities define nativeness in practice?',
    'If definition includes L2 learners with functional fluency: native-generative reading is accessible to learners; suppression drops substantially. If definition is strict L1-only: native-generative reading is exclusionary by design; suppression remains high.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(native_speaker_definition_boundary, empirical, 'Definitional boundary of ''native speaker'' in Hebrew revitalization').

omega_variable(
    revival_contingency_vs_necessity,
    'Is the native-generative model a contingent historical outcome of 20th-century Israeli nation-building and Zionist ideology, or a necessary structural requirement for any language revival?',
    'Comparative historical study of successful language revivals (Irish, Welsh, Basque, Maori, Hawaiian). Do all revitalized languages require native-speaker communities, or do some sustain vitality through alternative modes (institutional use, written literature, scholarly transmission)?',
    'If contingent: native-generative reading is one choice among alternatives; other readings (liturgical, bridge-pidginized) are viable. Kernel reclassifies to distributed authority. If necessary: native-generative reading captures a universal requirement; other readings fail linguistically.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(revival_contingency_vs_necessity, conceptual, 'Whether native-generative model is historically contingent or universally necessary for language revival').

omega_variable(
    knowledge_production_and_authority,
    'Does the native-generative reading''s authority as ''authentic Hebrew'' depend on excluding non-native linguistic expertise (textual scholarship, historical linguistics, comparative Semitic analysis), or can native and scholarly knowledge coexist as complementary authority structures?',
    'Institutional analysis: Hebrew Language Academy''s actual practice regarding scholarly vs. native-speaker input on standardization decisions. Documentation of whether scholarly linguistic research on biblical/medieval/modern Hebrew is incorporated or marginalized in official standardization processes.',
    'If coexistence is possible: victim set (diaspora scholars) can exit by reframing as complementary expertise; extraction drops. If native-generative authority requires excluding scholarship: the constraint structurally maintains scholarly expertise as illegitimate; Snare classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(knowledge_production_and_authority, empirical, 'Whether native-generative authority requires excluding scholarly linguistic expertise').

omega_variable(
    committer_axiom_foreclosure,
    'Does the native-generative reading''s core axiom (Hebrew continuity requires living native-speaker communities) logically foreclose the liturgical_preservation reading''s axiom (Hebrew continuity can be maintained through textual/liturgical transmission)? Or do these axioms coexist as different commitments within different communities?',
    'Philosophical analysis: Can a single framework (e.g., ''Hebrew is a continuous tradition'') simultaneously hold that Hebrew requires native speakers AND that liturgical-only transmission is sufficient? Or do these axioms represent fundamentally incompatible definitions of continuity?',
    'If foreclosing: reading_relations should be ''forecloses'' (rare). If coexisting: reading_relations should be ''coexists_with.'' This determines whether the three readings represent different legitimate choices or one reading''s logical negation of others.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_axiom_foreclosure, conceptual, 'Logical foreclosure status of native-generative axiom vs. liturgical-preservation axiom').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_continuity__native_generative, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(heb_native_tr_t0, hebrew_continuity__native_generative, theater_ratio, 0, 0.62).
narrative_ontology:measurement(heb_native_tr_t25, hebrew_continuity__native_generative, theater_ratio, 25, 0.55).
narrative_ontology:measurement(heb_native_tr_t50, hebrew_continuity__native_generative, theater_ratio, 50, 0.48).

% Extraction over time
narrative_ontology:measurement(heb_native_be_t0, hebrew_continuity__native_generative, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(heb_native_be_t25, hebrew_continuity__native_generative, base_extractiveness, 25, 0.45).
narrative_ontology:measurement(heb_native_be_t50, hebrew_continuity__native_generative, base_extractiveness, 50, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(heb_native_su_t0, hebrew_continuity__native_generative, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(heb_native_su_t25, hebrew_continuity__native_generative, suppression_requirement, 25, 0.62).
narrative_ontology:measurement(heb_native_su_t50, hebrew_continuity__native_generative, suppression_requirement, 50, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_continuity__native_generative, identity_coordination).
narrative_ontology:affects_constraint(hebrew_continuity__native_generative, hebrew_continuity__liturgical_preservation).
narrative_ontology:affects_constraint(hebrew_continuity__native_generative, hebrew_continuity__bridge_pidginized).
narrative_ontology:affects_constraint(hebrew_continuity__native_generative, israeli_national_identity_commitment).
narrative_ontology:affects_constraint(hebrew_continuity__native_generative, diaspora_jewish_cultural_authority).

% DUAL FORMULATION NOTE:
% The native-generative reading is upstream of the other two readings of Hebrew continuity. It provides the institutional and cultural authority that makes the liturgical-preservation and bridge-pidginized readings appear as alternatives or failures. The three readings form a constraint family linked by network affects. Each has its own ε, its own beneficiary/victim structure, and its own classification profile. They are not three perspectives on one constraint; they are three constraints on one kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(hebrew_continuity__native_generative, analytical, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
