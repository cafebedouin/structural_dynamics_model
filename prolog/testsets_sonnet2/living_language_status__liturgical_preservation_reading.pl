% ============================================================================
% CONSTRAINT STORY: living_language_status__liturgical_preservation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_living_language_status__liturgical_preservation_reading, []).

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
 *   constraint_id: living_language_status__liturgical_preservation_reading
 *   human_readable: Liturgical Transmission as Sufficient Condition for Hebrew's Living Status
 *   domain: sociolinguistics/religious_studies/nationalism_studies
 *
 * SUMMARY:
 *   This constraint isolates the liturgical-preservation reading of the
 *   contested kernel 'living language status' as applied to Hebrew: the claim
 *   that continuous recitation, study, and ritual use of sacred texts is
 *   SUFFICIENT to establish a language as living, independent of vernacular
 *   transmission. For most of the diaspora period this criterion described a
 *   real coordination achievement — dispersed communities across centuries
 *   and continents maintained a shared linguistic and religious standard
 *   without any common vernacular. After the 19th-20th century Hebrew revival
 *   produced a native speech community, the same criterion persists but now
 *   does structural work beyond coordination: it allows rabbinical and
 *   yeshiva institutions to retain interpretive authority over 'authentic'
 *   Hebrew regardless of the existence of millions of native vernacular
 *   speakers, and it recasts secular vernacular usage as a departure from or
 *   desecration of the true (liturgical) language rather than as the
 *   language's most vigorous contemporary form. The extraction here is low
 *   but real and enforcement-dependent (rising over time as vernacular
 *   Hebrew's dominance made the liturgical-sufficiency claim increasingly
 *   contestable and required more active assertion to sustain). This story is
 *   DELIBERATELY narrow: it does not evaluate whether Hebrew is 'really'
 *   alive, only the structural consequences of this ONE reading. The
 *   literary-continuity reading and the native-generation reading are
 *   separate constraints with different beneficiary/victim structures and
 *   different ε values — see kernel_context.
 *
 * KEY AGENTS:
 *   - rabbinical_authority: agenda-setter and beneficiary, institutional power, arbitrage exit — administers the liturgical-sufficiency standard and draws authority from it
 *   - yeshiva_institutions: beneficiary, organized power — institutional survival tied to liturgical study remaining the recognized site of linguistic vitality
 *   - secular_speech_community: payer, moderate power, constrained exit — native vernacular fluency delegitimized as evidence of the language's life under this reading
 *   - hebraist_revivalists: payer, moderate power — revivalist historiography undermined by a criterion that says the language was never dead
 *   - diaspora_liturgical_communities: beneficiary and payer both — validated by the reading but also bound entirely to liturgical fidelity for their standing
 *   - comparative_linguists: observer, analytical power — supplies the cross-linguistic comparison class other readings invoke
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(living_language_status__liturgical_preservation_reading, 0.28).
domain_priors:suppression_score(living_language_status__liturgical_preservation_reading, 0.42).
domain_priors:theater_ratio(living_language_status__liturgical_preservation_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(living_language_status__liturgical_preservation_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(living_language_status__liturgical_preservation_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(living_language_status__liturgical_preservation_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(living_language_status__liturgical_preservation_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(living_language_status__liturgical_preservation_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(living_language_status__liturgical_preservation_reading, tangled_rope).
narrative_ontology:human_readable(living_language_status__liturgical_preservation_reading, "Liturgical Transmission as Sufficient Condition for Hebrew's Living Status").
narrative_ontology:topic_domain(living_language_status__liturgical_preservation_reading, "sociolinguistics/religious_studies/nationalism_studies").

domain_priors:requires_active_enforcement(living_language_status__liturgical_preservation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(living_language_status__liturgical_preservation_reading, '52549eb9-e82f-4f1d-bd7c-f570563a8b2b').
narrative_ontology:cs_kernel_codification('52549eb9-e82f-4f1d-bd7c-f570563a8b2b', distributed).
narrative_ontology:cs_authority_grounding('52549eb9-e82f-4f1d-bd7c-f570563a8b2b', lineage).
narrative_ontology:cs_interpretation_layer_present('52549eb9-e82f-4f1d-bd7c-f570563a8b2b').
narrative_ontology:cs_reading_relation('52549eb9-e82f-4f1d-bd7c-f570563a8b2b', living_language_status__native_generation_reading, forecloses).
narrative_ontology:cs_reading_relation('52549eb9-e82f-4f1d-bd7c-f570563a8b2b', living_language_status__literary_continuity_reading, coexists_with).
narrative_ontology:cs_axiom('52549eb9-e82f-4f1d-bd7c-f570563a8b2b', foundational, ritual_recitation_constitutes_linguistic_life).
narrative_ontology:cs_axiom_status(ritual_recitation_constitutes_linguistic_life, holdable).
narrative_ontology:cs_axiom_grounding('52549eb9-e82f-4f1d-bd7c-f570563a8b2b', ritual_recitation_constitutes_linguistic_life, conventional).
narrative_ontology:cs_axiom('52549eb9-e82f-4f1d-bd7c-f570563a8b2b', secondary, vernacular_transmission_not_necessary_for_vitality).
narrative_ontology:cs_axiom_status(vernacular_transmission_not_necessary_for_vitality, holdable).
narrative_ontology:cs_axiom_grounding('52549eb9-e82f-4f1d-bd7c-f570563a8b2b', vernacular_transmission_not_necessary_for_vitality, conventional).
narrative_ontology:cs_reference_frame('52549eb9-e82f-4f1d-bd7c-f570563a8b2b', diaspora_liturgical_continuity_standard).
narrative_ontology:cs_drift_state('52549eb9-e82f-4f1d-bd7c-f570563a8b2b', post_hebrew_revival_israeli_statehood, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('52549eb9-e82f-4f1d-bd7c-f570563a8b2b', '').
narrative_ontology:cs_kernel_id(living_language_status__liturgical_preservation_reading, living_language_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(living_language_status__liturgical_preservation_reading, rabbinical_authority).
narrative_ontology:constraint_beneficiary(living_language_status__liturgical_preservation_reading, yeshiva_institutions).
narrative_ontology:constraint_victim(living_language_status__liturgical_preservation_reading, secular_speech_community).
narrative_ontology:constraint_victim(living_language_status__liturgical_preservation_reading, hebraist_revivalists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(living_language_status__liturgical_preservation_reading, diaspora_liturgical_communities).
narrative_ontology:constraint_victim(living_language_status__liturgical_preservation_reading, diaspora_liturgical_communities).
narrative_ontology:constraint_vindicates(living_language_status__liturgical_preservation_reading, sacred_text_continuity_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the standard by which Hebrew's status as a living, sacred, correctly-transmitted language is adjudicated — who may interpret difficult liturgical passages, whose recitation counts as authentic, and which departures from received pronunciation or grammar are permissible. Because liturgical continuity is defined as sufficient for life, the interpretive monopoly over that continuity is preserved regardless of whether anyone speaks the language outside ritual contexts. Draws authority, tuition, and communal deference from being the recognized custodian of unbroken transmission.
narrative_ontology:constraint_stakeholder(living_language_status__liturgical_preservation_reading, rabbinical_authority, agenda_setter,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(living_language_status__liturgical_preservation_reading, rabbinical_authority, beneficiary).

% Institutions that teach liturgical Hebrew reading, recitation, and textual study receive funding, students, and legitimacy premised on the claim that this transmission constitutes the language's continued life. Their curricular and institutional survival is tied to liturgical study remaining the recognized site of linguistic vitality rather than one preservation practice among several.
narrative_ontology:constraint_stakeholder(living_language_status__liturgical_preservation_reading, yeshiva_institutions, beneficiary,
    organized, generational, constrained, national).

% Speaks Modern Hebrew as a native, daily, generationally transmitted language in homes, schools, and civic life in Israel, entirely outside liturgical contexts. Under the liturgical-preservation reading, their vernacular fluency does not count as evidence of the language's life, and their linguistic practice — with its loanwords, grammatical innovations, and secular register — is read by traditionalist authorities as departure from or even desecration of the sacred register, delegitimizing their claim to be the language's true living carriers.
narrative_ontology:constraint_stakeholder(living_language_status__liturgical_preservation_reading, secular_speech_community, payer,
    moderate, biographical, constrained, national).

% The intellectual and political tradition (Ben-Yehuda and successors) that argued Hebrew's vitality required native transmission and everyday use, not merely ritual preservation. Under this reading, their entire revivalist project is structurally unnecessary or even suspect — if liturgical recitation already sufficed to keep Hebrew alive, the revival's claim to have resurrected a dead language is undermined or recast as a secular usurpation of an already-living sacred tongue.
narrative_ontology:constraint_stakeholder(living_language_status__liturgical_preservation_reading, hebraist_revivalists, payer,
    moderate, generational, constrained, national).

% Jewish communities outside Israel who maintain Hebrew solely through prayer, Torah study, and ritual recitation without vernacular use. This reading validates their relationship to Hebrew as sufficient and authentic, granting continuity of religious identity without requiring assimilation into a national vernacular project — but it also ties their linguistic legitimacy entirely to liturgical fidelity, making any drift in pronunciation or comprehension a threat to their claimed standing.
narrative_ontology:constraint_stakeholder(living_language_status__liturgical_preservation_reading, diaspora_liturgical_communities, beneficiary,
    organized, civilizational, constrained, global).
narrative_ontology:stakeholder_secondary_role(living_language_status__liturgical_preservation_reading, diaspora_liturgical_communities, payer).

% Study language death and revival cross-linguistically (Latin, Sanskrit, Coptic, Old Church Slavonic) and can compare Hebrew's liturgical-only preservation against cases where liturgical use persisted without generational transmission and the language is conventionally classified as dead or liturgical-only. Their comparative frame is not party to the dispute but supplies the evidentiary basis other readings invoke against this one.
narrative_ontology:constraint_stakeholder(living_language_status__liturgical_preservation_reading, comparative_linguists, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a religious and educational community around a single, stable, textually-fixed standard of Hebrew competence — correct recitation, grammar, and interpretation of the liturgical corpus — allowing dispersed communities across centuries and continents to share one recognized standard of linguistic and religious continuity without requiring shared vernacular life.
% TRANSFER_FUNCTION: Moves interpretive authority, communal legitimacy, and institutional resources (tuition, religious authority, gatekeeping over conversion and religious status) toward rabbinical and yeshiva institutions, and moves legitimacy away from vernacular speakers and revivalist historiography, whose living, spoken Hebrew is recast as secondary to or derivative of the liturgical register.
% ABSENT_VOICES: Native Modern Hebrew speakers in Israel are rarely consulted in debates about what counts as linguistic 'life' framed in liturgical terms — the debate is conducted largely among religious authorities and academic historians of the revival, not among the millions who speak the language daily. Hebraist revivalist descendants who view liturgical framing as erasing the revival's achievement are similarly absent from the liturgical-authority conversation.
% DISAPPEARANCE_RATIONALE: If the liturgical-preservation standard vanished as a recognized criterion, rabbinical authority over textual interpretation would not disappear (it has independent religious grounding) but would lose its exclusive claim to define linguistic vitality — the vernacular-speaking community's fluency would become uncontested evidence of the language's life, and diaspora communities whose only connection is liturgical would need a different, less totalizing justification for their relationship to Hebrew. Rabbinical authorities dispute this, holding that liturgical fidelity has always been and remains constitutive of Hebrew's identity independent of any vernacular status.
% FOUNDING_PROBLEM: In premodern diaspora, Hebrew had no native speakers generationally transmitting it as a mother tongue; the language's only continuous, unbroken use was in prayer, Torah study, and ritual across centuries and continents. The liturgical-preservation criterion was built to explain how Hebrew could be considered alive — spiritually and communally significant, not extinct like other ancient languages — despite this absence of vernacular transmission.
% FOUNDING_PROBLEM_CORROBORATION: Comparative linguists (an outside seat) attest that the founding problem — accounting for Hebrew's continuity absent vernacular transmission — was real for the diaspora period, but note it was largely resolved by the 20th-century revival, after which native transmission became available. Rabbinical authorities and yeshiva institutions attest the founding problem remains live independent of the revival, since most world Jewry's connection to Hebrew is still liturgical rather than vernacular. Hebraist revivalists and secular linguists dispute the criterion's continued necessity once native speakers exist.
narrative_ontology:disappearance_verdict(living_language_status__liturgical_preservation_reading, contested).
narrative_ontology:founding_problem_status(living_language_status__liturgical_preservation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(living_language_status__liturgical_preservation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(living_language_status__liturgical_preservation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(living_language_status__liturgical_preservation_reading, 0.28, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(living_language_status__liturgical_preservation_reading_tests).
:- end_tests(living_language_status__liturgical_preservation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.28 at interval end) because the core function — coordinating a dispersed community around a fixed liturgical standard — is a genuine and historically necessary coordination achievement, not primarily a rent-extraction device; the corpus is fixed and low-maintenance to coordinate around (hence the low Boltzmann floor for information-standard-like coordination). But it is not zero: the reading actively transfers legitimacy away from vernacular speakers toward rabbinical authority, and this transfer has grown more consequential over time as vernacular Hebrew became demographically dominant, which is why suppression_requirement rises from 0.20 to 0.42 — sustaining the liturgical-sufficiency claim against an increasingly obvious counterexample (millions of native speakers) requires increasingly active doctrinal assertion. Theater ratio rises in parallel (0.10 to 0.30) as more of the maintenance work becomes about defending the framing itself rather than performing the liturgical function.
 *
 * PERSPECTIVAL GAP:
 *   From the rabbinical/yeshiva seat, this is pure coordination: a stable, portable standard that let Hebrew survive centuries of dispersion without a shared vernacular — a genuine achievement with no victims, since nothing is taken from anyone by declaring the liturgical corpus sufficient. From the secular vernacular speaker's seat, the same declaration is a live act of delegitimization: it says their mother tongue's daily use counts for less than a rabbi's correct recitation of a fixed text, even though their speech community is Hebrew's largest and most demographically vital. This is exactly the seat divergence the engine should register — the constraint is genuinely low-extraction as pure coordination and genuinely extractive as legitimacy-transfer, held together in the same structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Rabbinical authority and yeshiva institutions derive as beneficiaries: institutional power, arbitrage/constrained exit, and they are the ones whose authority is preserved by the criterion — low d. Secular speech community and hebraist revivalists derive as targets: their moderate power and constrained exit options (they cannot simply exit the debate over what counts as 'living' Hebrew, since the criterion bears on real institutional and civic legitimacy) place them toward the high-d end, since the reading's operation delegitimizes their central claim to be the language's true continuers. Diaspora liturgical communities get a dual role deliberately — validated as living-language participants (a real benefit) but also made structurally dependent on liturgical fidelity as their only route to legitimacy (a real constraint), captured via the secondary_role dual-positioning mechanism.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (explaining Hebrew's continuity absent vernacular transmission in diaspora) was genuinely live for roughly eighteen centuries and is corroborated by comparative linguists as a real coordination achievement, not merely self-serving mythology. Its status is now contested rather than flatly dead: for the majority of world Jewry outside Israel, Hebrew remains liturgical-only, so the founding problem persists for that population even as it has been resolved for Israeli vernacular speakers. Classifying this as tangled_rope rather than snare or pure rope reflects that dual truth: the coordination function is real and ongoing for diaspora communities, but the same structure now also does asymmetric work — delegitimizing a much larger, unambiguously living vernacular speech community that the criterion was never designed to address and that emerged only after the criterion had already calcified into doctrine.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    liturgical_sufficiency_vs_native_generation,
    'Does liturgical recitation and study genuinely constitute a language''s ''life,'' or is it definitionally a preservation practice applied to a language that would otherwise be classified as dead by standard sociolinguistic criteria (no generational native transmission)?',
    'Cross-linguistic comparison: apply the same liturgical-sufficiency standard to Latin (Catholic liturgy), Sanskrit (Vedic recitation), Ge''ez (Ethiopian Orthodox liturgy), and Old Church Slavonic. If comparative linguists classify these as dead/liturgical-only despite continuous ritual recitation, consistency suggests pre-revival Hebrew should receive the same classification, and the liturgical-sufficiency criterion is a special pleading unique to Hebrew''s case.',
    'If liturgical sufficiency is rejected as a general criterion, this reading''s claim collapses into special pleading that exists specifically to preserve rabbinical interpretive authority — reclassifying the constraint toward snare. If accepted as a general and defensible criterion of linguistic vitality (not merely survival), the coordination function is genuinely load-bearing and the tangled_rope classification with low ε is well-supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(liturgical_sufficiency_vs_native_generation, conceptual, 'Whether liturgical-only preservation is a coherent general criterion for linguistic life or an ad hoc exception.').

omega_variable(
    reading_relation_to_native_generation_reading,
    'Given that this reading and the native_generation_reading make directly contradictory claims about what Hebrew''s status was BEFORE the revival (liturgically-preserved-and-alive vs. dead-and-resurrected), do these readings genuinely foreclose one another within any single observer''s framework, or can a single observer coherently hold both (e.g., ''liturgically alive but vernacularly dead'')?',
    'Examine whether major historical and religious authorities who hold the liturgical-sufficiency view also accept the revival narrative''s premise that Hebrew required resurrection — if the same authorities affirm both, the readings coexist in practice despite apparent contradiction; if affirming one requires denying the other, foreclosure is the correct relation.',
    'Determines whether cs_structure.reading_relations should declare ''forecloses'' or ''coexists_with'' toward native_generation_reading. This file declares ''forecloses'' on the premise that ''living because liturgically preserved'' and ''dead until natively re-generated'' cannot both be true of the SAME language at the SAME time under a single coherent framework, though a diachronic observer might hold liturgical-life-then-vernacular-death-then-vernacular-revival as a sequence rather than a contradiction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_relation_to_native_generation_reading, conceptual, 'Whether the liturgical and native-generation readings are logically incompatible or sequentially compatible.').

omega_variable(
    extraction_magnitude_uncertainty,
    'How much of rabbinical/yeshiva institutional authority and funding is actually causally dependent on the liturgical-sufficiency framing specifically, versus resting on independent religious grounds (interpretive authority over Jewish law generally) that would persist even if the linguistic-vitality claim were abandoned?',
    'Comparative institutional analysis: examine yeshiva funding, enrollment, and rabbinical authority in a counterfactual or historical period where the liturgical-sufficiency claim about Hebrew''s ''life'' was less emphasized or contested, and see whether institutional standing tracked the linguistic claim or was independent of it.',
    'If institutional authority is largely independent of the linguistic-vitality claim, the extraction attributable to THIS specific constraint is smaller than authored (0.28 may be too high) — the vindicated proposition does little real work beyond rhetoric. If institutional authority substantially depends on the linguistic claim (e.g., for legitimizing exclusive control over conversion, marriage law, or textual interpretation on grounds of unbroken linguistic-religious continuity), the extraction is real and possibly understated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extraction_magnitude_uncertainty, empirical, 'How much rabbinical institutional power is causally downstream of the liturgical-life claim specifically.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(living_language_status__liturgical_preservation_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(livi_tr_t0, living_language_status__liturgical_preservation_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(livi_tr_t20, living_language_status__liturgical_preservation_reading, theater_ratio, 20, 0.14).
narrative_ontology:measurement(livi_tr_t40, living_language_status__liturgical_preservation_reading, theater_ratio, 40, 0.18).
narrative_ontology:measurement(livi_tr_t60, living_language_status__liturgical_preservation_reading, theater_ratio, 60, 0.22).
narrative_ontology:measurement(livi_tr_t80, living_language_status__liturgical_preservation_reading, theater_ratio, 80, 0.26).
narrative_ontology:measurement(livi_tr_t100, living_language_status__liturgical_preservation_reading, theater_ratio, 100, 0.3).

% Extraction over time
narrative_ontology:measurement(livi_be_t0, living_language_status__liturgical_preservation_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(livi_be_t20, living_language_status__liturgical_preservation_reading, base_extractiveness, 20, 0.18).
narrative_ontology:measurement(livi_be_t40, living_language_status__liturgical_preservation_reading, base_extractiveness, 40, 0.22).
narrative_ontology:measurement(livi_be_t60, living_language_status__liturgical_preservation_reading, base_extractiveness, 60, 0.24).
narrative_ontology:measurement(livi_be_t80, living_language_status__liturgical_preservation_reading, base_extractiveness, 80, 0.26).
narrative_ontology:measurement(livi_be_t100, living_language_status__liturgical_preservation_reading, base_extractiveness, 100, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(livi_su_t0, living_language_status__liturgical_preservation_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(livi_su_t20, living_language_status__liturgical_preservation_reading, suppression_requirement, 20, 0.22).
narrative_ontology:measurement(livi_su_t40, living_language_status__liturgical_preservation_reading, suppression_requirement, 40, 0.25).
narrative_ontology:measurement(livi_su_t60, living_language_status__liturgical_preservation_reading, suppression_requirement, 60, 0.32).
narrative_ontology:measurement(livi_su_t80, living_language_status__liturgical_preservation_reading, suppression_requirement, 80, 0.38).
narrative_ontology:measurement(livi_su_t100, living_language_status__liturgical_preservation_reading, suppression_requirement, 100, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(living_language_status__liturgical_preservation_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(living_language_status__liturgical_preservation_reading, 0.1).
narrative_ontology:affects_constraint(living_language_status__liturgical_preservation_reading, living_language_status__native_generation_reading).
narrative_ontology:affects_constraint(living_language_status__liturgical_preservation_reading, living_language_status__literary_continuity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the living_language_status kernel, each authored as a separate story with its own ε (this reading: low, ~0.28; native_generation_reading expected higher given delegitimization of the entire liturgical-preservation apparatus as it applies to a demographically dominant vernacular; literary_continuity_reading expected moderate, coordinating around a different beneficiary class — Haskalah literary elites and modern Hebrew publishing institutions). All three link to each other via affects_constraints because a shift in any one reading's institutional dominance (e.g., a court or ministry of education formally adopting native_generation as the operative legal standard for 'living language' status, which affects funding/recognition) restructures the resource and legitimacy environment the other two operate in.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
