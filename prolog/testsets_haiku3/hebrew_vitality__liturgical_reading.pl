% ============================================================================
% CONSTRAINT STORY: hebrew_vitality__liturgical_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:suppression_profile/2,
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
 *   human_readable: Liturgical Reading of Hebrew Vitality: Ritual Preservation as Constitutive
 *   domain: sociolinguistics/language_revitalization
 *
 * SUMMARY:
 *   The Hebrew vitality kernel is a contested commitment: different readings
 *   instantiate different constraints by defining what 'vitality' means. The
 *   liturgical reading asserts that Hebrew's vitality is constituted by
 *   unbroken ritual recitation — the Hebrew of the prayer book, transmitted
 *   through rabbinic authority, recited in continuous practice from antiquity
 *   to the present. Under this reading, vitality is NOT measured by number of
 *   native speakers, daily comprehension, or literary innovation, but by the
 *   fidelity and continuity of the liturgical canon itself. This is a
 *   rope-type constraint: it solves the coordination problem of preserving
 *   Jewish religious and communal coherence across diaspora and centuries. It
 *   imposes minimal extraction (rabbinic authorities benefit from maintaining
 *   their interpretive role, but the constraint does not extract from
 *   participants — they voluntarily participate in prayer). The claim/metric
 *   independence is explicit: the reading CLAIMS this is a rope (genuine
 *   coordination, uncontested benefit), and the metrics describe low
 *   extractiveness, low suppression, and negligible theater — the reading's
 *   own internal coherence.
 *
 * KEY AGENTS:
 *   - Rabbinic authorities (institutional, agenda-setter): maintain and transmit the liturgical canon; ground authority in textual tradition and practice continuity.
 *   - Orthodox communities (organized, beneficiary): participate in daily and Sabbath liturgy; experience Hebrew vitality through unbroken communal prayer.
 *   - Heritage learners (moderate power, beneficiary): diaspora Jews learning liturgical Hebrew to participate in prayer; join the chain through study.
 *   - Secular revivalists (organized, excluded): twentieth-century advocates for vernacular Hebrew; excluded from the liturgical reading's frame because vitality is ritual possession, not universal vernacular.
 *   - Linguistic scholars (analytical observer): note that this reading defines vitality narrowly and excludes secular-linguistic measures.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_vitality__liturgical_reading, 0.18).
domain_priors:suppression_score(hebrew_vitality__liturgical_reading, 0.12).
domain_priors:theater_ratio(hebrew_vitality__liturgical_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_vitality__liturgical_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(hebrew_vitality__liturgical_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(hebrew_vitality__liturgical_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_vitality__liturgical_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(hebrew_vitality__liturgical_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_vitality__liturgical_reading, rope).
narrative_ontology:human_readable(hebrew_vitality__liturgical_reading, "Liturgical Reading of Hebrew Vitality: Ritual Preservation as Constitutive").
narrative_ontology:topic_domain(hebrew_vitality__liturgical_reading, "sociolinguistics/language_revitalization").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_vitality__liturgical_reading, '4396d6e1-9788-4a85-afc4-ca7cadbab69c').
narrative_ontology:cs_kernel_codification('4396d6e1-9788-4a85-afc4-ca7cadbab69c', fixed_text).
narrative_ontology:cs_authority_grounding('4396d6e1-9788-4a85-afc4-ca7cadbab69c', lineage).
narrative_ontology:cs_interpretation_layer_present('4396d6e1-9788-4a85-afc4-ca7cadbab69c').
narrative_ontology:cs_reading_relation('4396d6e1-9788-4a85-afc4-ca7cadbab69c', hebrew_vitality__native_daily_reading, coexists_with).
narrative_ontology:cs_reading_relation('4396d6e1-9788-4a85-afc4-ca7cadbab69c', hebrew_vitality__hybrid_continuity_reading, influences).
narrative_ontology:cs_axiom('4396d6e1-9788-4a85-afc4-ca7cadbab69c', foundational, liturgical_preservation_constitutes_vitality).
narrative_ontology:cs_axiom_status(liturgical_preservation_constitutes_vitality, holdable).
narrative_ontology:cs_axiom_grounding('4396d6e1-9788-4a85-afc4-ca7cadbab69c', liturgical_preservation_constitutes_vitality, conventional).
narrative_ontology:cs_axiom('4396d6e1-9788-4a85-afc4-ca7cadbab69c', foundational, unbroken_ritual_continuity_sufficient_for_identity).
narrative_ontology:cs_axiom_status(unbroken_ritual_continuity_sufficient_for_identity, holdable).
narrative_ontology:cs_axiom_grounding('4396d6e1-9788-4a85-afc4-ca7cadbab69c', unbroken_ritual_continuity_sufficient_for_identity, deontological).
narrative_ontology:cs_reference_frame('4396d6e1-9788-4a85-afc4-ca7cadbab69c', diaspora_liturgical_transmission).
narrative_ontology:cs_drift_state('4396d6e1-9788-4a85-afc4-ca7cadbab69c', modern_hebrew_revival_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('4396d6e1-9788-4a85-afc4-ca7cadbab69c', '').
narrative_ontology:cs_kernel_id(hebrew_vitality__liturgical_reading, hebrew_vitality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_vitality__liturgical_reading, rabbinic_authorities).
narrative_ontology:constraint_beneficiary(hebrew_vitality__liturgical_reading, orthodox_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(hebrew_vitality__liturgical_reading, heritage_learners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain and transmit the liturgical canon: prayer-book texts, liturgical melodies, ritual procedures for Shabbat and holiday observance. The rabbinic reading holds that this unbroken liturgical chain — from medieval codification through continuous recitation to the present — IS the Hebrew vitality the constraint preserves. Rabbinic authority grounds itself in textual tradition and community practice continuity.
narrative_ontology:constraint_stakeholder(hebrew_vitality__liturgical_reading, rabbinic_authorities, agenda_setter,
    institutional, civilizational, identity_locked, global).

% Participate in daily and Sabbath liturgy in Hebrew, experiencing the language as the living voice of prayer and communal belonging. Under this reading, their unbroken recitation of the same prayers their ancestors recited constitutes Hebrew vitality itself — the language lives in the mouth and heart of the praying community.
narrative_ontology:constraint_stakeholder(hebrew_vitality__liturgical_reading, orthodox_communities, beneficiary,
    organized, generational, identity_locked, global).

% In the twentieth century and after, advocated for Hebrew as a daily-use language for non-religious purposes — street, school, literature, state administration. Under the liturgical reading, their ambitions are orthogonal to Hebrew vitality as traditionally defined; the constraint does not account for them because vitality is liturgical possession, not universal vernacular spread.
narrative_ontology:constraint_stakeholder(hebrew_vitality__liturgical_reading, secular_revivalists, excluded,
    organized, generational, constrained, national).

% Diaspora Jews learning liturgical Hebrew to participate in prayer with competence and understanding. The liturgical reading provides them direct access to vitality through the canon without requiring native childhood acquisition — they join the unbroken chain by learning the prayers.
narrative_ontology:constraint_stakeholder(hebrew_vitality__liturgical_reading, heritage_learners, beneficiary,
    moderate, biographical, constrained, global).

% Study Hebrew as a historical and living language. From an analytical seat, they note that this reading defines vitality narrowly (ritual domain) and excludes other measures (native speakers, daily comprehension, literary creation in vernacular registers).
narrative_ontology:constraint_stakeholder(hebrew_vitality__liturgical_reading, linguistic_scholars, observer,
    analytical, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves Hebrew as a unified prayer language across diaspora and regional communities spanning centuries: all Jews, wherever located, recite the same core prayers in the same language, maintaining doctrinal and communal coherence across space and time.
% TRANSFER_FUNCTION: Transfers knowledge and practice from authorized rabbinic interpreters to participating communities; each generation is inducted into the received liturgical canon rather than innovating its own.
% ABSENT_VOICES: Secular modernizers who advocated for Hebrew as a language of daily life, state administration, and secular literature were not parties to the rabbinic reading's definition of vitality; they would argue that restricting vitality to the liturgical domain misses the revival's greatest achievement — native speakers who use Hebrew in schools, streets, and writing.
% DISAPPEARANCE_RATIONALE: The rabbinic reading asserts that if the constraint — unbroken liturgical recitation as the kernel of vitality — disappeared, Hebrew would be relegated to historical study or secular nationalism, no longer the living voice of Jewish prayer. Critics counter that Hebrew has demonstrably survived and flourished outside the liturgical domain (native speakers in Israel, literary tradition), so the constraint is necessary for a particular reading of vitality, not for the language's existence.
% FOUNDING_PROBLEM: After the Second Temple's destruction and Jewish dispersion, Hebrew ceased to be a native vernacular language but remained the language of prayer, study, and rabbinic interpretation. The founding problem was preserving Jewish religious and communal identity across exile: how could Jews remain one people when scattered across empires and languages? The answer was the unbroken recitation of Hebrew prayers and texts.
% FOUNDING_PROBLEM_CORROBORATION: The rabbinic tradition attests that the founding problem — maintaining Hebrew and Jewish unity across diaspora — was solved by perpetual liturgical practice. Historical scholars agree the liturgical chain was unbroken and did preserve Hebrew in a written and ritual form. However, the question whether this was SUFFICIENT for vitality is precisely where the contest lies: the native-daily and hybrid readings argue the problem was only FULLY solved when modern Hebrew became a native language again, in the twentieth century.
narrative_ontology:disappearance_verdict(hebrew_vitality__liturgical_reading, contested).
narrative_ontology:founding_problem_status(hebrew_vitality__liturgical_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_vitality__liturgical_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(hebrew_vitality__liturgical_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_vitality__liturgical_reading, 0.18, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is low (0.18) because the liturgical reading frames the constraint as uncoerced participation in a shared practice — prayer is voluntary, participation carries no material cost, and the benefit (connection to tradition, communal belonging, spiritual practice) accrues to participants themselves. Suppression is minimal (0.12) because the constraint does not require active enforcement against exit — no one is barred from the ritual, and no alternatives are suppressed; the constraint persists through voluntary continuity and communal culture, not coercion. Theater ratio is very low (0.08) because the ritual practice is functionally genuine: the liturgical recitation does accomplish real coordination (preserving Hebrew in a unified form, maintaining doctrinal coherence across communities). Accessibility collapse is moderate (0.25) because alternatives (learning secular Hebrew, abandoning the language, creating new liturgies) are possible and have been chosen by some communities — the liturgical domain does not foreclose other uses of the language or other forms of Jewish identity. Resistance is moderate (0.35) because secular modernizers actively contested the claim that liturgical preservation alone constitutes vitality — they argued for vernacular revival as a superior form of vitality, though the liturgical community sustained its own reading in parallel. Measurements hold stable across the interval (extractiveness rises slightly to 0.18 by century-end, tracking modest intensification of explicit rabbinic boundary-maintenance against secular appropriation; theater remains flat at 0.08, indicating the ritual function does not degrade into pure performance). The measurement series use one shared time grid (0, 20, 40, 60, 80, 100) for both metrics.
 *
 * PERSPECTIVAL GAP:
 *   The rabbinic authorities and participating orthodox communities should compute as identical rope from their respective seats because both view the constraint as beneficial coordination they choose to maintain. Secular revivalists, if authored as participants rather than excluded, would compute the constraint as a tower of misdefinition — they would hold that vitality requires native speakers and daily use, so restricting it to liturgical domains is a false summit claiming natural-law status for what is actually a parochial reading. The engine's per-seat classification will compute divergence along this axis: seats that accept the liturgical reading as normative will classify the constraint as rope; seats that reject it as incomplete will classify it as piton (atrophied function, maintained by inertia and authority assertion rather than real coordination). The commentary reflects this structural ambiguity through the omegas.
 *
 * DIRECTIONALITY LOGIC:
 *   Rabbinic authorities sit near d = 0.0 (beneficiary): they benefit from maintaining their interpretive role and canonical authority, but the constraint imposes no cost on them — they are identity-locked to the role and would maintain the tradition regardless. Orthodox communities sit near d = 0.3 (light beneficiary): they genuinely benefit from the communal practice and connection to tradition, incur no material cost, and participate voluntarily (identity-locked exit, but the identity is self-chosen through ongoing commitment). Heritage learners sit near d = 0.2 (beneficiary): they voluntarily invest effort to learn the liturgical language and gain access to the tradition; there is no extraction. Secular revivalists are outside the constraint entirely (excluded stakeholders); they do not participate in the liturgical domain and are not bound by its definition. The liturgical reading does not extract from any seat — it coordinates willing participants around a shared practice. The low d values across all seats reflect the genuine-coordination character of the rope.
 *
 * MANDATROPHY ANALYSIS:
 *   The liturgical reading avoids mandatrophy by holding that the founding problem — preserving Hebrew and Jewish unity across diaspora — remains LIVE. The rabbinic tradition attests that continuous liturgical recitation is the solution, and the solution persists because the problem persists: Jews are still scattered, Hebrew is still at risk of assimilation into local vernaculars, and the liturgical chain is still the anchor of doctrinal unity. However, the native-daily and hybrid readings contest this verdict: they hold the founding problem is DEAD (Hebrew has been revived as a native language in Israel) or PARTIALLY DEAD (vernacular revival completed the picture, and liturgical preservation alone is insufficient). The mandatrophy mismatch (live problem claim + sealed-off functionality in a single domain = theater accumulation risk) is the site where the contested readings diverge most acutely. The liturgical reading prevents mandatrophy by defining vitality narrowly enough that the founding problem remains open — only by restricting vitality to the ritual domain can the founding problem persist as justification for the constraint. This is not a failure of the reading but a structural fact: the reading's coherence depends on the founding problem staying live within its domain.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    vitality_definition_boundary,
    'Is ''vitality'' a language-internal property (fluency, native speakers, daily use) or a community-practice property (ritual continuity, doctrinal transmission, group identity)?',
    'No single empirical fact resolves this; the question is conceptual/definitional. Resolution comes through examining what the different readings are CLAIMING about Hebrew — what they assert vitality requires — and acknowledging the reading-dependence of the claim.',
    'If vitality is defined linguistically, the liturgical reading is incomplete: ritual recitation preserves a fixed register but does not sustain native-speaker competence or generational transmission of colloquial fluency. The constraint would compute as piton (theater-maintained) rather than rope. If vitality is defined as community-practice continuity, the liturgical reading is sufficient and constitutive: ritual unbrokenness IS the marker of cultural survival.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(vitality_definition_boundary, conceptual, 'Whether vitality is a linguistic or communal property; the reading-dependent answer determines this constraint''s classification.').

omega_variable(
    native_vs_liturgical_complementarity,
    'Were the liturgical chain and native revival complementary (both necessary for complete vitality) or competitive (native revival made liturgical preservation redundant)?',
    'Examine the narrative of modern Hebrew revival: did native-speaker acquisition require the liturgical substrate, or would secular Zionists have invented Hebrew as a modern language anyway? Did the existence of liturgical Hebrew enable or merely accelerate native revival?',
    'If complementary, the liturgical reading is vindicated as the necessary foundation that made native revival possible — a rope that facilitated a later rope. If competitive, the liturgical reading becomes a superseded predecessor, and the native-daily reading is the true vitality measure — this reading would compute as piton (maintained by nostalgia, not necessity).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(native_vs_liturgical_complementarity, empirical, 'Whether liturgical preservation was causally necessary for modern Hebrew revival or made it easier but not required.').

omega_variable(
    identity_lock_authenticity,
    'For contemporary rabbinically-educated Jews and orthodox community members, is the identity-lock to liturgical Hebrew (exit option = identity_locked) a genuine feature of their self-understanding, or a post-hoc rationalization of a bounded choice set?',
    'Qualitative research: interview rabbis and community members about counterfactual scenarios (What if you had been raised secular? Would you still define your vitality through liturgy? How do you experience the choice to maintain the practice?); examine conversion and assimilation narratives to see whether identity-lock is experienced as constitutive or as one option among available frames.',
    'If genuine identity-lock: the stakeholders are truly identity-locked, the constraint is genuine coordination (voluntary, identity-constitutive practice), and the rope classification holds. If post-hoc rationalization: the stakeholders are constrained rather than identity-locked, the constraint exhibits mild suppression (internalizing the authority''s narrative as identity), and the classification trends toward tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_authenticity, empirical, 'Whether liturgical participation is identity-constitutive or identity-constrained; the distinction determines directional stability.').

omega_variable(
    kernel_reading_incommensurability,
    'Is the liturgical reading one coherent interpretation within a single commitment framework (the Hebrew covenant, the transmission tradition), or does it rest on premises that are logically incommensurable with the native-daily and hybrid readings?',
    'Examine the foundational axioms of each reading (declared in cs_structure.axioms): do they assign the same referent (Hebrew vitality) and differ only in what constitutes it (process-dependent)? Or do they disagree about what the referent even IS (is it a linguistic property, a communal practice, a theological commitment)?',
    'If incommensurable: the readings are not competing answers to one question but competing definitions of the question itself. No single empirical evidence can adjudicate among them. The readings coexist by defining different domains. If commensurable: one reading is objectively more complete or accurate, and the divergence is a classification-relevant dispute about which.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_incommensurability, conceptual, 'Whether the kernel readings are logically incommensurable or incommensurable-by-definition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_vitality__liturgical_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t0, hebrew_vitality__liturgical_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement_basis(hebr_tr_t0, observed).
narrative_ontology:measurement(hebr_tr_t20, hebrew_vitality__liturgical_reading, theater_ratio, 20, 0.06).
narrative_ontology:measurement_basis(hebr_tr_t20, observed).
narrative_ontology:measurement(hebr_tr_t40, hebrew_vitality__liturgical_reading, theater_ratio, 40, 0.07).
narrative_ontology:measurement_basis(hebr_tr_t40, observed).
narrative_ontology:measurement(hebr_tr_t60, hebrew_vitality__liturgical_reading, theater_ratio, 60, 0.08).
narrative_ontology:measurement_basis(hebr_tr_t60, observed).
narrative_ontology:measurement(hebr_tr_t80, hebrew_vitality__liturgical_reading, theater_ratio, 80, 0.08).
narrative_ontology:measurement_basis(hebr_tr_t80, observed).
narrative_ontology:measurement(hebr_tr_t100, hebrew_vitality__liturgical_reading, theater_ratio, 100, 0.08).
narrative_ontology:measurement_basis(hebr_tr_t100, observed).

% Extraction over time
narrative_ontology:measurement(hebr_be_t0, hebrew_vitality__liturgical_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement_basis(hebr_be_t0, observed).
narrative_ontology:measurement(hebr_be_t20, hebrew_vitality__liturgical_reading, base_extractiveness, 20, 0.14).
narrative_ontology:measurement_basis(hebr_be_t20, observed).
narrative_ontology:measurement(hebr_be_t40, hebrew_vitality__liturgical_reading, base_extractiveness, 40, 0.16).
narrative_ontology:measurement_basis(hebr_be_t40, observed).
narrative_ontology:measurement(hebr_be_t60, hebrew_vitality__liturgical_reading, base_extractiveness, 60, 0.17).
narrative_ontology:measurement_basis(hebr_be_t60, observed).
narrative_ontology:measurement(hebr_be_t80, hebrew_vitality__liturgical_reading, base_extractiveness, 80, 0.18).
narrative_ontology:measurement_basis(hebr_be_t80, observed).
narrative_ontology:measurement(hebr_be_t100, hebrew_vitality__liturgical_reading, base_extractiveness, 100, 0.18).
narrative_ontology:measurement_basis(hebr_be_t100, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(hebrew_vitality__liturgical_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_vitality__liturgical_reading, attachment_coordination).
narrative_ontology:boltzmann_floor_override(hebrew_vitality__liturgical_reading, 0.08).
narrative_ontology:affects_constraint(hebrew_vitality__liturgical_reading, hebrew_vitality__native_daily_reading).
narrative_ontology:affects_constraint(hebrew_vitality__liturgical_reading, hebrew_vitality__hybrid_continuity_reading).

% DUAL FORMULATION NOTE:
% The hebrew_vitality kernel is instantiated by three distinct constraint stories: liturgical_reading (this file), native_daily_reading, and hybrid_continuity_reading. Each defines vitality differently and therefore authors different ε, beneficiary/victim structures, and type classifications. They are linked via network.affects_constraints in a family: the liturgical reading is the chronologically prior form (1800+ years) and influences both modern readings by providing the substrate and definitional target. The native-daily reading coexists with this one in contemporary discourse (no foreclosure — both are held by different communities). The hybrid-continuity reading influences both by proposing that both were necessary to vitality. See commentary.kernel_context for the full contest structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
