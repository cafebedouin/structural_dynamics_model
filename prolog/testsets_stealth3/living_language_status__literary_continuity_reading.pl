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
 *   constraint_id: living_language_status__literary_continuity_reading
 *   human_readable: Language-Vitality Criterion — Literary Continuity Reading (Haskalah Demonstration)
 *   domain: sociolinguistic/religious/cultural-national
 *
 * SUMMARY:
 *   This file instantiates ONE reading of the contested
 *   living_language_status kernel: the claim that a language counts as alive
 *   so long as it remains a productive medium for new literary and
 *   intellectual work, whatever its native-speaker demographics —
 *   demonstrated, on this reading's account, by the Haskalah periodicals
 *   (Ha-Me'assef onward) and the subsequent century of modern Hebrew
 *   literature produced by communities that spoke Yiddish, Ladino,
 *   Judeo-Arabic, or European languages at home. Structurally the reading
 *   operates as a low-extraction hybrid: it genuinely coordinates a dispersed
 *   intelligentsia that otherwise shares no spoken medium (real coordination
 *   function, sustained across generations), and through the same standard it
 *   concentrates cultural authority in the literate male elite while
 *   rendering the speech of the vernacular masses, of women, and of
 *   liturgical specialists evidentially weightless (asymmetric incidence
 *   through the identical structure). The kernel decomposes into three
 *   sibling constraints with different ε values and victim sets: the
 *   liturgical_preservation_reading concentrates authority in ritual
 *   custodians and excludes the secularly literate; the
 *   native_generation_reading renders all merely literary maintenance
 *   worthless and condemns it as necrolinguistic performance, extracting
 *   hardest precisely from the literati this reading benefits; this
 *   literary_continuity_reading extracts least of the three (elite
 *   coordination around print), which is why its ε sits low while its
 *   siblings diverge upward along different axes. Family linkage runs through
 *   network.affects_constraints; the literary demonstration is upstream of
 *   the native-generation movement, whose leaders argued from the existence
 *   of the literary corpus that spoken revival was possible. Assumption of
 *   record: interval T counts years since 1783 (founding of Ha-Me'assef), so
 *   T=240 corresponds to 2023; all measurement points report observed
 *   history, none projected.
 *
 * KEY AGENTS:
 *   - maskilim: Primary beneficiary (moderate/constrained) — composes the literary work the criterion counts as proof of life
 *   - modern_hebrew_literati: Primary beneficiary (moderate/constrained) — inherits and extends the demonstration across generations
 *   - hebrew_periodical_editors: Agenda setter, secondarily beneficiary (moderate/constrained) — administers the standard in daily editorial operation
 *   - yiddish_speaking_masses: Primary target (powerless/constrained) — their everyday speech is weighed and found not to count
 *   - hebrew_illiterate_women: Secondary target (powerless/trapped) — barred from the literacy the standard measures by construction
 *   - rabbinic_traditionalists: Excluded party (organized/identity_locked) — rejects the frame from outside; their objection never enters the adjudication
 *   - native_generation_revivalists: Excluded party (organized/identity_locked) — holds the rival demographic criterion and builds around rather than through this reading
 *   - sociolinguists_of_language_death: Analytical observer (institutional/analytical) — weighs this criterion as one index among several in the language-death literature
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(living_language_status__literary_continuity_reading, 0.24).
domain_priors:suppression_score(living_language_status__literary_continuity_reading, 0.28).
domain_priors:theater_ratio(living_language_status__literary_continuity_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(living_language_status__literary_continuity_reading, extractiveness, 0.24).
narrative_ontology:constraint_metric(living_language_status__literary_continuity_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(living_language_status__literary_continuity_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(living_language_status__literary_continuity_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(living_language_status__literary_continuity_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(living_language_status__literary_continuity_reading, tangled_rope).
narrative_ontology:human_readable(living_language_status__literary_continuity_reading, "Language-Vitality Criterion — Literary Continuity Reading (Haskalah Demonstration)").
narrative_ontology:topic_domain(living_language_status__literary_continuity_reading, "sociolinguistic/religious/cultural-national").

domain_priors:requires_active_enforcement(living_language_status__literary_continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(living_language_status__literary_continuity_reading, '37bff9f3-5b20-432a-8aea-93ea67ea3fd4').
narrative_ontology:cs_kernel_codification('37bff9f3-5b20-432a-8aea-93ea67ea3fd4', distributed).
narrative_ontology:cs_authority_grounding('37bff9f3-5b20-432a-8aea-93ea67ea3fd4', expertise).
narrative_ontology:cs_interpretation_layer_present('37bff9f3-5b20-432a-8aea-93ea67ea3fd4').
narrative_ontology:cs_reading_relation('37bff9f3-5b20-432a-8aea-93ea67ea3fd4', living_language_status__liturgical_preservation_reading, coexists_with).
narrative_ontology:cs_reading_relation('37bff9f3-5b20-432a-8aea-93ea67ea3fd4', living_language_status__native_generation_reading, forecloses).
narrative_ontology:cs_axiom('37bff9f3-5b20-432a-8aea-93ea67ea3fd4', foundational, literary_productivity_suffices_for_vitality).
narrative_ontology:cs_axiom_status(literary_productivity_suffices_for_vitality, holdable).
narrative_ontology:cs_axiom_grounding('37bff9f3-5b20-432a-8aea-93ea67ea3fd4', literary_productivity_suffices_for_vitality, instrumental).
narrative_ontology:cs_axiom('37bff9f3-5b20-432a-8aea-93ea67ea3fd4', foundational, native_status_irrelevant_to_vitality).
narrative_ontology:cs_axiom_status(native_status_irrelevant_to_vitality, holdable).
narrative_ontology:cs_axiom_grounding('37bff9f3-5b20-432a-8aea-93ea67ea3fd4', native_status_irrelevant_to_vitality, conventional).
narrative_ontology:cs_reference_frame('37bff9f3-5b20-432a-8aea-93ea67ea3fd4', continuous_composition_standard).
narrative_ontology:cs_drift_state('37bff9f3-5b20-432a-8aea-93ea67ea3fd4', contemporary_demographic_turn, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('37bff9f3-5b20-432a-8aea-93ea67ea3fd4', '').
narrative_ontology:cs_kernel_id(living_language_status__literary_continuity_reading, living_language_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(living_language_status__literary_continuity_reading, maskilim).
narrative_ontology:constraint_beneficiary(living_language_status__literary_continuity_reading, modern_hebrew_literati).
narrative_ontology:constraint_victim(living_language_status__literary_continuity_reading, yiddish_speaking_masses).
narrative_ontology:constraint_victim(living_language_status__literary_continuity_reading, hebrew_illiterate_women).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(living_language_status__literary_continuity_reading, hebrew_periodical_editors).
narrative_ontology:constraint_vindicates(living_language_status__literary_continuity_reading, literary_productivity_vitality_thesis).
narrative_ontology:constraint_vindicates(living_language_status__literary_continuity_reading, diaspora_renewal_without_territory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Educated adherents of the Jewish Enlightenment who compose essays, fiction, satire, and scientific popularization in Hebrew for periodicals such as Ha-Me'assef. Scattered from Berlin to Brody to Vilna, they share no spoken vernacular and rely on the common written language to reach one another and their readers. Composing in Hebrew marks them as carriers of a renewed culture; shifting wholesale into German or Russian would end their standing inside that project, even though individual careers in European letters were possible.
narrative_ontology:constraint_stakeholder(living_language_status__literary_continuity_reading, maskilim, beneficiary,
    moderate, biographical, constrained, continental).

% The later cohort of Hebrew novelists, poets, essayists, and journal founders — the Odessa circle around Ha-Shiloah, the poets of the 1890s and 1900s, and their successors in Europe, America, and Palestine — who sustain continuous new composition across more than a century. Cultural authority in Hebrew letters flows to them, and their careers rest on the recognition that publishing serious new Hebrew work counts as keeping the language alive.
narrative_ontology:constraint_stakeholder(living_language_status__literary_continuity_reading, modern_hebrew_literati, beneficiary,
    moderate, generational, constrained, global).

% Editors and publishers of the Hebrew journals decide what appears in print, set expectations for genre, register, and seriousness, and thereby adjudicate in daily operation whether new writing meets the bar of genuine literary-intellectual work. Through acceptance, rejection, review columns, and polemics against trivial or vernacular output they keep the standard current. The authority they exercise rests on the very criterion they administer.
narrative_ontology:constraint_stakeholder(living_language_status__literary_continuity_reading, hebrew_periodical_editors, agenda_setter,
    moderate, biographical, constrained, continental).
narrative_ontology:stakeholder_secondary_role(living_language_status__literary_continuity_reading, hebrew_periodical_editors, beneficiary).

% Millions of Eastern European Jews for whom Yiddish is the language of home, market, newspaper, and theater. Leading Hebrew intellectuals judge this speech a mere jargon unworthy of the term 'living language,' and campaigns against the Yiddish press and stage aim at the very media through which these communities earn, learn, and relax. Daily life offers them no quick route into Hebrew letters; the language hierarchy settles over their world without their consent.
narrative_ontology:constraint_stakeholder(living_language_status__literary_continuity_reading, yiddish_speaking_masses, payer,
    powerless, immediate, constrained, continental).

% Women in Ashkenazi communities typically learn to read prayers and Yiddish devotional works but are denied the text-based curriculum that leads to Hebrew composition. Whatever they speak, write, or read falls outside the literary record by which vitality is reckoned; the standard surveys a world they were barred from entering. No realistic path leads from their situation into the editorial and scholarly circles that set the measure.
narrative_ontology:constraint_stakeholder(living_language_status__literary_continuity_reading, hebrew_illiterate_women, payer,
    powerless, immediate, trapped, regional).

% Heads of yeshivot and communal councils who regard Hebrew as the sacred tongue of scripture and liturgy and oppose its use for novels, satire, and science. They never accepted a seat in the debate over secular vitality; their objection travels by sermon, ban, and communal regulation rather than through the periodicals. Abandoning their position would mean surrendering commitments that constitute their religious identity, so they stand outside the frame entirely.
narrative_ontology:constraint_stakeholder(living_language_status__literary_continuity_reading, rabbinic_traditionalists, excluded,
    organized, generational, identity_locked, continental).

% Teachers, settlers, and ideologues — from Ben-Yehuda's household onward — who insist that a language returns to life only when children absorb it as a mother tongue in street and kitchen. Within this reading's frame their demand registers as beside the point, so they respond by building kindergartens, coining words, and speaking Hebrew at home rather than arguing in its journals. Their program would replace the criterion, not amend it, and their commitment is fused with the Zionist project itself.
narrative_ontology:constraint_stakeholder(living_language_status__literary_continuity_reading, native_generation_revivalists, excluded,
    organized, generational, identity_locked, global).

% Researchers of the twentieth century and later who sort languages into vital, endangered, dormant, and extinct categories and must therefore decide what counts as evidence of life. They weigh literary production alongside intergenerational transmission, domains of use, and census data; their handbooks carry the literary criterion forward as one index among several, examined and compared rather than obeyed.
narrative_ontology:constraint_stakeholder(living_language_status__literary_continuity_reading, sociolinguists_of_language_death, observer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(living_language_status__literary_continuity_reading, modern_hebrew_literati).
narrative_ontology:fixing_cost_class(living_language_status__literary_continuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Connects a geographically dispersed intelligentsia that shares no spoken vernacular into a single literary public sphere; supplies editors, writers, and readers a shared standard for what counts as keeping the language alive; sustains uninterrupted Hebrew publication across the long stretch when no community transmits Hebrew natively.
% TRANSFER_FUNCTION: Moves cultural authority, prestige, and the designation 'living language' toward producers of new Hebrew literature; renders non-literary practices — vernacular daily speech, liturgical recitation, women's and popular reading — evidentially weightless in the vitality ledger; gradually draws communal aspiration, schooling, and philanthropy toward literary Hebrew institutions.
% ABSENT_VOICES: The vernacular-speaking majority, Hebrew-uneducated women, and rabbinic traditionalists sit nowhere on editorial boards or in the philological seminars that adjudicate vitality; native-transmission advocates are answered only from within a frame built to make their decisive objection look peripheral.
% DISAPPEARANCE_RATIONALE: Without the literary-productivity standard, Haskalah writers lose the shared warrant that composing in Hebrew constitutes keeping it alive rather than embalming it; the periodical network would have reframed itself as devotional exercise or as preparation for territorial return, altering recruitment, tone, and cross-regional participation. Prestige would tilt back toward Yiddish print and liturgical mastery, and the eventual revival would inherit a thinner literary substrate.
% FOUNDING_PROBLEM: By the Haskalah, Hebrew had been nobody's mother tongue for centuries; reformers who wanted a modern Jewish intellectual life in Hebrew needed to justify composing contemporary literature in a language that pragmatists called dead and rabbis called too holy for profane use — the criterion answers both objections at once.
% FOUNDING_PROBLEM_CORROBORATION: Modern language-death scholarship (Fishman's reversal language shift work, UNESCO vitality frameworks) still weighs literary production as one vitality indicator, engaging the question from outside any benefiting party; revival historiography disputes the reading's sufficiency while confirming the underlying problem is unresolved; no corroboration comes from within the literati alone.
narrative_ontology:disappearance_verdict(living_language_status__literary_continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(living_language_status__literary_continuity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(living_language_status__literary_continuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(living_language_status__literary_continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(living_language_status__literary_continuity_reading, 0.24, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is authored low (0.24 at interval end) per the manifest's Low bin: the criterion's costs fall on groups outside the literary sphere and arrive as status demotion and evidential erasure rather than coerced transfer; its benefits to participants are real and largely consumed as intended. Suppression (0.28, raw and unscaled by power or scope — only extractiveness is scaled) reflects discursive enforcement: editorial gatekeeping, polemic against 'jargon' print, and sustained defense of the criterion against two rival readings — forceful for a scholarly standard, far short of legal or physical coercion. Theater ratio stays low (0.15): the underlying literary production is functionally real; the small late rise tracks anniversary retrospectives and commemorative academic apparatus rather than decay of function. Accessibility_collapse is moderate (0.38) because alternatives visibly survive — rival vitality definitions, Yiddish print markets, liturgical mastery all remain available and practiced. Resistance (0.58) is substantial and organized: rabbinic bans, Yiddishist institution-building (later YIVO and the Yiddish school systems), and the entire native-transmission movement constitute standing opposition, which is also why the criterion requires active defense. The measurement series run on one shared grid (T = 0, 40, 80, 120, 160, 200, 240; every tracked metric authored at every point). The suppression_requirement series is authored deliberately — the story specifically traces enforcement capacity: discursive defense rose sharply through the revival controversy (peak near T=120, roughly 1903, when the demographic criterion threatened to obsolete the literary one), then eased as this reading settled into being one scholarly voice among several. Base extractiveness follows a shallow arc — climbing as the standard hardened and vernacular-stigma campaigns spread (T=0..120), cresting when the Hebrew literary establishment held institutional power while Hebrew still had no native speakers, then easing once the revival removed the criterion's sole burden. The claim (tangled_rope) and the metrics are authored independently: I assert the hybrid structure as structurally true and the low values as descriptively true; the engine computes per-seat classifications from the structural data, and any divergence between claim and computed verdict is the datum the corpus exists to collect.
 *
 * PERSPECTIVAL GAP:
 *   Seats compute differently from the same structure. From the editor's and literatus's chairs the criterion reads as faithful stewardship — someone must demonstrate that the language can still carry thought, and the exclusion of casual speech from the reckoning is simply rigor. From the vernacular majority's chair the identical standard is a ceiling placed over their world: everything they say, sell, and sing is scored against a game they were never taught, and the anti-jargon polemics attack their actual media. From the excluded traditionalist chair the frame is not wrong but illegitimate — a profane audit of a sacred tongue, to be refused rather than amended. Coalition dynamics matter for the powerless seats: the payer groups lacked standing individually, but the Yiddishist movement showed that printers, teachers, and writers could federate into an organized counter-public, which is precisely the resistance trajectory visible in the measurements after T≈80. The analytical seat weighs the criterion as one index among several, which is the seat from which the criterion's circularity (see omega criterion_circularity) is most visible.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries (maskilim, modern_hebrew_literati) derive directionality toward the beneficiary end — the criterion subsidizes their cultural authority, and their constrained-but-real exit (European-language careers) keeps them from full capture. The editors' agenda-setter seat sits nearer symmetric: they administer the standard, collect authority through it, and bear its defensive labor. Declared victims derive toward the target end: the vernacular masses (powerless, constrained) amplify effective extraction; hebrew_illiterate_women, trapped with no path into the measured sphere, sit nearest the full-target end of anyone in the story despite bearing the lightest nominal load. The excluded seats (traditionalists, revivalists) are identity-locked parties outside the adjudication; their costs are real (their practice is classified as non-life) but they never entered the conversation that set the measure, which is exactly the consensus-provenance gap recorded under absent_voices. Scope is continental-to-global, which modestly amplifies effective extraction for targets by making verification of the standard's fairness harder; suppression, again, is untouched by scope or power scaling.
 *
 * MANDATROPHY ANALYSIS:
 *   Decomposition prevents mislabeling in both directions. Read whole, the episode invites two symmetrical errors: calling it disinterested scholarship (which would miss the asymmetric incidence running through the identical structure — authority to the literati, evidential erasure for everyone else), or calling it ideological cover (which would miss the genuine coordination achievement — a functioning pan-diaspora public sphere with no spoken common tongue). Authoring beneficiaries, victims, and active enforcement separately forces both facts into the record and lands the hybrid honestly at low ε. On obsolescence: the founding problem — can a language be alive without native transmission — remains live in contemporary language-death scholarship, so no mandate has outlived its function; mandatrophy_resolved is left undeclared, and the mismatch consumer finds founding_problem_status=live paired with disappearance_verdict=world_rearranges, the coherent pairing. Had the revival retroactively refuted the reading (see omega revival_retrovalidation), the status would flip to dead and the arrangement would face the zombie-flag cross-check.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This story instantiates one reading of the living_language_status kernel; what structurally changes if a sibling reading governs instead?',
    'Cross-classify the three reading files: liturgical_preservation_reading (continuous ritual recitation and study define vitality; the excluded set becomes the secularly literate), native_generation_reading (mother-tongue transmission defines vitality; the Haskalah demonstration collapses and Hebrew 1783-1920 counts as a dead language), versus this literary-continuity reading (productive literary medium suffices; non-literary speakers drop from the ledger).',
    'Under the liturgical reading the victim set relocates to those outside ritual competence; under the native-generation reading this reading''s entire beneficiary structure evaporates and its ε referent ceases to exist as described — classification of the whole family flips.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer structure: one reading of the contested living_language_status kernel; the disagreement sits in what evidence counts as vitality.').

omega_variable(
    exclusion_cost_visibility,
    'Is the exclusion of non-literary speakers from the vitality ledger purely symbolic, or does it carry material consequence through schooling choices, communal funding, and the print marketplace?',
    'Trace Haskalah-era communal ledgers and curricula: whether Hebrew-literary institutions drew students and money away from vernacular instruction, and whether the anti-''jargon'' campaigns measurably depressed Yiddish publishing incomes and readerships.',
    'Symbolic-only exclusion leaves effective extraction near the identity-coordination floor and the reading close to pure coordination; materially consequential exclusion raises effective extraction for the payer seats and pushes the hybrid toward its heavier pole.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exclusion_cost_visibility, empirical, 'Severity of the reading''s exclusion of non-literary populations from the vitality reckoning.').

omega_variable(
    criterion_circularity,
    'Does the literary criterion grade vitality in terms its own practitioners control — is the reading self-certifying?',
    'Apply the criterion to third-party cases its practitioners do not staff (Classical Chinese literacy, ecclesiastical Latin, Sanskrit commentary traditions) and check convergence against independent language-death assessments.',
    'Convergence supports treating the low authored extraction as descriptive; divergence indicates an authority-preserving instrument whose favorable numbers reflect insider assessment, raising effective extraction for everyone outside the practitioner set.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(criterion_circularity, conceptual, 'Whether the literary-productivity standard is self-certifying for the community that administers it.').

omega_variable(
    revival_retrovalidation,
    'Does the eventual mother-tongue revival of Hebrew retroactively vindicate the literary-continuity reading, or refute its claim to have been sufficient on its own?',
    'Historical reconstruction of inheritance: trace lexical stock, idioms, and syntactic habits from Haskalah prose into revived speech through Ben-Yehuda-era coinage records and revival historiography.',
    'Vindication confines the reading''s extraction assessment to the pre-revival window and strengthens its coordination claim; refutation dates the reading''s failure around the 1920s and shifts its terminal trajectory toward inertial persistence on theatrical grounds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(revival_retrovalidation, empirical, 'Retroactive validity of the literary demonstration once native transmission resumed.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(living_language_status__literary_continuity_reading, 0, 240).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(livi_tr_t0, living_language_status__literary_continuity_reading, theater_ratio, 0, 0.06).
narrative_ontology:measurement_basis(livi_tr_t0, observed).
narrative_ontology:measurement(livi_tr_t40, living_language_status__literary_continuity_reading, theater_ratio, 40, 0.08).
narrative_ontology:measurement_basis(livi_tr_t40, observed).
narrative_ontology:measurement(livi_tr_t80, living_language_status__literary_continuity_reading, theater_ratio, 80, 0.09).
narrative_ontology:measurement_basis(livi_tr_t80, observed).
narrative_ontology:measurement(livi_tr_t120, living_language_status__literary_continuity_reading, theater_ratio, 120, 0.11).
narrative_ontology:measurement_basis(livi_tr_t120, observed).
narrative_ontology:measurement(livi_tr_t160, living_language_status__literary_continuity_reading, theater_ratio, 160, 0.13).
narrative_ontology:measurement_basis(livi_tr_t160, observed).
narrative_ontology:measurement(livi_tr_t200, living_language_status__literary_continuity_reading, theater_ratio, 200, 0.14).
narrative_ontology:measurement_basis(livi_tr_t200, observed).
narrative_ontology:measurement(livi_tr_t240, living_language_status__literary_continuity_reading, theater_ratio, 240, 0.15).
narrative_ontology:measurement_basis(livi_tr_t240, observed).

% Extraction over time
narrative_ontology:measurement(livi_be_t0, living_language_status__literary_continuity_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement_basis(livi_be_t0, observed).
narrative_ontology:measurement(livi_be_t40, living_language_status__literary_continuity_reading, base_extractiveness, 40, 0.16).
narrative_ontology:measurement_basis(livi_be_t40, observed).
narrative_ontology:measurement(livi_be_t80, living_language_status__literary_continuity_reading, base_extractiveness, 80, 0.2).
narrative_ontology:measurement_basis(livi_be_t80, observed).
narrative_ontology:measurement(livi_be_t120, living_language_status__literary_continuity_reading, base_extractiveness, 120, 0.26).
narrative_ontology:measurement_basis(livi_be_t120, observed).
narrative_ontology:measurement(livi_be_t160, living_language_status__literary_continuity_reading, base_extractiveness, 160, 0.29).
narrative_ontology:measurement_basis(livi_be_t160, observed).
narrative_ontology:measurement(livi_be_t200, living_language_status__literary_continuity_reading, base_extractiveness, 200, 0.27).
narrative_ontology:measurement_basis(livi_be_t200, observed).
narrative_ontology:measurement(livi_be_t240, living_language_status__literary_continuity_reading, base_extractiveness, 240, 0.24).
narrative_ontology:measurement_basis(livi_be_t240, observed).

% Suppression requirement over time
narrative_ontology:measurement(livi_su_t0, living_language_status__literary_continuity_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement_basis(livi_su_t0, observed).
narrative_ontology:measurement(livi_su_t40, living_language_status__literary_continuity_reading, suppression_requirement, 40, 0.18).
narrative_ontology:measurement_basis(livi_su_t40, observed).
narrative_ontology:measurement(livi_su_t80, living_language_status__literary_continuity_reading, suppression_requirement, 80, 0.3).
narrative_ontology:measurement_basis(livi_su_t80, observed).
narrative_ontology:measurement(livi_su_t120, living_language_status__literary_continuity_reading, suppression_requirement, 120, 0.46).
narrative_ontology:measurement_basis(livi_su_t120, observed).
narrative_ontology:measurement(livi_su_t160, living_language_status__literary_continuity_reading, suppression_requirement, 160, 0.44).
narrative_ontology:measurement_basis(livi_su_t160, observed).
narrative_ontology:measurement(livi_su_t200, living_language_status__literary_continuity_reading, suppression_requirement, 200, 0.36).
narrative_ontology:measurement_basis(livi_su_t200, observed).
narrative_ontology:measurement(livi_su_t240, living_language_status__literary_continuity_reading, suppression_requirement, 240, 0.28).
narrative_ontology:measurement_basis(livi_su_t240, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(living_language_status__literary_continuity_reading, identity_coordination).
narrative_ontology:affects_constraint(living_language_status__literary_continuity_reading, liturgical_preservation_reading).
narrative_ontology:affects_constraint(living_language_status__literary_continuity_reading, native_generation_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'is the language living?' (kernel living_language_status). Three readings, three constraints, three stable ε values: this literary-continuity reading (low ε — elite coordination around literary production; victims excluded from the literary ledger), liturgical_preservation_reading (authority concentrated in ritual custodians; the secularly literate are the excluded set), native_generation_reading (demographic criterion; highest extraction against the literati, whose lifework stops counting as life). The label conflated them because each reading's proponents cite the same Hebrew case; the ε-invariance principle splits them because measuring vitality by library output, liturgical continuity, or nursery speech yields different beneficiary/victim structures and different failure modes. Edge direction: this reading is upstream of native_generation_reading (revival advocates argued from the existence of the literary corpus), and lateral-contestatory toward liturgical_preservation_reading (each dismisses the other's evidence).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
