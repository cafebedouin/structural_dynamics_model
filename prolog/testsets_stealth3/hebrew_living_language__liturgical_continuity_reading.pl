% ============================================================================
% CONSTRAINT STORY: hebrew_living_language__liturgical_continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_living_language__liturgical_continuity_reading, []).

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
 *   constraint_id: hebrew_living_language__liturgical_continuity_reading
 *   human_readable: Liturgical Continuity of Hebrew Across the Diaspora
 *   domain: historical_linguistics/language_revitalization
 *
 * SUMMARY:
 *   Across seventeen centuries of dispersion, Jewish communities maintained
 *   Hebrew as the language of prayer, public scripture reading, legal
 *   formulae, and elite study, long after Aramaic, Greek, Arabic, and later
 *   Yiddish, Ladino, and other vernaculars absorbed daily speech. The claim
 *   under contest in this reading is that this unbroken recitational and
 *   textual use constitutes the language remaining alive - not dormant, not
 *   merely preserved. The claim/metric gap discipline applies here as
 *   anywhere: claimed_type=rope states the structure I believe true (genuine
 *   coordination, voluntary participation, net-beneficiary seats), while the
 *   metric values describe operation honestly, including the slow theater
 *   rise as vernacular distance grew. Family decomposition note: the
 *   colloquial label 'Hebrew never died' splits into three structurally
 *   distinct claims with different epsilon values and beneficiary sets; this
 *   file authors the liturgical-continuity member, whose epsilon (~0.10)
 *   reflects this reading's assessment that costs were willingly borne
 *   continuity investments. The sibling files - native_generation_reading,
 *   which authors substantially higher theater and an effective cost-bearing
 *   learner set under its lights, and literary_revival_reading, which
 *   re-dates the interval and shifts beneficiaries toward print-era
 *   intellectuals - overlap this arrangement but measure different things.
 *
 * KEY AGENTS:
 *   - rabbinical_leadership - agenda setter (institutional / identity_locked): administers the liturgical standard and study curriculum; its authority is constituted by the canon it transmits.
 *   - diaspora_jewish_communities - primary beneficiary (organized / constrained): fund and staff the apparatus; receive portable ritual-legal unity across dispersion.
 *   - adult_male_lay_reciters - participant bearing study cost (powerless individually / constrained): recite the fixed rite; dual-positioned, paying in childhood years for lifetime textual access.
 *   - hebrew_educators_melamdim - paid beneficiary (moderate / constrained): receive stipends and tuition; livelihood tied to instruction.
 *   - torah_scribes_soferim - paid beneficiary (moderate / constrained): produce ritually valid texts on commission across long distances.
 *   - women_without_hebrew_schooling - excluded seat (powerless / trapped): devout but shut out of the Hebrew-literate sphere and its decision-making.
 *   - historical_linguists_philologists - analytical observer (analytical / analytical): assess the continuity claim from outside the practice.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_living_language__liturgical_continuity_reading, 0.1).
domain_priors:suppression_score(hebrew_living_language__liturgical_continuity_reading, 0.08).
domain_priors:theater_ratio(hebrew_living_language__liturgical_continuity_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_living_language__liturgical_continuity_reading, extractiveness, 0.1).
narrative_ontology:constraint_metric(hebrew_living_language__liturgical_continuity_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(hebrew_living_language__liturgical_continuity_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_living_language__liturgical_continuity_reading, accessibility_collapse, 0.22).
narrative_ontology:constraint_metric(hebrew_living_language__liturgical_continuity_reading, resistance, 0.18).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_living_language__liturgical_continuity_reading, rope).
narrative_ontology:human_readable(hebrew_living_language__liturgical_continuity_reading, "Liturgical Continuity of Hebrew Across the Diaspora").
narrative_ontology:topic_domain(hebrew_living_language__liturgical_continuity_reading, "historical_linguistics/language_revitalization").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_living_language__liturgical_continuity_reading, 'd05f95fe-3501-4091-8b34-7b47748d4bf0').
narrative_ontology:cs_kernel_codification('d05f95fe-3501-4091-8b34-7b47748d4bf0', fixed_text).
narrative_ontology:cs_authority_grounding('d05f95fe-3501-4091-8b34-7b47748d4bf0', lineage).
narrative_ontology:cs_interpretation_layer_present('d05f95fe-3501-4091-8b34-7b47748d4bf0').
narrative_ontology:cs_reading_relation('d05f95fe-3501-4091-8b34-7b47748d4bf0', hebrew_living_language__native_generation_reading, influences).
narrative_ontology:cs_reading_relation('d05f95fe-3501-4091-8b34-7b47748d4bf0', hebrew_living_language__literary_revival_reading, influences).
narrative_ontology:cs_axiom('d05f95fe-3501-4091-8b34-7b47748d4bf0', foundational, continuous_liturgical_voicing_constitutes_life).
narrative_ontology:cs_axiom_status(continuous_liturgical_voicing_constitutes_life, holdable).
narrative_ontology:cs_axiom_grounding('d05f95fe-3501-4091-8b34-7b47748d4bf0', continuous_liturgical_voicing_constitutes_life, theological).
narrative_ontology:cs_axiom('d05f95fe-3501-4091-8b34-7b47748d4bf0', secondary, masoretic_chain_never_broken).
narrative_ontology:cs_axiom_status(masoretic_chain_never_broken, holdable).
narrative_ontology:cs_axiom_grounding('d05f95fe-3501-4091-8b34-7b47748d4bf0', masoretic_chain_never_broken, empirically_contingent).
narrative_ontology:cs_reference_frame('d05f95fe-3501-4091-8b34-7b47748d4bf0', unbroken_masoretic_liturgical_continuity).
narrative_ontology:cs_drift_state('d05f95fe-3501-4091-8b34-7b47748d4bf0', contemporary_linguistic_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('d05f95fe-3501-4091-8b34-7b47748d4bf0', '').
narrative_ontology:cs_kernel_id(hebrew_living_language__liturgical_continuity_reading, hebrew_living_language).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_living_language__liturgical_continuity_reading, diaspora_jewish_communities).
narrative_ontology:constraint_beneficiary(hebrew_living_language__liturgical_continuity_reading, hebrew_educators_melamdim).
narrative_ontology:constraint_beneficiary(hebrew_living_language__liturgical_continuity_reading, torah_scribes_soferim).
narrative_ontology:constraint_beneficiary(hebrew_living_language__liturgical_continuity_reading, rabbinical_leadership).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(hebrew_living_language__liturgical_continuity_reading, adult_male_lay_reciters).
narrative_ontology:constraint_victim(hebrew_living_language__liturgical_continuity_reading, adult_male_lay_reciters).
narrative_ontology:constraint_vindicates(hebrew_living_language__liturgical_continuity_reading, masoretic_unbroken_transmission).
narrative_ontology:constraint_vindicates(hebrew_living_language__liturgical_continuity_reading, lashon_hakodesh_sanctity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the liturgical rite, rules on pronunciation and prayer-text variants, and prescribes the curriculum through which boys encounter Hebrew texts; issues rulings that keep usage aligned with the inherited corpus from the Geonic academies onward. Its standing rests entirely on mastery and transmission of the Hebrew canon; stepping outside that canon would dissolve the basis of its own office. It collects deference and appointment power rather than fees, though academy posts and communal offices flow through it.
narrative_ontology:constraint_stakeholder(hebrew_living_language__liturgical_continuity_reading, rabbinical_leadership, agenda_setter,
    institutional, generational, identity_locked, continental).

% Sustain synagogues, schools, and scribal workshops out of communal funds and volunteer labor; in return they receive a portable ritual and legal language that works identically in Cairo, Cordoba, and Krakow, letting merchants, marriage contracts, and mourners' prayers travel intact. A community could shift its rites toward the surrounding vernacular - some laments and devotional poems were in fact composed in Yiddish, Ladino, and Judeo-Arabic - but doing so would cut its members off from the shared corpus and from every other community still using it.
narrative_ontology:constraint_stakeholder(hebrew_living_language__liturgical_continuity_reading, diaspora_jewish_communities, beneficiary,
    organized, civilizational, constrained, global).

% Teach the alphabet, prayer-book fluency, and eventually Talmud to community boys for tuition and communal salary; their livelihood and local rank depend on the continuation of Hebrew instruction. A teacher whose pupils shifted to vernacular schooling would need to retrain into another trade.
narrative_ontology:constraint_stakeholder(hebrew_living_language__liturgical_continuity_reading, hebrew_educators_melamdim, beneficiary,
    moderate, biographical, constrained, regional).

% Copy Torah scrolls, phylacteries, and mezuzot by hand, and from the sixteenth century operate presses for Hebrew books; commissioned per item by communities and individuals across long distances. Their craft exists only so long as demand for ritually valid Hebrew texts continues; the specialized skill transfers to almost nothing else.
narrative_ontology:constraint_stakeholder(hebrew_living_language__liturgical_continuity_reading, torah_scribes_soferim, beneficiary,
    moderate, biographical, constrained, continental).

% Pray three times daily from the Hebrew rite and hear the Torah reading each Sabbath, having paid for that ability with childhood years in school; individually they have no vote on the rite's wording, which reaches them already fixed. Vernacular sermons and translations run alongside for those who want them, and anyone may disaffiliate entirely, but within communal life the recited Hebrew is simply the medium everything runs through.
narrative_ontology:constraint_stakeholder(hebrew_living_language__liturgical_continuity_reading, adult_male_lay_reciters, beneficiary,
    powerless, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(hebrew_living_language__liturgical_continuity_reading, adult_male_lay_reciters, payer).

% In most places and centuries received no Hebrew schooling; they pray in the vernacular, rely on Yiddish tkhines and Ladino devotional verse, and encounter scripture through translation. Nothing bars their piety, but the Hebrew-literate sphere - its councils, its stipends, its prestige - is closed to them, and they hold no seat where the rite or the schooling budget is decided.
narrative_ontology:constraint_stakeholder(hebrew_living_language__liturgical_continuity_reading, women_without_hebrew_schooling, excluded,
    powerless, biographical, trapped, global).

% Reconstruct the language's history from manuscripts, inscriptions, and printed corpora; date the retreat of Hebrew from daily speech, measure the widening distance to the surrounding vernaculars, and argue over whether continuous recitation constitutes linguistic life. They stand outside the practice, owe it no observance, and are moved by neither sermon nor stipend.
narrative_ontology:constraint_stakeholder(hebrew_living_language__liturgical_continuity_reading, historical_linguists_philologists, observer,
    analytical, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hebrew_living_language__liturgical_continuity_reading, hebrew_educators_melamdim).
narrative_ontology:fixing_cost_class(hebrew_living_language__liturgical_continuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Keeps a single sacred-and-legal language mutually intelligible across politically disconnected communities: fixed prayer rites, public scripture reading, and contract formulae in Hebrew mean a scholar or merchant relocating from Baghdad to Mainz enters a working linguistic world; communal calendars, lifecycle rites, and courts all run off one shared corpus.
% TRANSFER_FUNCTION: Moves household study-time (boys' school and academy years) and communal funds (teacher stipends, scribal commissions, synagogue and academy upkeep, book reproduction) from families and kehillot to Hebrew specialists and institutions; returns textual access, ritual competence, and intergenerational continuity to participants; concentrates interpretive prestige among Hebrew-literate men.
% ABSENT_VOICES: Women of the traditional communities are the structural absence: devout participants excluded from Hebrew schooling, they built parallel vernacular devotional literatures (Yiddish tkhines, Ladino bakashot) and would object that the continuity being celebrated was purchased with their exclusion from the literate sphere. Also under-heard inside the arrangement: would-be vernacularizers and rite reformers, who sat within the system and exited it rather than reshaping it.
% DISAPPEARANCE_RATIONALE: If recitation and study of Hebrew ceased across the diaspora overnight, liturgies would fragment along vernacular lines within a generation, contract and divorce formulae would lose their common form and with them cross-communal legal portability, rabbinic authority would lose its linguistic instrument, and the preserved corpus that later revival efforts drew on would simply not exist in usable shape.
% FOUNDING_PROBLEM: After the destruction of the Temple and the failed revolt, a territorially dispersed minority needed to preserve its constitutive texts, law, and identity across generations without sovereignty, common territory, or control of the host-language environments its members lived in.
% FOUNDING_PROBLEM_CORROBORATION: Academic historiography of post-Temple Jewry - survey histories of communal continuity and the documentary record of expulsions, forced conversions, and assimilation pressure - attests the survival problem from outside the liturgical establishment; the problem's persistence is independently visible in continuing assimilation and discontinuity data, not merely asserted by the communities that benefit from the arrangement.
narrative_ontology:disappearance_verdict(hebrew_living_language__liturgical_continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_living_language__liturgical_continuity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_living_language__liturgical_continuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(hebrew_living_language__liturgical_continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_living_language__liturgical_continuity_reading, 0.1, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_living_language__liturgical_continuity_reading_tests).
:- end_tests(hebrew_living_language__liturgical_continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon is authored low (scalar 0.10; series 0.16 falling to 0.10) because the arrangement's costs are educational investment and communal upkeep that participants fund knowingly and could redirect; no surplus is skimmed under compulsion. The gentle decline tracks falling reproduction costs (printing displacing hand-copying from the sixteenth century) and the normalization of schooling obligations across the male population. Suppression (0.08) is structural residue only: no enforcement machinery punishes abandonment; a family that skipped Hebrew school met gossip, not sanction, and whole groups running parallel practices were never interfered with by this arrangement itself. Because the enforcement picture is static across the interval - soft normative administration throughout, no machinery built up or decayed - no suppression_requirement series is authored; the scalar carries it. Theater_ratio (scalar 0.15; series 0.04 rising to 0.15) grows with vernacular distance: as Yiddish, Ladino, and Judeo-Arabic absorbed daily life, a growing share of recitation proceeded without full comprehension - phonologically faithful, semantically thin. Within this reading recitation itself is the claimed continuity mechanism, so even hollowed recitation performs the function; the rising series marks the growing fraction that carried sound but little else. Accessibility_collapse is low (0.22): translation lanes (Targum custom, vernacular glosses, women's devotional literature) kept real alternatives alive throughout, so understanding the arrangement did not close off exits to it. Resistance is low (0.18): rite-family rivalries and Karaite objections to post-biblical Hebrew never organized against the arrangement as such. Claim and metrics are authored independently; the engine computes per-seat classifications from the structural data.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from the same structure. Rabbinical leadership experiences the arrangement as its own substance - exit would be self-dissolution, so its seat reads as stable coordination from inside. Adult lay reciters carry the study-years cost but collect the portable corpus; their dual position should compute nearer the middle than either pole, and their individual powerlessness should be weighed against their collective role as the communities that fund everything. Paid specialists collect income streams sized by demand they do not set. Women outside Hebrew schooling receive neither the bill nor the goods: their seat registers exclusion rather than extraction, and their directionality should derive from absence-of-access rather than from payment. The largest perspectival hazard is cross-kernel rather than intra-story: an analyst holding the native-generation reading looks at identical recitations and sees memorized performance rather than language use. That divergence belongs to the sibling stories and to the omega variables below, not to this file's classification.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries anchor the derivation. Diaspora communities sit near the beneficiary pole: they receive the continuity good and pay only what they vote themselves. Educators and scribes derive low-moderate: they receive paid flows but supply the labor those flows purchase. Rabbinical leadership derives low despite administering, because what it collects (deference, appointments) rides on the same continuity it provides. Lay reciters, dual-positioned, derive mid-range: they pay in childhood years and collect in lifetime textual access. Excluded women fall outside the transfer loop entirely - no subsidy received, no fee levied - so their d derives from structural omission, which the derivation chain handles without help. No directionality_overrides are authored: every seat's relationship to the constraint is already expressed by the beneficiary declarations and exit atoms, and inventing overrides here would fabricate precision the structure does not need.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem - transmitting a text-law-identity package across dispersion without sovereignty - remains live, and the disappearance verdict is world_rearranges: remove the recited-and-studied corpus overnight and liturgies fragment into vernaculars within a generation, legal formulae lose portability, and the raw material later revivals drew on goes unmaintained. Live-status paired with world_rearranges is the consistent cell, so no mandatrophy or zombie flag should fire. The discipline cuts both ways here: without declared beneficiaries and a stated coordination function, a voluntary low-extraction arrangement risks misreading as piton (why keep a hard language alive at cost?), while the rising theater series invites a premature snare reading. The beneficiary set, the absent-victim fact, the flat suppression picture, and the live founding problem together hold the structure as rope - coordination doing real work at low overhead, with no seat capturing rents.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_index,
    'This constraint is one reading of the kernel hebrew_living_language - what changes structurally if the corpus adopts a sibling reading instead?',
    'Cross-file comparison of the three reading stories: native_generation_reading authors higher theater_ratio and adds an effectively cost-bearing learner set (years invested in non-generative competence); literary_revival_reading shifts the beneficiary set toward print-era intellectuals and re-dates the interval''s active phase.',
    'Adopting native_generation_reading converts this file''s low theater into evidence of inertia and pushes the arrangement piton-ward with raised effective extraction; adopting literary_revival_reading demotes this reading to upstream infrastructure with reduced standalone standing. The disagreement is located in the vitality criterion, not in any factual dispute about recitation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_index, conceptual, 'Reading-index of kernel hebrew_living_language: this file is the liturgical-continuity reading; siblings differ on what counts as a language being alive.').

omega_variable(
    recitation_comprehension_functionality,
    'Does liturgical recitation without full generative comprehension constitute functional linguistic activity, or performance that preserves phonology only?',
    'Psycholinguistic testing of liturgically-educated populations for passive grammatical competence (morphological parsing, register recognition, comprehension of unmemorized text) against matched unexposed controls.',
    'If recitation proves functionally hollow, theater_ratio rises well past 0.5, the continuity claim weakens toward the native-generation sibling''s verdict, and the vindicated masoretic proposition loses its empirical support.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(recitation_comprehension_functionality, empirical, 'Whether sound-preserving recitation counts as language use under this reading''s own sufficiency claim.').

omega_variable(
    unbroken_chain_attestation,
    'Was transmission actually unbroken - do forced-conversion zones (Iberia after 1391 and 1492), geographically isolated communities, and rite schisms leave the chain intact everywhere the claim extends?',
    'Attestation-density mapping: dated manuscripts, colophons, and liturgy fragments per region-century; a region-century with no bridging attestations would localize a break in the chain.',
    'Localized breaks would strain the unbroken-chain axiom empirically, confirm axiom_overriding drift in cs_structure, and weaken the vindicated propositions this constraint carries; intact coverage would consolidate the rope reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unbroken_chain_attestation, empirical, 'Empirical integrity of the ''unbroken'' qualifier across regions and centuries.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_living_language__liturgical_continuity_reading, 100, 1880).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(heb_lit_cont_tr_t100, hebrew_living_language__liturgical_continuity_reading, theater_ratio, 100, 0.04).
narrative_ontology:measurement_basis(heb_lit_cont_tr_t100, observed).
narrative_ontology:measurement(heb_lit_cont_tr_t500, hebrew_living_language__liturgical_continuity_reading, theater_ratio, 500, 0.06).
narrative_ontology:measurement_basis(heb_lit_cont_tr_t500, observed).
narrative_ontology:measurement(heb_lit_cont_tr_t900, hebrew_living_language__liturgical_continuity_reading, theater_ratio, 900, 0.09).
narrative_ontology:measurement_basis(heb_lit_cont_tr_t900, observed).
narrative_ontology:measurement(heb_lit_cont_tr_t1300, hebrew_living_language__liturgical_continuity_reading, theater_ratio, 1300, 0.11).
narrative_ontology:measurement_basis(heb_lit_cont_tr_t1300, observed).
narrative_ontology:measurement(heb_lit_cont_tr_t1600, hebrew_living_language__liturgical_continuity_reading, theater_ratio, 1600, 0.13).
narrative_ontology:measurement_basis(heb_lit_cont_tr_t1600, observed).
narrative_ontology:measurement(heb_lit_cont_tr_t1750, hebrew_living_language__liturgical_continuity_reading, theater_ratio, 1750, 0.14).
narrative_ontology:measurement_basis(heb_lit_cont_tr_t1750, observed).
narrative_ontology:measurement(heb_lit_cont_tr_t1880, hebrew_living_language__liturgical_continuity_reading, theater_ratio, 1880, 0.15).
narrative_ontology:measurement_basis(heb_lit_cont_tr_t1880, observed).

% Extraction over time
narrative_ontology:measurement(heb_lit_cont_be_t100, hebrew_living_language__liturgical_continuity_reading, base_extractiveness, 100, 0.16).
narrative_ontology:measurement_basis(heb_lit_cont_be_t100, observed).
narrative_ontology:measurement(heb_lit_cont_be_t500, hebrew_living_language__liturgical_continuity_reading, base_extractiveness, 500, 0.15).
narrative_ontology:measurement_basis(heb_lit_cont_be_t500, observed).
narrative_ontology:measurement(heb_lit_cont_be_t900, hebrew_living_language__liturgical_continuity_reading, base_extractiveness, 900, 0.13).
narrative_ontology:measurement_basis(heb_lit_cont_be_t900, observed).
narrative_ontology:measurement(heb_lit_cont_be_t1300, hebrew_living_language__liturgical_continuity_reading, base_extractiveness, 1300, 0.12).
narrative_ontology:measurement_basis(heb_lit_cont_be_t1300, observed).
narrative_ontology:measurement(heb_lit_cont_be_t1600, hebrew_living_language__liturgical_continuity_reading, base_extractiveness, 1600, 0.11).
narrative_ontology:measurement_basis(heb_lit_cont_be_t1600, observed).
narrative_ontology:measurement(heb_lit_cont_be_t1750, hebrew_living_language__liturgical_continuity_reading, base_extractiveness, 1750, 0.1).
narrative_ontology:measurement_basis(heb_lit_cont_be_t1750, observed).
narrative_ontology:measurement(heb_lit_cont_be_t1880, hebrew_living_language__liturgical_continuity_reading, base_extractiveness, 1880, 0.1).
narrative_ontology:measurement_basis(heb_lit_cont_be_t1880, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(hebrew_living_language__liturgical_continuity_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_living_language__liturgical_continuity_reading, identity_coordination).
narrative_ontology:affects_constraint(hebrew_living_language__liturgical_continuity_reading, hebrew_living_language__native_generation_reading).
narrative_ontology:affects_constraint(hebrew_living_language__liturgical_continuity_reading, hebrew_living_language__literary_revival_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'Hebrew never died' decomposes per the epsilon-invariance principle into three structurally distinct claims - liturgical continuity, native generation, literary revival - with different epsilon values, beneficiary sets, intervals, and failure modes. This file authors the liturgical-continuity member. Edges run from this story to both siblings because the recitationally preserved corpus is the raw material each sibling claim operates on: the revival's lexical and grammatical stock, and the maskilim's training, both came through the liturgical-educational channel. Upstream supply, not logical containment; neither sibling reading is foreclosed by this one.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
