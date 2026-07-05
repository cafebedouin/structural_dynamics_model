% ============================================================================
% CONSTRAINT STORY: jewish_territorial_claim__cultural_zionism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_territorial_claim__cultural_zionism_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: jewish_territorial_claim__cultural_zionism_reading
 *   human_readable: Cultural Zionist Reading: Jewish Spiritual Center Without Sovereignty Requirement
 *   domain: political_history/settler_colonialism/nationalism_studies
 *
 * SUMMARY:
 *   This constraint models the cultural Zionist reading of the Jewish
 *   territorial claim to Palestine — the Ahad Ha'am tradition holding that
 *   Palestine should become a spiritual and cultural center for the Jewish
 *   people through quality institution-building, without necessarily
 *   requiring political sovereignty or a Jewish demographic majority, and
 *   leaving open the possibility of a binational political framework in which
 *   Arab presence is not treated as an existential threat. This is a
 *   genuinely distinct structural claim from the sibling readings in the same
 *   kernel contest: political Zionism (statehood requiring Jewish majority),
 *   labor Zionism (national regeneration through settlement and land
 *   conquest), and revisionist Zionism (maximalist territory secured by
 *   force). The cultural reading is lower-extraction and lower-suppression
 *   than its siblings because it does not require displacing an Arab majority
 *   or compelling political submission through force — but it is not
 *   extraction-free: land purchase for cultural institutions still displaces
 *   Arab smallholders, and the reading's institutional victories (Hebrew
 *   University, Hebrew press) still depended on a colonial Mandate framework
 *   that adjudicated land and immigration in the Zionist movement's favor as
 *   a bloc. The reading's own internal victims are the binational and
 *   cultural-only advocates who lost the internal Zionist contest for
 *   resources and legitimacy once 1930s-40s events shifted movement consensus
 *   toward sovereignty-focused readings.
 *
 * KEY AGENTS:
 *   - hebrew_cultural_institutions: primary beneficiary (organized/constrained) — recipients of land, funding, and legitimacy under this reading's framing
 *   - yishuv_educators_and_writers: agenda-setting beneficiaries (moderate/constrained) — articulate and defend the cultural-center vision institutionally
 *   - palestinian_arab_landholders_displaced_by_land_purchase: primary victim (powerless/trapped) — bear land transfer costs even under the gentler reading
 *   - political_zionist_settlers_whose_majority_project_the_reading_constrains: secondary victim (organized/constrained) — experience the reading as withholding resources from majority-building
 *   - binational_advocates_marginalized_when_reading_loses_political_contest: excluded voice (powerless/trapped) — take the reading's own logic to a conclusion mainstream Zionism abandons
 *   - british_mandate_authorities: institutional observer/referee (institutional/analytical) — adjudicate land and immigration policy that determines which reading gains ground
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_territorial_claim__cultural_zionism_reading, 0.32).
domain_priors:suppression_score(jewish_territorial_claim__cultural_zionism_reading, 0.28).
domain_priors:theater_ratio(jewish_territorial_claim__cultural_zionism_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_territorial_claim__cultural_zionism_reading, extractiveness, 0.32).
narrative_ontology:constraint_metric(jewish_territorial_claim__cultural_zionism_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(jewish_territorial_claim__cultural_zionism_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_territorial_claim__cultural_zionism_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(jewish_territorial_claim__cultural_zionism_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_territorial_claim__cultural_zionism_reading, tangled_rope).
narrative_ontology:human_readable(jewish_territorial_claim__cultural_zionism_reading, "Cultural Zionist Reading: Jewish Spiritual Center Without Sovereignty Requirement").
narrative_ontology:topic_domain(jewish_territorial_claim__cultural_zionism_reading, "political_history/settler_colonialism/nationalism_studies").

domain_priors:requires_active_enforcement(jewish_territorial_claim__cultural_zionism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_territorial_claim__cultural_zionism_reading, '8734f322-b8da-458e-9ee7-e5fd058f1a98').
narrative_ontology:cs_kernel_codification('8734f322-b8da-458e-9ee7-e5fd058f1a98', distributed).
narrative_ontology:cs_authority_grounding('8734f322-b8da-458e-9ee7-e5fd058f1a98', distributed).
narrative_ontology:cs_reading_relation('8734f322-b8da-458e-9ee7-e5fd058f1a98', jewish_territorial_claim__political_zionism_reading, influences).
narrative_ontology:cs_reading_relation('8734f322-b8da-458e-9ee7-e5fd058f1a98', jewish_territorial_claim__labor_zionism_reading, influences).
narrative_ontology:cs_reading_relation('8734f322-b8da-458e-9ee7-e5fd058f1a98', jewish_territorial_claim__revisionist_zionism_reading, coexists_with).
narrative_ontology:cs_axiom('8734f322-b8da-458e-9ee7-e5fd058f1a98', foundational, sovereignty_not_necessary_for_national_regeneration).
narrative_ontology:cs_axiom_status(sovereignty_not_necessary_for_national_regeneration, overridden).
narrative_ontology:cs_axiom_grounding('8734f322-b8da-458e-9ee7-e5fd058f1a98', sovereignty_not_necessary_for_national_regeneration, conventional).
narrative_ontology:cs_axiom('8734f322-b8da-458e-9ee7-e5fd058f1a98', foundational, arab_presence_compatible_with_jewish_cultural_center).
narrative_ontology:cs_axiom_status(arab_presence_compatible_with_jewish_cultural_center, holdable).
narrative_ontology:cs_axiom_grounding('8734f322-b8da-458e-9ee7-e5fd058f1a98', arab_presence_compatible_with_jewish_cultural_center, instrumental).
narrative_ontology:cs_reference_frame('8734f322-b8da-458e-9ee7-e5fd058f1a98', ahad_haam_spiritual_center_doctrine).
narrative_ontology:cs_drift_state('8734f322-b8da-458e-9ee7-e5fd058f1a98', post_1948_state_formation, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('8734f322-b8da-458e-9ee7-e5fd058f1a98', '').
narrative_ontology:cs_kernel_id(jewish_territorial_claim__cultural_zionism_reading, jewish_territorial_claim).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__cultural_zionism_reading, hebrew_cultural_institutions).
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__cultural_zionism_reading, yishuv_educators_and_writers).
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__cultural_zionism_reading, diaspora_jews_seeking_spiritual_center).
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__cultural_zionism_reading, ahad_haam_intellectual_tradition).
narrative_ontology:constraint_victim(jewish_territorial_claim__cultural_zionism_reading, palestinian_arab_landholders_displaced_by_land_purchase).
narrative_ontology:constraint_victim(jewish_territorial_claim__cultural_zionism_reading, political_zionist_settlers_whose_majority_project_the_reading_constrains).
narrative_ontology:constraint_victim(jewish_territorial_claim__cultural_zionism_reading, binational_advocates_marginalized_when_reading_loses_political_contest).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Institutions like the Hebrew University and Hebrew-language press build a spiritual and linguistic revival center in Palestine. They receive land, funding, and legitimacy from the reading's framing of Palestine as cultural homeland rather than exclusive sovereign territory. Their continuation depends on some form of Jewish presence, but not on demographic majority or statehood.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__cultural_zionism_reading, hebrew_cultural_institutions, beneficiary,
    organized, generational, constrained, regional).

% Writers, teachers, and communal leaders in the Yishuv who articulate and defend the cultural-center vision against both assimilationism and maximalist political Zionism. They shape institutional priorities (schools, press, cultural societies) toward spiritual regeneration rather than territorial conquest, but hold little coercive power to enforce this vision against rival factions.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__cultural_zionism_reading, yishuv_educators_and_writers, beneficiary,
    moderate, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(jewish_territorial_claim__cultural_zionism_reading, yishuv_educators_and_writers, agenda_setter).

% Jews outside Palestine who do not intend to emigrate but want a living cultural and spiritual reference point — a center that regenerates Hebrew language, literature, and religious practice. They benefit from the reading without bearing settlement costs or land-conflict exposure themselves.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__cultural_zionism_reading, diaspora_jews_seeking_spiritual_center, beneficiary,
    moderate, civilizational, mobile, global).

% The intellectual lineage (Ahad Ha'am and successors) that articulates the doctrine itself: sets the normative terms — quality of settlement over quantity, spiritual center over sovereign state — against which other Zionist factions are judged. Wields no state power but shapes legitimacy discourse within the movement.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__cultural_zionism_reading, ahad_haam_intellectual_tradition, agenda_setter,
    moderate, civilizational, analytical, global).

% Arab tenant farmers and smallholders whose land is purchased for Jewish settlement and cultural institutions even under the 'quality over quantity' framework. The reading's rejection of demographic maximalism does not eliminate land transfer or the accompanying displacement, only its pace and declared ultimate aim. Their exit options are essentially nonexistent once land title changes hands.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__cultural_zionism_reading, palestinian_arab_landholders_displaced_by_land_purchase, payer,
    powerless, biographical, trapped, local).

% Settlers and organizers pursuing demographic majority and eventual statehood experience the cultural-center reading as a structural drag: it withholds moral and institutional backing from mass immigration and majority-building efforts, arguing these are unnecessary or premature. They pay in delayed resources, contested legitimacy, and internal movement friction.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__cultural_zionism_reading, political_zionist_settlers_whose_majority_project_the_reading_constrains, payer,
    organized, generational, constrained, regional).

% Intellectuals and small political groups (Brit Shalom and successors) who take the cultural-center logic to its fuller conclusion — a binational polity with no Jewish demographic or sovereign supremacy. As political Zionism and later Revisionism win the internal contest, these advocates are pushed to the margins of the movement's institutions and historiography, their position treated as naive rather than as the reading's own logical endpoint.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__cultural_zionism_reading, binational_advocates_marginalized_when_reading_loses_political_contest, excluded,
    powerless, generational, trapped, regional).

% Administer Palestine under the Mandate, adjudicate land and immigration policy, and referee among competing Zionist factions and Arab political leadership. Their policy choices (land transfer regulations, immigration quotas) determine which reading of the Jewish claim gains practical traction, without holding a fixed commitment to any one Zionist current.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__cultural_zionism_reading, british_mandate_authorities, observer,
    institutional, biographical, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a Jewish spiritual, linguistic, and cultural revival in Palestine — Hebrew-language institutions, religious and literary renewal, a 'center' that diaspora Jewry can orient toward — without committing the movement's resources and legitimacy to the harder, more contested goal of sovereign statehood or demographic majority.
% TRANSFER_FUNCTION: Moves land, cultural capital, and institutional legitimacy toward Hebrew cultural and educational institutions in Palestine; moves land specifically away from Arab smallholders through purchase (even if at a gentler pace and smaller scale than under labor or revisionist readings); moves internal movement authority away from maximalist statehood factions toward cultural-institutional leadership.
% ABSENT_VOICES: Palestinian Arab political leadership and landholders are not party to the internal Zionist debate this reading is embedded in — the 'binational framework' the reading gestures toward is discussed among Jewish intellectuals largely without Arab representation at the table. Binational advocates who take the logic furthest are pushed out of mainstream institutional memory as the political and revisionist readings win the contest for movement resources.
% DISAPPEARANCE_RATIONALE: Cultural Zionist institutions (Hebrew University, Hebrew press, literary revival) persisted and were absorbed into the eventual state's cultural apparatus, so their concrete legacy did not vanish with the reading's political defeat. But the specific claim — that a spiritual center suffices without sovereignty or majority — was politically overtaken by 1948; whether 'the world rearranges' depends on whether one asks about the cultural institutions (largely continuous) or the political claim itself (superseded).
% FOUNDING_PROBLEM: Diaspora Jewish assimilation and the erosion of a living Hebrew cultural and religious tradition, addressed by establishing a cultural-spiritual anchor in Palestine that could regenerate Jewish civilization without requiring every Jew to emigrate or a Jewish state to exist.
% FOUNDING_PROBLEM_CORROBORATION: Historians of the Zionist movement (both sympathetic and critical, including scholars outside the Ahad Ha'am tradition) attest the cultural-revival problem was real and partly addressed by Hebrew University and the literary renaissance; political Zionist and revisionist successors within the same movement attest the founding problem was superseded by 1930s–40s events (Nazi persecution, Mandate restrictions) that made sovereignty, not cultural presence, the operative problem — corroboration exists on both sides of the status question, from within and adjacent to the movement, not only from the reading's own beneficiaries.
narrative_ontology:disappearance_verdict(jewish_territorial_claim__cultural_zionism_reading, contested).
narrative_ontology:founding_problem_status(jewish_territorial_claim__cultural_zionism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_territorial_claim__cultural_zionism_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(jewish_territorial_claim__cultural_zionism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_territorial_claim__cultural_zionism_reading, 0.32, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_territorial_claim__cultural_zionism_reading_tests).
:- end_tests(jewish_territorial_claim__cultural_zionism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored moderate (0.32 by 1948) — well below what a political or revisionist reading of the same kernel would show, because this reading explicitly rejects displacement-through-majority as its aim. But it is not zero: land purchase for cultural and educational institutions still transfers land away from Arab smallholders, and the reading operates inside a Mandate structure that already privileges Jewish immigration and institution-building as a matter of policy. Suppression is lower (0.28) because the reading does not require an 'Iron Wall' of compelled Arab acceptance — it explicitly leaves open a binational framework — but active enforcement is still required in the loose sense of internal movement politics: cultural Zionist leaders had to actively defend resource allocation toward cultural institutions against louder claims for land conquest and mass settlement. Theater ratio is modest but rising (0.10 to 0.24 across the interval) reflecting how, as the reading loses the internal contest for movement resources after the 1930s, its remaining institutional presence increasingly functions as legitimating gloss ('we always said this was about culture, not conquest') rather than as the movement's operative logic.
 *
 * DIRECTIONALITY LOGIC:
 *   Hebrew cultural institutions and the Ahad Ha'am intellectual tradition sit near the beneficiary end: they receive resources and legitimacy without bearing the costs of majority-building conflict. Arab landholders sit at the full-target end: trapped, powerless, bearing displacement regardless of which Zionist reading is operative. Political Zionist settlers occupy an unusual position — they are structurally organized and powerful in the broader movement, but WITHIN this specific constraint (the cultural reading), they are payers: the reading actively withholds institutional backing from their majority project, delaying resources and contesting legitimacy. This is a case where the same real-world actor (political Zionist settlers) would show a different directionality under the sibling reading (political_zionism_reading) than under this one — precisely the ε-invariance discipline the kernel-reading structure is meant to preserve: this file's ε and structural claims are about THIS reading only.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (diaspora cultural erosion, need for a living Hebrew civilizational center) was substantially addressed by 1948 through Hebrew University, the literary revival, and Hebrew-language press — yet the reading's institutions persisted and were absorbed into the eventual sovereign state's apparatus even after the state-building political readings won the internal contest. This produces the contested founding_problem_status: the cultural mandate was arguably fulfilled or at least stabilized by the 1930s, but the reading's institutions did not sunset — they were repurposed as cultural infrastructure for a state the reading's own logic held to be unnecessary. This is not classic mandatrophy (a fully dead mandate persisting only through inertia) because live corroborating voices exist for both the 'still live' and 'superseded' readings of the founding problem's status.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cultural_claim_vs_land_transfer_independence,
    'Can a genuine spiritual/cultural center be built in Palestine without the underlying land purchases and demographic presence that structurally resemble the settlement mechanics of the more extractive sibling readings?',
    'Comparative institutional history: examine whether Hebrew University and comparable cultural institutions could have been established on leased, jointly-held, or non-displacing land arrangements, versus the actual historical pattern of land acquisition that accompanied even ''cultural'' Zionist institution-building.',
    'If cultural institution-building is shown to be inseparable in practice from land transfer displacing Arab smallholders, the reading''s lower extraction score is better understood as a matter of degree and pace rather than of structural kind relative to its siblings — narrowing but not eliminating the ε gap between readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cultural_claim_vs_land_transfer_independence, empirical, 'Whether cultural Zionism''s lower extraction is structural or merely a slower version of the same mechanism.').

omega_variable(
    binational_framework_genuineness,
    'Was the binational framework this reading gestures toward a genuine, actionable political alternative, or a rhetorical placeholder that was never seriously pursued by the reading''s own mainstream institutional leadership?',
    'Examine institutional resource allocation and political advocacy records: how much of the cultural Zionist establishment''s actual political capital, as opposed to individual intellectuals'' writings, was spent advancing binational political structures versus merely tolerating the idea as compatible with cultural-center goals.',
    'If binationalism was genuinely pursued institutionally, this reading''s coordination function (a shared cultural project non-threatening to Arab presence) is more substantiated. If it was rhetorical only, the reading''s lower suppression score partly reflects a claim not tested against the resistance it would have generated had it been seriously implemented.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(binational_framework_genuineness, conceptual, 'Whether the binational framework was a live political program or a compatible-sounding aspiration never institutionally tested.').

omega_variable(
    reading_selection_evidentiary_basis,
    'Given that all four Zionist readings existed as live currents within the same historical movement and often the same individuals shifted between them over time, on what basis is the cultural reading treated as a stable, separable constraint rather than a phase or minority current within a single more extractive trajectory?',
    'This is the CS-framing under-determination the kernel/reading structure is designed to surface: the alternative framing (treating ''Zionism'' as one constraint with an internally shifting ε) was considered and rejected per the ε-invariance principle, because the cultural, labor, political, and revisionist currents had genuinely different beneficiary/victim structures, different institutional bases, and different relationships to Arab political actors at any given historical moment — they are not merely different observables of one claim, they made different substantive commitments (e.g., on demographic majority as a goal) that a single ε cannot represent without erasing the internal contest documented in this reading''s own six_questions.absent_voices and founding_problem_status fields.',
    'Adopting the alternative single-constraint framing would collapse this reading''s distinctively lower extraction/suppression profile into an average with the more extractive sibling readings, obscuring the internal movement contest that the historical record shows was real and consequential (e.g., Brit Shalom''s marginalization). The decomposition into four sibling readings is retained as the more accurate structural representation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_selection_evidentiary_basis, conceptual, 'Alternative framing considered: one Zionism constraint with shifting ε, versus four sibling kernel readings — the latter was adopted.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_territorial_claim__cultural_zionism_reading, 1897, 1948).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t1897, jewish_territorial_claim__cultural_zionism_reading, theater_ratio, 1897, 0.1).
narrative_ontology:measurement(jewi_tr_t1907, jewish_territorial_claim__cultural_zionism_reading, theater_ratio, 1907, 0.13).
narrative_ontology:measurement(jewi_tr_t1917, jewish_territorial_claim__cultural_zionism_reading, theater_ratio, 1917, 0.16).
narrative_ontology:measurement(jewi_tr_t1929, jewish_territorial_claim__cultural_zionism_reading, theater_ratio, 1929, 0.19).
narrative_ontology:measurement(jewi_tr_t1939, jewish_territorial_claim__cultural_zionism_reading, theater_ratio, 1939, 0.24).
narrative_ontology:measurement(jewi_tr_t1948, jewish_territorial_claim__cultural_zionism_reading, theater_ratio, 1948, 0.22).

% Extraction over time
narrative_ontology:measurement(jewi_be_t1897, jewish_territorial_claim__cultural_zionism_reading, base_extractiveness, 1897, 0.18).
narrative_ontology:measurement(jewi_be_t1907, jewish_territorial_claim__cultural_zionism_reading, base_extractiveness, 1907, 0.22).
narrative_ontology:measurement(jewi_be_t1917, jewish_territorial_claim__cultural_zionism_reading, base_extractiveness, 1917, 0.26).
narrative_ontology:measurement(jewi_be_t1929, jewish_territorial_claim__cultural_zionism_reading, base_extractiveness, 1929, 0.3).
narrative_ontology:measurement(jewi_be_t1939, jewish_territorial_claim__cultural_zionism_reading, base_extractiveness, 1939, 0.34).
narrative_ontology:measurement(jewi_be_t1948, jewish_territorial_claim__cultural_zionism_reading, base_extractiveness, 1948, 0.32).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t1897, jewish_territorial_claim__cultural_zionism_reading, suppression_requirement, 1897, 0.15).
narrative_ontology:measurement(jewi_su_t1907, jewish_territorial_claim__cultural_zionism_reading, suppression_requirement, 1907, 0.18).
narrative_ontology:measurement(jewi_su_t1917, jewish_territorial_claim__cultural_zionism_reading, suppression_requirement, 1917, 0.2).
narrative_ontology:measurement(jewi_su_t1929, jewish_territorial_claim__cultural_zionism_reading, suppression_requirement, 1929, 0.24).
narrative_ontology:measurement(jewi_su_t1939, jewish_territorial_claim__cultural_zionism_reading, suppression_requirement, 1939, 0.27).
narrative_ontology:measurement(jewi_su_t1948, jewish_territorial_claim__cultural_zionism_reading, suppression_requirement, 1948, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_territorial_claim__cultural_zionism_reading, identity_coordination).
narrative_ontology:affects_constraint(jewish_territorial_claim__cultural_zionism_reading, political_zionism_reading).
narrative_ontology:affects_constraint(jewish_territorial_claim__cultural_zionism_reading, labor_zionism_reading).
narrative_ontology:affects_constraint(jewish_territorial_claim__cultural_zionism_reading, revisionist_zionism_reading).

% DUAL FORMULATION NOTE:
% This story is one of four sibling readings of the jewish_territorial_claim kernel. Each reading is authored as a separate, ε-invariant constraint with its own beneficiary/victim structure per the ε-invariance principle: cultural_zionism_reading (this file, tangled_rope, lower ε/suppression), labor_zionism_reading (settlement-through-labor, higher ε via land conquest), political_zionism_reading (statehood-with-majority, higher ε via demographic displacement requirement), revisionist_zionism_reading (maximalist territory via compelled force, highest ε and suppression). The readings are linked via affects_constraints because they compete for the same institutional resources and legitimacy within the historical Zionist movement, and outcomes in one reading's political fortunes (e.g., the political reading winning institutional dominance by the 1930s-40s) structurally influence resource availability and legitimacy for the others, particularly this cultural reading and its marginalized binational extension.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
