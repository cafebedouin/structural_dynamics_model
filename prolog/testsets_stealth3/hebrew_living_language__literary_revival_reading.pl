% ============================================================================
% CONSTRAINT STORY: hebrew_living_language__literary_revival_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   constraint_id: hebrew_living_language__literary_revival_reading
 *   human_readable: Haskalah Literary Chain Reading of Hebrew Language Vitality
 *   domain: historical linguistics / language revitalization
 *
 * SUMMARY:
 *   Between roughly 1780 and 1920 Hebrew persisted as a working literary
 *   medium across the diaspora although nobody spoke it as a native daily
 *   vernacular: maskilim composed novels, journalism, satire, and science
 *   popularization in a language learned from books, sustaining an unbroken
 *   written chain from medieval Hebrew into the modern period. This story
 *   instantiates ONE reading of the contested kernel hebrew_living_language —
 *   the literary-revival reading, which holds that written generative
 *   competence sufficed to keep the language alive. Per the
 *   epsilon-invariance principle, only this reading is authored here, as a
 *   clean single-constraint story: the referent of epsilon is the standing
 *   elite-literary arrangement itself, assessed by this reading's own lights,
 *   which yields very low extraction and no victim set. The sibling readings
 *   — liturgical_continuity_reading (liveness carried by unbroken liturgical
 *   recitation) and native_generation_reading (liveness requiring native
 *   daily generative speech) — are separate constraint files with their own
 *   epsilon values and stakeholder sets; they enter this file only through
 *   network edges and committer omegas. The family decomposes the colloquial
 *   label 'Hebrew is a living language,' which conflates three structurally
 *   distinct claims: this file's epsilon (~0.09) differs sharply from the
 *   sibling instances because the arrangements under assessment differ, not
 *   because one thing was measured three ways. Notably, this arrangement has
 *   NO agenda_setter: no seat administers it — coordination was
 *   self-organizing among writers, patrons, and readers, which is central to
 *   its low-coercion profile.
 *
 * KEY AGENTS:
 *   - - haskalah_maskilim: Primary producers (moderate/mobile) — sustain the written chain; bear uncompensated labor and poverty costs voluntarily
 *   - - hebrew_press_readership: Beneficiary constituency (moderate/mobile) — gains a cross-border medium their spoken languages cannot provide
 *   - - later_revival_movement: Downstream beneficiary (organized/identity_locked) — inherits the fully worked-out written standard on which the spoken revival was built
 *   - - rabbinic_traditionalists: Cost-bearing opposition seat (institutional/trapped) — loses exclusivity over the sacred register while retaining the liturgical register untouched
 *   - - yiddish_language_advocates: Excluded rival-medium advocates (organized) — object energetically but outside the Hebrew-medium arrangement
 *   - - linguistic_historians: Analytical observer (analytical/analytical) — adjudicates continuity, reachability, and the vitality criterion itself
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_living_language__literary_revival_reading, 0.09).
domain_priors:suppression_score(hebrew_living_language__literary_revival_reading, 0.08).
domain_priors:theater_ratio(hebrew_living_language__literary_revival_reading, 0.17).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_living_language__literary_revival_reading, extractiveness, 0.09).
narrative_ontology:constraint_metric(hebrew_living_language__literary_revival_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(hebrew_living_language__literary_revival_reading, theater_ratio, 0.17).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_living_language__literary_revival_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(hebrew_living_language__literary_revival_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_living_language__literary_revival_reading, rope).
narrative_ontology:human_readable(hebrew_living_language__literary_revival_reading, "Haskalah Literary Chain Reading of Hebrew Language Vitality").
narrative_ontology:topic_domain(hebrew_living_language__literary_revival_reading, "historical linguistics / language revitalization").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_living_language__literary_revival_reading, 'bf5dcd87-2ea7-495b-81d1-9690193520bf').
narrative_ontology:cs_kernel_codification('bf5dcd87-2ea7-495b-81d1-9690193520bf', distributed).
narrative_ontology:cs_authority_grounding('bf5dcd87-2ea7-495b-81d1-9690193520bf', expertise).
narrative_ontology:cs_interpretation_layer_present('bf5dcd87-2ea7-495b-81d1-9690193520bf').
narrative_ontology:cs_reading_relation('bf5dcd87-2ea7-495b-81d1-9690193520bf', hebrew_living_language__liturgical_continuity_reading, influences).
narrative_ontology:cs_reading_relation('bf5dcd87-2ea7-495b-81d1-9690193520bf', hebrew_living_language__native_generation_reading, forecloses).
narrative_ontology:cs_axiom('bf5dcd87-2ea7-495b-81d1-9690193520bf', foundational, written_generative_competence_suffices_for_language_life).
narrative_ontology:cs_axiom_status(written_generative_competence_suffices_for_language_life, holdable).
narrative_ontology:cs_axiom_grounding('bf5dcd87-2ea7-495b-81d1-9690193520bf', written_generative_competence_suffices_for_language_life, empirically_contingent).
narrative_ontology:cs_axiom('bf5dcd87-2ea7-495b-81d1-9690193520bf', secondary, unbroken_written_chain_preserves_structural_continuity).
narrative_ontology:cs_axiom_status(unbroken_written_chain_preserves_structural_continuity, holdable).
narrative_ontology:cs_axiom_grounding('bf5dcd87-2ea7-495b-81d1-9690193520bf', unbroken_written_chain_preserves_structural_continuity, empirically_contingent).
narrative_ontology:cs_reference_frame('bf5dcd87-2ea7-495b-81d1-9690193520bf', unbroken_written_chain_reference).
narrative_ontology:cs_drift_state('bf5dcd87-2ea7-495b-81d1-9690193520bf', post_native_revival_scrutiny, gap(axiom_overriding, minor, true)).
narrative_ontology:cs_created_at('bf5dcd87-2ea7-495b-81d1-9690193520bf', '').
narrative_ontology:cs_kernel_id(hebrew_living_language__literary_revival_reading, hebrew_living_language).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_living_language__literary_revival_reading, haskalah_maskilim).
narrative_ontology:constraint_beneficiary(hebrew_living_language__literary_revival_reading, hebrew_press_readership).
narrative_ontology:constraint_beneficiary(hebrew_living_language__literary_revival_reading, later_revival_movement).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(hebrew_living_language__literary_revival_reading, haskalah_maskilim).
narrative_ontology:constraint_victim(hebrew_living_language__literary_revival_reading, rabbinic_traditionalists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Educated men in Odessa, Vilna, Vienna, Warsaw, and Berlin who write novels, satire, scientific popularization, and journalism in a Hebrew learned entirely from books and school drills rather than childhood speech. Most hold day jobs or depend on patron stipends; Hebrew editing pays little or nothing, and the movement's journals survive on subsidies and unpaid labor. Many publish in Yiddish or Russian alongside Hebrew and can shift effort between language markets; choosing Hebrew is vocational and reputational, not contractual. They gain standing, a public, and a place in the emerging canon, and they personally absorb the economic cost of producing it.
narrative_ontology:constraint_stakeholder(hebrew_living_language__literary_revival_reading, haskalah_maskilim, beneficiary,
    moderate, biographical, mobile, continental).
narrative_ontology:stakeholder_secondary_role(hebrew_living_language__literary_revival_reading, haskalah_maskilim, payer).

% Graduates of heder and yeshiva who acquire literary Hebrew as a second or third language and subscribe to Ha-Melitz, Ha-Shahar, and the maskilic book series. The medium reaches across imperial borders their spoken languages do not cross, connecting Odessa to Vienna to Jerusalem in one conversation. Subscription is elective; lapsing costs nothing beyond leaving the discussion, and the same readers typically read Yiddish and Russian press as well.
narrative_ontology:constraint_stakeholder(hebrew_living_language__literary_revival_reading, hebrew_press_readership, beneficiary,
    moderate, biographical, mobile, continental).

% Teachers, journalists, and settlers of the Second Aliyah who inherit a fully worked-out written standard — lexicon, genre conventions, orthography, a century of compositional practice — and build schools and eventually household speech on top of it. Their projects are defined around Hebrew; abandoning it would dissolve the enterprise itself, so the commitment is constitutive rather than strategic. They treat the literary corpus as the language's lifeline and defend that account publicly in pedagogy and polemic.
narrative_ontology:constraint_stakeholder(hebrew_living_language__literary_revival_reading, later_revival_movement, beneficiary,
    organized, generational, identity_locked, continental).

% Communal rabbis and hasidic courts who hold custody of Hebrew as the sacred register of prayer and study. Secular fiction and journalism in that register reads to them as desecration of the holy tongue; they answer with bans (the 1786 Brody cherem against Mendele's first book), denunciation from the pulpit, and pressure on printers and subscribers. The bans fail to stop the practice. What they preserve — the liturgical and study register — continues undisturbed in parallel; their loss is exclusivity over the language's prestige, not possession of the language itself. Accommodating the secular register would dissolve their own custodial role, so the position is not negotiable from where they stand.
narrative_ontology:constraint_stakeholder(hebrew_living_language__literary_revival_reading, rabbinic_traditionalists, payer,
    institutional, civilizational, identity_locked, global).

% Writers and organizers — later the Bund, YIVO circles, and the Yiddishist literary movement — who argue that the Jewish folk speaks Yiddish and that elite investment in Hebrew diverts national-cultural energy from the language of the millions, including the overwhelmingly Yiddish-literate female half of the population. They build a vast parallel press and school movement. Inside the Hebrew-language journals, academies, and later university they have no seat; their objections are vigorous but register entirely in their own institutions, and crossing into the Hebrew-medium conversation would require abandoning their organizing premise.
narrative_ontology:constraint_stakeholder(hebrew_living_language__literary_revival_reading, yiddish_language_advocates, excluded,
    organized, generational, constrained, continental).

% Philologists and sociolinguists, from Heinrich Graetz's generation through Ullendorff, Rabin, and contemporary Hebrew linguistics, who trace acquisition pathways, measure continuity between medieval rabbinic Hebrew and the maskilic styles, document the melitzah technique, and argue about what 'living' should mean for a language. They collect nothing from any participant and depend on no patronage from the revival; their assessments are the closest thing the arrangement has to an outside audit.
narrative_ontology:constraint_stakeholder(hebrew_living_language__literary_revival_reading, linguistic_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hebrew_living_language__literary_revival_reading, diffuse).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of maintaining a shared high-register medium across a geographically dispersed, multilingual population with no common spoken vernacular: register and orthography standardization, transnational periodical networks, canon formation, and a training pipeline that turns text-only learners into composers — solved once, in print, instead of per-community.
% TRANSFER_FUNCTION: Moves writing labor, editorial labor, and attention from a small trilingual elite to a continent-wide readership; moves status, canonical permanence, and a shared cultural conversation to producers and readers alike; moves no monetary rent to any seat — patrons absorbed the deficits and the writers donated the margin.
% ABSENT_VOICES: Yiddish-language advocates and the mass of Yiddish-literate Jews — most acutely women, whose literacy was Yiddish-only — had no seat inside the Hebrew-medium public sphere. They did not suffer silence: they objected voluminously, but in their own institutions, and the Hebrew arrangement's boundaries are precisely what kept their objection from registering within it.
% DISAPPEARANCE_RATIONALE: Remove the literary chain overnight and the subsequent spoken revival loses its substrate: no worked-out modern lexicon, no genre conventions, no orthographic norms, and — decisively — no cadre of people already fluent in composing Hebrew, which is what the Second Aliyah teachers drew on. The revival either delays by generations or proceeds from rabbinic registers alone, producing a recognizably different language. Every named seat's situation changes materially; the world rearranges.
% FOUNDING_PROBLEM: After emancipation began dissolving traditional communal structures, Hebrew faced reduction to a prayer-book language; the arrangement was built to demonstrate that the language could carry contemporary science, fiction, journalism, and criticism in print — to prove viability before any native speaker existed, and thereby keep the language a going concern.
% FOUNDING_PROBLEM_CORROBORATION: Corroboration exists outside the beneficiary set: Heinrich Graetz's histories (sympathetic but institutionally independent of the maskilic circle) celebrate the literary revival as accomplished fact; Russian imperial censorship archives track the Hebrew press's growth as a phenomenon to be managed, indifferent to its self-description; rabbinic opponents corroborate inadvertently — the Brody cherem and successor bans testify that contemporaries outside the arrangement judged it significant enough to suppress; and twentieth-century descriptive linguistics (Ullendorff's and Rabin's continuity analyses) attests the unbroken written chain from outside the revival's advocacy. No load-bearing attestation comes from inside the beneficiary set.
narrative_ontology:disappearance_verdict(hebrew_living_language__literary_revival_reading, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_living_language__literary_revival_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_living_language__literary_revival_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(hebrew_living_language__literary_revival_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_living_language__literary_revival_reading, 0.09, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is very low (0.09) because no seat administers the arrangement and no rent flows anywhere: Hebrew editing famously impoverished its practitioners, patrons absorbed the deficits, and readers paid only elective subscription and acquisition effort — the largest cost in the system was the writers' own uncompensated labor, a voluntary expenditure, not a transfer collected by anyone. Suppression is near-zero (0.08) because the practice was enforced by nothing but vocation: all coercive pressure in the record ran AGAINST the practice (rabbinic bans, communal ostracism of maskilim), not in its defense. Theater is low (0.17): the corpus did real work — newspapers, translated science, original fiction, a developing critical vocabulary — though the mid-century polemical journal wars and melitzah display virtuosity raised the performative share before the late-period canon consolidated functionally. Accessibility collapse is moderate-low (0.30): Yiddish and German alternatives not only existed but thrived — Mendele Mocher Sforim published major work in both Yiddish and Hebrew — so understanding the arrangement never foreclosed exit from it. Resistance is the highest metric (0.42): hasidic hostility, the 1786 Brody cherem, traditionalist denunciations, and later Yiddishist counter-mobilization were persistent and sincere, yet failed to interrupt the chain. The measurement series run on one shared seven-point grid; both trajectories are roughly flat (extractiveness peaks mildly at 0.12 in the Ha-Shahar-era poverty decade, theater crests at 0.20 amid the journal feuds) — there is no cyclical dynamic and no enforcement-capacity history, so per the static-enforcement rule no suppression_requirement series is authored: the scalar 0.08 captures a constant picture. Claim and metrics are independent authored facts: rope is claimed from structure (voluntary participation, net-beneficiary seats, no enforcement machinery), and the metrics describe observed operation without being tuned to certify anything.
 *
 * PERSPECTIVAL GAP:
 *   Seats should compute differently. From the maskil seat the arrangement is a subsidized vocation: hard, poorly paid, chosen. From the revivalist seat it is providential infrastructure — the necessary bridge that made native speech possible, and their ideological identity (Hebrew as the vessel of national renewal) fuses with the arrangement so completely that exit is unthinkable; break that frame and the revivalist seat flips to the native-generation reading wholesale. From the traditionalist seat the same literary flourishing reads as slow theft of sanctity — a real cost borne with no compensation, experienced from a civilizational time horizon. From the Yiddishist seat it is elite vanity diverting scarce cultural energy from the language of the millions. Same nominal population, four different constraints experienced. Identity-lock operates on TWO seats here in opposite directions: revivalists are locked IN (ideological identity), traditionalists are locked AGAINST accommodation (religious identity fused with register sanctity — they could not license the secular register without dissolving their own custodial role). The engine derives per-seat classifications from the structural atoms; this commentary only maps where the divergence comes from.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the derivation: maskilim (mobile exit, voluntary participation) sit near the beneficiary end; the readership sits slightly above symmetric (medium gains, small acquisition costs); the revival movement's identity_locked exit locks it INTO the subsidy side — identity lock amplifies commitment, not exposure, for a beneficiary. The one authored override concerns rabbinic_traditionalists: the structural derivation from a payer-role stakeholder with trapped exit would push d toward the full-target end (0.85+), but that derivation errs here because nothing is transferred FROM them — their loss is devaluation of positional exclusivity over the sacred register, a competitive-devaluation cost, not collected rent; meanwhile the liturgical register they guard continues undisturbed in a disjoint register-space. Hence the override to d=0.60: substantial target-side position, short of full target because no seat receives anything extracted from them. Yiddish advocates derive as excluded outsiders — high nominal grievance, no extraction flowing over them. Linguistic historians are analytical seats with no stake in the outcome.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — demonstrating that Hebrew could carry contemporary thought in print before it had native speakers — was resolved by the mid-nineteenth century: the Smolenskin/Braude journal era settled viability beyond serious internal doubt. Founding-problem status is therefore authored dead, while the disappearance verdict is world_rearranges: remove the literary chain and the Ben-Yehuda-era revival starts from rabbinic registers alone, without a worked-out modern lexicon, genre conventions, or a cadre already fluent in composition. The dead-plus-world_rearranges combination normally raises a capture-or-zombie flag; here the flag should not stick, and the reasons are structural rather than rhetorical: theater_ratio stays low throughout (0.17 at interval end), gain_flow is diffuse (no seat captures — checked affirmatively against every stakeholder situation), and the persistence is load-bearing rather than inertial — the corpus is the grammatical and lexical substrate of Modern Hebrew, not a ceremony performed over a vacancy. The mandate did not outlive its function; it was fulfilled and transmuted from campaign into infrastructure. This classification guards against two misreadings: against calling the whole literary phase a piton on the strength of its melitzah theatrical surface (the functional share dominated throughout), and against complacent rope-in-perpetuity framing that ignores that the arrangement's original job is finished.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This constraint is one reading (literary_revival_reading) of the contested kernel hebrew_living_language; what would resolving the vitality-criterion dispute change structurally?',
    'Adjudication of the criterion dispute among the three sibling readings: if the native-generation criterion wins, the epsilon referent migrates to the post-1907 displacement contest and this reading''s low-extraction profile becomes moot for classification; if the liturgical criterion wins, the referent moves to synagogue practice.',
    'Epsilon is reading-indexed over a fixed referent (the standing elite-literary arrangement): this file''s very low value reflects THIS reading''s lights, not the topic. A sibling resolution relocates the referent and the beneficiary/victim structure entirely — the three readings are separate constraints, not one measured three ways.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer-frame routing: which reading of the living-Hebrew kernel this story instantiates and what sibling adoption would change.').

omega_variable(
    strict_reachability_of_written_chain,
    'Is every link in the 1780-1920 written chain strictly reachable — did each generation''s writers acquire generative competence solely from the prior generation''s readable output, with no hidden oral scaffolding?',
    'Archival acquisition-pathway studies: letters, diaries, and pedagogic manuals recording how Ahad Ha''am, Bialik, and Smolenskin actually learned to compose — text-only immersion versus chevruta drill, tutor correction, or recitation-plus-analysis hybrids.',
    'If strict reachability fails (oral scaffolding proves constitutive), this reading''s independence-from-speech claim weakens toward a hybrid with the liturgical/oral readings, and the sufficiency axiom''s evidential base narrows; if it holds, the written-chain reference frame stands intact.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(strict_reachability_of_written_chain, empirical, 'Whether the written chain is strictly self-contained or orally buttressed at its links.').

omega_variable(
    generativity_vs_recitation_boundary,
    'How much maskilic output was genuinely generative (novel composition) versus elaborate recomposition of canonical phrases — the melitzah mosaic style of scripture-quoting pastiche?',
    'Stylometric novelty analysis of the Haskalah corpus: rate of non-canonical syntactic frames, newly coined lexemes, and original narrative structures versus recycled prooftext chains.',
    'A high melitzah share would lower effective generativity, weaken the sufficiency axiom, and raise the theater_ratio retroactively — pushing this reading toward the inertial end; a low share confirms the generative-competence claim that distinguishes this reading from memorized-recitation accounts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(generativity_vs_recitation_boundary, empirical, 'Where the generative-versus-recitative boundary sits inside the Haskalah corpus.').

omega_variable(
    exclusion_cost_attribution,
    'Were women and the Yiddish-literate masses cost-bearers of the arrangement, or outsiders to it?',
    'Conceptual adjudication of attribution: the arrangement itself collected nothing from them — Hebrew literacy was never compulsory and their own institutions (Yiddish press, women''s devotional literature, tkhines) thrived concurrently; a victim reading would require attributing the general exclusivity of high Hebrew culture to this specific arrangement.',
    'If victims were admitted, the claimed type drifts toward tangled_rope and extractiveness rises materially; under the current no-victim-set attribution the profile stays low-extraction coordination. The delta''s ''no victim set'' is an authored judgment, not a measurement — this omega keeps it revisable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exclusion_cost_attribution, conceptual, 'Whether structural exclusion from Hebrew literacy converts outsiders into a victim set.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_living_language__literary_revival_reading, 1780, 1920).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t1780, hebrew_living_language__literary_revival_reading, theater_ratio, 1780, 0.08).
narrative_ontology:measurement(hebr_tr_t1806, hebrew_living_language__literary_revival_reading, theater_ratio, 1806, 0.1).
narrative_ontology:measurement(hebr_tr_t1832, hebrew_living_language__literary_revival_reading, theater_ratio, 1832, 0.14).
narrative_ontology:measurement(hebr_tr_t1858, hebrew_living_language__literary_revival_reading, theater_ratio, 1858, 0.18).
narrative_ontology:measurement(hebr_tr_t1884, hebrew_living_language__literary_revival_reading, theater_ratio, 1884, 0.2).
narrative_ontology:measurement(hebr_tr_t1902, hebrew_living_language__literary_revival_reading, theater_ratio, 1902, 0.19).
narrative_ontology:measurement(hebr_tr_t1920, hebrew_living_language__literary_revival_reading, theater_ratio, 1920, 0.17).

% Extraction over time
narrative_ontology:measurement(hebr_be_t1780, hebrew_living_language__literary_revival_reading, base_extractiveness, 1780, 0.07).
narrative_ontology:measurement(hebr_be_t1806, hebrew_living_language__literary_revival_reading, base_extractiveness, 1806, 0.08).
narrative_ontology:measurement(hebr_be_t1832, hebrew_living_language__literary_revival_reading, base_extractiveness, 1832, 0.11).
narrative_ontology:measurement(hebr_be_t1858, hebrew_living_language__literary_revival_reading, base_extractiveness, 1858, 0.12).
narrative_ontology:measurement(hebr_be_t1884, hebrew_living_language__literary_revival_reading, base_extractiveness, 1884, 0.1).
narrative_ontology:measurement(hebr_be_t1902, hebrew_living_language__literary_revival_reading, base_extractiveness, 1902, 0.09).
narrative_ontology:measurement(hebr_be_t1920, hebrew_living_language__literary_revival_reading, base_extractiveness, 1920, 0.09).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(hebrew_living_language__literary_revival_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_living_language__literary_revival_reading, information_standard).
narrative_ontology:affects_constraint(hebrew_living_language__literary_revival_reading, hebrew_living_language__liturgical_continuity_reading).
narrative_ontology:affects_constraint(hebrew_living_language__literary_revival_reading, hebrew_living_language__native_generation_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'Hebrew is a living language' decomposes into three structurally distinct claims (epsilon-invariance decomposition). Upstream: liturgical_continuity_reading supplies the textual base and continuous practice this reading draws on; this reading's literary success in turn changed the liturgical reading's legitimacy conditions (recitation shifted from sole sign of life to one strand among several) — hence the influences edge. Downstream: native_generation_reading assesses the endpoint of the chain this reading carries; its necessity premise directly contradicts this reading's sufficiency premise, a genuine foreclosure pair within any single framework. Each member file authors its own epsilon, beneficiaries, and victims over its own referent; this file's ~0.09 versus the siblings' differing profiles reflects different constraints, not observable-dependent measurement of one.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(hebrew_living_language__literary_revival_reading, institutional, 0.6).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
