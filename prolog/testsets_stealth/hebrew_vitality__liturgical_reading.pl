% ============================================================================
% CONSTRAINT STORY: hebrew_vitality__liturgical_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
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
 *   constraint_id: hebrew_vitality__liturgical_reading
 *   human_readable: Liturgical Preservation as Hebrew Vitality (Unbroken Ritual Use Occupies the Kernel)
 *   domain: sociolinguistic/religious
 *
 * SUMMARY:
 *   Between the decline of Hebrew as a daily vernacular (c. 200-400 CE) and
 *   its revival as a spoken language (1880s-1920s), Hebrew survived as the
 *   language of prayer, scripture reading, and rabbinic study. This story
 *   instantiates the liturgical_reading of the kernel hebrew_vitality: the
 *   claim that this unbroken ritual use was not mere preservation but the
 *   language's life itself — continuous liturgical voicing occupies the
 *   kernel of Hebrew's continuity. As a standing arrangement it coordinated a
 *   polyglot diaspora around one sacred linguistic medium, trained each
 *   generation of males into liturgical competence through heder and yeshiva,
 *   and reserved the holy tongue from mundane use. Epsilon is authored low
 *   (0.18) over this reading's own referent: the arrangement's costs (years
 *   of study, reciting in a non-spoken register) are constitutive of the
 *   religious practice rather than rents taken from participants, and no
 *   victim class is declared. This is one file in a three-member constraint
 *   family; the sibling readings author different epsilon values over
 *   adjacent referents, and the decomposition is documented in the network
 *   note and kernel_context.
 *
 * KEY AGENTS:
 *   - rabbinic_authorities: agenda-setting beneficiary (institutional/identity_locked) — administer the liturgical canon and its schools; their authority is constituted by the chain they maintain
 *   - liturgical_communities: net beneficiary with carried costs (organized/constrained) — pray and study in Hebrew across a vernacular-speaking diaspora
 *   - liturgical_learners: cost-bearing entrants (powerless/constrained) — acquire a non-spoken register under communal and family expectation
 *   - vernacular_praying_women: excluded from the competence structure (powerless/constrained) — devotional life conducted in translation
 *   - maskilim_and_reformers: excluded proposers (organized/mobile) — vernacular prayer and secular Hebrew, adjudicated out of the traditional forums
 *   - secular_hebrew_revivalists: excluded successors (organized/mobile) — declared the liturgical register a museum piece and built a spoken language from its preserved layers
 *   - linguistic_historians: analytical observers (analytical/analytical) — reconstruct the demographic and textual record from outside all holding communities
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_vitality__liturgical_reading, 0.18).
domain_priors:suppression_score(hebrew_vitality__liturgical_reading, 0.25).
domain_priors:theater_ratio(hebrew_vitality__liturgical_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_vitality__liturgical_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(hebrew_vitality__liturgical_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(hebrew_vitality__liturgical_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_vitality__liturgical_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(hebrew_vitality__liturgical_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_vitality__liturgical_reading, rope).
narrative_ontology:human_readable(hebrew_vitality__liturgical_reading, "Liturgical Preservation as Hebrew Vitality (Unbroken Ritual Use Occupies the Kernel)").
narrative_ontology:topic_domain(hebrew_vitality__liturgical_reading, "sociolinguistic/religious").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_vitality__liturgical_reading, '783c941c-682a-44b8-affc-ea23de4826fb').
narrative_ontology:cs_kernel_codification('783c941c-682a-44b8-affc-ea23de4826fb', distributed).
narrative_ontology:cs_authority_grounding('783c941c-682a-44b8-affc-ea23de4826fb', lineage).
narrative_ontology:cs_interpretation_layer_present('783c941c-682a-44b8-affc-ea23de4826fb').
narrative_ontology:cs_reading_relation('783c941c-682a-44b8-affc-ea23de4826fb', hebrew_vitality__native_daily_reading, forecloses).
narrative_ontology:cs_reading_relation('783c941c-682a-44b8-affc-ea23de4826fb', hebrew_vitality__hybrid_continuity_reading, forecloses).
narrative_ontology:cs_axiom('783c941c-682a-44b8-affc-ea23de4826fb', foundational, ritual_use_constitutes_linguistic_vitality).
narrative_ontology:cs_axiom_status(ritual_use_constitutes_linguistic_vitality, holdable).
narrative_ontology:cs_axiom_grounding('783c941c-682a-44b8-affc-ea23de4826fb', ritual_use_constitutes_linguistic_vitality, empirically_contingent).
narrative_ontology:cs_axiom('783c941c-682a-44b8-affc-ea23de4826fb', secondary, holy_tongue_integrity_requires_liturgical_exclusivity).
narrative_ontology:cs_axiom_status(holy_tongue_integrity_requires_liturgical_exclusivity, holdable).
narrative_ontology:cs_axiom_grounding('783c941c-682a-44b8-affc-ea23de4826fb', holy_tongue_integrity_requires_liturgical_exclusivity, theological).
narrative_ontology:cs_reference_frame('783c941c-682a-44b8-affc-ea23de4826fb', unbroken_liturgical_transmission).
narrative_ontology:cs_drift_state('783c941c-682a-44b8-affc-ea23de4826fb', contemporary, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('783c941c-682a-44b8-affc-ea23de4826fb', '').
narrative_ontology:cs_kernel_id(hebrew_vitality__liturgical_reading, hebrew_vitality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_vitality__liturgical_reading, rabbinic_authorities).
narrative_ontology:constraint_beneficiary(hebrew_vitality__liturgical_reading, liturgical_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(hebrew_vitality__liturgical_reading, liturgical_learners).
narrative_ontology:constraint_victim(hebrew_vitality__liturgical_reading, liturgical_communities).
narrative_ontology:constraint_victim(hebrew_vitality__liturgical_reading, liturgical_learners).
narrative_ontology:constraint_vindicates(hebrew_vitality__liturgical_reading, mesorah_continuity_doctrine).
narrative_ontology:constraint_vindicates(hebrew_vitality__liturgical_reading, liturgical_substrate_hypothesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Define the liturgical canon, standardize prayer texts and pronunciation traditions, and administer the schools through which Hebrew reading is taught. Their standing rests on an unbroken chain of transmission they both inherit and certify; stepping outside the chain — vernacularizing the liturgy, declaring the language's ritual life insufficient — would dissolve the authority they hold. Every question about the sacred language routes back to them, and interpretive centrality accrues to their seat.
narrative_ontology:constraint_stakeholder(hebrew_vitality__liturgical_reading, rabbinic_authorities, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(hebrew_vitality__liturgical_reading, rabbinic_authorities, beneficiary).

% Pray, study, and mark life events in Hebrew across a diaspora that otherwise speaks Yiddish, Ladino, Judeo-Arabic, and dozens of other vernaculars. The shared liturgical language lets a traveler from one community join another's service and ties each generation to the same texts its grandparents voiced. The cost they carry is maintaining Hebrew competence generation after generation in environments where the language has no daily function.
narrative_ontology:constraint_stakeholder(hebrew_vitality__liturgical_reading, liturgical_communities, beneficiary,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(hebrew_vitality__liturgical_reading, liturgical_communities, payer).

% Children and young men in traditional schooling spend years decoding scripture and prayerbook Hebrew they do not speak. What they gain is access: the ability to take a literate part in the liturgy and to enter advanced textual study. What it costs is years of effort in a register with no conversational payoff, under family and communal expectations that make declining difficult.
narrative_ontology:constraint_stakeholder(hebrew_vitality__liturgical_reading, liturgical_learners, payer,
    powerless, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(hebrew_vitality__liturgical_reading, liturgical_learners, beneficiary).

% In most traditional communities women were not taught Hebrew literacy. They prayed in the vernacular — Yiddish devotional prose, Ladino devotional verse — and experienced the sacred language from outside its competence structure, as a text recited on their behalf rather than a skill they held.
narrative_ontology:constraint_stakeholder(hebrew_vitality__liturgical_reading, vernacular_praying_women, excluded,
    powerless, biographical, constrained, global).

% Enlightenment-era writers and Reform congregations proposed praying in the vernacular and writing secular Hebrew. Traditional authorities ruled against vernacular prayer and barred some of their publications; the reformers left the adjudicating forums, founded their own schools and synagogues, and their proposals were answered from outside the arrangement rather than within it.
narrative_ontology:constraint_stakeholder(hebrew_vitality__liturgical_reading, maskilim_and_reformers, excluded,
    organized, biographical, mobile, continental).

% Late nineteenth-century nationalists in Eastern Europe and Ottoman Palestine declared the liturgical register a museum piece and set out to make Hebrew a spoken daily language. They drew their lexicon and grammar from the preserved textual layers while denying that preservation itself was life; they built their own schools, press, and speech communities outside the traditional system.
narrative_ontology:constraint_stakeholder(hebrew_vitality__liturgical_reading, secular_hebrew_revivalists, excluded,
    organized, generational, mobile, continental).

% Reconstruct the demographic and textual record: when Hebrew ceased to be spoken, what the liturgical chain preserved, and how much of the modern revival's vocabulary descends from which layer. They assess claims about the language's continuity from outside any of the communities that hold them.
narrative_ontology:constraint_stakeholder(hebrew_vitality__liturgical_reading, linguistic_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hebrew_vitality__liturgical_reading, rabbinic_authorities).
narrative_ontology:fixing_cost_class(hebrew_vitality__liturgical_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a single sacred linguistic medium across a polyglot diaspora: prayer, Torah reading, and rabbinic study are conducted in one shared language, so a Jew from any community can participate in any other's liturgy, and the language of the received texts remains continuously transmitted between generations.
% TRANSFER_FUNCTION: Moves study effort and communal resources from each generation of learners into liturgical competence and textual transmission, and consolidates interpretive authority in the rabbinic class that administers the holy tongue.
% ABSENT_VOICES: Vernacular-prayer advocates and secular-Hebrew writers were adjudicated out rather than answered within the arrangement's own forums; women, excluded from most Hebrew literacy education, experienced the arrangement from outside its competence structure entirely. All three groups would contest the claim that preservation imposed no cost.
% DISAPPEARANCE_RATIONALE: Had liturgical use ceased in the early centuries — had prayer and study shifted wholly to Aramaic, Greek, Arabic, or the later vernaculars — the through-line breaks: no continuously transmitted Mishnaic grammar, no shared prayerbook Hebrew, no trained readers. The nineteenth-century revivalists would have faced scholarly reconstruction from manuscripts rather than a living chain of competent readers; the modern language's lexicon, morphology, and idiomatic core descend substantially from the preserved layers. Jewish liturgical life, textual study, and the very possibility of the rapid revival all rearrange.
% FOUNDING_PROBLEM: After Hebrew ceased to be a daily vernacular (c. 200-400 CE), the community faced the problem of keeping the language of scripture, prayer, and rabbinic discourse intelligible and continuously transmitted across a diaspora that spoke Aramaic, Greek, Arabic, and later Yiddish, Ladino, and Judeo-Arabic.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated outside the beneficiary set by the revivalists themselves: Ben-Yehuda and the Hebrew Language Committee mined the preserved Mishnaic and liturgical layers for the revived lexicon, an implicit admission that the preserved material was the raw material of reconstruction. Secular linguistic historiography of the revival likewise treats the continuous textual tradition as the substrate without which the revival lacked a base. No source outside the traditional communities attests that preservation alone constituted vitality; what outsiders attest is narrower — that preservation was real, continuous, and load-bearing.
narrative_ontology:disappearance_verdict(hebrew_vitality__liturgical_reading, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_vitality__liturgical_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_vitality__liturgical_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(hebrew_vitality__liturgical_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_vitality__liturgical_reading, 0.18, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is authored low because the arrangement's costs are constitutive: study of the holy tongue is the religious practice itself, and the rabbinic class collects interpretive centrality without charging participants a rent they could refuse — there is no price mechanism, no captive revenue stream, and no suppressed cheaper alternative to the good provided. Suppression is moderate-low (0.25): the holiness boundary channeled Hebrew away from vernacular use for centuries, but it never closed the exits that mattered — daily life proceeded in Yiddish, Ladino, and Judeo-Arabic, and defection to vernacular prayer was possible and eventually taken at scale. Theater_ratio is low through most of the interval because recitation was functionally load-bearing inside the frame (obligation fulfilled, chain maintained), rising late as secularization thinned comprehension and rote performance grew. Accessibility_collapse is moderate (0.35): the alternatives — vernacular prayer, secular Hebrew, full revival — remained thinkable throughout and were eventually built. Resistance is moderate (0.45), concentrated in the modern era when the arrangement's monopoly on the vitality claim was first seriously contested. The suppression_requirement series traces enforcement capacity rather than achieved closure: it consolidates through the Geonic and medieval period (standardized liturgy, communal schooling mandates), peaks near 1100, then declines as emancipation fragments communal authority — the modern bans on vernacular prayer were loud but reached shrinking populations, which is why the series falls while resistance rises. The claim (rope) and the metrics are authored independently: the metrics describe a low-extraction coordination whose late-interval drift the engine weighs on its own.
 *
 * PERSPECTIVAL GAP:
 *   The rabbinic seat experiences the arrangement as the language's life itself — transmission is not a means to vitality but its substance, and the chain's continuity is the community's covenantal spine. The learner seat experiences years of decoding a register with no conversational payoff; inside the frame that cost is constitutive, from outside it reads as effort without return. The excluded seats experience the arrangement as a boundary: women outside the literacy system, reformers outside the adjudicating forums, revivalists accepting the substrate while denying it was life. Same nominal tradition, radically different seats — the engine computes these divergences from the power and exit data rather than from the reading's self-description.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary declarations drive low directionality for the rabbinic class (collects the authority premium; identity-locked exit keeps them at the arrangement's center by constitution rather than choice) and for the liturgical communities (collect continuity and liturgical access while carrying maintenance costs). The learners, declared payers with a beneficiary secondary role, sit mildly target-ward: their study cost is real but is the price of the access they are formed to seek, so their d sits near-symmetric rather than at the target pole. No victims are declared, so no seat computes at the full-target end; the excluded seats stand outside the benefit-and-cost flow but register the arrangement's boundary. Spatial scope is global — the diaspora-wide chain — which would amplify any extraction present; with epsilon this low the amplification has little to work on. Suppression remains a raw structural property here and is not scaled by power or scope; only extractiveness is.
 *
 * MANDATROPHY ANALYSIS:
 *   The rope claim guards against two mislabels. Against the snare reading — the polemic that the rabbis kept a corpse and charged admission — the structural data show a genuine coordination function (pan-diaspora liturgical unity and textual transmission across some seventy generations) with no victim set and no suppressed cheaper alternative to the good provided. Against the piton reading — pure performance maintained by inertia — the theater_ratio series stays low for most of the interval because recitation did real work inside the frame. The drift to watch is the modern one the measurements record: as comprehension thinned and enforcement reach declined, the arrangement's maintenance grew more theatrical even while the chain itself stayed unbroken; a corpus extending past 1920 should test whether the arrangement completes a piton drift in communities where recitation-without-comprehension becomes the norm. The founding problem — transmission across a non-speaking diaspora — remains live in traditional communities, so no mandatrophy is declared.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_vitality_predicate_contestation,
    'This constraint is one reading of the kernel hebrew_vitality: is vitality constituted by continuous ritual use (this reading), by native generation only (native_daily_reading), or by substrate plus reconstruction (hybrid_continuity_reading)? Which predicate the kernel takes determines the entire beneficiary and cost structure of the arrangement.',
    'No empirical resolution is available: the dispute is over the constitutive predicate, not the historical facts, so resolution would be a stipulation adopted by a community of assessment. The sibling stories carry the alternative structures as separate constraints.',
    'Under the native reading this arrangement becomes preservation-without-life: the recited language is a relic and the rabbinic class collects custody of a dead letter. Under the hybrid reading the arrangement is a necessary enabler with partial credit. Either restructure changes the epsilon referent and the seat map.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_vitality_predicate_contestation, conceptual, 'Which constitutive predicate the hebrew_vitality kernel takes is the irreducible framing dispute this reading sits inside.').

omega_variable(
    excluded_competence_cost_ambiguity,
    'Does the arrangement impose real costs on those kept outside liturgical competence — historically women, and poorer boys excluded from extended schooling — such that the no-victim-set declaration fails?',
    'Historical-demographic work on literacy rates, religious participation costs, and vernacular devotional substitution by gender and class in traditional communities.',
    'A documented cost-bearing class would add a victim set, raise epsilon, and push the computed type toward tangled_rope; its absence would confirm the rope reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(excluded_competence_cost_ambiguity, empirical, 'Whether exclusion from liturgical competence constitutes a cost-bearing class the no-victim declaration misses.').

omega_variable(
    rote_recitation_share,
    'What share of liturgical recitation, especially after emancipation-era secularization, proceeds without comprehension — and does recitation-without-understanding still count as the use that this reading says constitutes vitality?',
    'Comprehension surveys across traditional communities; the reading''s own criterion (use equals life) makes the rote share internally diagnostic.',
    'A high rote share raises theater_ratio and pressures the reading from within: the practice persists while the claimed function thins, producing piton-side drift for the arrangement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rote_recitation_share, empirical, 'Share of recitation without comprehension, and whether it still qualifies as vitality-constituting use under the reading''s own lights.').

omega_variable(
    revival_substrate_counterfactual,
    'Was the liturgically preserved substrate causally load-bearing for the revival''s success, or would reconstruction from manuscripts have sufficed — that is, did the unbroken chain actually occupy the kernel?',
    'Comparative linguistics of language revivals with and without continuous liturgical substrates (Hebrew versus Cornish, Manx, and other symbolic revivals), plus philological tracing of the revived lexicon''s layer origins.',
    'Confirmed substrate-dependence strengthens the reading''s core causal claim; a showing that manuscript reconstruction would have sufficed reduces the chain''s occupancy of the kernel and shifts weight to the hybrid reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(revival_substrate_counterfactual, empirical, 'Counterfactual dependence of the revival on the liturgically preserved substrate.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_vitality__liturgical_reading, 200, 1920).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t200, hebrew_vitality__liturgical_reading, theater_ratio, 200, 0.1).
narrative_ontology:measurement(hebr_tr_t500, hebrew_vitality__liturgical_reading, theater_ratio, 500, 0.1).
narrative_ontology:measurement(hebr_tr_t800, hebrew_vitality__liturgical_reading, theater_ratio, 800, 0.12).
narrative_ontology:measurement(hebr_tr_t1100, hebrew_vitality__liturgical_reading, theater_ratio, 1100, 0.15).
narrative_ontology:measurement(hebr_tr_t1400, hebrew_vitality__liturgical_reading, theater_ratio, 1400, 0.18).
narrative_ontology:measurement(hebr_tr_t1700, hebrew_vitality__liturgical_reading, theater_ratio, 1700, 0.24).
narrative_ontology:measurement(hebr_tr_t1920, hebrew_vitality__liturgical_reading, theater_ratio, 1920, 0.28).

% Extraction over time
narrative_ontology:measurement(hebr_be_t200, hebrew_vitality__liturgical_reading, base_extractiveness, 200, 0.08).
narrative_ontology:measurement(hebr_be_t500, hebrew_vitality__liturgical_reading, base_extractiveness, 500, 0.09).
narrative_ontology:measurement(hebr_be_t800, hebrew_vitality__liturgical_reading, base_extractiveness, 800, 0.1).
narrative_ontology:measurement(hebr_be_t1100, hebrew_vitality__liturgical_reading, base_extractiveness, 1100, 0.11).
narrative_ontology:measurement(hebr_be_t1400, hebrew_vitality__liturgical_reading, base_extractiveness, 1400, 0.12).
narrative_ontology:measurement(hebr_be_t1700, hebrew_vitality__liturgical_reading, base_extractiveness, 1700, 0.14).
narrative_ontology:measurement(hebr_be_t1920, hebrew_vitality__liturgical_reading, base_extractiveness, 1920, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t200, hebrew_vitality__liturgical_reading, suppression_requirement, 200, 0.3).
narrative_ontology:measurement(hebr_su_t500, hebrew_vitality__liturgical_reading, suppression_requirement, 500, 0.32).
narrative_ontology:measurement(hebr_su_t800, hebrew_vitality__liturgical_reading, suppression_requirement, 800, 0.35).
narrative_ontology:measurement(hebr_su_t1100, hebrew_vitality__liturgical_reading, suppression_requirement, 1100, 0.35).
narrative_ontology:measurement(hebr_su_t1400, hebrew_vitality__liturgical_reading, suppression_requirement, 1400, 0.3).
narrative_ontology:measurement(hebr_su_t1700, hebrew_vitality__liturgical_reading, suppression_requirement, 1700, 0.28).
narrative_ontology:measurement(hebr_su_t1920, hebrew_vitality__liturgical_reading, suppression_requirement, 1920, 0.25).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_vitality__liturgical_reading, identity_coordination).
narrative_ontology:affects_constraint(hebrew_vitality__liturgical_reading, hebrew_vitality__native_daily_reading).
narrative_ontology:affects_constraint(hebrew_vitality__liturgical_reading, hebrew_vitality__hybrid_continuity_reading).

% DUAL FORMULATION NOTE:
% The kernel hebrew_vitality is a single contested commitment decomposed into three readings with distinct epsilon referents and structures. This liturgical_reading authors low epsilon (0.18) over the ritual-preservation arrangement: costs are constitutive and no victim set exists. The native_daily_reading treats ritual recitation as preservation without life and authors its epsilon over a referent in which recitation-without-comprehension is imposed cost on a community told the language is alive. The hybrid_continuity_reading splits the referent into substrate (preserved, low extraction) and reconstruction (contested, career- and nation-load-bearing). The readings disagree on the constitutive predicate, not the facts; each is authored as a separate epsilon-invariant constraint and linked here. Upstream/downstream: the liturgical reading's preserved substrate is the evidentiary base both siblings argue over, so this story sits upstream of both.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
