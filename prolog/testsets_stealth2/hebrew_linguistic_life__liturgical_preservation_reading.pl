% ============================================================================
% CONSTRAINT STORY: hebrew_linguistic_life__liturgical_preservation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_linguistic_life__liturgical_preservation_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: hebrew_linguistic_life__liturgical_preservation_reading
 *   human_readable: Liturgical-Preservation Criterion of Hebrew Linguistic Life
 *   domain: sociolinguistics/religious_studies/nationalism
 *
 * SUMMARY:
 *   From roughly the third century CE, Hebrew ceased to be a spoken
 *   vernacular while remaining the language of prayer, scripture reading, and
 *   rabbinic study, carried by an unbroken chain of recitation and
 *   transmission across a stateless diaspora. This story instantiates the
 *   liturgical_preservation_reading of the kernel hebrew_linguistic_life: on
 *   this reading, that unbroken chain IS the language's life - Hebrew never
 *   died, because life is defined by continuous sacred transmission
 *   regardless of vernacular use - and Ben-Yehuda's revival was therefore not
 *   a resurrection but a desecration, with the injured party being the sacred
 *   tradition itself rather than any human population. Per the
 *   epsilon-invariance principle, the colloquial question 'was Hebrew alive?'
 *   decomposes into three structurally distinct constraints (this reading and
 *   its two siblings), each with its own epsilon, victim set, and
 *   classification; they are linked through network.affects_constraints. The
 *   claim/metric gap is deliberate: the reading presents the criterion as a
 *   covenantal given (claimed mountain, emerges naturally), while the
 *   authored metrics independently describe moderate extraction, substantial
 *   suppression of the vernacular alternative, and low theatricality - the
 *   engine measures that divergence rather than the author reconciling it.
 *
 * KEY AGENTS:
 *   - - rabbinic_scholarly_establishment: Agenda-setting custodian (institutional/identity_locked) - defines the criterion, administers the chain, collects interpretive authority
 *   - - yeshiva_and_liturgical_institutions: Institutional beneficiary (organized/constrained) - chartered and funded by the transmission mandate
 *   - - observant_lay_communities: Primary payer with secondary beneficiary position (organized/identity_locked) - bears recitation and study labor, receives continuity
 *   - - sacred_tradition_of_lashon_hakodesh: Declared victim under this reading (non-agent; powerless/trapped) - the injured party when the tongue is profaned
 *   - - hebrew_revivalists_secular_movement: Excluded violator-seat (powerful/mobile) - holds no legitimate voice inside the arrangement
 *   - - comparative_linguistics_scholarship: Analytical observer (analytical/analytical) - attests the chain, disputes the criterion
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_linguistic_life__liturgical_preservation_reading, 0.36).
domain_priors:suppression_score(hebrew_linguistic_life__liturgical_preservation_reading, 0.62).
domain_priors:theater_ratio(hebrew_linguistic_life__liturgical_preservation_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_linguistic_life__liturgical_preservation_reading, extractiveness, 0.36).
narrative_ontology:constraint_metric(hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_linguistic_life__liturgical_preservation_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(hebrew_linguistic_life__liturgical_preservation_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_linguistic_life__liturgical_preservation_reading, mountain).
narrative_ontology:human_readable(hebrew_linguistic_life__liturgical_preservation_reading, "Liturgical-Preservation Criterion of Hebrew Linguistic Life").
narrative_ontology:topic_domain(hebrew_linguistic_life__liturgical_preservation_reading, "sociolinguistics/religious_studies/nationalism").

domain_priors:requires_active_enforcement(hebrew_linguistic_life__liturgical_preservation_reading).
domain_priors:emerges_naturally(hebrew_linguistic_life__liturgical_preservation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_linguistic_life__liturgical_preservation_reading, '684c3682-485e-46dd-8962-3204500d185c').
narrative_ontology:cs_kernel_codification('684c3682-485e-46dd-8962-3204500d185c', fixed_text).
narrative_ontology:cs_authority_grounding('684c3682-485e-46dd-8962-3204500d185c', lineage).
narrative_ontology:cs_interpretation_layer_present('684c3682-485e-46dd-8962-3204500d185c').
narrative_ontology:cs_reading_relation('684c3682-485e-46dd-8962-3204500d185c', hebrew_linguistic_life__native_generational_reading, forecloses).
narrative_ontology:cs_reading_relation('684c3682-485e-46dd-8962-3204500d185c', hebrew_linguistic_life__marketplace_pidgin_reading, coexists_with).
narrative_ontology:cs_axiom('684c3682-485e-46dd-8962-3204500d185c', foundational, liturgical_continuity_constitutes_linguistic_life).
narrative_ontology:cs_axiom_status(liturgical_continuity_constitutes_linguistic_life, holdable).
narrative_ontology:cs_axiom_grounding('684c3682-485e-46dd-8962-3204500d185c', liturgical_continuity_constitutes_linguistic_life, theological).
narrative_ontology:cs_axiom('684c3682-485e-46dd-8962-3204500d185c', foundational, vernacular_use_desecrates_sacred_tongue).
narrative_ontology:cs_axiom_status(vernacular_use_desecrates_sacred_tongue, holdable).
narrative_ontology:cs_axiom_grounding('684c3682-485e-46dd-8962-3204500d185c', vernacular_use_desecrates_sacred_tongue, theological).
narrative_ontology:cs_reference_frame('684c3682-485e-46dd-8962-3204500d185c', unbroken_liturgical_continuity_state).
narrative_ontology:cs_drift_state('684c3682-485e-46dd-8962-3204500d185c', post_revival_contemporary, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('684c3682-485e-46dd-8962-3204500d185c', '').
narrative_ontology:cs_kernel_id(hebrew_linguistic_life__liturgical_preservation_reading, hebrew_linguistic_life).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__liturgical_preservation_reading, rabbinic_scholarly_establishment).
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__liturgical_preservation_reading, yeshiva_and_liturgical_institutions).
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__liturgical_preservation_reading, observant_lay_communities).
narrative_ontology:constraint_victim(hebrew_linguistic_life__liturgical_preservation_reading, sacred_tradition_of_lashon_hakodesh).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(hebrew_linguistic_life__liturgical_preservation_reading, observant_lay_communities).
narrative_ontology:constraint_vindicates(hebrew_linguistic_life__liturgical_preservation_reading, unbroken_masoretic_transmission_chain).
narrative_ontology:constraint_vindicates(hebrew_linguistic_life__liturgical_preservation_reading, sanctity_requires_separation_from_vernacular_use).
narrative_ontology:constraint_vindicates(hebrew_linguistic_life__liturgical_preservation_reading, covenantal_recitation_mandate).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Defines what counts as the Hebrew language's continued life, sets the curriculum of recitation and study, and rules on which uses of the tongue preserve or violate its sanctity. Collects interpretive authority, institutional permanence, and communal deference from administering the transmission chain. Its office exists only inside this arrangement; abandoning custody would dissolve the authority the office confers.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, rabbinic_scholarly_establishment, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(hebrew_linguistic_life__liturgical_preservation_reading, rabbinic_scholarly_establishment, beneficiary).

% Academies, synagogues, and printing houses whose enrollment, budgets, and stated purpose rest on the mandate of continuous recitation and study. They receive students, funding, and social license from the arrangement; pivoting to vernacular-oriented instruction would forfeit their charter and their constituency.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, yeshiva_and_liturgical_institutions, beneficiary,
    organized, generational, constrained, global).

% Devote daily hours to prescribed recitation, annual reading cycles, and childhood study of texts in a language they do not speak at home, conducting mundane life in Yiddish, Ladino, Judeo-Arabic, or surrounding vernaculars. They bear the labor and the narrowed linguistic repertoire, and they receive communal continuity, textual inheritance, and identity across dispersion. Leaving the practice means leaving the covenant community that organizes marriage, burial, and belonging.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, observant_lay_communities, payer,
    organized, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(hebrew_linguistic_life__liturgical_preservation_reading, observant_lay_communities, beneficiary).

% The corpus of scripture, mishnah, liturgy, and its sanctioned interpretation, which exists only insofar as living mouths recite and transmit it. Under this reading it is the injured party when the tongue is turned to mundane use: sanctity, once diffused into street speech, cannot be recalled, and the tradition's distinct register is worn away. It has no agency of its own; its persistence depends entirely on the communities that maintain the chain.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, sacred_tradition_of_lashon_hakodesh, payer,
    powerless, civilizational, trapped, global).
narrative_ontology:stakeholder_non_agent(hebrew_linguistic_life__liturgical_preservation_reading, sacred_tradition_of_lashon_hakodesh).

% Ben-Yehuda's circle and the households that raised children in spoken Hebrew, later the state with its schools, newspapers, and army. Inside this arrangement they hold no legitimate seat: their premise that vernacular vitality constitutes the language's life is heard only as the sin of profanation, not as a position to be answered. They built their own institutions outside the custodial frame and need nothing from it.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, hebrew_revivalists_secular_movement, excluded,
    powerful, generational, mobile, national).

% Documents the chain of textual transmission, the diglossia that kept Hebrew off the street for some seventeen centuries, and the speed of the revival once vernacularization began. Attests that the recitation chain never broke, while noting that every other sacred-language community faces the same definitional choice now confronting Hebrew's custodians.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, comparative_linguistics_scholarship, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hebrew_linguistic_life__liturgical_preservation_reading, rabbinic_scholarly_establishment).
narrative_ontology:fixing_cost_class(hebrew_linguistic_life__liturgical_preservation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains unbroken textual fidelity and communal identity for a stateless, globally dispersed population: fixed recitation cycles, a standardized consonantal text, and a shared study regimen solve the continuity problem across generations and continents without territory or sovereignty.
% TRANSFER_FUNCTION: Moves daily labor - recitation, memorization, study, teaching time - from every observant generation, lay and elite alike, into the maintenance of the transmission chain; moves interpretive authority and institutional permanence upward to the custodial class; and, on this reading's own account, places sanctity itself at risk whenever the tongue crosses into mundane use.
% ABSENT_VOICES: The revivalist premise - that vernacular vitality constitutes the language's life - has no legitimate seat inside the arrangement; it enters only as condemned desecration. The sibling criteria (native generational acquisition, practical inter-communal function) are equally absent voices, located outside the beit midrash in the secular press, the university, and the revived street.
% DISAPPEARANCE_RATIONALE: Overnight removal would not restore anyone's ancestral vernacular; it would collapse the custodial warrant - the establishment's criteria-setting office, the institutions' charter, the sanctity boundary that polices mundane use - and leave the definitional field to the rival criteria already operating in the revived vernacular. Human carriers would keep praying, but the claim that this alone constitutes the language's life would lose its enforcing apparatus.
% FOUNDING_PROBLEM: After the destruction of the Second Temple and the gradual loss of Hebrew as a spoken vernacular in late antiquity, a text-centered people needed its covenantal language kept continuous without territorial institutions: the arrangement was built to preserve textual fidelity and communal cohesion through recitation and study when Hebrew was nobody's mother tongue.
% FOUNDING_PROBLEM_CORROBORATION: That the founding condition was real is corroborated outside the beneficiary set: secular historiography and linguistics document the shift to Aramaic, Greek, and later Yiddish, Ladino, and Judeo-Arabic vernaculars, and the diaspora-wide learned correspondence conducted in Hebrew. That the problem REMAINS live is attested almost solely by the custodial parties themselves; outside scholarship treats the founding condition as superseded by the revival and reads the continuing liveness claim as boundary maintenance. Stated plainly: no external source attests the founding problem's continuing primacy.
narrative_ontology:disappearance_verdict(hebrew_linguistic_life__liturgical_preservation_reading, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_linguistic_life__liturgical_preservation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_linguistic_life__liturgical_preservation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(hebrew_linguistic_life__liturgical_preservation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_linguistic_life__liturgical_preservation_reading, 0.36, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_linguistic_life__liturgical_preservation_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(hebrew_linguistic_life__liturgical_preservation_reading, ExtMetricName, E),
    domain_priors:suppression_score(hebrew_linguistic_life__liturgical_preservation_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(hebrew_linguistic_life__liturgical_preservation_reading),
    narrative_ontology:constraint_metric(hebrew_linguistic_life__liturgical_preservation_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(hebrew_linguistic_life__liturgical_preservation_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(hebrew_linguistic_life__liturgical_preservation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.36 at interval end): the arrangement moves real labor - daily recitation, annual reading cycles, childhood study hours - from every observant generation into the chain, and consolidates interpretive authority in the custodial class, but the reading's own lights discount this as covenantal service rather than rent, and the reading-indexed value reflects that discount. Suppression is substantial (0.62) because the arrangement's persistence has always required holding the vernacular alternative down - first by communal norms, latterly by explicit refusal to recognize the revived Ivrit - and because much of that suppression is internalized as holiness consciousness. Theater is low (0.15) because recitation is not a proxy for the function here: under this criterion the recitation IS the function, so performance and purpose coincide; the slow rise tracks rote transmission without comprehension and post-revival performative insistence. Accessibility collapse is moderate (0.58): within the governed population the alternative (mundane Hebrew) is nearly unthinkable, but the alternative demonstrably exists and thrives outside the arrangement, so collapse is not near-total. Resistance is moderate (0.48): maskilic and revivalist practice, and the simple fact of millions speaking Ivrit, constitute standing resistance the arrangement must actively oppose. The measurement series run on one shared grid (centuries 200-2000 CE, seven points, all three metrics at every point). Enforcement shows a non-cyclical arc rather than oscillation: kehillah-era communal discipline peaks in the medieval period, erodes with emancipation (1700 trough), and resurges as anti-revival enforcement after the revival creates a new enforcement front. Base extractiveness drifts slowly upward with custodial consolidation - no crisis cycle drives it.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the custodial seat the arrangement is faithful stewardship of a trust - the closest thing to a natural order the tradition knows. From the lay payer seat the same structure is a lifelong yoke borne under identity lock: exit means leaving the community that organizes marriage, burial, and belonging, so the costs are amplified by trapped position. From the revivalist seat - which this arrangement refuses a legitimate voice - the whole structure looks like elaborate tomb-keeping around a language its custodians declined to let live; that seat's classification belongs to the sibling stories, not this one. The analytical observer sees all three at once. The engine computes this divergence from the structural data; the authored mountain claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the derivation: the establishment and the chartered institutions sit near the beneficiary end (low d, subsidized or damped chi), with the establishment's identity_lock noted - its office exists only inside the arrangement, but that locks it INTO benefit, not out. Observant lay communities carry the payer role with identity_locked exit, pushing them toward the full-target end: they bear the labor and cannot cheaply leave. The sacred tradition is authored as victim per this reading's distinctive delta, but it is a non-agent (agent: false) and is therefore excluded from the directionality arithmetic - its victimhood is doctrinal, not a structural extraction relationship, and the engine correctly declines to feed a personification into chi. The revivalists hold the excluded role with mobile exit: they built institutions outside the arrangement and need nothing from it, so the arrangement's enforcement reaches them only as condemnation, not as extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem - keeping a covenant's language continuous for a people that had lost vernacular speech - was real and is externally corroborated; after the revival it is transformed rather than solved, since the chain continues to operate inside communities that now coexist with a thriving vernacular Hebrew they decline to recognize. The mandate has not atrophied into performance (theater is low, the recitation is the function), so this is not a piton; nor have the gains been captured into a monopoly post-revival, since the custodians no longer control the linguistic field they once defined. The classification's main protective work here is against two mislabels at once: against calling the arrangement pure extraction (it solves a genuine continuity problem no sibling mechanism solved for eighteen centuries), and against letting the mountain presentation stand unexamined while identifiable custodians collect authority from administering the criterion - which is precisely what the false-summit evaluation exists to test.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    naturality_of_liturgical_criterion,
    'Is the equation of linguistic life with unbroken liturgical transmission a structural feature of the tradition''s own nature, or a constructed criterion maintained because identifiable custodians collect authority from administering it?',
    'Comparative sacred-language history: examine whether the custodial classes of liturgical Latin, ecclesiastical Slavonic, and Qur''anic Arabic defend the same equation of life with liturgy, and whether the equation tracks custodial benefit or independent textual outcomes such as fidelity and continuity.',
    'If constructed, the mountain presentation fails and the arrangement recomputes as a hybrid in which a genuine continuity function coexists with custodial rent concentrated in the transmitting class; if natural, the named beneficiaries are stewards collecting no more than their keep.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(naturality_of_liturgical_criterion, conceptual, 'Natural-law versus constructed status of the liturgical criterion of linguistic life.').

omega_variable(
    kernel_reading_commitment,
    'This constraint is one reading of the kernel hebrew_linguistic_life; what would the sibling readings change structurally, and where exactly is the disagreement located?',
    'Adjudication of the vitality criterion itself: the native_generational_reading makes pre-revival Hebrew dead and the revival a resurrection; the marketplace_pidgin_reading makes the learned written register an inter-communal practical medium and the revival a register shift; this reading alone locates life in the recitation chain and the injury in desecration.',
    'Sibling adoption empties this reading''s victim set (the sacred tradition ceases to be the injured party), reverses the desecration verdict on the revival project, and reassigns epsilon to a different referent; classifications across readings are not comparable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: one of three rival criteria of Hebrew''s linguistic life.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of vernacular Hebrew use structural (communal sanction, educational steering, institutional refusal to recognize Ivrit) or internalized (holiness consciousness that experiences mundane speech as profanity even absent sanction)?',
    'Post-exit suppression trajectory: track secularized descendants and traditionally raised individuals living in secular Hebrew-speaking environments for residual aversion to mundane use; if aversion persists after sanction machinery is removed, the internalized share is substantial.',
    'Internalized suppression travels with the agent after exit, raising effective suppression above the structural measure and hardening the identity-lock of the payer seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression of vernacular use.').

omega_variable(
    victim_attribution_location,
    'Does the injury of vernacularization fall on the sacred tradition itself (this reading''s attribution), on the generations denied native acquisition (the native-generational attribution), or on no one at all?',
    'Traceable-harm analysis: specify what measurably degrades when a sacred language is vernacularized - textual drift, interpretive-authority loss, register erosion - versus mere change with winners and losers.',
    'Determines whether the victim entry names a real injured party or a doctrinal personification; a personified victim cannot ground extraction claims and shifts the arrangement''s computed classification toward pure coordination carrying ideological enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_attribution_location, conceptual, 'Location of the injury this reading attributes to desecration.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_linguistic_life__liturgical_preservation_reading, 200, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(heb_lit_pres_tr_t200, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 200, 0.08).
narrative_ontology:measurement_basis(heb_lit_pres_tr_t200, observed).
narrative_ontology:measurement(heb_lit_pres_tr_t500, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 500, 0.09).
narrative_ontology:measurement_basis(heb_lit_pres_tr_t500, observed).
narrative_ontology:measurement(heb_lit_pres_tr_t800, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 800, 0.1).
narrative_ontology:measurement_basis(heb_lit_pres_tr_t800, observed).
narrative_ontology:measurement(heb_lit_pres_tr_t1100, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 1100, 0.11).
narrative_ontology:measurement_basis(heb_lit_pres_tr_t1100, observed).
narrative_ontology:measurement(heb_lit_pres_tr_t1400, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 1400, 0.12).
narrative_ontology:measurement_basis(heb_lit_pres_tr_t1400, observed).
narrative_ontology:measurement(heb_lit_pres_tr_t1700, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 1700, 0.13).
narrative_ontology:measurement_basis(heb_lit_pres_tr_t1700, observed).
narrative_ontology:measurement(heb_lit_pres_tr_t2000, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 2000, 0.15).
narrative_ontology:measurement_basis(heb_lit_pres_tr_t2000, observed).

% Extraction over time
narrative_ontology:measurement(heb_lit_pres_be_t200, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 200, 0.26).
narrative_ontology:measurement_basis(heb_lit_pres_be_t200, observed).
narrative_ontology:measurement(heb_lit_pres_be_t500, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 500, 0.28).
narrative_ontology:measurement_basis(heb_lit_pres_be_t500, observed).
narrative_ontology:measurement(heb_lit_pres_be_t800, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 800, 0.31).
narrative_ontology:measurement_basis(heb_lit_pres_be_t800, observed).
narrative_ontology:measurement(heb_lit_pres_be_t1100, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 1100, 0.33).
narrative_ontology:measurement_basis(heb_lit_pres_be_t1100, observed).
narrative_ontology:measurement(heb_lit_pres_be_t1400, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 1400, 0.35).
narrative_ontology:measurement_basis(heb_lit_pres_be_t1400, observed).
narrative_ontology:measurement(heb_lit_pres_be_t1700, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 1700, 0.35).
narrative_ontology:measurement_basis(heb_lit_pres_be_t1700, observed).
narrative_ontology:measurement(heb_lit_pres_be_t2000, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 2000, 0.36).
narrative_ontology:measurement_basis(heb_lit_pres_be_t2000, observed).

% Suppression requirement over time
narrative_ontology:measurement(heb_lit_pres_su_t200, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 200, 0.55).
narrative_ontology:measurement_basis(heb_lit_pres_su_t200, observed).
narrative_ontology:measurement(heb_lit_pres_su_t500, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 500, 0.52).
narrative_ontology:measurement_basis(heb_lit_pres_su_t500, observed).
narrative_ontology:measurement(heb_lit_pres_su_t800, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 800, 0.5).
narrative_ontology:measurement_basis(heb_lit_pres_su_t800, observed).
narrative_ontology:measurement(heb_lit_pres_su_t1100, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 1100, 0.58).
narrative_ontology:measurement_basis(heb_lit_pres_su_t1100, observed).
narrative_ontology:measurement(heb_lit_pres_su_t1400, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 1400, 0.6).
narrative_ontology:measurement_basis(heb_lit_pres_su_t1400, observed).
narrative_ontology:measurement(heb_lit_pres_su_t1700, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 1700, 0.44).
narrative_ontology:measurement_basis(heb_lit_pres_su_t1700, observed).
narrative_ontology:measurement(heb_lit_pres_su_t2000, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 2000, 0.62).
narrative_ontology:measurement_basis(heb_lit_pres_su_t2000, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_linguistic_life__liturgical_preservation_reading, identity_coordination).
narrative_ontology:affects_constraint(hebrew_linguistic_life__liturgical_preservation_reading, hebrew_linguistic_life__native_generational_reading).
narrative_ontology:affects_constraint(hebrew_linguistic_life__liturgical_preservation_reading, hebrew_linguistic_life__marketplace_pidgin_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial question 'is/was Hebrew alive?' conflates three structurally distinct criteria, decomposed per the epsilon-invariance principle into three linked stories. This reading (liturgical_preservation) is the historically upstream criterion: for eighteen centuries it was the operative definition, and the sibling readings define themselves against it - the native_generational_reading by denying its sufficiency, the marketplace_pidgin_reading by relocating vitality to practical function. Each member carries its own epsilon, victim set, and claimed type; no member hedges across readings. Upstream influence runs from this reading to both siblings, whose legitimacy conditions were formed in reaction to it.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
