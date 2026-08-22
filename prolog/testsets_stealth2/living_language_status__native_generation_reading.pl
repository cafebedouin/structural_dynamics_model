% ============================================================================
% CONSTRAINT STORY: living_language_status__native_generation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_living_language_status__native_generation_reading, []).

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
 *   constraint_id: living_language_status__native_generation_reading
 *   human_readable: Native Generational Transmission Criterion for Language Vitality
 *   domain: sociolinguistics/religious_studies/nationalism_studies
 *
 * SUMMARY:
 *   The native-generation criterion — a language is living only if native
 *   speakers transmit it generationally as a mother tongue in daily life —
 *   operates today as the anchoring standard of official language-vitality
 *   assessment: national academies count speakers, international atlases rank
 *   endangerment, school systems and revival programs are funded against it,
 *   and nationalist movements draw legitimacy from producing native-speaker
 *   populations. This story models the criterion-as-deployed. The standing
 *   arrangement under contest is the vitality-assessment regime organized
 *   around native generational transmission, and epsilon is authored for THAT
 *   arrangement — not for the liturgical-preservation arrangements the
 *   criterion displaces, and not for any endorsed alternative standard. Its
 *   coordination function is real: intergenerational mother-tongue
 *   transmission is the strongest demographic predictor of a language's
 *   continued existence, and a shared metric directs scarce revitalization
 *   resources accordingly. Its extraction is also real: the same instrument
 *   strips standing from communities that transmit sacred languages
 *   liturgically, scores partial diaspora transmission as failure, discounts
 *   fluent non-native users, and converts the whole apparatus into legitimacy
 *   production for linguistic-sovereignty projects. Claim and metrics are
 *   authored independently: claimed_type tangled_rope reflects the authoring
 *   judgment that genuine coordination and asymmetric extraction run through
 *   the same structure; the metric values describe the arrangement's actual
 *   operation. Sibling readings of the living_language_status kernel are
 *   separate constraint stories (see network and kernel_context); none of
 *   their content is averaged into this one. KEY AGENTS (by structural
 *   relationship): - secular_nationalist_movements: agenda-setting
 *   beneficiary (powerful/constrained) — draws legitimacy and mobilization
 *   from the criterion's verdicts - national_language_academies: agenda
 *   setter (institutional/constrained) — administers speaker counts,
 *   curricula, and official standards; mandate depends on the criterion -
 *   academic_linguistics_establishment: beneficiary (institutional/mobile) —
 *   careers, journals, and grant lines organized on the native-speaker
 *   yardstick - liturgical_only_communities: primary target
 *   (moderate/identity_locked) — centuries-deep liturgical transmission
 *   scored as corpse maintenance - diaspora_heritage_communities: target
 *   (moderate/constrained) — partial transmission scored as failure -
 *   non_native_fluent_users: target and unseated objector (moderate/mobile) —
 *   productive fluency discounted as inauthentic - revived_language_speakers:
 *   dual-positioned (moderate/constrained) — embody the criterion's success
 *   while bearing its purist policing - unesco_endangerment_assessors:
 *   analytical observer (institutional/analytical) — translates the criterion
 *   into global red-list categories
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(living_language_status__native_generation_reading, 0.58).
domain_priors:suppression_score(living_language_status__native_generation_reading, 0.5).
domain_priors:theater_ratio(living_language_status__native_generation_reading, 0.34).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(living_language_status__native_generation_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(living_language_status__native_generation_reading, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(living_language_status__native_generation_reading, theater_ratio, 0.34).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(living_language_status__native_generation_reading, accessibility_collapse, 0.28).
narrative_ontology:constraint_metric(living_language_status__native_generation_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(living_language_status__native_generation_reading, tangled_rope).
narrative_ontology:human_readable(living_language_status__native_generation_reading, "Native Generational Transmission Criterion for Language Vitality").
narrative_ontology:topic_domain(living_language_status__native_generation_reading, "sociolinguistics/religious_studies/nationalism_studies").

domain_priors:requires_active_enforcement(living_language_status__native_generation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(living_language_status__native_generation_reading, 'ddfd4a6c-d8b9-4d3c-9bc6-124349238cfc').
narrative_ontology:cs_kernel_codification('ddfd4a6c-d8b9-4d3c-9bc6-124349238cfc', distributed).
narrative_ontology:cs_authority_grounding('ddfd4a6c-d8b9-4d3c-9bc6-124349238cfc', expertise).
narrative_ontology:cs_interpretation_layer_present('ddfd4a6c-d8b9-4d3c-9bc6-124349238cfc').
narrative_ontology:cs_reading_relation('ddfd4a6c-d8b9-4d3c-9bc6-124349238cfc', living_language_status__liturgical_preservation_reading, forecloses).
narrative_ontology:cs_reading_relation('ddfd4a6c-d8b9-4d3c-9bc6-124349238cfc', living_language_status__literary_continuity_reading, forecloses).
narrative_ontology:cs_axiom('ddfd4a6c-d8b9-4d3c-9bc6-124349238cfc', foundational, vitality_requires_intergenerational_mother_tongue_transmission).
narrative_ontology:cs_axiom_status(vitality_requires_intergenerational_mother_tongue_transmission, holdable).
narrative_ontology:cs_axiom_grounding('ddfd4a6c-d8b9-4d3c-9bc6-124349238cfc', vitality_requires_intergenerational_mother_tongue_transmission, empirically_contingent).
narrative_ontology:cs_axiom('ddfd4a6c-d8b9-4d3c-9bc6-124349238cfc', foundational, liturgical_recitation_preserves_corpus_not_speech_community).
narrative_ontology:cs_axiom_status(liturgical_recitation_preserves_corpus_not_speech_community, holdable).
narrative_ontology:cs_axiom_grounding('ddfd4a6c-d8b9-4d3c-9bc6-124349238cfc', liturgical_recitation_preserves_corpus_not_speech_community, empirically_contingent).
narrative_ontology:cs_axiom('ddfd4a6c-d8b9-4d3c-9bc6-124349238cfc', secondary, national_legitimacy_requires_vernacular_sovereignty).
narrative_ontology:cs_axiom_status(national_legitimacy_requires_vernacular_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('ddfd4a6c-d8b9-4d3c-9bc6-124349238cfc', national_legitimacy_requires_vernacular_sovereignty, conventional).
narrative_ontology:cs_reference_frame('ddfd4a6c-d8b9-4d3c-9bc6-124349238cfc', mother_tongue_nation_sovereignty).
narrative_ontology:cs_drift_state('ddfd4a6c-d8b9-4d3c-9bc6-124349238cfc', contemporary_new_speaker_scholarship, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('ddfd4a6c-d8b9-4d3c-9bc6-124349238cfc', '').
narrative_ontology:cs_kernel_id(living_language_status__native_generation_reading, living_language_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(living_language_status__native_generation_reading, secular_nationalist_movements).
narrative_ontology:constraint_beneficiary(living_language_status__native_generation_reading, national_language_academies).
narrative_ontology:constraint_beneficiary(living_language_status__native_generation_reading, academic_linguistics_establishment).
narrative_ontology:constraint_victim(living_language_status__native_generation_reading, liturgical_only_communities).
narrative_ontology:constraint_victim(living_language_status__native_generation_reading, diaspora_heritage_communities).
narrative_ontology:constraint_victim(living_language_status__native_generation_reading, non_native_fluent_users).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(living_language_status__native_generation_reading, revived_language_speakers).
narrative_ontology:constraint_victim(living_language_status__native_generation_reading, revived_language_speakers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Draft national language platforms that define the nation's language as the people's daily mother tongue, and campaign for state schooling, census categories, and official status built on that definition. Collect electoral legitimacy, mobilization narratives, and claims to sovereignty from successful revival programs. Civic or multiethnic definitions of the nation remain available as alternatives but carry weaker mobilizing force, and movement leadership careers are bound to the linguistic-sovereignty project.
narrative_ontology:constraint_stakeholder(living_language_status__native_generation_reading, secular_nationalist_movements, agenda_setter,
    powerful, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(living_language_status__native_generation_reading, secular_nationalist_movements, beneficiary).

% Administer orthography, census language categories, school curricula, and official proficiency standards; decide which forms of use count toward official statistics of language health. Funded and staffed on the premise that their mandate is producing and counting native speakers. Adopting a different standard of language health would dissolve much of their mandate, budget, and reason for existence.
narrative_ontology:constraint_stakeholder(living_language_status__native_generation_reading, national_language_academies, agenda_setter,
    institutional, generational, constrained, national).

% Produces the endangerment assessments, vitality scales, and documentation grants through which languages are ranked as thriving, endangered, or dormant. Careers, journals, and funding lines are organized around intergenerational transmission as the master variable. Individual scholars can and do propose alternative framings, and some subfields have moved, but the grant-and-atlas infrastructure continues to run on the native-speaker yardstick.
narrative_ontology:constraint_stakeholder(living_language_status__native_generation_reading, academic_linguistics_establishment, beneficiary,
    institutional, generational, mobile, global).

% Maintain sacred languages across centuries through daily prayer, recitation, textual study, and scribal training, without domestic everyday use. Under the prevailing standard their practice is scored as maintenance of a dead language, which costs them standing in heritage funding, academic sympathy, and public esteem, and exposes them to campaigns urging conversion of their transmission into vernacular use. Leaving the liturgical relationship would mean abandoning the practice that constitutes the community, so they do not exit; they contest the scoring instead.
narrative_ontology:constraint_stakeholder(living_language_status__native_generation_reading, liturgical_only_communities, payer,
    moderate, civilizational, identity_locked, global).

% Transmit ancestral languages partially — weekend schools, home rituals, grandparent speech — while conducting daily life in a majority language. Official metrics score them as failed or declining transmitters regardless of the cultural density of what they maintain, which shapes whether their programs receive support and whether their children are counted as speakers at all. Assimilation is always available as an exit, and many take it, which is precisely the outcome the scoring regime is built to prevent or penalize.
narrative_ontology:constraint_stakeholder(living_language_status__native_generation_reading, diaspora_heritage_communities, payer,
    moderate, biographical, constrained, global).

% Adults who learn the language to high fluency — clergy, scholars, poets, translators, second-language writers — and produce new work in it. The native-speaker standard discounts their fluency as inauthentic, excluding them from speaker counts, from native-speaker modeling roles in teaching, and from authority over pronunciation and idiom. They argue their productive use is exactly what vitality means, but they hold no formal seat in the bodies that score languages.
narrative_ontology:constraint_stakeholder(living_language_status__native_generation_reading, non_native_fluent_users, payer,
    moderate, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(living_language_status__native_generation_reading, non_native_fluent_users, excluded).

% Raised through immersion schooling or home revival projects to speak the language natively; they embody the standard's success and receive the belonging, employment, and status that flow to recognized native speakers. They also inherit its policing: their speech is corrected against purist norms, their grandparents' language practices are recast as the dead weight the revival escaped, and their own children's loyalty to the language becomes a public measure of national health.
narrative_ontology:constraint_stakeholder(living_language_status__native_generation_reading, revived_language_speakers, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(living_language_status__native_generation_reading, revived_language_speakers, payer).

% Compile the international atlases and vitality scales that rank the world's languages, translating academic criteria into red-list categories that drive diplomatic attention and NGO funding. They take testimony from academies, communities, and researchers, and have broadened their factor lists over time while keeping intergenerational transmission as the anchor variable.
narrative_ontology:constraint_stakeholder(living_language_status__native_generation_reading, unesco_endangerment_assessors, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(living_language_status__native_generation_reading, secular_nationalist_movements).
narrative_ontology:fixing_cost_class(living_language_status__native_generation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Supplies one operationalizable answer to 'is this language alive?' so that states, funders, and communities can rank languages, target revitalization spending, and coordinate schooling and home-transmission policy around the one factor that demographically predicts survival: children acquiring the language natively.
% TRANSFER_FUNCTION: Moves legitimacy, official recognition, census standing, and program funding toward movements and institutions that produce native-speaker populations; moves stigma, statistical invisibility, and resource exclusion onto communities whose transmission is liturgical, literary, or partial, and discounts the standing of fluent non-native users.
% ABSENT_VOICES: Fluent non-native users (clergy, scholars, poets, translators) would contest the equation of vitality with nativeness but hold no seat in academy or atlas bodies; liturgical practitioners without state representation and elders of oral traditions outside census categories are likewise unrepresented. Language-policy tables are staffed almost entirely by academy officials, revival leadership, and academic assessors — the unanimity of the standard partly reflects who was never invited.
% DISAPPEARANCE_RATIONALE: If the criterion vanished overnight, endangerment atlases, school-funding formulas, census speaker categories, and nationalist legitimacy claims would all lose their anchor; liturgical communities' standing would rehabilitate, diaspora programs would be scored by different measures, and the language-policy world would reorganize around whichever successor standard — literary productivity, ritual continuity, or a multiplicative index — took its place.
% FOUNDING_PROBLEM: Nineteenth-century nation-builders needed to distinguish a nation's own vernacular — the people's daily mother tongue — from the empires' cosmopolitan languages of scripture, scholarship, and administration, and to justify mass schooling that would turn a peasant dialect or a book-language into a national speech community.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: nineteenth-century philological society records, Habsburg and Ottoman census manuals, and missionary-linguistics archives document the vernacular-versus-sacred-language problem as contemporaries framed it; historians of the Czech, Finnish, Hebrew, and Irish revivals — including scholars hostile to nationalist narratives — attest both the problem's reality and the dispute over whether the corpse-framing corollary was ever anything but instrument. The liturgical communities themselves never claimed their transmission model served nation-building, which is independent testimony that the founding problem was the nationalists', not theirs.
narrative_ontology:disappearance_verdict(living_language_status__native_generation_reading, world_rearranges).
narrative_ontology:founding_problem_status(living_language_status__native_generation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(living_language_status__native_generation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(living_language_status__native_generation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(living_language_status__native_generation_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(living_language_status__native_generation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(living_language_status__native_generation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(living_language_status__native_generation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate-to-substantial (0.58 at interval end) because the criterion's costs are levied through classification rather than confiscation: delegitimation, statistical invisibility, and redirected funding. Suppression (0.50) is the residual of a heavier historic layer — school regimes that punished minority and ancestral tongues, census categories that erased partial speakers — which has decayed into gatekeeping over who counts as a speaker and which practices count as maintenance; the suppression_requirement series is authored precisely because enforcement capacity changed twice over the interval (build-up through the high-nationalist decades, then decay into discursive gatekeeping). Theater (0.34) has risen from a low base as symbolic revival activity — signage, festivals, token curriculum hours — outpaces actual home transmission in many programs. Accessibility_collapse is low (0.28): rival standards remain fully available, the sibling readings are live in scholarship and community practice, and the criterion dominates official scoring without closing alternatives. Resistance is substantial (0.62): liturgical communities openly reject the corpse framing, heritage movements contest census categories, and new-speaker scholarship attacks the native-speaker construct itself. The tangled_rope claim rests on the joint presence the canonical classifier requires: a genuine coordination function (shared operational metric for survival-critical resources), identifiable beneficiaries and victims on the same instrument, and active enforcement (academies, atlases, accreditation) holding the standard in place. All three metric series run on one shared time grid (t = 0..140 step 20, roughly the criterion's consolidation circa the 1880s to the present) so no metric row borrows another's endpoints.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seats experience the criterion as neutral science of language health — from the academy and assessor seats the arrangement computes as coordination they administer in good faith. The payer seats experience the same structure as erasure: a civilizational-time-horizon practice (centuries of uninterrupted liturgical transmission) scored as failure by a metric whose scoring window is generational at best — the instrument literally cannot register what those communities maintain. The engine computes divergent per-seat types from this structural data; the divergence between 'objective demography' and 'our extinction is being declared by people who redefine life' is the perspectival gap the corpus exists to measure, not something the authored claim adjudicates.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the derivation: secular_nationalist_movements and national_language_academies sit near the subsidy end (low d) — the criterion's verdicts manufacture their legitimacy and mandate — with the academies' constrained exit keeping them heavily invested in criterion stability. academic_linguistics_establishment derives low d through its grant-and-atlas infrastructure despite individually mobile members. Victims derive high d: liturgical_only_communities are identity_locked (exit means abandoning the practice that constitutes the community), placing them nearest the full-target end; diaspora_heritage_communities are constrained (assimilation exits exist and are taken, which is part of the cost structure); non_native_fluent_users are mobile in body but status-excluded, tempering their d below the liturgical seat. revived_language_speakers sit near symmetric: they receive the belonging and standing the criterion distributes while paying in purist policing and inherited stigma toward their own grandparents' practice. No directionality overrides were needed — the beneficiary/victim declarations plus exit options produce the correct map.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against mislabeling in both directions. Reading the criterion as pure snare would erase its real coordination output: revitalization resources demonstrably follow it, and intergenerational transmission is the strongest demographic predictor of survival — the coordination story is not cover. Reading it as pure rope would erase the asymmetric extraction that rides the same instrument: delegitimation of liturgical transmission is not a coordination cost anyone needs to bear, and it purchases legitimacy for identifiable seats. On the mandatrophy question: the founding problem (vernacular nation-building) is contested rather than dead — stateless nations still pursue linguistic sovereignty — so no zombie flag fires; but if the corpse-framing corollary dies (as multiplicative vitality metrics spread) while academy scoring persists as bureaucratic routine, the arrangement drifts toward inertial maintenance. The theater_ratio series is the designated monitor for that transition: its rise past 0.5 would signal proxy goals replacing the transmission function the criterion nominally serves.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest_location,
    'This constraint is one reading of the living_language_status kernel — the native_generation_reading. What exactly would the sibling readings (liturgical_preservation_reading, literary_continuity_reading) change structurally, and where is the disagreement located?',
    'Comparative classification of the three sibling stories: if the siblings classify differently (different beneficiary/victim sets, different epsilon), the kernel decomposes into genuinely distinct constraints; if they converge, the contest is rhetorical rather than structural.',
    'A sibling adopting liturgical sufficiency would relocate beneficiaries to liturgical institutions and victims to revival bureaucracies, flipping this story''s directionality map. The disagreement sits specifically in the modal structure of the definition: this reading makes native generational transmission a NECESSARY condition of vitality, which directly negates the siblings'' SUFFICIENCY claims (ritual continuity, literary productivity).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest_location, conceptual, 'Committer structure: one of three rival readings of the living-language kernel; disagreement located in necessary-vs-sufficient condition clauses.').

omega_variable(
    naturalness_of_transmission_criterion,
    'Is the native-transmission standard a discovered regularity of language demography (no child speakers, no language) or a constructed criterion that serves identifiable interests?',
    'Cross-cultural comparison of language-health outcomes under rival standards, plus historiography of the criterion''s adoption: whether its spread tracks demographic evidence or nation-building calendars.',
    'If substantially constructed, the criterion''s presentation as a natural law of language life is a false-summit pattern and its beneficiaries'' role strengthens; if discovered, part of its authority is earned and the extraction reading narrows to the delegitimation surplus.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(naturalness_of_transmission_criterion, empirical, 'Whether the criterion is natural law of language demography or constructed standard with beneficiaries.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the suppression borne by liturgical and heritage communities structural (funding rules, census categories, historic school regimes) or internalized (shame of the ''dead language'' label accepted by the communities themselves)?',
    'Post-recognition trajectory: compare communities that secured official respect for liturgical transmission without changing practice — if self-deprecation and program attrition persist after structural barriers fall, a large share is internalized.',
    'If internalized, effective suppression exceeds the structural measure and persists after policy reform; if structural, statutory and budgetary changes would release it quickly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural vs internalized suppression mechanism in the delegitimation of liturgical transmission.').

omega_variable(
    payer_coalition_potential,
    'Could the dispersed payer seats — liturgical communities across confessions, diaspora heritage communities, non-native fluent users — form a coalition capable of setting a rival vitality standard?',
    'Track cross-confessional liturgical-language conferences, shared advocacy for multiplicative vitality metrics, and whether fluent-user guilds gain seats on assessment bodies.',
    'An effective coalition would convert scattered resistance into standard-setting power, moving the arrangement toward negotiated coordination; failure leaves each payer negotiating alone against nationally embedded academies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(payer_coalition_potential, empirical, 'Coalition potential of the multiple victim seats against the entrenched assessment regime.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(living_language_status__native_generation_reading, 0, 140).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lls_natgen_tr_t0, living_language_status__native_generation_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(lls_natgen_tr_t0, observed).
narrative_ontology:measurement(lls_natgen_tr_t20, living_language_status__native_generation_reading, theater_ratio, 20, 0.18).
narrative_ontology:measurement_basis(lls_natgen_tr_t20, observed).
narrative_ontology:measurement(lls_natgen_tr_t40, living_language_status__native_generation_reading, theater_ratio, 40, 0.22).
narrative_ontology:measurement_basis(lls_natgen_tr_t40, observed).
narrative_ontology:measurement(lls_natgen_tr_t60, living_language_status__native_generation_reading, theater_ratio, 60, 0.27).
narrative_ontology:measurement_basis(lls_natgen_tr_t60, observed).
narrative_ontology:measurement(lls_natgen_tr_t80, living_language_status__native_generation_reading, theater_ratio, 80, 0.33).
narrative_ontology:measurement_basis(lls_natgen_tr_t80, observed).
narrative_ontology:measurement(lls_natgen_tr_t100, living_language_status__native_generation_reading, theater_ratio, 100, 0.38).
narrative_ontology:measurement_basis(lls_natgen_tr_t100, observed).
narrative_ontology:measurement(lls_natgen_tr_t120, living_language_status__native_generation_reading, theater_ratio, 120, 0.36).
narrative_ontology:measurement_basis(lls_natgen_tr_t120, observed).
narrative_ontology:measurement(lls_natgen_tr_t140, living_language_status__native_generation_reading, theater_ratio, 140, 0.34).
narrative_ontology:measurement_basis(lls_natgen_tr_t140, observed).

% Extraction over time
narrative_ontology:measurement(lls_natgen_be_t0, living_language_status__native_generation_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(lls_natgen_be_t0, observed).
narrative_ontology:measurement(lls_natgen_be_t20, living_language_status__native_generation_reading, base_extractiveness, 20, 0.47).
narrative_ontology:measurement_basis(lls_natgen_be_t20, observed).
narrative_ontology:measurement(lls_natgen_be_t40, living_language_status__native_generation_reading, base_extractiveness, 40, 0.52).
narrative_ontology:measurement_basis(lls_natgen_be_t40, observed).
narrative_ontology:measurement(lls_natgen_be_t60, living_language_status__native_generation_reading, base_extractiveness, 60, 0.56).
narrative_ontology:measurement_basis(lls_natgen_be_t60, observed).
narrative_ontology:measurement(lls_natgen_be_t80, living_language_status__native_generation_reading, base_extractiveness, 80, 0.6).
narrative_ontology:measurement_basis(lls_natgen_be_t80, observed).
narrative_ontology:measurement(lls_natgen_be_t100, living_language_status__native_generation_reading, base_extractiveness, 100, 0.62).
narrative_ontology:measurement_basis(lls_natgen_be_t100, observed).
narrative_ontology:measurement(lls_natgen_be_t120, living_language_status__native_generation_reading, base_extractiveness, 120, 0.6).
narrative_ontology:measurement_basis(lls_natgen_be_t120, observed).
narrative_ontology:measurement(lls_natgen_be_t140, living_language_status__native_generation_reading, base_extractiveness, 140, 0.58).
narrative_ontology:measurement_basis(lls_natgen_be_t140, observed).

% Suppression requirement over time
narrative_ontology:measurement(lls_natgen_su_t0, living_language_status__native_generation_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(lls_natgen_su_t0, observed).
narrative_ontology:measurement(lls_natgen_su_t20, living_language_status__native_generation_reading, suppression_requirement, 20, 0.45).
narrative_ontology:measurement_basis(lls_natgen_su_t20, observed).
narrative_ontology:measurement(lls_natgen_su_t40, living_language_status__native_generation_reading, suppression_requirement, 40, 0.55).
narrative_ontology:measurement_basis(lls_natgen_su_t40, observed).
narrative_ontology:measurement(lls_natgen_su_t60, living_language_status__native_generation_reading, suppression_requirement, 60, 0.6).
narrative_ontology:measurement_basis(lls_natgen_su_t60, observed).
narrative_ontology:measurement(lls_natgen_su_t80, living_language_status__native_generation_reading, suppression_requirement, 80, 0.63).
narrative_ontology:measurement_basis(lls_natgen_su_t80, observed).
narrative_ontology:measurement(lls_natgen_su_t100, living_language_status__native_generation_reading, suppression_requirement, 100, 0.62).
narrative_ontology:measurement_basis(lls_natgen_su_t100, observed).
narrative_ontology:measurement(lls_natgen_su_t120, living_language_status__native_generation_reading, suppression_requirement, 120, 0.55).
narrative_ontology:measurement_basis(lls_natgen_su_t120, observed).
narrative_ontology:measurement(lls_natgen_su_t140, living_language_status__native_generation_reading, suppression_requirement, 140, 0.5).
narrative_ontology:measurement_basis(lls_natgen_su_t140, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(living_language_status__native_generation_reading, identity_coordination).
narrative_ontology:affects_constraint(living_language_status__native_generation_reading, liturgical_preservation_reading).
narrative_ontology:affects_constraint(living_language_status__native_generation_reading, literary_continuity_reading).

% DUAL FORMULATION NOTE:
% 'Living language' is a colloquial label covering three structurally distinct claims with different epsilon values, decomposed per the epsilon-invariance principle into a constraint family: native generational transmission (this story — moderate epsilon, nationalist beneficiaries, liturgical victims), liturgical preservation (separate story — beneficiaries are liturgical institutions, victims are revival bureaucracies), and literary continuity (separate story — beneficiaries are literate elites, victims are nativist gatekeepers). Upstream/downstream citation runs from this reading outward: endangerment science exports the native-speaker yardstick into heritage policy, changing the resource environment in which the sibling readings' claims are evaluated. Each family member links the others via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
