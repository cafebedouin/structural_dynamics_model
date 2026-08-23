% ============================================================================
% CONSTRAINT STORY: hebrew_vitality__native_daily_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-01
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_vitality__native_daily_reading, []).

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
 *   constraint_id: hebrew_vitality__native_daily_reading
 *   human_readable: Nativity Criterion of Hebrew Vitality — Vernacular Enforcement Reading
 *   domain: sociolinguistic/national-identity/religious
 *
 * SUMMARY:
 *   Between 1890 and 2020 the Zionist movement, and after 1948 the State of
 *   Israel, converted Hebrew from a liturgical and literary language into the
 *   sole public vernacular of a polyglot immigrant society, on the strength
 *   of a definitional criterion: a language lives only when children acquire
 *   it natively; centuries of recited liturgical continuity count as
 *   preservation, not life. This story instantiates the native_daily_reading
 *   of the hebrew_vitality kernel and authors epsilon for the standing
 *   arrangement under contest — the vernacularization regime as it actually
 *   operated — assessed by this reading's own lights: the coordination
 *   achievement is credited, while enforcement-era costs (public-sphere
 *   closure against Yiddish, transit-camp Hebraization, forced adult
 *   acquisition labor, and the permanent desacralization of the liturgical
 *   register) are booked against it. Claim and metrics are independent
 *   authored facts: the reading claims tangled_rope because the structure
 *   carries both a demonstrable collective-action success and asymmetric,
 *   partly irreversible costs held in place by active enforcement for most of
 *   the interval; the metrics describe observed operation, not the claim. The
 *   sibling readings are other files linked through the network: the
 *   liturgical reading books the desecration itself as the injury and would
 *   author far higher epsilon over a different victim set; the hybrid
 *   continuity reading distributes credit between substrate and
 *   reconstruction and attributes less extraction. Their epsilon values
 *   differ because each reads the same referent through a different criterion
 *   of vitality — the confusion lives in the label 'Hebrew vitality', not in
 *   the mathematics. KEY AGENTS (by structural relationship): -
 *   zionist_national_institutions: Agenda setter
 *   (institutional/identity_locked) — authored the criterion and built the
 *   enforcement machinery - hebrew_language_committee: Co-administrator
 *   (institutional/identity_locked) — lexical authority riding the criterion
 *   - yishuv_immigrant_communities: Net beneficiary with heavy acquisition
 *   costs (organized/constrained) - native_hebrew_generations: Subsidized
 *   beneficiary (organized/mobile) - traditional_liturgical_communities:
 *   Primary target — permanent desacralization (organized/identity_locked) -
 *   yiddish_speaking_immigrants: Target — public-sphere closure
 *   (moderate/trapped) - mizrahi_sephardi_immigrants: Sharpest per-capita
 *   language loss, net citizens (moderate/trapped) -
 *   haredi_yiddish_retentionists: Refusing target — exited the criterion's
 *   terms (organized/identity_locked) - yiddishist_cultural_movement:
 *   Excluded voice — alternate future never seated (moderate/trapped) -
 *   comparative_sociolinguistics: Analytical observer (analytical/analytical)
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_vitality__native_daily_reading, 0.52).
domain_priors:suppression_score(hebrew_vitality__native_daily_reading, 0.32).
domain_priors:theater_ratio(hebrew_vitality__native_daily_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_vitality__native_daily_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(hebrew_vitality__native_daily_reading, suppression_requirement, 0.32).
narrative_ontology:constraint_metric(hebrew_vitality__native_daily_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_vitality__native_daily_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(hebrew_vitality__native_daily_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_vitality__native_daily_reading, tangled_rope).
narrative_ontology:human_readable(hebrew_vitality__native_daily_reading, "Nativity Criterion of Hebrew Vitality — Vernacular Enforcement Reading").
narrative_ontology:topic_domain(hebrew_vitality__native_daily_reading, "sociolinguistic/national-identity/religious").

domain_priors:requires_active_enforcement(hebrew_vitality__native_daily_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_vitality__native_daily_reading, 'a2d611b9-fad1-4f0b-b7e5-85a781a73319').
narrative_ontology:cs_kernel_codification('a2d611b9-fad1-4f0b-b7e5-85a781a73319', formalized).
narrative_ontology:cs_authority_grounding('a2d611b9-fad1-4f0b-b7e5-85a781a73319', expertise).
narrative_ontology:cs_interpretation_layer_present('a2d611b9-fad1-4f0b-b7e5-85a781a73319').
narrative_ontology:cs_reading_relation('a2d611b9-fad1-4f0b-b7e5-85a781a73319', hebrew_vitality__liturgical_reading, forecloses).
narrative_ontology:cs_reading_relation('a2d611b9-fad1-4f0b-b7e5-85a781a73319', hebrew_vitality__hybrid_continuity_reading, coexists_with).
narrative_ontology:cs_axiom('a2d611b9-fad1-4f0b-b7e5-85a781a73319', foundational, exclusive_nativity_criterion_of_language_vitality).
narrative_ontology:cs_axiom_status(exclusive_nativity_criterion_of_language_vitality, holdable).
narrative_ontology:cs_axiom_grounding('a2d611b9-fad1-4f0b-b7e5-85a781a73319', exclusive_nativity_criterion_of_language_vitality, empirically_contingent).
narrative_ontology:cs_axiom('a2d611b9-fad1-4f0b-b7e5-85a781a73319', secondary, ritual_recitation_is_preservation_not_life).
narrative_ontology:cs_axiom_status(ritual_recitation_is_preservation_not_life, holdable).
narrative_ontology:cs_axiom_grounding('a2d611b9-fad1-4f0b-b7e5-85a781a73319', ritual_recitation_is_preservation_not_life, conventional).
narrative_ontology:cs_reference_frame('a2d611b9-fad1-4f0b-b7e5-85a781a73319', vernacular_nativity_as_legitimate_state).
narrative_ontology:cs_drift_state('a2d611b9-fad1-4f0b-b7e5-85a781a73319', contemporary_corpus_linguistic_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('a2d611b9-fad1-4f0b-b7e5-85a781a73319', '').
narrative_ontology:cs_kernel_id(hebrew_vitality__native_daily_reading, hebrew_vitality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_vitality__native_daily_reading, zionist_national_institutions).
narrative_ontology:constraint_beneficiary(hebrew_vitality__native_daily_reading, hebrew_language_committee).
narrative_ontology:constraint_beneficiary(hebrew_vitality__native_daily_reading, yishuv_immigrant_communities).
narrative_ontology:constraint_beneficiary(hebrew_vitality__native_daily_reading, native_hebrew_generations).
narrative_ontology:constraint_victim(hebrew_vitality__native_daily_reading, traditional_liturgical_communities).
narrative_ontology:constraint_victim(hebrew_vitality__native_daily_reading, yiddish_speaking_immigrants).
narrative_ontology:constraint_victim(hebrew_vitality__native_daily_reading, mizrahi_sephardi_immigrants).
narrative_ontology:constraint_victim(hebrew_vitality__native_daily_reading, haredi_yiddish_retentionists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(hebrew_vitality__native_daily_reading, mizrahi_sephardi_immigrants).
narrative_ontology:constraint_victim(hebrew_vitality__native_daily_reading, yishuv_immigrant_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The Zionist Executive, Jewish Agency, Yishuv assemblies, and after 1948 the state ministries financed Hebrew-only kindergartens and schools, teacher seminars, ulpan networks, army Hebrew instruction, and the Hebrew University's Hebrew-language requirement. They wrote the nativity criterion into policy and built the machinery around it. The revival was constitutive of their national project: softening toward multilingual pragmatism was debated internally in every decade and rejected every time, because the language decision was fused with who they understood themselves to be.
narrative_ontology:constraint_stakeholder(hebrew_vitality__native_daily_reading, zionist_national_institutions, agenda_setter,
    institutional, generational, identity_locked, global).

% Va'ad HaLashon from 1890, chartered under the Mandate, statutory Academy since 1953: coinage committees, grammar rulings, the adoption of Sephardi phonology, terminology banks for medicine, law, and engineering. The criterion is the charter of its authority — the body's vocation and permanence rest on managing the living vernacular's continuous expansion, and its members' professional identities formed inside that mission.
narrative_ontology:constraint_stakeholder(hebrew_vitality__native_daily_reading, hebrew_language_committee, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(hebrew_vitality__native_daily_reading, hebrew_language_committee, beneficiary).

% Arriving from dozens of mutually unintelligible speech communities, they gained one public language that opened schooling, work, unions, army service, and citizenship. The price ran through the household: parents labored in night ulpanim while children came home monolingual, and within two generations most families could no longer speak to their own grandparents. Leaving the arrangement was not realistic — they had already migrated into a Hebrew-administered polity, and reverting to a multilingual public sphere would have cost access to everything they had come for.
narrative_ontology:constraint_stakeholder(hebrew_vitality__native_daily_reading, yishuv_immigrant_communities, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(hebrew_vitality__native_daily_reading, yishuv_immigrant_communities, payer).

% Born into finished infrastructure — Hebrew schools, youth movements, army units, radio, a complete technical vocabulary — they acquired the national language at zero acquisition cost and inherit the criterion's celebratory self-description: they are the native generation the definition names. Their mobility inside the polity is total; the arrangement asks nothing further of them.
narrative_ontology:constraint_stakeholder(hebrew_vitality__native_daily_reading, native_hebrew_generations, beneficiary,
    organized, generational, mobile, national).

% Rabbinic academies, synagogue life, and observant households hold Hebrew as lashon ha-kodesh, the holy tongue, reserved for prayer and sacred study. Across the twentieth century the language of their prayers became the language of bus stops, slang, and pop songs, and the criterion explicitly judged their millennia of recited continuity 'preservation, not life.' Their self-concept includes the holiness of the language, so the verdict cannot be walked away from without dissolving the tradition itself; what they bear is a permanent public demotion of their register, incurred by agents constitutionally unable to abandon the relationship.
narrative_ontology:constraint_stakeholder(hebrew_vitality__native_daily_reading, traditional_liturgical_communities, payer,
    organized, civilizational, identity_locked, global).

% In the Yishuv years Yiddish was the largest single mother tongue, with newspapers, theatres, and party presses. The public sphere closed around it: Hebrew-only schooling rules, language-defense patrolmen breaking up street conversations and tearing down Yiddish posters, hiring discrimination in state-adjacent employers. The European Yiddish cultural project — already gutted by the Holocaust — lost the one place it might plausibly have rebuilt as state-supported culture; the practical exits were silence, emigration, or private persistence.
narrative_ontology:constraint_stakeholder(hebrew_vitality__native_daily_reading, yiddish_speaking_immigrants, payer,
    moderate, biographical, trapped, continental).

% Mass arrivals from 1948 to 1964 — speakers of Judeo-Arabic, Ladino, Judeo-Persian, Kurdish — spent years in transit camps where Hebrew classes were tied to housing and job allocation and children entered Hebrew-only classrooms. They became full citizens of a functioning Hebrew-speaking state, and their heritage languages largely ended outside the home within a generation or two. Their exit options were nil: they were housed, processed, and schooled inside the machinery that assigned their linguistic future.
narrative_ontology:constraint_stakeholder(hebrew_vitality__native_daily_reading, mizrahi_sephardi_immigrants, payer,
    moderate, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(hebrew_vitality__native_daily_reading, mizrahi_sephardi_immigrants, beneficiary).

% Post-war ultra-Orthodox communities rejected the new street Hebrew as profanation of the holy tongue and kept Yiddish as the language of home and study. They absorbed decades of mockery as backward, exclusion from national cultural funding, and poverty bound to Torah-centered life. Their refusal of the nativity criterion is total — accepting it would dissolve the communal walls the language guards — and their speech communities now number in the hundreds of thousands across Israel and the diaspora, persisting precisely because their identity frame never accepted the arrangement's terms.
narrative_ontology:constraint_stakeholder(hebrew_vitality__native_daily_reading, haredi_yiddish_retentionists, payer,
    organized, civilizational, identity_locked, global).

% YIVO scholars, secular Yiddishist educators, and writers advanced an alternate future: Yiddish as the national language of the Jewish people, or a negotiated multilingual Jewish culture. They held no seat in any Hebrew language-planning body; their argument — that vitality-talk was a claim on cultural capital rather than a neutral metric — survives only in their own journals and exile archives, voiced after every relevant decision had been taken.
narrative_ontology:constraint_stakeholder(hebrew_vitality__native_daily_reading, yiddishist_cultural_movement, excluded,
    moderate, generational, trapped, continental).

% Revival scholarship measures Hebrew against Irish, Welsh, Maori, and Cornish cases and against language-shift theory. It documents both the coordination achievement and the costs booked to minority languages and to the liturgical register, and hosts the live dispute over which criterion of vitality the historical record actually vindicates — transmission-gap studies, hybrid-origin analyses, and shift-reversal theory all bear on the kernel without settling it.
narrative_ontology:constraint_stakeholder(hebrew_vitality__native_daily_reading, comparative_sociolinguistics, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hebrew_vitality__native_daily_reading, zionist_national_institutions).
narrative_ontology:fixing_cost_class(hebrew_vitality__native_daily_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: A polyglot immigrant society assembling statehood — speakers of Yiddish, Ladino, Judeo-Arabic, Persian, Kurdish, German, Russian, Polish, Romanian — needed one shared public language for schooling, commerce, military command, courts, press, and civic membership. Single-vernacular standardization solved that collective-action problem once, centrally, instead of leaving every institution to improvise translation.
% TRANSFER_FUNCTION: Moves children's first-language slots, adult acquisition labor, public-sphere participation, publishing subsidies, and cultural prestige toward the Hebrew vernacular and its administering institutions; moves the sacred register's public authority toward secular national uses; strips participation rights and cultural capital from Yiddish, Ladino, and Judeo-Arabic public life.
% ABSENT_VOICES: The Yiddishist cultural movement, rabbinic defenders of liturgical primacy, Sephardi and Mizrahi communal figures, and advocates of Mandate-style trilingualism were never seated in the Hebrew language-planning organs; the criterion was ratified by consensus among the very institutions it empowered. Their objections surface only in their own presses, responsa literature, and exile archives.
% DISAPPEARANCE_RATIONALE: Had the criterion and its enforcement vanished overnight near its peak, the lingua-franca question reopens: German (the Technikum faction), Yiddish (the demographic plurality), and English (the Mandate administration) compete; school systems, army command, courts, and press reorganize around the winner or around a negotiated multilingualism; the state-building timeline lengthens by decades. Today the residual criterion's disappearance would leave Hebrew reproducing organically but retire the Academy's mandate and reopen heritage-language policy.
% FOUNDING_PROBLEM: An immigrant society drawn from mutually unintelligible speech communities lacked any shared public language with which to constitute a nation and operate its institutions; Zionist ideology additionally required that the chosen language mark rupture with diaspora life rather than continuity with it.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: British Mandate census and education records document the multilingual starting condition; the contemporaneous Yiddish press (Haynt, Moment) and YIVO publications attest the contested language question from the excluded seat; rabbinic responsa attest the desacralization objection from the liturgical seat; modern comparative sociolinguistics (Fishman's shift-reversal work, Cooper's spread studies, Spolsky and Shohamy's language-policy analyses) independently evaluates both the founding problem and its resolution. No attestation relies solely on the Zionist institutions' own account.
narrative_ontology:disappearance_verdict(hebrew_vitality__native_daily_reading, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_vitality__native_daily_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_vitality__native_daily_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(hebrew_vitality__native_daily_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_vitality__native_daily_reading, 0.52, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_vitality__native_daily_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(hebrew_vitality__native_daily_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(hebrew_vitality__native_daily_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness ends at 0.52: a genuine coordination achievement subtracts from extraction, but the enforcement-era bookings are real and partly irreversible — dead heritage languages do not return and a desacralized register does not re-consecrate. Suppression ends at 0.32 because the coercive machinery was dismantled as native transmission made it redundant; the suppression_requirement series traces the full enforcement arc (build-up 1890-1948, peak through the state-formation decade, decay thereafter) — this is exactly the enforcement-capacity dynamic the series exists to capture, and it runs on the same seven-point shared grid as the other tracked metrics so no row samples a substituted scalar. Theater rises from 0.12 to 0.35: early coinage was urgent and adopted wholesale; late-period Academy coinage increasingly loses to loanwords while anniversary pageantry mythologizes the revival — a classic Goodhart signature as the proxy (managed coinage) outlives the need (filling lexical gaps). Accessibility_collapse 0.55: inside the committed nationalist framework the multilingual alternative was foreclosed almost completely, yet the Haredi exit proves the collapse was never total — alternatives survived wherever an identity frame rejected the criterion itself. Resistance 0.5: organized Yiddishist, rabbinic, and Germanist resistance was substantial, lost, and persisted at the margins for decades. Coalition note: the victim seats never combined — secular Yiddishists, rabbinic authorities, and Mizrahi communal leaders distrusted each other more than the criterion, which is why moderately-powered targets failed to convert numbers into leverage. Receipt surface: the constraint's gains accrued as state-building capacity to the national institutions; fixing is prohibitive because the principal injuries (extinct secular Yiddish culture, desacralized register) are irreversible at any expenditure.
 *
 * PERSPECTIVAL GAP:
 *   Four seats experience four different constraint types from one structure. From the agenda-setter seat the arrangement is the founding miracle it administered — coordination so successful it reads as a rope it built and can retire. From the traditional liturgical seat it operates as a categorical judgment plus a permanent cultural injury that cannot be abandoned without dissolving the tradition's self-concept. From the native-generation seat it is invisible — pure subsidy, descriptive fact rather than imposition. From the Haredi seat it is an external verdict whose terms were wholly refused; their realized burden is capped by non-participation, which is why identity-locked refusal, not coalition, was the historically effective response. The engine computes these divergences from the structural data; the authored claim adjudicates none of them.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary declarations map to low directionality: the national institutions and the language committee sit near the beneficiary pole (they collect the arrangement's product and administer its terms); native generations sit nearest zero (subsidized without acquisition cost). The yishuv immigrant communities derive mid-low: genuine benefit offset by real acquisition labor. Victim declarations drive high directionality: traditional liturgical communities carry the irreversible desacralization injury and their identity lock places them nearer the full-target end than a mobile payer would sit; yiddish-speaking immigrants carry public-sphere closure with trapped exit; mizrahi and sephardi immigrants carry sharp per-capita loss offset by citizenship benefit; haredi retentionists derive high directionality but realize reduced effective burden through total refusal of the criterion's terms. Suppression is authored as a raw structural property — 0.32 today, unscaled by power or scope; only extractiveness is scaled by the engine's directionality and scope arithmetic.
 *
 * MANDATROPHY ANALYSIS:
 *   The enforcement mandate has genuinely outlived its function — but through successful completion, not decay: native transmission made the patrols and exclusions redundant, and coercion was retired rather than ratcheted. This distinguishes the arrangement from a piton, whose administrator maintains performance without function; here the administrator's remaining function (terminological planning) is live though increasingly theatrical, a localized piton-drift risk inside a still-functional structure. The classification prevents mislabeling in both directions: calling the whole arrangement a snare would erase the most successful language revival of the modern era and mistake its coordination achievement for cover (it demonstrably worked); calling it a rope would erase the booked victims — extinguished secular Yiddish culture, lost heritage languages, a permanently demoted sacred register. The tangled-rope claim holds both truths; the founding-problem interview records the mandate as contested rather than dead, so no zombie flag fires, and the mismatch consumer finds status and verdict consistent with a completed-but-disputed transition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest_nativity_definition,
    'Does the exclusive native-generation criterion correctly delimit linguistic vitality, or is this file merely one of three structurally distinct readings of the hebrew_vitality kernel whose adoption reallocates the beneficiary and victim sets?',
    'Cross-reading adjudication: evaluate each sibling reading''s empirical commitments against the historical record (transmission-gap studies, liturgical-continuity demographics, hybrid-origin genetics) and observe which criterion the surviving speaker ecology vindicates.',
    'Adopting the liturgical sibling removes the desacralization victim and re-authors epsilon over a different referent assessment; adopting the hybrid sibling splits benefit credit between substrate and reconstruction and lowers attributed extraction. This story''s classification holds only under its own reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest_nativity_definition, conceptual, 'Committer-frame ambiguity: one kernel, three readings, three different victim/benefit allocations.').

omega_variable(
    desacralization_cost_magnitude,
    'How large is the cultural and spiritual cost borne by liturgical tradition from the permanent conversion of the holy tongue into a street vernacular?',
    'Comparative language-attitude and religiosity surveys across observant cohorts; pre/post studies of liturgical practice intensity; archival comparison of rabbinic responses before and after vernacularization.',
    'A large magnitude strengthens the extraction leg of the tangled-rope structure and raises effective chi for the liturgical payer seat; a near-zero magnitude would support reading the desacralization as a costless byproduct of coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(desacralization_cost_magnitude, empirical, 'Magnitude of the desacralization injury to the liturgical register.').

omega_variable(
    counterfactual_multilingual_coordination,
    'Could equivalent public-sphere coordination have been achieved through negotiated multilingualism rather than single-vernacular enforcement?',
    'Comparative-polity analysis (Switzerland, Belgium, Canada, Singapore) plus within-case variation from the Mandate era''s trilingual administration; test whether coordination outcomes track language count or enforcement intensity.',
    'An affirmative answer implies part of the enforcement exceeded coordination need and books as pure suppression; a negative answer prices the enforcement as the necessary cost of the coordination achieved.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(counterfactual_multilingual_coordination, empirical, 'Whether the enforcement component was coordination-necessary or surplus.').

omega_variable(
    heritage_language_loss_attribution,
    'What share of Judeo-Arabic, Ladino, and other heritage-language loss among post-1948 immigrants is attributable to Hebraization policy versus generic migration assimilation?',
    'Cohort comparison of comparable migrant groups under different language regimes; education-record analysis of transit-camp language policy; intergenerational transmission statistics by origin community.',
    'Higher policy-attributable share concentrates victim burden on the mizrahi_sephardi_immigrants seat and raises measured extraction; lower share redistributes the injury to diffuse assimilation and softens that seat''s target status.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(heritage_language_loss_attribution, empirical, 'Attribution of heritage-language loss between policy and generic assimilation.').

omega_variable(
    internalized_negation_of_diaspora_shame,
    'Was the suppression operating on heritage-language speakers structural (patrols, school exclusion, hiring discrimination) or internalized (self-censorship driven by negation-of-the-diaspora ideology and immigrant shame)?',
    'Post-exit suppression trajectory: Haredi Yiddish retention and post-1990 Russian-immigrant multilingualism show heritage persistence becomes viable once structural force is removed; oral-history interviews measuring self-reported shame versus reported barriers across cohorts.',
    'If a large share is internalized, effective suppression exceeds the structural measure and traveled with speakers after enforcement decayed; if mostly structural, the falling suppression series accurately tracks liberation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_negation_of_diaspora_shame, empirical, 'Structural versus internalized composition of suppression on heritage speakers.').

omega_variable(
    academy_coinage_uptake_rate,
    'What fraction of Academy-coined terms achieve actual use versus losing to borrowed forms?',
    'Corpus-frequency analysis comparing coined forms against English and loanword competitors across registers and decades.',
    'Validates the rising theater_ratio trajectory: high failure rates confirm the growing performative share of language planning as urgency fades; high uptake rates would falsify the drift reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(academy_coinage_uptake_rate, empirical, 'Empirical validation of the coinage-theater drift signal.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_vitality__native_daily_reading, 1890, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t1890, hebrew_vitality__native_daily_reading, theater_ratio, 1890, 0.12).
narrative_ontology:measurement(hebr_tr_t1914, hebrew_vitality__native_daily_reading, theater_ratio, 1914, 0.16).
narrative_ontology:measurement(hebr_tr_t1936, hebrew_vitality__native_daily_reading, theater_ratio, 1936, 0.19).
narrative_ontology:measurement(hebr_tr_t1948, hebrew_vitality__native_daily_reading, theater_ratio, 1948, 0.17).
narrative_ontology:measurement(hebr_tr_t1965, hebrew_vitality__native_daily_reading, theater_ratio, 1965, 0.22).
narrative_ontology:measurement(hebr_tr_t1990, hebrew_vitality__native_daily_reading, theater_ratio, 1990, 0.29).
narrative_ontology:measurement(hebr_tr_t2020, hebrew_vitality__native_daily_reading, theater_ratio, 2020, 0.35).

% Extraction over time
narrative_ontology:measurement(hebr_be_t1890, hebrew_vitality__native_daily_reading, base_extractiveness, 1890, 0.34).
narrative_ontology:measurement(hebr_be_t1914, hebrew_vitality__native_daily_reading, base_extractiveness, 1914, 0.42).
narrative_ontology:measurement(hebr_be_t1936, hebrew_vitality__native_daily_reading, base_extractiveness, 1936, 0.55).
narrative_ontology:measurement(hebr_be_t1948, hebrew_vitality__native_daily_reading, base_extractiveness, 1948, 0.63).
narrative_ontology:measurement(hebr_be_t1965, hebrew_vitality__native_daily_reading, base_extractiveness, 1965, 0.66).
narrative_ontology:measurement(hebr_be_t1990, hebrew_vitality__native_daily_reading, base_extractiveness, 1990, 0.57).
narrative_ontology:measurement(hebr_be_t2020, hebrew_vitality__native_daily_reading, base_extractiveness, 2020, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t1890, hebrew_vitality__native_daily_reading, suppression_requirement, 1890, 0.22).
narrative_ontology:measurement(hebr_su_t1914, hebrew_vitality__native_daily_reading, suppression_requirement, 1914, 0.38).
narrative_ontology:measurement(hebr_su_t1936, hebrew_vitality__native_daily_reading, suppression_requirement, 1936, 0.68).
narrative_ontology:measurement(hebr_su_t1948, hebrew_vitality__native_daily_reading, suppression_requirement, 1948, 0.74).
narrative_ontology:measurement(hebr_su_t1965, hebrew_vitality__native_daily_reading, suppression_requirement, 1965, 0.66).
narrative_ontology:measurement(hebr_su_t1990, hebrew_vitality__native_daily_reading, suppression_requirement, 1990, 0.42).
narrative_ontology:measurement(hebr_su_t2020, hebrew_vitality__native_daily_reading, suppression_requirement, 2020, 0.32).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_vitality__native_daily_reading, identity_coordination).
narrative_ontology:affects_constraint(hebrew_vitality__native_daily_reading, hebrew_vitality__liturgical_reading).
narrative_ontology:affects_constraint(hebrew_vitality__native_daily_reading, hebrew_vitality__hybrid_continuity_reading).

% DUAL FORMULATION NOTE:
% The label 'Hebrew vitality' decomposes into three structurally distinct claims (epsilon-invariant decomposition of the kernel): (1) hebrew_vitality__liturgical_reading — continuity of ritual use constitutes vitality; its epsilon referent assessment books the vernacularization itself as the injury, yielding far higher epsilon over a different victim set; (2) hebrew_vitality__native_daily_reading (this file) — only native generation constitutes vitality; moderate epsilon, beneficiary = Zionist state-building, victim = liturgical tradition via desacralization; (3) hebrew_vitality__hybrid_continuity_reading — liturgical substrate was necessary enabler but insufficient; credit distributed, lowest attributed extraction. Upstream/downstream: the liturgical substrate (millennia of transmitted text) is cited by the nativity reading as raw material while being denied vitality status — the upstream claim feeds the downstream justification. All three files link one another through affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
