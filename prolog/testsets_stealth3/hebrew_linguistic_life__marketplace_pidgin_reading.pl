% ============================================================================
% CONSTRAINT STORY: hebrew_linguistic_life__marketplace_pidgin_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_linguistic_life__marketplace_pidgin_reading, []).

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
 *   constraint_id: hebrew_linguistic_life__marketplace_pidgin_reading
 *   human_readable: Death-and-Revival Settlement of Hebrew (Marketplace-Pidgin Reading)
 *   domain: sociolinguistic/historiographic/nationalist
 *
 * SUMMARY:
 *   This story instantiates the marketplace-pidgin reading of the contested
 *   kernel of Hebrew's linguistic life: a language is alive when it functions
 *   as an inter-communal medium of practical coordination, regardless of
 *   native-speaker status or sacred function. On this reading the standing
 *   arrangement under contest is the death-and-revival historiographical
 *   settlement — the account, consolidated with the Yishuv and the state,
 *   that Hebrew ceased to function as a spoken language in antiquity and was
 *   deliberately revived from print after 1880. Assessed by this reading's
 *   own lights, that settlement is substantially costly to those it governs:
 *   it converts centuries of documented marketplace and inter-communal Hebrew
 *   practice into evidential nullity, banks the recovered credit as national
 *   creation-narrative, and distributes curriculum time, commemoration, and
 *   scholarly authority accordingly. The authored extractiveness measures
 *   THAT settlement, never this reading's endorsed alternative. Claim and
 *   metrics are independent authored facts: claimed_type tangled_rope
 *   reflects the settlement's dual face — genuine coordination of
 *   national-linguistic identity plus asymmetric credit transfer held in
 *   place by active institutional maintenance — while the metric values
 *   describe its observed operation. KEY AGENTS (by structural relationship):
 *   hebrew_language_academy (institutional/identity_locked) — administers the
 *   settlement and banks its authority; national_curriculum_authorities
 *   (institutional/constrained) — teach it as settled fact;
 *   revivalist_historiographers (organized/identity_locked) — collect
 *   standing from it; old_yishuv_descendant_communities
 *   (moderate/identity_locked) — inherit the erased practice;
 *   revisionist_sociolinguists (moderate/mobile) — pay the gatekeeping tax
 *   for counter-evidence; arab_merchant_counterparties (powerless/trapped) —
 *   historical participants whose coordination work the record denies;
 *   comparative_sociolinguistics_community (organized/analytical) — sees the
 *   full structure cross-linguistically. Family decomposition is recorded in
 *   network.dual_formulation_note; sibling readings are separate constraints,
 *   not described here.
 *
 * KEY AGENTS:
 *   - hebrew_language_academy: Agenda-setter and receipt seat (institutional/identity_locked) — administers the settlement; its mandate presupposes the discontinuity it certifies
 *   - national_curriculum_authorities: Agenda-setter (institutional/constrained) — reproduce the account through ordinary administration
 *   - revivalist_historiographers: Beneficiary (organized/identity_locked) — collect professional standing from the discontinuity frame
 *   - old_yishuv_descendant_communities: Payer (moderate/identity_locked) — inherit continuous practice the account narrates out of existence
 *   - revisionist_sociolinguists: Payer (moderate/mobile) — pay gatekeeping costs for counter-evidence, with usable exit into comparative venues
 *   - arab_merchant_counterparties: Payer and excluded voice (powerless/trapped) — historical transactors whose testimony has no seat
 *   - comparative_sociolinguistics_community: Observer (organized/analytical) — evaluates the case against functional criteria across languages
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_linguistic_life__marketplace_pidgin_reading, 0.63).
domain_priors:suppression_score(hebrew_linguistic_life__marketplace_pidgin_reading, 0.44).
domain_priors:theater_ratio(hebrew_linguistic_life__marketplace_pidgin_reading, 0.33).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_linguistic_life__marketplace_pidgin_reading, extractiveness, 0.63).
narrative_ontology:constraint_metric(hebrew_linguistic_life__marketplace_pidgin_reading, suppression_requirement, 0.44).
narrative_ontology:constraint_metric(hebrew_linguistic_life__marketplace_pidgin_reading, theater_ratio, 0.33).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_linguistic_life__marketplace_pidgin_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(hebrew_linguistic_life__marketplace_pidgin_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_linguistic_life__marketplace_pidgin_reading, tangled_rope).
narrative_ontology:human_readable(hebrew_linguistic_life__marketplace_pidgin_reading, "Death-and-Revival Settlement of Hebrew (Marketplace-Pidgin Reading)").
narrative_ontology:topic_domain(hebrew_linguistic_life__marketplace_pidgin_reading, "sociolinguistic/historiographic/nationalist").

domain_priors:requires_active_enforcement(hebrew_linguistic_life__marketplace_pidgin_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_linguistic_life__marketplace_pidgin_reading, '1047ac63-5f89-4c65-bf20-bcf31625c244').
narrative_ontology:cs_kernel_codification('1047ac63-5f89-4c65-bf20-bcf31625c244', distributed).
narrative_ontology:cs_authority_grounding('1047ac63-5f89-4c65-bf20-bcf31625c244', expertise).
narrative_ontology:cs_reading_relation('1047ac63-5f89-4c65-bf20-bcf31625c244', hebrew_linguistic_life__liturgical_preservation_reading, coexists_with).
narrative_ontology:cs_reading_relation('1047ac63-5f89-4c65-bf20-bcf31625c244', hebrew_linguistic_life__native_generational_reading, forecloses).
narrative_ontology:cs_axiom('1047ac63-5f89-4c65-bf20-bcf31625c244', foundational, intercommunal_function_confers_aliveness).
narrative_ontology:cs_axiom_status(intercommunal_function_confers_aliveness, holdable).
narrative_ontology:cs_axiom_grounding('1047ac63-5f89-4c65-bf20-bcf31625c244', intercommunal_function_confers_aliveness, empirically_contingent).
narrative_ontology:cs_axiom('1047ac63-5f89-4c65-bf20-bcf31625c244', foundational, native_status_irrelevant_to_vitality).
narrative_ontology:cs_axiom_status(native_status_irrelevant_to_vitality, holdable).
narrative_ontology:cs_axiom_grounding('1047ac63-5f89-4c65-bf20-bcf31625c244', native_status_irrelevant_to_vitality, conventional).
narrative_ontology:cs_reference_frame('1047ac63-5f89-4c65-bf20-bcf31625c244', intercommunal_function_sufficiency).
narrative_ontology:cs_drift_state('1047ac63-5f89-4c65-bf20-bcf31625c244', contemporary_post_revisionist_debate, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('1047ac63-5f89-4c65-bf20-bcf31625c244', '').
narrative_ontology:cs_kernel_id(hebrew_linguistic_life__marketplace_pidgin_reading, hebrew_linguistic_life).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__marketplace_pidgin_reading, hebrew_language_academy).
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__marketplace_pidgin_reading, national_curriculum_authorities).
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__marketplace_pidgin_reading, revivalist_historiographers).
narrative_ontology:constraint_victim(hebrew_linguistic_life__marketplace_pidgin_reading, old_yishuv_descendant_communities).
narrative_ontology:constraint_victim(hebrew_linguistic_life__marketplace_pidgin_reading, revisionist_sociolinguists).
narrative_ontology:constraint_victim(hebrew_linguistic_life__marketplace_pidgin_reading, arab_merchant_counterparties).
narrative_ontology:constraint_vindicates(hebrew_linguistic_life__marketplace_pidgin_reading, continuous_practice_thesis).
narrative_ontology:constraint_vindicates(hebrew_linguistic_life__marketplace_pidgin_reading, modified_medieval_hebrew_pidgin_hypothesis).
narrative_ontology:constraint_vindicates(hebrew_linguistic_life__marketplace_pidgin_reading, functional_vitality_criterion).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Founded to preside over the revived national language: sets orthographic and grammatical standards, publishes the authoritative historical account in which Hebrew died as speech in antiquity and was rebuilt by deliberate planning, and reviews curricula and terminology. Its mandate, budget, and scholarly authority are premised on the discontinuity account; a continuous-practice history would recast it from guardian of a resurrection to steward of an unbroken vernacular. Leaving that position would mean dissolving the premise of its own authority.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__marketplace_pidgin_reading, hebrew_language_academy, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(hebrew_linguistic_life__marketplace_pidgin_reading, hebrew_language_academy, beneficiary).

% Approve textbooks, examinations, and commemorative programming that teach the death-and-revival account as settled fact to every school cohort. They coordinate teachers, publishers, and examiners around a single origin story; revising it would require renegotiating materials, teacher training, and the ceremonial calendar across the whole system, so the account reproduces itself through ordinary administration.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__marketplace_pidgin_reading, national_curriculum_authorities, agenda_setter,
    institutional, generational, constrained, national).

% Professional historians, biographers, and schoolbook authors whose publications, chairs, and commemorative commissions rest on the discontinuity frame. Their work collects citations, prizes, and curricular placement; shifting to a continuity frame would strand their accumulated oeuvre and professional networks.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__marketplace_pidgin_reading, revivalist_historiographers, beneficiary,
    organized, biographical, identity_locked, national).

% Descendants of the pre-1880 Jerusalem, Safed, and Jaffa communities — Sephardi, Musta'arabi, Maghrebi, and Ashkenazi — whose families used Hebrew alongside Yiddish, Ladino, and Judeo-Arabic for commerce and inter-communal dealings. The prevailing account narrates their ancestors' practice out of existence, discounting the inheritance they actually hold; the erasure touches identity directly and cannot be exited by changing affiliation.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__marketplace_pidgin_reading, old_yishuv_descendant_communities, payer,
    moderate, generational, identity_locked, national).

% Scholars documenting marketplace Hebrew, pidgin strata, and usage records predating 1880. They publish against the default account, meeting skeptical referees, footnote dismissal of their sources as curiosities, and slower advancement; their comparative-vitality toolkit travels well outside the national frame, giving them venues and colleagues beyond the disputed case.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__marketplace_pidgin_reading, revisionist_sociolinguists, payer,
    moderate, biographical, mobile, global).

% Ottoman-era merchants, brokers, and artisans — Muslim and Christian Arabic speakers of Jerusalem, Jaffa, and Hebron — who transacted across communal lines in a Hebrew-inflected trade register. Their side of the practice survives in scattered ledgers, petitions, and travel accounts; no seat in the historiographical conversation represents their testimony, and the record that would speak for them is filed as anecdote.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__marketplace_pidgin_reading, arab_merchant_counterparties, payer,
    powerless, immediate, trapped, local).
narrative_ontology:stakeholder_secondary_role(hebrew_linguistic_life__marketplace_pidgin_reading, arab_merchant_counterparties, excluded).

% Researchers assessing language vitality across many cases with functional metrics — domains of use, intergenerational transmission, register breadth. They treat the Hebrew case as one data point among hundreds, neither collecting from the prevailing account nor paying its costs, and their cross-case comparisons expose how much the verdict on Hebrew depends on which criterion is applied.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__marketplace_pidgin_reading, comparative_sociolinguistics_community, observer,
    organized, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hebrew_linguistic_life__marketplace_pidgin_reading, hebrew_language_academy).
narrative_ontology:fixing_cost_class(hebrew_linguistic_life__marketplace_pidgin_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a real coordination problem: a newly consolidating polity needed one teachable origin story for its official language, identical across schools, army, media, and diaspora institutions, plus a motivation frame that made acquiring a printed liturgical language as a mother tongue feel like national resurrection rather than linguistic engineering. The discontinuity account supplied both, and gave planners a clean-slate narrative that justified top-down coinage and pronunciation decisions.
% TRANSFER_FUNCTION: Moves linguistic-heritage credit, curriculum time, commemorative infrastructure, and scholarly authority away from the communities that continuously used Hebrew in markets and inter-communal trade and toward the revival project — its heroes, its planning institutions, and the historiographers who narrate it. It also reclassifies the movement's actual mother tongues (Yiddish, Ladino, Judeo-Arabic) as mere substrates of the revived language.
% ABSENT_VOICES: The marketplace participants themselves — Ottoman-era brokers, women vendors, Arab counterparties — would object if represented: their transactions are the data the account classifies as dead, and they hold no seat in the historiographical conversation that adjudicates them. Mizrahi communal historians sat outside the Ashkenazi-centered academy for most of the interval and entered it late.
% DISAPPEARANCE_RATIONALE: If the death-and-revival account vanished overnight, curricula, textbooks, commemorative holidays, the academy's mandate language, and diaspora-teaching framings would all reorganize around whichever continuity-or-discontinuity story replaced it; the Hebrew language itself would continue uninterrupted. What rearranges is the credit economy — who counts as the language's maker, keeper, and heir — and the pedagogical apparatus built on the heroic-resurrection frame.
% FOUNDING_PROBLEM: A modernizing national movement needed its ancient sacred language legitimated as everyday speech. The death-and-revival settlement answered the legitimacy question — how can scripture's tongue be our street speech? — with a controlled discontinuity: the language died, was deliberately resurrected, and its modern form is therefore a national creation rather than an inherited vernacular accident. It simultaneously handled the status problem of the movement's actual mother tongues, which the discontinuity frame demoted to substrates.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: language-planning scholarship (the status- and corpus-planning literature) documents the narrative's legitimation function; Ottoman-era social historians and Mizrahi historiography outside the revival establishment document the continuous practice the account set aside; the academy and curriculum authorities themselves attest the founding problem remains live for diaspora identity transmission. No neutral arbiter adjudicates among these attestations — the contest itself is the corroborated state.
narrative_ontology:disappearance_verdict(hebrew_linguistic_life__marketplace_pidgin_reading, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_linguistic_life__marketplace_pidgin_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_linguistic_life__marketplace_pidgin_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(hebrew_linguistic_life__marketplace_pidgin_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_linguistic_life__marketplace_pidgin_reading, 0.63, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_linguistic_life__marketplace_pidgin_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(hebrew_linguistic_life__marketplace_pidgin_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(hebrew_linguistic_life__marketplace_pidgin_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   All scores describe the standing arrangement (the death-and-revival historiographical settlement) as this reading assesses it, at interval end. Extractiveness 0.63: the settlement converts documented continuous practice into evidential nullity and banks the recovered credit as national creation-narrative — real but epistemic and status extraction rather than material dispossession, hence well short of coercive maxima. Suppression 0.44: enforcement is curricular and canonical (textbook approval, referee skepticism, footnote dismissal of marketplace sources), not prohibitory; the series shows machinery building through statehood, peaking in the orthodoxy decades as the last living witnesses of marketplace Hebrew died out, then relaxing as revisionist contest normalized. Theater 0.33: commemorative ritual (revival day, street names, anniversary pageantry) layers onto genuinely functional pedagogy. Accessibility collapse 0.35: the rival criteria remain fully available — sibling readings are publishable positions, not collapsed alternatives. Resistance 0.60: sustained revisionist-sociolinguistic and Mizrahi-historiographic pressure, plus the comparative-vitality literature's implicit challenge to any single-criterion verdict. All three tracked series share one seven-point grid (1910, 1948, 1965, 1980, 1995, 2010, 2025); end-state values equal the base_properties scalars. Identity-lock note: the academy's exit is identity_locked in the strong sense — the institution has become the resurrection's custodian, and a continuity history would convert its mandate from guardian-of-a-rebuilt-tongue to steward-of-an-unbroken-vernacular, dissolving the authority premise rather than relocating it. Suppression mechanism: predominantly structural (curricular and canonical control) with an internalized residue — generations schooled into the account treat it as common sense, which is what the gatekeeping_vs_evidential_caution omega probes. Receipt surface: the settlement's gains demonstrably bank at the academy seat (mandate, budget, adjudicating authority), so gain_flow names it; fixing is prohibitive because the only agents who could formally revise the account would, by revising it, dissolve the premise of their own authority — the cost to the fixer exceeds any benefit they capture.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seats the settlement is stewardship: a hard-won, functioning national-linguistic order administered responsibly. From the payer seats the same apparatus operates as enforced erasure — an inherited practice ruled out of existence and a counter-literature taxed at the referee's desk. The comparative-sociolinguistics observer computes neither: it sees a classification whose verdict flips with the criterion chosen. The engine derives these divergent per-seat classifications from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to real collection points: the academy banks adjudicating authority, curriculum authorities bank reproducible pedagogy, historiographers bank careers — all sit near the beneficiary end of directionality. Victim declarations: descendant communities inherit the erased practice and cannot exit their ancestry (identity_locked, near full-target); revisionist scholars bear the gatekeeping tax but retain mobile exit into comparative venues (elevated but moderated); the historical marketplace participants, powerless and unrepresented, sit nearest full-target — the erasure lands on them with no counterweight. Spatial scope amplification applies modestly: the settlement reaches globally through diaspora Hebrew teaching, raising verification difficulty for the erasure claim.
 *
 * MANDATROPHY ANALYSIS:
 *   Reading the settlement as pure extraction would overclaim: no party is coerced, exits exist, the rival criteria remain publishable — the transfer is of standing and narrative, enforced by gatekeeping rather than prohibition. Reading it as pure coordination would underclaim: the same apparatus that coordinates identity systematically strips credit from continuous practice and requires active institutional maintenance against accumulating counter-evidence. The tangled-rope classification holds both faces in view and prevents either mislabel. The R5 interview shows founding_problem_status=contested with disappearance_verdict=world_rearranges — no dead-mandate signature; the settlement still performs its legitimating function for diaspora transmission even as its historiographical core is contested, so no obsolescence flag is warranted.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    aliveness_criterion_underdetermination,
    'Which functional test fixes ''linguistic life'': inter-communal practical coordination (this reading), unbroken sacred transmission (liturgical sibling), or child acquisition with full mundane function (native-generational sibling)?',
    'No dataset resolves a definitional choice; resolution is framing-level. Each sibling criterion re-sorts the same historical record into alive/dead differently, so the resolution procedure is exhaustive per-reading reclassification, not evidence accumulation.',
    'Under this reading the settlement''s central cost is erasure of continuous inter-communal function; under the native-generational sibling the pidgin''s lack of child acquisition weakens the continuity claim and shifts the contested ground toward native-speaker ideology; under the liturgical sibling market function is classificationally inert and the settlement''s death-claim largely stands.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(aliveness_criterion_underdetermination, conceptual, 'The kernel''s aliveness criterion is a framing choice among sibling readings; epsilon and victim sets re-sort with the choice.').

omega_variable(
    pidgin_continuity_evidential_basis,
    'Was pre-1880 marketplace Hebrew a sustained inter-communal medium (a stable modified Medieval Hebrew pidgin with transactional grammar across Sephardi, Ashkenazi, Musta''arabi, and Arab counterparties), or episodic formulaic exchange later inflated into continuity?',
    'Systematic archival triangulation: travelers'' diaries, consular and missionary reports, maskilic correspondence, halakhic responsa touching market Hebrew, and sociolinguistic analysis of loan traffic and word order in the pidgin stratum.',
    'Rich corroboration hardens the erasure charge and raises the extraction this reading attributes to the settlement; thin corroboration collapses this reading''s historical delta and restores force to the settlement''s death-claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pidgin_continuity_evidential_basis, empirical, 'Whether the continuous-market-aliveness claim has an adequate documentary base.').

omega_variable(
    gatekeeping_vs_evidential_caution,
    'Does the settlement''s marginalization of marketplace evidence reflect active gatekeeping or ordinary evidential caution about sparse, formulaic sources?',
    'Trace referee and editorial histories of rejected continuity theses; compare the source-handling standards applied to marketplace evidence against standards applied to equally sparse evidence accepted in other language-death cases.',
    'A gatekeeping finding supports the authored suppression level and the active-enforcement claim; a caution finding lowers suppression toward plain coordination and reframes the settlement as defensible scholarship rather than defended orthodoxy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gatekeeping_vs_evidential_caution, empirical, 'Whether the measured suppression is enforcement machinery or scholarly prudence.').

omega_variable(
    retroactive_recognition_materiality,
    'If this reading displaced the settlement, would standing actually transfer to descendant communities and erased participants, or would recognition remain symbolic while present distributions hold?',
    'Observe whether curriculum revisions, commemorative reallocation, and funding shifts follow scholarly acceptance in analogous cases of reassessed language histories.',
    'Material transfer would make the reading''s remedy consequential and raise the stakes of the classification contest; purely symbolic recognition would lower the effective weight of the settlement''s persistence.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(retroactive_recognition_materiality, preference, 'Whether correcting the credit ledger changes anything beyond the ledger.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_linguistic_life__marketplace_pidgin_reading, 1910, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hll_mkt_pidgin_tr_t1910, hebrew_linguistic_life__marketplace_pidgin_reading, theater_ratio, 1910, 0.15).
narrative_ontology:measurement_basis(hll_mkt_pidgin_tr_t1910, observed).
narrative_ontology:measurement(hll_mkt_pidgin_tr_t1948, hebrew_linguistic_life__marketplace_pidgin_reading, theater_ratio, 1948, 0.3).
narrative_ontology:measurement_basis(hll_mkt_pidgin_tr_t1948, observed).
narrative_ontology:measurement(hll_mkt_pidgin_tr_t1965, hebrew_linguistic_life__marketplace_pidgin_reading, theater_ratio, 1965, 0.42).
narrative_ontology:measurement_basis(hll_mkt_pidgin_tr_t1965, observed).
narrative_ontology:measurement(hll_mkt_pidgin_tr_t1980, hebrew_linguistic_life__marketplace_pidgin_reading, theater_ratio, 1980, 0.45).
narrative_ontology:measurement_basis(hll_mkt_pidgin_tr_t1980, observed).
narrative_ontology:measurement(hll_mkt_pidgin_tr_t1995, hebrew_linguistic_life__marketplace_pidgin_reading, theater_ratio, 1995, 0.4).
narrative_ontology:measurement_basis(hll_mkt_pidgin_tr_t1995, observed).
narrative_ontology:measurement(hll_mkt_pidgin_tr_t2010, hebrew_linguistic_life__marketplace_pidgin_reading, theater_ratio, 2010, 0.36).
narrative_ontology:measurement_basis(hll_mkt_pidgin_tr_t2010, observed).
narrative_ontology:measurement(hll_mkt_pidgin_tr_t2025, hebrew_linguistic_life__marketplace_pidgin_reading, theater_ratio, 2025, 0.33).
narrative_ontology:measurement_basis(hll_mkt_pidgin_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(hll_mkt_pidgin_be_t1910, hebrew_linguistic_life__marketplace_pidgin_reading, base_extractiveness, 1910, 0.45).
narrative_ontology:measurement_basis(hll_mkt_pidgin_be_t1910, observed).
narrative_ontology:measurement(hll_mkt_pidgin_be_t1948, hebrew_linguistic_life__marketplace_pidgin_reading, base_extractiveness, 1948, 0.62).
narrative_ontology:measurement_basis(hll_mkt_pidgin_be_t1948, observed).
narrative_ontology:measurement(hll_mkt_pidgin_be_t1965, hebrew_linguistic_life__marketplace_pidgin_reading, base_extractiveness, 1965, 0.7).
narrative_ontology:measurement_basis(hll_mkt_pidgin_be_t1965, observed).
narrative_ontology:measurement(hll_mkt_pidgin_be_t1980, hebrew_linguistic_life__marketplace_pidgin_reading, base_extractiveness, 1980, 0.72).
narrative_ontology:measurement_basis(hll_mkt_pidgin_be_t1980, observed).
narrative_ontology:measurement(hll_mkt_pidgin_be_t1995, hebrew_linguistic_life__marketplace_pidgin_reading, base_extractiveness, 1995, 0.68).
narrative_ontology:measurement_basis(hll_mkt_pidgin_be_t1995, observed).
narrative_ontology:measurement(hll_mkt_pidgin_be_t2010, hebrew_linguistic_life__marketplace_pidgin_reading, base_extractiveness, 2010, 0.65).
narrative_ontology:measurement_basis(hll_mkt_pidgin_be_t2010, observed).
narrative_ontology:measurement(hll_mkt_pidgin_be_t2025, hebrew_linguistic_life__marketplace_pidgin_reading, base_extractiveness, 2025, 0.63).
narrative_ontology:measurement_basis(hll_mkt_pidgin_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(hll_mkt_pidgin_su_t1910, hebrew_linguistic_life__marketplace_pidgin_reading, suppression_requirement, 1910, 0.35).
narrative_ontology:measurement_basis(hll_mkt_pidgin_su_t1910, observed).
narrative_ontology:measurement(hll_mkt_pidgin_su_t1948, hebrew_linguistic_life__marketplace_pidgin_reading, suppression_requirement, 1948, 0.55).
narrative_ontology:measurement_basis(hll_mkt_pidgin_su_t1948, observed).
narrative_ontology:measurement(hll_mkt_pidgin_su_t1965, hebrew_linguistic_life__marketplace_pidgin_reading, suppression_requirement, 1965, 0.62).
narrative_ontology:measurement_basis(hll_mkt_pidgin_su_t1965, observed).
narrative_ontology:measurement(hll_mkt_pidgin_su_t1980, hebrew_linguistic_life__marketplace_pidgin_reading, suppression_requirement, 1980, 0.6).
narrative_ontology:measurement_basis(hll_mkt_pidgin_su_t1980, observed).
narrative_ontology:measurement(hll_mkt_pidgin_su_t1995, hebrew_linguistic_life__marketplace_pidgin_reading, suppression_requirement, 1995, 0.52).
narrative_ontology:measurement_basis(hll_mkt_pidgin_su_t1995, observed).
narrative_ontology:measurement(hll_mkt_pidgin_su_t2010, hebrew_linguistic_life__marketplace_pidgin_reading, suppression_requirement, 2010, 0.47).
narrative_ontology:measurement_basis(hll_mkt_pidgin_su_t2010, observed).
narrative_ontology:measurement(hll_mkt_pidgin_su_t2025, hebrew_linguistic_life__marketplace_pidgin_reading, suppression_requirement, 2025, 0.44).
narrative_ontology:measurement_basis(hll_mkt_pidgin_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_linguistic_life__marketplace_pidgin_reading, identity_coordination).
narrative_ontology:affects_constraint(hebrew_linguistic_life__marketplace_pidgin_reading, hebrew_linguistic_life__liturgical_preservation_reading).
narrative_ontology:affects_constraint(hebrew_linguistic_life__marketplace_pidgin_reading, hebrew_linguistic_life__native_generational_reading).

% DUAL FORMULATION NOTE:
% The colloquial question 'was Hebrew ever really dead?' decomposes into three structurally distinct constraints — sibling readings of kernel hebrew_linguistic_life — each with its own extractiveness over the same standing arrangement (the death-and-revival historiographical settlement): this file (marketplace_pidgin_reading) assesses the settlement's erasure of continuous inter-communal function; liturgical_preservation_reading assesses it against transmission-chain custody; native_generational_reading assesses it against child-acquisition failure. Same referent, reading-indexed values; the stories are linked, not merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
