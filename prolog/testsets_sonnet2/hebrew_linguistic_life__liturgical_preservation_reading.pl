% ============================================================================
% CONSTRAINT STORY: hebrew_linguistic_life__liturgical_preservation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   human_readable: Liturgical-Transmission Standard of Hebrew Linguistic Life
 *   domain: sociolinguistics/religious_studies/nationalism_studies
 *
 * SUMMARY:
 *   This story instantiates the liturgical-preservation reading of the
 *   contested 'is Hebrew alive' kernel: a language is alive when its sacred
 *   texts are continuously recited, studied, and transmitted in an unbroken
 *   chain, regardless of whether anyone speaks it as a mundane vernacular.
 *   Under this reading, Hebrew never died across the diaspora centuries — it
 *   was continuously used for prayer, study, and textual transmission in
 *   every generation. This has a sharp structural consequence: Ben-Yehuda's
 *   late-19th/early-20th century revival project is not resurrection of a
 *   dead language but imposition of a secular vernacular function onto a
 *   language that was already fully alive by the liturgical standard. The
 *   victim set under this reading is the sacred tradition itself and the
 *   institutions that steward it — not, as in the revivalist reading,
 *   'Hebrew' as an abstract endangered entity needing rescue. The extraction
 *   this story measures is the institutional cost the liturgical-preservation
 *   standard imposes on those who built their legitimacy on the revival
 *   narrative and on secular speakers whose vernacular fluency the standard
 *   treats as beside the point (or, in stronger versions, as corrosive to the
 *   chain).
 *
 * KEY AGENTS:
 *   - rabbinic_authorities: agenda_setter (institutional/arbitrage) — administers the transmission standard
 *   - yeshiva_institutions: beneficiary (institutional/arbitrage) — draws funding and prestige from being the transmission sites
 *   - diaspora_liturgical_communities: beneficiary (organized/constrained) — their existing practice is validated as sufficient
 *   - hebrew_revivalist_movement: payer (organized/constrained) — their founding 'resurrection' narrative is reclassified as desecration
 *   - secular_israeli_hebrew_speakers: payer (moderate/trapped) — their vernacular fluency is rendered irrelevant or suspect
 *   - vernacularizing_reformers: payer (moderate/constrained) — their reform efforts are opposed as chain-breaking
 *   - linguistic_historians: observer (analytical) — assesses the historical continuity claim independently
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_linguistic_life__liturgical_preservation_reading, 0.58).
domain_priors:suppression_score(hebrew_linguistic_life__liturgical_preservation_reading, 0.4).
domain_priors:theater_ratio(hebrew_linguistic_life__liturgical_preservation_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_linguistic_life__liturgical_preservation_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_linguistic_life__liturgical_preservation_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(hebrew_linguistic_life__liturgical_preservation_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_linguistic_life__liturgical_preservation_reading, tangled_rope).
narrative_ontology:human_readable(hebrew_linguistic_life__liturgical_preservation_reading, "Liturgical-Transmission Standard of Hebrew Linguistic Life").
narrative_ontology:topic_domain(hebrew_linguistic_life__liturgical_preservation_reading, "sociolinguistics/religious_studies/nationalism_studies").

domain_priors:requires_active_enforcement(hebrew_linguistic_life__liturgical_preservation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_linguistic_life__liturgical_preservation_reading, '1b4b1c84-aabc-4c5e-8253-b3b5de53754e').
narrative_ontology:cs_kernel_codification('1b4b1c84-aabc-4c5e-8253-b3b5de53754e', fixed_text).
narrative_ontology:cs_authority_grounding('1b4b1c84-aabc-4c5e-8253-b3b5de53754e', lineage).
narrative_ontology:cs_interpretation_layer_present('1b4b1c84-aabc-4c5e-8253-b3b5de53754e').
narrative_ontology:cs_reading_relation('1b4b1c84-aabc-4c5e-8253-b3b5de53754e', hebrew_linguistic_life__native_generational_reading, coexists_with).
narrative_ontology:cs_reading_relation('1b4b1c84-aabc-4c5e-8253-b3b5de53754e', hebrew_linguistic_life__marketplace_pidgin_reading, coexists_with).
narrative_ontology:cs_axiom('1b4b1c84-aabc-4c5e-8253-b3b5de53754e', foundational, textual_recitation_constitutes_linguistic_life).
narrative_ontology:cs_axiom_status(textual_recitation_constitutes_linguistic_life, holdable).
narrative_ontology:cs_axiom_grounding('1b4b1c84-aabc-4c5e-8253-b3b5de53754e', textual_recitation_constitutes_linguistic_life, conventional).
narrative_ontology:cs_axiom('1b4b1c84-aabc-4c5e-8253-b3b5de53754e', secondary, vernacular_acquisition_is_not_necessary_for_liveness).
narrative_ontology:cs_axiom_status(vernacular_acquisition_is_not_necessary_for_liveness, holdable).
narrative_ontology:cs_axiom_grounding('1b4b1c84-aabc-4c5e-8253-b3b5de53754e', vernacular_acquisition_is_not_necessary_for_liveness, conventional).
narrative_ontology:cs_created_at('1b4b1c84-aabc-4c5e-8253-b3b5de53754e', '').
narrative_ontology:cs_kernel_id(hebrew_linguistic_life__liturgical_preservation_reading, hebrew_linguistic_life).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__liturgical_preservation_reading, rabbinic_authorities).
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__liturgical_preservation_reading, yeshiva_institutions).
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__liturgical_preservation_reading, diaspora_liturgical_communities).
narrative_ontology:constraint_victim(hebrew_linguistic_life__liturgical_preservation_reading, hebrew_revivalist_movement).
narrative_ontology:constraint_victim(hebrew_linguistic_life__liturgical_preservation_reading, secular_israeli_hebrew_speakers).
narrative_ontology:constraint_victim(hebrew_linguistic_life__liturgical_preservation_reading, vernacularizing_reformers).
narrative_ontology:constraint_vindicates(hebrew_linguistic_life__liturgical_preservation_reading, unbroken_chain_transmission_doctrine).
narrative_ontology:constraint_vindicates(hebrew_linguistic_life__liturgical_preservation_reading, sacred_language_persistence_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer the standard by which Hebrew's liveness is judged: continuous liturgical recitation and study, generation to generation, without break. They set curricula, certify who may read Torah publicly, and control the institutional apparatus (yeshivot, batei din) that reproduces the chain. They can adapt the standard's application without needing anyone's permission, and they collect deference, authority, and continued institutional relevance from the reading being accepted.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, rabbinic_authorities, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Draw funding, prestige, and enrollment from being the recognized sites of unbroken transmission. Their institutional value depends on liturgical Hebrew being treated as the measure of the language's life, independent of whether anyone speaks it at the market.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, yeshiva_institutions, beneficiary,
    institutional, civilizational, arbitrage, global).

% Recite and study Hebrew liturgically for centuries without vernacular fluency. Under this reading their practice already constitutes linguistic life in full — they need no revival, no secular competence, nothing added. The reading validates their existing practice as sufficient and complete.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, diaspora_liturgical_communities, beneficiary,
    organized, generational, constrained, global).

% Built the case for Hebrew's 'death' as a spoken vernacular in order to justify Ben-Yehuda-style resurrection as a nationalist project. Under this reading their founding narrative is false on its own terms: the language never died, so the revivalists' central claim to historic achievement is recast as either a misunderstanding or a deliberate secularizing rupture with the sacred chain. They bear the cost of having their entire legitimating narrative reclassified as desecration rather than rescue.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, hebrew_revivalist_movement, payer,
    organized, generational, constrained, national).

% Speak Modern Hebrew as a native, secular, daily-use vernacular — for shopping, arguing, falling in love, filing taxes. Under this reading, their vernacular fluency is irrelevant to whether the language is 'alive'; worse, some readings of the standard treat their secularization of liturgical vocabulary as a rupture of the sacred chain, effectively erasing their generations-deep linguistic practice from the definition of life the reading enforces. They cannot exit the linguistic community they were born into, and the reading does not recognize what they do as the relevant evidence.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, secular_israeli_hebrew_speakers, payer,
    moderate, biographical, trapped, national).

% Argue for expanding Hebrew's domains of use, translating liturgy, simplifying study for broader access. Under the liturgical-preservation standard, their reforms threaten the unbroken chain itself and are opposed by the agenda-setting authorities as a corrupting influence, regardless of how much they increase actual comprehension or use.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, vernacularizing_reformers, payer,
    moderate, biographical, constrained, regional).

% Document the actual continuity of Hebrew liturgical use across two millennia of diaspora, and assess whether 'never died' is a defensible historical claim or a retrospective doctrine serving present institutional interests.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, linguistic_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hebrew_linguistic_life__liturgical_preservation_reading, diffuse).
narrative_ontology:fixing_cost_class(hebrew_linguistic_life__liturgical_preservation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, transmissible standard for identifying which texts, pronunciations, and interpretive traditions count as authentic Hebrew across dispersed, disconnected communities over centuries — solving the coordination problem of maintaining a shared sacred canon without a central state or continuous territorial base.
% TRANSFER_FUNCTION: Moves interpretive authority, institutional funding, and definitional control over 'what counts as Hebrew being alive' toward rabbinic and yeshiva institutions, and away from the nationalist revival narrative and from secular vernacular speakers whose daily practice is not liturgical.
% ABSENT_VOICES: Secular Israeli speakers who treat vernacular fluency as self-evidently linguistic life are not parties to the liturgical standard's adjudication; the standard was set and is administered by textual authorities who do not need their agreement. Ben-Yehuda's own historical voice, framed within his movement as heroic resurrection, is recast entirely from outside his tradition.
% DISAPPEARANCE_RATIONALE: If the liturgical-preservation standard vanished, rabbinic and yeshiva institutions would lose a load-bearing justification for their centrality to Hebrew's identity claim, and the revivalist narrative would go uncontested as the sole account of the language's life-and-death arc. Whether the 'world rearranges' or stays put depends on which party you ask: liturgical communities say their practice is unaffected either way (they will keep reciting regardless of doctrine), while institutional actors who use the standard to claim primacy over the revivalist account would lose significant rhetorical ground.
% FOUNDING_PROBLEM: How to certify that a sacred textual tradition remains authentically transmitted, letter and cantillation intact, across geographically scattered communities with no common state, in the absence of continuous vernacular use.
% FOUNDING_PROBLEM_CORROBORATION: Historians of the Cairo Geniza corpus and comparative liturgical scholars outside the rabbinic establishment attest that continuous liturgical Hebrew use across the diaspora is well documented and did not require revival; they do not, however, corroborate the further claim that this makes secular vernacular Hebrew a 'desecration' — that evaluative step is asserted only from within the liturgical-preservation tradition itself, and no outside corroboration for it is offered here.
narrative_ontology:disappearance_verdict(hebrew_linguistic_life__liturgical_preservation_reading, contested).
narrative_ontology:founding_problem_status(hebrew_linguistic_life__liturgical_preservation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_linguistic_life__liturgical_preservation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(hebrew_linguistic_life__liturgical_preservation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_linguistic_life__liturgical_preservation_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_linguistic_life__liturgical_preservation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(hebrew_linguistic_life__liturgical_preservation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(hebrew_linguistic_life__liturgical_preservation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.58 at interval end) reflects the real cost this standard imposes: it does not merely describe liturgical continuity, it actively delegitimizes a century of nationalist revival scholarship and displaces secular vernacular fluency as evidence of linguistic life. Suppression is moderate (0.40) — the standard is maintained mostly through institutional authority and canonical control over textual certification rather than coercive force, but it does actively work to marginalize reform movements that would translate or simplify liturgy. Theater ratio is low-moderate (0.22) because the core transmission function (actual continuous recitation and study) is real and substantial, not merely performed — but a growing share of institutional energy over the measured interval goes toward defending the doctrinal framing against the revivalist counter-narrative rather than toward transmission itself, hence the rising trajectory.
 *
 * PERSPECTIVAL GAP:
 *   From the rabbinic/yeshiva seat, this is straightforward tangled-rope coordination: a real, functioning standard that has held a scattered textual tradition together for two millennia, now defending itself against a nationalist counter-narrative that would erase its historical priority. From the revivalist and secular-speaker seats, the same standard operates as an extraction of legitimacy — their generations of vernacular development and their movement's foundational claim to have 'saved' Hebrew are structurally erased by a doctrine that asserts the language never needed saving. The engine should compute these as different seat-level classifications from the same structural data, not reconcile them.
 *
 * DIRECTIONALITY LOGIC:
 *   Rabbinic authorities and yeshiva institutions sit at the beneficiary end: they administer the standard, and its acceptance directly funds and legitimizes them (low d). Diaspora liturgical communities benefit by having their existing, centuries-old practice validated as already-sufficient — no revival needed, no deficiency to fix (low-moderate d). The hebrew_revivalist_movement, secular_israeli_hebrew_speakers, and vernacularizing_reformers sit toward the target end: the standard actively reclassifies their central achievements (resurrection, vernacular fluency, liturgical reform) as either irrelevant or corrosive, and their exit options are constrained-to-trapped because they cannot simply opt out of the community whose linguistic identity is being adjudicated.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — certifying authentic sacred-text transmission across a scattered diaspora — remains genuinely live for liturgical communities (they still recite, still study, still need the standard to adjudicate authenticity disputes). It is not dead. But its application to the separate, later question of vernacular revival is a scope expansion: the standard was built to certify liturgical continuity, not to adjudicate whether secular nationalist vernacularization counts as legitimate or as desecration. Where the standard is used only for its founding function it is closer to a rope; where it is extended to delegitimize the revivalist and vernacular readings entirely, it becomes tangled-rope, coordinating real transmission while extracting legitimacy from a rival linguistic-life claim.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    unbroken_chain_historicity,
    'Is the claim that Hebrew liturgical transmission was genuinely unbroken across the diaspora an accurate historical description, or a retrospective doctrine constructed to serve present institutional legitimacy claims against the revivalist narrative?',
    'Comparative philological and historical analysis of liturgical Hebrew usage records (Cairo Geniza documents, medieval responsa literature, diaspora community records) to establish continuity or identify genuine discontinuities the doctrine papers over.',
    'If genuinely unbroken, the liturgical-preservation reading''s core claim is empirically well-grounded and the revivalist ''resurrection'' narrative is factually overstated. If discontinuities existed and were smoothed over retrospectively, the liturgical-preservation reading itself becomes a constructed doctrine competing with the revivalist one on similarly rhetorical grounds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unbroken_chain_historicity, empirical, 'Whether unbroken liturgical transmission is historically accurate or a legitimating retrospective construction.').

omega_variable(
    kernel_reading_incommensurability,
    'Are the liturgical_preservation, native_generational, and marketplace_pidgin readings of ''Hebrew linguistic life'' genuinely asking the same question with different answers, or are they answering three different questions that the single natural-language label ''is Hebrew alive'' conflates?',
    'Formal specification of what each reading treats as the operative unit of evidence (canonical text recitation vs. child acquisition vs. functional inter-communal use) and whether these units are commensurable on a single scale or are orthogonal criteria smuggled under one label.',
    'If orthogonal, ''Hebrew was alive/dead'' disputes between revivalists and traditionalists are category errors — both can be simultaneously correct about disjoint criteria, dissolving the apparent contradiction the kernel currently frames as a live contest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_incommensurability, conceptual, 'Whether the three kernel readings share a genuine evaluative target or answer disjoint questions under one label.').

omega_variable(
    desecration_vs_rupture_evaluative_step,
    'Is characterizing the revivalist secular-vernacular project as ''desecration'' a defensible extension of the liturgical-preservation standard''s own logic, or an additional evaluative claim not entailed by the continuity thesis itself?',
    'Trace whether traditional rabbinic sources historically treated vernacular secularization of sacred vocabulary as desecration prior to the 20th century revival, or whether this framing emerged specifically as a reaction to Ben-Yehuda''s project.',
    'If the desecration framing predates and is independent of the revival reaction, the liturgical-preservation reading''s victim characterization (revival as harm to the sacred tradition) is well-grounded. If it emerged only as reactive framing, the extraction claim against revivalists is weaker than authored and the reading''s ε may be inflated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(desecration_vs_rupture_evaluative_step, conceptual, 'Whether ''desecration'' is entailed by the continuity thesis or is a separate reactive evaluative addition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_linguistic_life__liturgical_preservation_reading, 0, 150).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t0, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(hebr_tr_t30, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 30, 0.12).
narrative_ontology:measurement(hebr_tr_t60, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 60, 0.15).
narrative_ontology:measurement(hebr_tr_t90, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 90, 0.18).
narrative_ontology:measurement(hebr_tr_t120, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 120, 0.2).
narrative_ontology:measurement(hebr_tr_t150, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 150, 0.22).

% Extraction over time
narrative_ontology:measurement(hebr_be_t0, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(hebr_be_t30, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 30, 0.35).
narrative_ontology:measurement(hebr_be_t60, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 60, 0.42).
narrative_ontology:measurement(hebr_be_t90, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 90, 0.5).
narrative_ontology:measurement(hebr_be_t120, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 120, 0.55).
narrative_ontology:measurement(hebr_be_t150, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 150, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t0, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(hebr_su_t30, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 30, 0.28).
narrative_ontology:measurement(hebr_su_t60, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 60, 0.3).
narrative_ontology:measurement(hebr_su_t90, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 90, 0.33).
narrative_ontology:measurement(hebr_su_t120, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 120, 0.37).
narrative_ontology:measurement(hebr_su_t150, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 150, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_linguistic_life__liturgical_preservation_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(hebrew_linguistic_life__liturgical_preservation_reading, 0.1).
narrative_ontology:affects_constraint(hebrew_linguistic_life__liturgical_preservation_reading, hebrew_linguistic_life__native_generational_reading).
narrative_ontology:affects_constraint(hebrew_linguistic_life__liturgical_preservation_reading, hebrew_linguistic_life__marketplace_pidgin_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the hebrew_linguistic_life kernel, decomposed per the ε-invariance principle because the natural-language question 'is Hebrew alive' conflates three structurally distinct evaluative criteria (liturgical continuity, native acquisition, functional inter-communal use) that produce different victim sets and different ε values. This reading's ε (0.58) measures the extraction the liturgical-preservation standard imposes on the revivalist movement and secular vernacular speakers; the native_generational_reading's ε would instead measure extraction from Diaspora liturgical communities whose practice it would deem insufficient for 'life'; the marketplace_pidgin_reading's ε would measure a different set again. All three share the kernel but are separate constraints with separate stakeholder sets.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
