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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: hebrew_linguistic_life__liturgical_preservation_reading
 *   human_readable: Hebrew Language Continuity via Liturgical Chain (Preservation Reading)
 *   domain: sociolinguistics/religious_studies/nationalism_studies
 *
 * SUMMARY:
 *   The liturgical preservation reading claims that Hebrew remained alive not
 *   through vernacular speech but through unbroken chain of sacred textual
 *   study and recitation—a trans-generational, trans-geographic transmission
 *   independent of native speakers or contemporary usage. This reading treats
 *   Ben-Yehuda's revival project and modern Israeli vernacular Hebrew not as
 *   resurrection of a dead language but as desecration of a sacred object.
 *   The constraint mechanism operates by delegitimizing vernacular
 *   innovation, granting exclusive interpretive authority to religious
 *   establishments, and framing the question 'Is Hebrew alive?' as a question
 *   about textual transmission fidelity rather than speaker functionality.
 *   The sibling readings (native_generational_reading,
 *   marketplace_pidgin_reading) answer the same question radically
 *   differently: they claim Hebrew is alive precisely because children speak
 *   it natively, or because it functions as inter-communal market medium.
 *   This story instantiates ONLY the liturgical preservation reading; the
 *   other readings are separate constraints with separate ε values and
 *   victim/beneficiary structures.
 *
 * KEY AGENTS:
 *   - Religious authority structures (rabbinate, yeshiva hierarchies, religious courts): institutional power, civilizational time horizon, trapped exit — control enforcement of the chain
 *   - Hebrew vernacular speakers (primarily Israeli, post-1920s): powerful institutional power, biographical time horizon, mobile exit — claim to use Hebrew as living national language
 *   - Sacred textual tradition (Torah, Talmud, liturgical corpus): non-agent, analytical power, civilizational horizon, trapped exit — locked into single interpretive chain
 *   - Ben-Yehuda revival movement and successors: powerful institutional power, biographical horizon, constrained exit — excluded from this reading's legitimacy framework as desecrators
 *   - Diaspora religious communities: organized power, generational time horizon, constrained exit — receive coordination benefit of liturgical continuity
 *   - Secular Israeli state: institutional power, generational horizon, constrained exit — institutionally excludes this reading by treating Hebrew as national language
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_linguistic_life__liturgical_preservation_reading, 0.68).
domain_priors:suppression_score(hebrew_linguistic_life__liturgical_preservation_reading, 0.72).
domain_priors:theater_ratio(hebrew_linguistic_life__liturgical_preservation_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_linguistic_life__liturgical_preservation_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_linguistic_life__liturgical_preservation_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(hebrew_linguistic_life__liturgical_preservation_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_linguistic_life__liturgical_preservation_reading, tangled_rope).
narrative_ontology:human_readable(hebrew_linguistic_life__liturgical_preservation_reading, "Hebrew Language Continuity via Liturgical Chain (Preservation Reading)").
narrative_ontology:topic_domain(hebrew_linguistic_life__liturgical_preservation_reading, "sociolinguistics/religious_studies/nationalism_studies").

domain_priors:requires_active_enforcement(hebrew_linguistic_life__liturgical_preservation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_linguistic_life__liturgical_preservation_reading, '0f5d5b4c-7538-4339-912c-cb748bfc53d5').
narrative_ontology:cs_kernel_codification('0f5d5b4c-7538-4339-912c-cb748bfc53d5', fixed_text).
narrative_ontology:cs_authority_grounding('0f5d5b4c-7538-4339-912c-cb748bfc53d5', lineage).
narrative_ontology:cs_interpretation_layer_present('0f5d5b4c-7538-4339-912c-cb748bfc53d5').
narrative_ontology:cs_reading_relation('0f5d5b4c-7538-4339-912c-cb748bfc53d5', hebrew_linguistic_life__native_generational_reading, forecloses).
narrative_ontology:cs_reading_relation('0f5d5b4c-7538-4339-912c-cb748bfc53d5', hebrew_linguistic_life__marketplace_pidgin_reading, coexists_with).
narrative_ontology:cs_axiom('0f5d5b4c-7538-4339-912c-cb748bfc53d5', foundational, linguistic_life_independent_of_vernacular_vitality).
narrative_ontology:cs_axiom_status(linguistic_life_independent_of_vernacular_vitality, holdable).
narrative_ontology:cs_axiom_grounding('0f5d5b4c-7538-4339-912c-cb748bfc53d5', linguistic_life_independent_of_vernacular_vitality, deontological).
narrative_ontology:cs_axiom('0f5d5b4c-7538-4339-912c-cb748bfc53d5', foundational, sacred_textual_chain_preserves_linguistic_continuity).
narrative_ontology:cs_axiom_status(sacred_textual_chain_preserves_linguistic_continuity, holdable).
narrative_ontology:cs_axiom_grounding('0f5d5b4c-7538-4339-912c-cb748bfc53d5', sacred_textual_chain_preserves_linguistic_continuity, conventional).
narrative_ontology:cs_reference_frame('0f5d5b4c-7538-4339-912c-cb748bfc53d5', unbroken_liturgical_transmission).
narrative_ontology:cs_drift_state('0f5d5b4c-7538-4339-912c-cb748bfc53d5', contemporary_vernacular_dominance, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('0f5d5b4c-7538-4339-912c-cb748bfc53d5', '2026-06-12T14:32:15Z').
narrative_ontology:cs_kernel_id(hebrew_linguistic_life__liturgical_preservation_reading, hebrew_linguistic_life).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__liturgical_preservation_reading, religious_authority_structures).
narrative_ontology:constraint_victim(hebrew_linguistic_life__liturgical_preservation_reading, sacred_textual_tradition).
narrative_ontology:constraint_victim(hebrew_linguistic_life__liturgical_preservation_reading, hebrew_vernacular_speakers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__liturgical_preservation_reading, liturgical_practitioners).
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__liturgical_preservation_reading, diaspora_communities).
narrative_ontology:constraint_victim(hebrew_linguistic_life__liturgical_preservation_reading, liturgical_practitioners).
narrative_ontology:constraint_vindicates(hebrew_linguistic_life__liturgical_preservation_reading, sacred_transmission_continuity_doctrine).
narrative_ontology:constraint_vindicates(hebrew_linguistic_life__liturgical_preservation_reading, liturgical_centrality_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Rabbinical institutions, yeshiva hierarchies, and religious courts maintain and enforce the unbroken chain of textual study and recitation. They control curricula, validate interpretive authority, set liturgical standards, and determine what counts as proper transmission. They benefit from this constraint by maintaining exclusive interpretive authority and institutional continuity across diaspora communities. Exit would mean dissolution of the religious knowledge monopoly.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, religious_authority_structures, agenda_setter,
    institutional, civilizational, trapped, global).

% Observant practitioners participate in daily prayer, holiday observances, and ritual study. They receive the coordination benefit of joining a trans-generational, trans-geographic community through shared liturgical language and practice. They also bear the cost of maintaining a liturgical register distinct from and often inaccessible to vernacular speakers, and of defending the chain against vernacular displacement as the 'living' standard of the language.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, liturgical_practitioners, beneficiary,
    moderate, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(hebrew_linguistic_life__liturgical_preservation_reading, liturgical_practitioners, payer).

% Native speakers (primarily in Israel since the 1920s) who use Hebrew for all daily functions—commerce, governance, childhood socialization, secular education. They pay the cost of this constraint by having their linguistic practice delegitimized as profane corruption of the sacred register, by educational curricula that prioritize liturgical Hebrew over contemporary usage, and by institutional pressure to justify vernacular innovation through liturgical precedent rather than functional necessity. Their exit option is to treat Hebrew as a regular national language without sacred constraint, which they substantially exercise.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, hebrew_vernacular_speakers, payer,
    powerful, biographical, mobile, regional).

% The corpus of Torah, Talmud, liturgical poetry, and halakhic texts. This non-agent entity bears extraction because the constraint locks interpretation into a single authoritative chain: new meanings must be derived from precedent rather than created freely; the texts cannot drift into vernacular idiom or colloquial meaning-shifts without threatening the constraint's coherence. The tradition is instrumentalized to serve the chain itself rather than existing for its own interpretive richness.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, sacred_textual_tradition, payer,
    analytical, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(hebrew_linguistic_life__liturgical_preservation_reading, sacred_textual_tradition).

% Eliezer Ben-Yehuda and the Hebrew revival movement (late 19th–20th century) sought to make Hebrew the vernacular language of Zionist settlement. From the liturgical preservation reading's framework, this project is not resurrection but desecration—it attempts to strip Hebrew of its sacred constraint, treat it as a utilitarian national language, and validate vernacular innovation over textual fidelity. This reading excludes the revival as a legitimate claim about what kept Hebrew 'alive,' viewing it instead as a displacement attack on the authentic constraint.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, ben_yehuda_revival_project, excluded,
    powerful, biographical, constrained, regional).

% Jewish communities across the world participate in the liturgical chain without using Hebrew vernacularly in daily life. They receive the coordination benefit of linguistic and religious continuity across centuries and continents—a child in Brooklyn and a child in Baghdad can pray together in the same language without learning each other's vernaculars. They carry the cost of maintaining a dead language in active use and of defending its sacredness against claims that 'real' linguistic life requires vernacular function.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, diaspora_communities, beneficiary,
    organized, generational, constrained, global).

% The Israeli state (post-1948) institutionally excludes this reading by enshrining Hebrew as a national language subject to secular development, curriculum innovation, and functional extension. The state apparatus validates vernacular innovation, encourages new coinages, and treats Hebrew as a living language precisely because children acquire it natively and use it for mundane functions. From the liturgical preservation reading, this exclusion is institutional desecration—the state has captured the language for secular nationalism.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, secular_hebrew_state, excluded,
    institutional, generational, constrained, regional).

% Academic researchers study Hebrew's historical transmission, textual traditions, and phonological evolution. They analyze whether the constraint actually preserves the language or whether it freezes it in amber and examine whether vernacular drift necessarily breaks the chain or merely adds registers to it. They sit outside the constraint's enforcement but provide evidence for debates about its structural necessity.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, textual_scholars_and_linguists, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hebrew_linguistic_life__liturgical_preservation_reading, religious_authority_structures).
narrative_ontology:fixing_cost_class(hebrew_linguistic_life__liturgical_preservation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains trans-generational and trans-geographic Jewish religious community through a shared liturgical language stable across centuries and diaspora dispersal. Solves the problem of how separated communities separated by geography, vernacular, and time can participate in synchronous religious practice and textual study.
% TRANSFER_FUNCTION: Moves interpretive authority from individual speakers and communities to centralized religious authority (rabbinate, yeshiva hierarchies, canonical texts). Moves linguistic innovation potential away from vernacular usage and toward constrained derivation from precedent. Extracts from Hebrew speakers the right to treat the language as a utilitarian tool subject to functional innovation.
% ABSENT_VOICES: Secular Israeli speakers whose Hebrew practice contradicts the liturgical preservation reading; the Ben-Yehuda revival movement and its contemporary successors; children for whom Hebrew is a native language but liturgical study is optional or absent; linguistic communities for whom the constraint feels like imposed religious dominance over a language they treat as national property.
% DISAPPEARANCE_RATIONALE: If this constraint vanished—if the unbroken liturgical chain ceased to be the legitimating criterion for Hebrew's linguistic life—then Hebrew would be fully claimed by the secular state, vernacular innovation would accelerate without need to justify it through textual precedent, and religious authority's monopoly over linguistic legitimacy would dissolve. The language would reorganize as a utilitarian national tool rather than a sacred object.
% FOUNDING_PROBLEM: How does Hebrew remain recognizable as a single language across diaspora communities that do not speak it vernacularly and centuries of contact with other languages, without a living community of native speakers to keep it adaptive? The liturgical preservation reading answers: through unbroken chain of sacred textual study and recitation, which makes linguistic continuity independent of vernacular vitality.
% FOUNDING_PROBLEM_CORROBORATION: Religious authorities and textual scholars attest that the founding problem was real and urgent (second Temple period through early modern diaspora). Secular Israeli linguistic historians and Ben-Yehuda movement historians attest that the problem was SOLVED by native acquisition and vernacular speech, and that the religious reading's answer is not necessary—the language lives through children speaking it, not through liturgical chains. The contest is live: defenders of the liturgical preservation reading argue vernacular Hebrew is not 'real' linguistic life but rather desecration of a sacred object; Israeli linguists and demographers argue Hebrew is precisely alive because it functions as a living language for daily use.
narrative_ontology:disappearance_verdict(hebrew_linguistic_life__liturgical_preservation_reading, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_linguistic_life__liturgical_preservation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_linguistic_life__liturgical_preservation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(hebrew_linguistic_life__liturgical_preservation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_linguistic_life__liturgical_preservation_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is moderate-high (0.68 at interval end) because the constraint systematically subordinates vernacular innovation to sacred preservation: new meanings must be justified through textual precedent rather than created freely. The measurement series shows rising extractiveness over the 40-year interval, reflecting accelerating conflict between liturgical preservation claims and Israeli vernacular dominance—as vernacular Hebrew became genuinely established as children's first language, the constraint's cost to the vernacular community increased. Suppression is high (0.72) because the constraint's persistence depends on institutional enforcement: religious curricula prioritize liturgical register, rabbinical authority adjudicates linguistic legitimacy, and vernacular innovation faces institutional delegitimization. Theater is moderate (0.41) because the constraint maintains a genuinely real coordination function (trans-generational liturgical continuity) but increasingly performs that function theatrically for vernacular speakers who treat it as an imposed sacred register rather than a living linguistic necessity. The measurement trajectory shows rising theater: as secular linguistic practice became dominant, the proportion of liturgical activity that serves coordination versus cultural performance increased. Rising extractiveness + rising theater signals a constraint whose coordination function is increasingly subordinated to authority maintenance.
 *
 * PERSPECTIVAL GAP:
 *   The religious authority seat and the vernacular-speaker seat compute radically different types from identical structural data because their directionalities diverge. From the religious authority perspective, the constraint is genuine coordination (unites diaspora, preserves tradition, solves the transmission problem)—d is low, extractiveness is low, type is rope. From the vernacular-speaker perspective, the constraint is enforced extraction (imposes a dead register, delegitimizes living speech, maintains institutional rent on linguistic authority)—d is high, extractiveness is high, type is tangled_rope or snare. The engine computes both from the structural data (beneficiary/victim declarations, power atoms, exit options); the authored claim (tangled_rope from this reading's perspective) and metrics (extractiveness 0.68, suppression 0.72) reflect the power asymmetry: those who enforce the constraint benefit from it, those who speak the language vernacularly pay for it.
 *
 * DIRECTIONALITY LOGIC:
 *   Religious authority structures sit at the beneficiary end (d near 1.0): they control the constraint, collect the interpretive rent, and have no meaningful exit—their institutional authority depends on maintaining the chain. Hebrew vernacular speakers sit at the target end but with higher mobility than the typical victim (d ~0.7): they bear costs through institutional delegitimization and curriculum pressure, but they have substantial exit options through secularization and state-level linguistic authority. Liturgical practitioners are near symmetric (d ~0.45): they receive genuine coordination benefit (participation in trans-generational community) but also carry identity lock that makes exit costly even when material costs rise. The sacred textual tradition is analytically positioned as a victim (d ~1.0 in effect) because the constraint locks it into a single interpretive chain rather than allowing it the adaptive drift that living traditions undergo. Ben-Yehuda's movement and modern Israeli vernacular are excluded rather than seated, which this reading interprets as proper: from the liturgical preservation frame, their claims to represent 'real' Hebrew are not legitimate positions to accommodate but rather threats to the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (maintaining linguistic continuity without native speakers) was real and urgent through the early 20th century. The founding_problem_status is 'contested' because the Israeli state and vernacular-speaker communities claim the problem is SOLVED through native acquisition (vernacular speakers), while the liturgical preservation reading claims the solution is desecration, not resolution. This reading refuses the resolution because accepting it would dissolve the constraint: if Hebrew is 'alive' because children speak it vernacularly, then the liturgical chain is no longer the criterion of linguistic life. The constraint persists by denying that the founding problem is solved—by redefining what counts as 'linguistic life' to exclude vernacular speakers who do not participate in the chain. This is a clear case of mandate atrophy: the constraint's founding problem has been objectively answered by vernacular acquisition, but the constraint persists through redefinition of success criteria rather than by continuing to solve the original problem.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_chain_vs_living_tradition,
    'Does maintaining unbroken textual transmission necessarily preserve the language, or does it freeze it in amber and create a second dead language (liturgical Hebrew) alongside modern Hebrew?',
    'Longitudinal study of phonological, morphological, and semantic drift in liturgical Hebrew across diaspora communities versus native-speaker Hebrew; examination of whether divergence creates mutual unintelligibility or registers within a single language.',
    'If liturgical freezing causes divergence into two languages, the constraint fails to preserve Hebrew and instead creates a sacred register divorced from linguistic continuity. If drift is minimal and registers remain mutually intelligible, the constraint succeeds but at the cost of suppressing natural language evolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_chain_vs_living_tradition, empirical, 'Whether textual transmission preserves a living language or creates a fossilized register.').

omega_variable(
    desecration_vs_evolution,
    'Is Ben-Yehuda''s project best understood as desecration of a sacred object (the liturgical preservation reading) or as natural language evolution where a dead language is revived for utilitarian function (the native generational reading)?',
    'This is a committer-axis question: the frame (sacred vs. utilitarian) determines the vocabulary. Resolution requires examining whether the same structural events (children speaking Hebrew, state institutions standardizing Hebrew, secular literature developing in Hebrew) are categorized as desecration or revival.',
    'If desecration framing is correct, the constraint is being attacked and its persistence represents successful resistance; if evolution framing is correct, the constraint has already failed and its persistence is pure theater. The classification hinges on the reading''s axioms, not on empirical facts about what happened.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(desecration_vs_evolution, conceptual, 'Whether modern Hebrew is treated as evolution or violation of sacred linguistic life.').

omega_variable(
    suppression_internalization_mechanism,
    'Is the measured suppression (0.72) structural (institutional exclusion of vernacular innovation, curriculum controls, rabbinical authority) or internalized (Hebrew speakers have internalized the sacred-profane binary and self-regulate their language use)?',
    'Post-secularization trajectory: if suppression persists after institutional authority weakens (as secular Israeli society has grown), reclassify as partially internalized. If suppression declines with institutional decline, classify as structural.',
    'Structural suppression can be addressed by institutional change; internalized suppression persists even after structural barriers are removed. This affects exit-option assessment: Hebrew speakers may appear mobile (they have secular state authority to back them) but remain trapped if they have internalized the sacred-profane constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_mechanism, empirical, 'Mechanism of suppression: structural barriers versus internalized norms.').

omega_variable(
    reading_foreclosure_status,
    'Does the native_generational reading logically foreclose this liturgical_preservation reading within any single coherent framework, or do they merely coexist as incompatible positions held by different parties?',
    'Test whether a single framework (e.g., Israeli law, rabbinical halakha, linguistic science) can hold both readings without contradiction. If a framework can hold both (e.g., ''Hebrew is alive both through vernacular speech AND through liturgical transmission''), the readings coexist. If holding both requires logical contradiction (e.g., ''the criterion of linguistic life is both dependent and independent of vernacular use''), foreclosure holds.',
    'Foreclosure would mean one reading must eventually win and the other be formally rejected; coexistence means the readings can persist in different jurisdictions or institutional contexts indefinitely. Current reality suggests coexistence within Israeli pluralism but foreclosure within religious frameworks and foreclosure within secular frameworks.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_status, conceptual, 'Whether sibling readings are logically contradictory or merely incompatible.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_linguistic_life__liturgical_preservation_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t0, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(hebr_tr_t0, observed).
narrative_ontology:measurement(hebr_tr_t5, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 5, 0.26).
narrative_ontology:measurement_basis(hebr_tr_t5, observed).
narrative_ontology:measurement(hebr_tr_t10, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 10, 0.32).
narrative_ontology:measurement_basis(hebr_tr_t10, observed).
narrative_ontology:measurement(hebr_tr_t15, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 15, 0.37).
narrative_ontology:measurement_basis(hebr_tr_t15, observed).
narrative_ontology:measurement(hebr_tr_t25, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 25, 0.4).
narrative_ontology:measurement_basis(hebr_tr_t25, observed).
narrative_ontology:measurement(hebr_tr_t40, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 40, 0.41).
narrative_ontology:measurement_basis(hebr_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(hebr_be_t0, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(hebr_be_t0, observed).
narrative_ontology:measurement(hebr_be_t5, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement_basis(hebr_be_t5, observed).
narrative_ontology:measurement(hebr_be_t10, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement_basis(hebr_be_t10, observed).
narrative_ontology:measurement(hebr_be_t15, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 15, 0.62).
narrative_ontology:measurement_basis(hebr_be_t15, observed).
narrative_ontology:measurement(hebr_be_t25, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 25, 0.66).
narrative_ontology:measurement_basis(hebr_be_t25, observed).
narrative_ontology:measurement(hebr_be_t40, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(hebr_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t0, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(hebr_su_t0, observed).
narrative_ontology:measurement(hebr_su_t5, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 5, 0.62).
narrative_ontology:measurement_basis(hebr_su_t5, observed).
narrative_ontology:measurement(hebr_su_t10, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 10, 0.66).
narrative_ontology:measurement_basis(hebr_su_t10, observed).
narrative_ontology:measurement(hebr_su_t15, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 15, 0.69).
narrative_ontology:measurement_basis(hebr_su_t15, observed).
narrative_ontology:measurement(hebr_su_t25, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 25, 0.71).
narrative_ontology:measurement_basis(hebr_su_t25, observed).
narrative_ontology:measurement(hebr_su_t40, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 40, 0.72).
narrative_ontology:measurement_basis(hebr_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_linguistic_life__liturgical_preservation_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(hebrew_linguistic_life__liturgical_preservation_reading, 0.12).
narrative_ontology:affects_constraint(hebrew_linguistic_life__liturgical_preservation_reading, hebrew_linguistic_life__native_generational_reading).
narrative_ontology:affects_constraint(hebrew_linguistic_life__liturgical_preservation_reading, hebrew_linguistic_life__marketplace_pidgin_reading).
narrative_ontology:affects_constraint(hebrew_linguistic_life__liturgical_preservation_reading, israeli_national_identity_construction).
narrative_ontology:affects_constraint(hebrew_linguistic_life__liturgical_preservation_reading, rabbinical_authority_maintenance).

% DUAL FORMULATION NOTE:
% This constraint is one of three decomposed readings of the HEBREW_LINGUISTIC_LIFE kernel. The three readings have structurally distinct ε values and victim/beneficiary sets because they answer fundamentally different questions about what constitutes linguistic life. The liturgical_preservation_reading treats the answer as 'unbroken chain of sacred transmission' and identifies the sacred tradition itself as a victim. The native_generational_reading treats the answer as 'children acquiring mother tongue' and identifies religious authority constraints as victims. The marketplace_pidgin_reading treats the answer as 'inter-communal functional coordination' and identifies speakers locked into single registers as victims. All three readings share the same referent (the Hebrew language's historical status) but instantiate different constraints because they measure linguistic life against different criteria.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(hebrew_linguistic_life__liturgical_preservation_reading, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
