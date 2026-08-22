% ============================================================================
% CONSTRAINT STORY: orthographic_kernel__rupture_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_orthographic_kernel__rupture_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: orthographic_kernel__rupture_reading
 *   human_readable: Latin Script Orthographic Rupture as State-Mandated Cultural Severing
 *   domain: political/cultural/commitment_system
 *
 * SUMMARY:
 *   In the early 20th century, a post-Ottoman state enacts a law mandating
 *   transition from Arabic script to Latin script. The law is framed as
 *   modernization and educational efficiency — enabling mass literacy and
 *   severing feudal hierarchies. Under the rupture reading instantiated here,
 *   the script change operates as a deliberate cultural extraction mechanism:
 *   it renders Ottoman literate populations' accumulated cultural capital
 *   worthless, severs Islamic textual authority, transfers interpretive
 *   monopoly to the state, and makes pre-reform cultural memory accessible
 *   only through state-approved translation. The beneficiaries are the
 *   post-reform state apparatus and secular nationalist intelligentsia; the
 *   victims are the entire pre-reform literate population whose identity,
 *   professional credentials, and cultural continuity are script-constituted.
 *   This is ONE reading of a contested kernel — the orthographic kernel —
 *   contested by the continuity_reading (script preserves Ottoman continuity)
 *   and the modernization_reading (script change enables scientific progress
 *   while preserving linguistic identity). This story instantiates the
 *   rupture reading exclusively: it does not hedge across readings or average
 *   over them.
 *
 * KEY AGENTS:
 *   - post_reform_state_apparatus: institutional power, agenda-setter — controls enforcement, education, printing monopoly
 *   - literate_ottoman_class: powerful payer, identity-locked — accumulated script-specific cultural capital rendered worthless
 *   - islamic_scholars_ulama: organized payer, identity-locked — religious authority fused with script; loss of monopoly on interpretation
 *   - secular_nationalist_intelligentsia: powerful beneficiary — cultural authority over modernity narrative, state favor
 *   - post_reform_generation_youth: powerless, observer-beneficiary — naturalize rupture as inevitable; isolated from pre-reform culture
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(orthographic_kernel__rupture_reading, 0.89).
domain_priors:suppression_score(orthographic_kernel__rupture_reading, 0.92).
domain_priors:theater_ratio(orthographic_kernel__rupture_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(orthographic_kernel__rupture_reading, extractiveness, 0.89).
narrative_ontology:constraint_metric(orthographic_kernel__rupture_reading, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(orthographic_kernel__rupture_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(orthographic_kernel__rupture_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(orthographic_kernel__rupture_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(orthographic_kernel__rupture_reading, tangled_rope).
narrative_ontology:human_readable(orthographic_kernel__rupture_reading, "Latin Script Orthographic Rupture as State-Mandated Cultural Severing").
narrative_ontology:topic_domain(orthographic_kernel__rupture_reading, "political/cultural/commitment_system").

domain_priors:requires_active_enforcement(orthographic_kernel__rupture_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(orthographic_kernel__rupture_reading, '1a5344c5-2ab4-4396-85e3-74f281de90c4').
narrative_ontology:cs_kernel_codification('1a5344c5-2ab4-4396-85e3-74f281de90c4', formalized).
narrative_ontology:cs_authority_grounding('1a5344c5-2ab4-4396-85e3-74f281de90c4', extraction).
narrative_ontology:cs_interpretation_layer_present('1a5344c5-2ab4-4396-85e3-74f281de90c4').
narrative_ontology:cs_reading_relation('1a5344c5-2ab4-4396-85e3-74f281de90c4', orthographic_kernel__continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('1a5344c5-2ab4-4396-85e3-74f281de90c4', orthographic_kernel__modernization_reading, influences).
narrative_ontology:cs_axiom('1a5344c5-2ab4-4396-85e3-74f281de90c4', foundational, script_change_is_deliberate_cultural_severing).
narrative_ontology:cs_axiom_status(script_change_is_deliberate_cultural_severing, holdable).
narrative_ontology:cs_axiom_grounding('1a5344c5-2ab4-4396-85e3-74f281de90c4', script_change_is_deliberate_cultural_severing, empirically_contingent).
narrative_ontology:cs_axiom('1a5344c5-2ab4-4396-85e3-74f281de90c4', foundational, ottoman_identity_script_constituted).
narrative_ontology:cs_axiom_status(ottoman_identity_script_constituted, holdable).
narrative_ontology:cs_axiom_grounding('1a5344c5-2ab4-4396-85e3-74f281de90c4', ottoman_identity_script_constituted, deontological).
narrative_ontology:cs_reference_frame('1a5344c5-2ab4-4396-85e3-74f281de90c4', ottoman_islamic_script_authority).
narrative_ontology:cs_drift_state('1a5344c5-2ab4-4396-85e3-74f281de90c4', post_reform_state_consolidation, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('1a5344c5-2ab4-4396-85e3-74f281de90c4', '').
narrative_ontology:cs_kernel_id(orthographic_kernel__rupture_reading, orthographic_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(orthographic_kernel__rupture_reading, post_reform_state_apparatus).
narrative_ontology:constraint_beneficiary(orthographic_kernel__rupture_reading, secular_nationalist_intelligentsia).
narrative_ontology:constraint_victim(orthographic_kernel__rupture_reading, literate_ottoman_class).
narrative_ontology:constraint_victim(orthographic_kernel__rupture_reading, islamic_scholars_ulama).
narrative_ontology:constraint_victim(orthographic_kernel__rupture_reading, traditional_scribal_guilds).
narrative_ontology:constraint_victim(orthographic_kernel__rupture_reading, ottoman_cultural_continuity_bearers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(orthographic_kernel__rupture_reading, post_reform_generation_youth).
narrative_ontology:constraint_beneficiary(orthographic_kernel__rupture_reading, printing_industry_modernizers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The state enforces the script law through education ministry, courts, public administration, printing monopoly, and credentialing systems. It controls what texts are produced in the new script, what Ottoman texts are translated, and what education the next generation receives. The state justifies the change as modernization and claims it severs feudal Ottoman hierarchies. It directly benefits by consolidating cultural authority — alternative sources of legitimacy (Islamic scholars, Ottoman continuity advocates) are structurally weakened when the script barrier rises.
narrative_ontology:constraint_stakeholder(orthographic_kernel__rupture_reading, post_reform_state_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% Ottoman officials, lawyers, merchants, and educated families lose the value of their script-based literacy. Their professional credentials become obsolete. Their private documents (property deeds, wills, correspondence) become unreadable to the next generation without state-mediated translation. Their identity as educated Ottomans is coded in script fluency; they cannot abandon Ottoman identity without accepting that their entire life of learning is now useless. The exit option identity_locked means they cannot migrate to the new script without existential identity loss.
narrative_ontology:constraint_stakeholder(orthographic_kernel__rupture_reading, literate_ottoman_class, payer,
    powerful, biographical, identity_locked, national).

% Islamic religious scholars lose their textual monopoly. The Quran, hadith, and classical jurisprudence in Arabic script become inaccessible to the general population unless the state produces approved translations. Religious authority, which flowed from direct engagement with sacred texts, now flows from state-approved mediation. The scholars lose the ability to transmit unmediated religious knowledge and cannot re-establish authority in a system where the state controls which texts are translated and how. Religious identity is inseparable from script; they cannot exit without abandoning religious legitimacy.
narrative_ontology:constraint_stakeholder(orthographic_kernel__rupture_reading, islamic_scholars_ulama, payer,
    organized, generational, identity_locked, national).

% Professional scribes, calligraphers, and document authenticators become economically obsolete overnight. Their craft-specific knowledge (beautiful Arabic script, document authentication, formal Ottoman administrative writing) is not transferable to Latin script. They cannot retrain and position themselves as authorities in a system where the state has already monopolized script certification through schools. The guild structure dissolves as the state defunds and delegitimizes traditional scribal practice.
narrative_ontology:constraint_stakeholder(orthographic_kernel__rupture_reading, traditional_scribal_guilds, payer,
    moderate, biographical, trapped, national).

% Citizens embedded in Ottoman cultural practice — people who transmit oral and written tradition, family histories, poetry, and cultural memory — face the constraint as cultural erasure. Their children cannot read family documents without state translation. Access to the past becomes mediated and controlled. They cannot resist because they lack institutional power; they can only watch cultural memory become illegible. Their identity is tied to cultural continuity, so accepting the new script means accepting that their children are severed from their ancestors.
narrative_ontology:constraint_stakeholder(orthographic_kernel__rupture_reading, ottoman_cultural_continuity_bearers, payer,
    powerless, biographical, identity_locked, national).

% Intellectuals and reformers who champion script change gain cultural authority by defining what is modern, rational, and progressive. They control discourse about the nation and shape education policy. They benefit by having their aesthetic preferences (Latin script as scientific, European, forward-looking) become law. They position themselves as interpreters of the rupture narrative and gain prestige as architects of the new nation. They can exit the constraint if they choose — they have alternatives — but they don't because the constraint serves their interests in cultural authority.
narrative_ontology:constraint_stakeholder(orthographic_kernel__rupture_reading, secular_nationalist_intelligentsia, beneficiary,
    powerful, generational, mobile, national).

% Children educated in the new script only have access to what the state chooses to produce and translate. They are beneficiaries in that they learn more efficiently (one script, standardized education) and don't face the burden of dual literacy. They are also restricted — they cannot access Ottoman culture without state intermediation and are naturalized into the rupture as inevitable. They experience it as normal that the past is illegible unless officially translated. Their constrained exit means they depend on the state for access to cultural memory.
narrative_ontology:constraint_stakeholder(orthographic_kernel__rupture_reading, post_reform_generation_youth, beneficiary,
    powerless, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(orthographic_kernel__rupture_reading, post_reform_generation_youth, observer).

% Neighboring Ottoman-tradition states and Islamic centers of scholarship that maintain Arabic script and Ottoman cultural continuity are excluded from the domestic conversation about script change. They are treated as irrelevant or hostile to national identity. Their existence as alternative cultural frameworks would constitute competing legitimacy sources — their exclusion is enforced by treating the script change as inevitable modernization, not as a deliberate choice among alternatives.
narrative_ontology:constraint_stakeholder(orthographic_kernel__rupture_reading, rival_ottoman_empires_cultural_authorities, excluded,
    institutional, generational, trapped, global).

% New printing enterprises and technology companies benefit from the script change by capturing the market for newspapers, textbooks, and state publications. They position themselves as agents of modernity and progress. The state apparatus favors them through subsidy, licensing, and monopoly protection while excluding traditional scribal and Ottoman-script production. They gain market access and prestige through the constraint.
narrative_ontology:constraint_stakeholder(orthographic_kernel__rupture_reading, printing_industry_modernizers, beneficiary,
    organized, biographical, mobile, national).

% Foreign diplomats, scholars, and military advisors observe the script change and validate it as modernization and Europeanization. They provide external legitimacy and pressure. They do not participate in enforcement but their analytical validation reinforces the state's framing. They perceive the script change as progress and liberation from backwardness, contributing to the narrative hegemony that makes resistance appear irrational.
narrative_ontology:constraint_stakeholder(orthographic_kernel__rupture_reading, international_observers_european_powers, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(orthographic_kernel__rupture_reading, post_reform_state_apparatus).
narrative_ontology:fixing_cost_class(orthographic_kernel__rupture_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordination of post-national, unified literacy system decoupled from Ottoman feudal administrative hierarchies. The state standardizes reading and writing in a single script, enabling mass public education and unified administration. This solves the coordination problem of replacing estate-based literacy (Ottoman Arabic-script hierarchies, where only the educated Ottoman class could read) with egalitarian national literacy (Latin-script public education for all). The coordination function is real: mass literacy is a genuine benefit; unified national language is a genuine coordination solution.
% TRANSFER_FUNCTION: Transfers cultural authority from the Ottoman literate class and Islamic scholars to the post-reform state apparatus. Transfers script-based professional credentials from the category of valuable experience to the category of obsolescence. Transfers control over what is readable, transmissible, and legitimate knowledge from distributed scribal practice and religious scholarship to state-controlled education and publishing monopoly. Transfers interpretive power over national identity from Ottoman-Islamic tradition to secular nationalist ideology.
% ABSENT_VOICES: Ottoman cultural continuity advocates who would argue for script preservation or parallel scripts; Islamic scholars who would defend textual tradition; Ottoman-tradition intellectuals who would contest the modernization narrative. Pre-reform voices are excluded from the state-controlled consensus-building about the script change. Their exclusion is enforced by treating the change as inevitable progress, making dissent appear as backwardness rather than legitimate alternative position. Rival Ottoman-tradition states and Islamic centers of scholarship that maintain Arabic script are also absent from the domestic conversation, treated as irrelevant to national identity.
% DISAPPEARANCE_RATIONALE: If the script law vanished overnight, Ottoman literacy would remain partially accessible without state translation. Family documents would be readable. Islamic scholarship would be available in original form. Professional scribal practice would revive. State authority over cultural interpretation would weaken. Alternative legitimacy sources (Ottoman continuity, Islamic tradition) would become cognitively available to the population without state mediation. The constraint operates as the mechanism by which one vision of national identity (secular, rupture-based, severed from Ottoman past) became inescapable and naturalized. Its removal would resurrect alternatives and redistribute authority.
% FOUNDING_PROBLEM: Ottoman administrative hierarchy is script-coded into Islamic and feudal power structures. The rupture reading frames the founding problem as: How do we sever Ottoman feudalism and create a genuinely post-national state? The answer under this reading is: Script change. By making the old script illegible, we break the institutional dependency on Ottoman-trained elites and Islamic scholarship, and we consolidate state authority over what counts as legitimate knowledge.
% FOUNDING_PROBLEM_CORROBORATION: Post-reform state narratives and secular nationalist intellectuals attest the founding problem — Ottoman feudalism and Islamic hierarchy — required script rupture to resolve. International observers and European powers validated this reading as modernization and liberation. However, Ottoman-tradition scholars and cultural continuity advocates — excluded from the consensus conversation — attest the founding problem was solvable through alternative means: transliteration, parallel scripts, gradual transition, or selective adoption. They maintain the founding problem was EXAGGERATED and used as cover for cultural extraction rather than structural necessity. No voice from outside the beneficiary class and state apparatus publicly corroborates the rupture reading in real time; corroboration comes only from post-hoc nationalist historiography.
narrative_ontology:disappearance_verdict(orthographic_kernel__rupture_reading, world_rearranges).
narrative_ontology:founding_problem_status(orthographic_kernel__rupture_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(orthographic_kernel__rupture_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(orthographic_kernel__rupture_reading, 'none', 1).
narrative_ontology:epsilon_provenance(orthographic_kernel__rupture_reading, 0.89, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(orthographic_kernel__rupture_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(orthographic_kernel__rupture_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(orthographic_kernel__rupture_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness reaches 0.89 by interval end because the constraint operates as wholesale cultural authority transfer: the beneficiary set (state apparatus, nationalist intelligentsia) captures not just economic rents but the power to define what is readable, transmissible, and legitimate. Suppression is very high (0.92) because the constraint's persistence depends on actively erasing alternatives — Ottoman texts must become illegible to the general population, scribal guilds must be displaced, and resistance from the literate class must be suppressed through law and education monopoly. Theater ratio remains low (0.18) because the constraint's function is genuinely extractive, not performative — the state has real incentive to maintain script rupture, the literacy consolidation is real, and the cultural authority capture is sustained by genuine institutional dependency, not theatrical maintenance. The measurement series shows extraction and suppression rising over the interval as the constraint hardens: initial implementation (high but contested) → institutional consolidation → normalization in the second generation → institutional irreversibility. Measurement time points share one grid; every metric is authored at every examined time point.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter (state apparatus) perceives the constraint as genuine coordination — solving the coordination problem of replacing feudal estate-based literacy with unified national literacy. The payers (literate Ottoman class, Islamic scholars) perceive the same structure as pure extraction — their identity and cultural authority are seized without compensation or choice. The engine computes this divergence from the structural data (beneficiary vs. victim declarations, power atoms, exit options, directional asymmetry). The post-reform state reads the constraint as liberation; the Ottoman literate reads it as cultural destruction. Both readings are authored into the stakeholder surface; the engine derives per-seat classification from the asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   The post_reform_state_apparatus is the structural beneficiary: captures authority, controls what is readable, monopolizes education, collects the symbolic capital of defining modernity. Directionality near 0.0 (full beneficiary from the constraint). The literate_ottoman_class bears costs: loses professional credibility, cannot transmit cultural knowledge, faces forced re-education or marginalization. Identity-locked exit means they cannot migrate out of the constraint — Ottoman identity and script are fused; escaping the constraint would require abandoning cultural continuity. Directionality near 1.0 (full target). Islamic scholars are similarly identity-locked: religious authority is script-constituted and cannot be re-based without loss of legitimacy. The state can override the natural directionality derivation for some seats if domain knowledge suggests the automatic derivation misses structural reality — but here the derivation tracks structural reality correctly. High suppression is earned: state must actively prevent alternative script systems, prosecute Ottoman-script use in courts, refuse to credential Ottoman education, and monopolize printing to maintain the rupture.
 *
 * MANDATROPHY ANALYSIS:
 *   This is NOT a piton (theatrical maintenance of a dead function). The state apparatus has real, sustained interest in maintaining script rupture because it consolidates state authority over cultural legitimacy and prevents the resurgence of competing (Ottoman, Islamic) sources of authority. The constraint remains extractive and enforced, not a zombie. The founding problem — Ottoman feudal hierarchy — is genuinely addressed (though contestably: alternatives like gradual transliteration or parallel scripts could have solved it without rupture). The constraint is a tangled rope: it solves a real coordination problem (mass public literacy standardization) AND extracts asymmetric cultural authority. Both are structural, both are sustained by enforcement. The constraint does not decay into piton because beneficiaries (state, nationalist intelligentsia) actively maintain enforcement and have institutional capacity to do so.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    rupture_vs_necessary_modernization,
    'Was script change structurally necessary for technological modernization and mass literacy, or was it a deliberate choice among viable alternatives (gradual transliteration, parallel scripts, selective adoption)?',
    'Comparative historical analysis: script transitions in other post-imperial states (India, Indonesia) that achieved mass literacy without rupture; technical analysis of printing and typesetting feasibility for Arabic script in early 20th-century technology; testimony from Ottoman reformers about alternative approaches they rejected.',
    'If necessary: the constraint is more defensible as coordination with unavoidable extraction cost. If choice: the rupture reading is vindicated — script change was selected for cultural authority transfer, not technological requirement. If mixed (necessary for mass literacy but unnecessary for the extent of Ottoman erasure): the constraint decomposes into coordination and extraction layers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rupture_vs_necessary_modernization, empirical, 'Whether script change was technologically necessary or a deliberate policy choice among alternatives.').

omega_variable(
    identity_lock_mechanism_internalized_vs_structural,
    'Is the measured suppression imposed externally (legal barriers, economic dependency) or internalized (the literate class believing they deserve displacement, accepting the narrative that Ottoman literacy is backward)?',
    'Post-enforcement trajectory study: if suppression persists after script monopoly is relaxed (e.g., late-century renewed interest in Ottoman texts), the suppression is partially internalized — targets carry the constraint''s framing after the barrier dissolves. If suppression collapses when barriers fall, it was primarily structural.',
    'If internalized: the constraint''s effective suppression is higher than the structural measure suggests — targets carry cultural shame beyond material barriers. The constraint''s persistence becomes dependent on continued narrative hegemony, not just enforcement. If structural: enforcement can maintain the constraint even without internalized shame — harder to reverse once enforcement drops.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_mechanism_internalized_vs_structural, empirical, 'Structural vs. internalized mechanism of script-rupture suppression.').

omega_variable(
    reading_under_determination_coordinate_vs_narrative_frame,
    'Does the rupture reading''s classification depend on whether we treat the constraint as the SCRIPT CHANGE ITSELF (a linguistic choice) or the NARRATIVE FRAME around it (the deliberate-rupture story)?',
    'Counterfactual framing: if the same script change were authorized with a continuity narrative (Latin script preserving Turkish-as-inheritor-of-Ottoman), would the constraint''s extractiveness change? If yes, the reading is partly frame-dependent; if no, rupture is structurally inherent to script change, not narrative.',
    'If frame-dependent: a sibling continuity_reading of the same script law could produce lower ε and different beneficiary structure. If structurally inherent: the script change IS extractive regardless of narrative framing — rupture is the material fact, not a reading choice. This determines whether the constraint family decomposes into genuine alternative readings (different ε, different beneficiaries) or into observer-relative framings of the same constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_under_determination_coordinate_vs_narrative_frame, conceptual, 'Whether rupture reading depends on historical narrative frame or is structurally inherent to script change.').

omega_variable(
    beneficiary_capture_state_vs_intelligentsia,
    'Who actually captures the gains from script rupture: the state apparatus as institution, or the secular nationalist intelligentsia as a class that gains cultural authority?',
    'Institutional analysis of where script-rupture rents accumulate: do state budgets show revenue concentration, or do intellectual/cultural prestige gains accrue to nationalist intelligentsia while state bears enforcement costs?',
    'If state captures: the constraint is straightforward institutional extraction. If intelligentsia captures: the constraint is class-based cultural extraction using state machinery — the beneficiary is the intellectual class, not the bureaucratic institution. This affects gain_flow assignment and piton-vs-snare diagnosis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_capture_state_vs_intelligentsia, empirical, 'Distribution of gains between state apparatus and intellectual beneficiaries.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(orthographic_kernel__rupture_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(orth_tr_t0, orthographic_kernel__rupture_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(orth_tr_t5, orthographic_kernel__rupture_reading, theater_ratio, 5, 0.1).
narrative_ontology:measurement(orth_tr_t10, orthographic_kernel__rupture_reading, theater_ratio, 10, 0.12).
narrative_ontology:measurement(orth_tr_t15, orthographic_kernel__rupture_reading, theater_ratio, 15, 0.14).
narrative_ontology:measurement(orth_tr_t25, orthographic_kernel__rupture_reading, theater_ratio, 25, 0.16).
narrative_ontology:measurement(orth_tr_t50, orthographic_kernel__rupture_reading, theater_ratio, 50, 0.18).

% Extraction over time
narrative_ontology:measurement(orth_be_t0, orthographic_kernel__rupture_reading, base_extractiveness, 0, 0.72).
narrative_ontology:measurement(orth_be_t5, orthographic_kernel__rupture_reading, base_extractiveness, 5, 0.76).
narrative_ontology:measurement(orth_be_t10, orthographic_kernel__rupture_reading, base_extractiveness, 10, 0.81).
narrative_ontology:measurement(orth_be_t15, orthographic_kernel__rupture_reading, base_extractiveness, 15, 0.85).
narrative_ontology:measurement(orth_be_t25, orthographic_kernel__rupture_reading, base_extractiveness, 25, 0.88).
narrative_ontology:measurement(orth_be_t50, orthographic_kernel__rupture_reading, base_extractiveness, 50, 0.89).

% Suppression requirement over time
narrative_ontology:measurement(orth_su_t0, orthographic_kernel__rupture_reading, suppression_requirement, 0, 0.68).
narrative_ontology:measurement(orth_su_t5, orthographic_kernel__rupture_reading, suppression_requirement, 5, 0.74).
narrative_ontology:measurement(orth_su_t10, orthographic_kernel__rupture_reading, suppression_requirement, 10, 0.81).
narrative_ontology:measurement(orth_su_t15, orthographic_kernel__rupture_reading, suppression_requirement, 15, 0.86).
narrative_ontology:measurement(orth_su_t25, orthographic_kernel__rupture_reading, suppression_requirement, 25, 0.9).
narrative_ontology:measurement(orth_su_t50, orthographic_kernel__rupture_reading, suppression_requirement, 50, 0.92).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(orthographic_kernel__rupture_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(orthographic_kernel__rupture_reading, 0.12).
narrative_ontology:affects_constraint(orthographic_kernel__rupture_reading, orthographic_kernel__continuity_reading).
narrative_ontology:affects_constraint(orthographic_kernel__rupture_reading, orthographic_kernel__modernization_reading).

% DUAL FORMULATION NOTE:
% The orthographic_kernel decomposes into three structurally distinct constraint readings. This story (rupture_reading) treats script change as deliberate cultural extraction and severing from Ottoman-Islamic identity. The continuity_reading treats the same script law as preserving Ottoman cultural continuity through translation and adaptation. The modernization_reading treats script change as enabling technological progress independent of cultural rupture. All three share the SAME KERNEL (the orthographic law) but instantiate different ε values, different beneficiary/victim structures, and different persistent mechanisms because they are READ DIFFERENTLY by the state apparatus (rupture frame), Ottoman continuity advocates (continuity frame), and technology modernizers (modernization frame). Each reading is a separate constraint story with its own metrics and stakeholders; this story is the rupture reading exclusively.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(orthographic_kernel__rupture_reading, organized, 0.94).
constraint_indexing:directionality_override(orthographic_kernel__rupture_reading, powerful, 0.91).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
