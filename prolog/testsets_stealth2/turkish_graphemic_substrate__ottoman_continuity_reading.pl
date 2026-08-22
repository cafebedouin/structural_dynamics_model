% ============================================================================
% CONSTRAINT STORY: turkish_graphemic_substrate__ottoman_continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_turkish_graphemic_substrate__ottoman_continuity_reading, []).

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
 *   constraint_id: turkish_graphemic_substrate__ottoman_continuity_reading
 *   human_readable: Ottoman-Continuity Reading: Arabic Script as the Legitimate Graphemic Substrate of Turkish
 *   domain: political linguistics / state formation / cultural engineering
 *
 * SUMMARY:
 *   For roughly six centuries Ottoman Turkish was written in a modified
 *   Perso-Arabic script, taught through medrese and household channels,
 *   administered by a licensed scribal class, and guarded by print controls
 *   that kept rival orthographies marginal. This story models that
 *   arrangement as instantiated by the ottoman_continuity_reading of the
 *   turkish_graphemic_substrate kernel: the claim that Turkish linguistic
 *   identity is continuous with Ottoman-Islamic civilization and that the
 *   Arabic script is therefore the legitimate graphemic substrate. The
 *   interval runs from the Hamidian enforcement peak (1898) through the 1908
 *   press liberalization, the Committee of Union and Progress retightening,
 *   the armistice collapse of enforcement capacity, the Republic's
 *   indifference, and the 1928 abolition. Claim/metric independence is
 *   load-bearing here: the reading itself presents the script as a
 *   civilizational given — a false-summit flavor, naturality asserted for
 *   what is in fact a maintained institution — while the authored metrics
 *   describe a structure with genuine coordination function (chancery
 *   standard, corpus access, devotional literacy) AND asymmetric gatekeeping
 *   extraction sustained by enforcement. My claimed_type is tangled_rope, my
 *   structural judgment; the reading-indexed epsilon is low because the
 *   reading assesses its own endorsed arrangement as legitimate
 *   custodianship, not extraction. That divergence between the reading's
 *   self-assessment and the structural profile is exactly the datum the
 *   corpus exists to take.
 *
 * KEY AGENTS:
 *   - - ulema_establishment: agenda-setting administrator (institutional/identity_locked) — runs the schools, courts, and licensing through which the script reproduces itself
 *   - - ottoman_palace_establishment: dynastic beneficiary (institutional/identity_locked) — caliphal legitimacy rides visibly on the revealed script
 *   - - scribal_bureaucracy: concentrated beneficiary (organized/constrained) — career capital is command of the court hand
 *   - - turkish_peasantry: diffuse target (powerless/trapped) — excluded from literacy by script difficulty and school scarcity
 *   - - ottoman_women: diffuse target (powerless/trapped) — outside the medrese track, dependent on male intermediaries for the written word
 *   - - modernizing_officer_civil_servants: organized target with arbitrage exit (organized/arbitrage) — routes around the script via French and parallel institutions; ultimately seizes the state and abolishes the arrangement
 *   - - minority_script_publishers: excluded voice (moderate/arbitrage) — serves unserved readers in parallel alphabets outside the recognized press
 *   - - european_diplomatic_observers: analytical observer (institutional/analytical) — compiles the comparison cases the reformers cite
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(turkish_graphemic_substrate__ottoman_continuity_reading, 0.28).
domain_priors:suppression_score(turkish_graphemic_substrate__ottoman_continuity_reading, 0.45).
domain_priors:theater_ratio(turkish_graphemic_substrate__ottoman_continuity_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(turkish_graphemic_substrate__ottoman_continuity_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(turkish_graphemic_substrate__ottoman_continuity_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(turkish_graphemic_substrate__ottoman_continuity_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(turkish_graphemic_substrate__ottoman_continuity_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(turkish_graphemic_substrate__ottoman_continuity_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(turkish_graphemic_substrate__ottoman_continuity_reading, tangled_rope).
narrative_ontology:human_readable(turkish_graphemic_substrate__ottoman_continuity_reading, "Ottoman-Continuity Reading: Arabic Script as the Legitimate Graphemic Substrate of Turkish").
narrative_ontology:topic_domain(turkish_graphemic_substrate__ottoman_continuity_reading, "political linguistics / state formation / cultural engineering").

domain_priors:requires_active_enforcement(turkish_graphemic_substrate__ottoman_continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(turkish_graphemic_substrate__ottoman_continuity_reading, 'c7d0bcf7-6613-4d51-8374-b31fa6e1b8d0').
narrative_ontology:cs_kernel_codification('c7d0bcf7-6613-4d51-8374-b31fa6e1b8d0', fixed_text).
narrative_ontology:cs_authority_grounding('c7d0bcf7-6613-4d51-8374-b31fa6e1b8d0', lineage).
narrative_ontology:cs_interpretation_layer_present('c7d0bcf7-6613-4d51-8374-b31fa6e1b8d0').
narrative_ontology:cs_reading_relation('c7d0bcf7-6613-4d51-8374-b31fa6e1b8d0', turkish_graphemic_substrate__secular_nationalist_reading, forecloses).
narrative_ontology:cs_reading_relation('c7d0bcf7-6613-4d51-8374-b31fa6e1b8d0', turkish_graphemic_substrate__gradual_transition_reading, coexists_with).
narrative_ontology:cs_axiom('c7d0bcf7-6613-4d51-8374-b31fa6e1b8d0', foundational, civilizational_fidelity_to_inherited_script).
narrative_ontology:cs_axiom_status(civilizational_fidelity_to_inherited_script, holdable).
narrative_ontology:cs_axiom_grounding('c7d0bcf7-6613-4d51-8374-b31fa6e1b8d0', civilizational_fidelity_to_inherited_script, deontological).
narrative_ontology:cs_axiom('c7d0bcf7-6613-4d51-8374-b31fa6e1b8d0', foundational, arabic_script_quranic_inseparability).
narrative_ontology:cs_axiom_status(arabic_script_quranic_inseparability, holdable).
narrative_ontology:cs_axiom_grounding('c7d0bcf7-6613-4d51-8374-b31fa6e1b8d0', arabic_script_quranic_inseparability, theological).
narrative_ontology:cs_reference_frame('c7d0bcf7-6613-4d51-8374-b31fa6e1b8d0', caliphal_orthographic_unity).
narrative_ontology:cs_drift_state('c7d0bcf7-6613-4d51-8374-b31fa6e1b8d0', republican_abolition_1928, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('c7d0bcf7-6613-4d51-8374-b31fa6e1b8d0', '').
narrative_ontology:cs_kernel_id(turkish_graphemic_substrate__ottoman_continuity_reading, turkish_graphemic_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(turkish_graphemic_substrate__ottoman_continuity_reading, ulema_establishment).
narrative_ontology:constraint_beneficiary(turkish_graphemic_substrate__ottoman_continuity_reading, scribal_bureaucracy).
narrative_ontology:constraint_beneficiary(turkish_graphemic_substrate__ottoman_continuity_reading, ottoman_palace_establishment).
narrative_ontology:constraint_victim(turkish_graphemic_substrate__ottoman_continuity_reading, turkish_peasantry).
narrative_ontology:constraint_victim(turkish_graphemic_substrate__ottoman_continuity_reading, ottoman_women).
narrative_ontology:constraint_victim(turkish_graphemic_substrate__ottoman_continuity_reading, modernizing_officer_civil_servants).
narrative_ontology:constraint_vindicates(turkish_graphemic_substrate__ottoman_continuity_reading, caliphal_guardianship_doctrine).
narrative_ontology:constraint_vindicates(turkish_graphemic_substrate__ottoman_continuity_reading, quranic_script_inviolability).
narrative_ontology:constraint_vindicates(turkish_graphemic_substrate__ottoman_continuity_reading, ottoman_civilizational_continuity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Staff the medrese schools, courts, and mosques where reading and writing are taught; license teachers, certify calligraphers through master-to-apprentice chains reaching back generations, and rule on what may be printed. Their standing, income, and daily practice are constituted by the lettered tradition they administer; setting it aside would dissolve the very authority doing the setting-aside. After 1924 the state closes their schools and their administrative grip unravels within four years.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__ottoman_continuity_reading, ulema_establishment, agenda_setter,
    institutional, generational, identity_locked, continental).

% The dynasty and its household claim the caliphate and anchor legitimacy in guardianship of Islamic learning; the script of revelation makes that bond visible on every document of state. Adopting a European alphabet would read as surrendering the caliphal inheritance, so the palace resists orthographic proposals however practical, and loses the protective state that upheld its claim in 1922.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__ottoman_continuity_reading, ottoman_palace_establishment, beneficiary,
    institutional, generational, identity_locked, global).

% Chancery clerks who draft, copy, and file the empire's paperwork in the court hand. Command of the script's conventions is their career capital, passed through household and apprenticeship networks; position and promotion flow through it. Retraining into a different written medium would strand that capital, so they defend the existing conventions in every council debate on reform.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__ottoman_continuity_reading, scribal_bureaucracy, beneficiary,
    organized, biographical, constrained, continental).

% Farm and herd across Anatolia and the Balkans with little schooling within reach; the letters they would need for reading do not mark the vowels they speak, so lessons take years and qualified teachers are scarce. Most remain outside the written economy, signing by mark or thumbprint, and no alternative channel to literacy in their own tongue exists within reach.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__ottoman_continuity_reading, turkish_peasantry, payer,
    powerless, immediate, trapped, regional).

% Largely outside the medrese track that trains male readers; taught at home where at all, often in recitation without systematic writing instruction. Household correspondence and market dealings run through male kin or hired letter-writers. Opting out of the arrangement would mean leaving the household and community structures that organize their lives.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__ottoman_continuity_reading, ottoman_women, payer,
    powerless, biographical, trapped, local).

% Graduates of the military and civil-service academies, where French is the language of technical instruction and European books the source of professional knowledge. They acquire what the traditional curriculum does not teach by learning European languages alongside the court script, staffing parallel institutions — engineering schools, medical faculties, the general staff — that increasingly set the terms of statecraft. By the 1920s they hold the army and the ministries and redirect cultural policy against the established letter.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__ottoman_continuity_reading, modernizing_officer_civil_servants, payer,
    organized, biographical, arbitrage, continental).

% Armenian and Greek printers who publish Turkish-language newspapers and books in their communities' own alphabets, serving readers the recognized channels do not reach. They operate outside the capital's licensed press, tolerated unevenly, and hold no seat in the councils where orthographic policy is argued; their parallel editions are themselves an argument those councils never hear.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__ottoman_continuity_reading, minority_script_publishers, excluded,
    moderate, biographical, arbitrage, regional).

% Embassy staff and scholars reporting on the empire's modernization debates; they compile literacy figures, translate reform proposals, and advise their governments on the stability of the Ottoman order. They take no part in domestic argument, but their dispatches circulate the comparison cases — Japan's script decisions, Egypt's press debates — that reformers cite at home.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__ottoman_continuity_reading, european_diplomatic_observers, observer,
    institutional, immediate, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(turkish_graphemic_substrate__ottoman_continuity_reading, ulema_establishment).
narrative_ontology:fixing_cost_class(turkish_graphemic_substrate__ottoman_continuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: One script served as the shared encoding for a multilingual empire's administration, law, commerce, and religious life, and attached Turkish readers to the Arabic-Persian scholarly corpus without translation.
% TRANSFER_FUNCTION: Moved literacy access and textual authority from the general population to the credentialed religious and scribal classes who controlled instruction and certification; oriented cultural traffic toward the Islamic ecumene; placed the recurring labor of acquiring a difficult orthography on every learner, with the returns accruing to the gatekeepers.
% ABSENT_VOICES: The unschooled peasantry and women who bore the literacy exclusion had no seat in any council; minority-script printers argued only through parallel editions no council read; provincial petitions from village notables were filtered through channels the establishment staffed. They were in the fields, the households, and the minority neighborhoods outside the capital's policy circles.
% DISAPPEARANCE_RATIONALE: When the arrangement was abolished in 1928 everything it organized rearranged within a decade: mass literacy campaigns in the new alphabet, a generation cut off from the Ottoman corpus except through transliteration projects, religious education rebuilt under a state directorate, and the reading public reoriented from Mecca and Cairo toward Paris and Berlin. Nothing of the prior settlement survived intact.
% FOUNDING_PROBLEM: The early Ottoman state needed a single chancery standard for a multilingual realm, a visible bond between dynasty and caliphate, and admission for Turkish to the prestige economy of Islamic letters, which Arabic and Persian dominated.
% FOUNDING_PROBLEM_CORROBORATION: The administrative half is attested from outside the benefiting parties by Tanzimat-era reform memoranda, European diplomatic reporting, and the speed of bureaucratic conversion after 1928 once the standard changed. The devotional half — Quranic literacy as a continuing communal need — is attested by religious historians and by Qur'an-course enrollment in the republic and diaspora, sources outside the defunct scribal class. The ulema's own attestations that the founding problem remains live are self-interested and discounted accordingly; no fully disinterested corroborator exists for the claim that the original problem is still unsolved, and that absence is itself signal.
narrative_ontology:disappearance_verdict(turkish_graphemic_substrate__ottoman_continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(turkish_graphemic_substrate__ottoman_continuity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(turkish_graphemic_substrate__ottoman_continuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(turkish_graphemic_substrate__ottoman_continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(turkish_graphemic_substrate__ottoman_continuity_reading, 0.28, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(turkish_graphemic_substrate__ottoman_continuity_reading_tests).
:- end_tests(turkish_graphemic_substrate__ottoman_continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon is reading-indexed over a fixed referent: the standing Arabic-script arrangement, assessed by the ottoman-continuity reading's own lights. From inside that reading the arrangement is custodianship of revelation and corpus, so base extractiveness is low (0.28 at interval end) with only a gentle rise as even sympathetic observers registered the chancery class ossifying into sinecure. Suppression (0.45) is a raw structural property, unscaled: print licensing, censorship peaks under Abdulhamid, exclusion of rival-script Turkish publication, and educational channeling were real, but much of the arrangement's persistence was habit and devotion rather than policing. Accessibility_collapse is moderate-low (0.38) because alternatives never vanished — Latin-adaptation proposals circulated from the 1860s, and Armeno-Turkish and Greco-Turkish presses operated visibly — so understanding the arrangement did not close the exits, it priced them. Resistance (0.62) reflects a sixty-year modernizer campaign that ended in total victory. Theater_ratio (0.45 at endpoint) is the honest terminal figure: after the 1924 closure of the medreses the arrangement's defense was largely parliamentary rhetoric and protest, performative maintenance of a function the state had stopped funding — a piton-flavored end-state that does not retroactively define the whole-life constraint, hence the tangled_rope claim stands on the full-interval structure. The measurement series share one six-point grid (1898, 1908, 1913, 1918, 1923, 1928) with every tracked metric authored at every point. The suppression_requirement series deliberately traces enforcement-capacity dynamics — Hamidian peak, 1908 liberalization, CUP ratchet attempt, armistice collapse, Republican indifference, abolition — a rise-fall arc distinct from the scalar, which profiles whole-life structural coercion rather than the endpoint of the enforcement machinery.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute divergent types from identical structure. From the ulema seat the arrangement is coordination it personally staffs, certifies, and transmits — a rope-like lived experience anchored by identity_lock: abandoning the script dissolves the authority that abandoning it would require. From the peasant and women's seats the same structure operates as a wall around literacy with no alternative channel — a snare-like lived experience with trapped exit. The officer-civil-servant seat experiences it as friction routable through arbitrage: French education, parallel technical institutions, minority-script reading — which damps its effective extraction and funds the opposition that finally abolished the arrangement. The engine computes these per-seat classifications from the authored power/exit data; the divergence is the finding, not a defect to be reconciled.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations anchor the administrator seats near the beneficiary end: the ulema (agenda_setter, identity_locked) and the palace (beneficiary, identity_locked) sit nearest d=0, with the scribal_bureaucracy (beneficiary, constrained exit) close behind — identity lock here stabilizes beneficiaries rather than targets, holding them at their subsidized position. Victim declarations push the trapped targets toward the full-target end: turkish_peasantry and ottoman_women (powerless, trapped, no alternative literacy channel) approach d=1, and the engine amplifies their effective extraction accordingly. The modernizing_officer_civil_servants are declared victims but carry arbitrage-grade exit, placing them meaningfully below the trapped targets despite bearing comparable formal costs — the clearest case in this story where exit options, not power, differentiate directionality among similarly educated actors. Minority_script_publishers bear circumvention costs from outside the conversation; european_diplomatic_observers hold the analytical seat at symmetry. Gain receipt is not diffuse: instructional fees, waqf-funded positions, certification authority, and script-contingent judicial and chancery posts demonstrably accrued to the ulema_establishment, which is why gain_flow names that seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — a single chancery standard for a multilingual empire, a visible dynastic-caliphal bond, admission of Turkish to the prestige economy of Islamic letters — died with the empire itself in 1922-23; what persisted to 1928 was defense of the devotional residue. Authoring founding_problem_status as contested (rather than dead) blocks the dead-plus-world_rearranges zombie flag honestly: the Quranic-literacy function the arrangement served has genuine living claimants, so obsolescence is disputed between seats rather than settled. The tangled_rope classification prevents the two symmetrical mislabels: calling the whole arrangement a snare erases the real coordination (one script for a multilingual administration, untranslated access to the Arabic-Persian corpus, standardized legal documentation); calling it a rope erases the gatekeeping extraction (a script that marks three of eight vowels, taught through channels that excluded most of the population, sustaining a credentialed intermediary class). The terminal theater_ratio rise is recorded as a symptom of atrophy without reclassifying the whole-life constraint as piton — theatricality arrived only after the enforcement substrate was dismantled.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This story instantiates one reading — ottoman_continuity_reading — of the kernel turkish_graphemic_substrate. The sibling secular_nationalist_reading would relocate the victim set to the generation severed from its corpus by Latin imposition and author high epsilon for the same decades as rupture; the sibling gradual_transition_reading would split benefits and costs across a dual-script window. Where exactly does the disagreement bite?',
    'Classify all three readings of the kernel against the same historical record and locate which structural element (the identity-continuity premise, the corpus-access accounting, or transition feasibility) drives divergent verdicts across the family.',
    'If the continuity premise is rejected, this reading''s low epsilon over the Arabic-script arrangement flips to the sibling''s high-epsilon account of the identical referent; the 1928 episode classifies as liberation under one reading and as expropriation under another.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: one reading of a three-reading kernel; sibling readings restructure beneficiaries, victims, and epsilon over the same referent.').

omega_variable(
    literacy_cost_attribution,
    'How much of the era''s low literacy is attributable to the script''s phonological mismatch with Turkish (eight vowels, three marked), versus school scarcity, poverty, and war disruption?',
    'Comparative pedagogy studies and the post-1928 literacy-campaign record controlling for state investment levels; adult-education trials teaching equivalent content in both orthographies.',
    'If script-intrinsic, the arrangement taxed every learner it coordinated and the coordination-function credit shrinks materially; if environmental, the gatekeeping case weakens and the arrangement sits closer to coordination burdened by external obstacles.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(literacy_cost_attribution, empirical, 'Attribution of literacy costs between script design and environment.').

omega_variable(
    enforcement_vs_habitual_reproduction,
    'Was the arrangement''s persistence maintained by active coercion (print licensing, censorship, exclusion of rival scripts) or by habitual religious reproduction that would have continued without enforcement?',
    'Examine the press-liberalization window after 1908: did rival-script Turkish publishing surge when controls lifted, and did the establishment seek reimposition of controls?',
    'If habit-dominated, the suppression metric overstates coercion and the arrangement trends toward rope; if enforcement-dependent, removing enforcement collapses it — as 1928 in fact demonstrated — supporting the tangled-rope reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_vs_habitual_reproduction, empirical, 'Coercion versus habit as the persistence mechanism.').

omega_variable(
    parallel_script_demand_signal,
    'Do the Armeno-Turkish and Greco-Turkish presses evidence broad suppressed demand for a phonetic Turkish orthography, or a narrow sectarian accommodation?',
    'Circulation figures and reader demographics of the parallel-script presses compared against Arabic-script provincial newspapers.',
    'A strong demand signal raises accessibility_collapse (alternatives were viable and known) and strengthens the exclusion case; a weak signal supports treating the script''s difficulties as manageable within the tradition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(parallel_script_demand_signal, empirical, 'Size of the latent reading public the parallel scripts served.').

omega_variable(
    transition_feasibility_counterfactual,
    'Could a managed dual-script transition of five to fifteen years have preserved corpus access while enabling mass literacy, or was the choice effectively binary?',
    'Comparative cases — Soviet latinization in Central Asia, Serbo-Croatian digraphia, Japanese mixed-script maintenance — plus cost modeling of bilingual-window administration and schooling.',
    'If transition was feasible, the continuity reading''s core benefit (corpus continuity) was obtainable without its exclusions and its veto loses warrant; if infeasible, the reading''s all-or-nothing stance reflects a real trade-off rather than gatekeeping.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(transition_feasibility_counterfactual, conceptual, 'Feasibility of the sibling gradual-transition path.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(turkish_graphemic_substrate__ottoman_continuity_reading, 1898, 1928).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(turk_tr_t1898, turkish_graphemic_substrate__ottoman_continuity_reading, theater_ratio, 1898, 0.1).
narrative_ontology:measurement_basis(turk_tr_t1898, observed).
narrative_ontology:measurement(turk_tr_t1908, turkish_graphemic_substrate__ottoman_continuity_reading, theater_ratio, 1908, 0.11).
narrative_ontology:measurement_basis(turk_tr_t1908, observed).
narrative_ontology:measurement(turk_tr_t1913, turkish_graphemic_substrate__ottoman_continuity_reading, theater_ratio, 1913, 0.13).
narrative_ontology:measurement_basis(turk_tr_t1913, observed).
narrative_ontology:measurement(turk_tr_t1918, turkish_graphemic_substrate__ottoman_continuity_reading, theater_ratio, 1918, 0.17).
narrative_ontology:measurement_basis(turk_tr_t1918, observed).
narrative_ontology:measurement(turk_tr_t1923, turkish_graphemic_substrate__ottoman_continuity_reading, theater_ratio, 1923, 0.29).
narrative_ontology:measurement_basis(turk_tr_t1923, observed).
narrative_ontology:measurement(turk_tr_t1928, turkish_graphemic_substrate__ottoman_continuity_reading, theater_ratio, 1928, 0.45).
narrative_ontology:measurement_basis(turk_tr_t1928, observed).

% Extraction over time
narrative_ontology:measurement(turk_be_t1898, turkish_graphemic_substrate__ottoman_continuity_reading, base_extractiveness, 1898, 0.16).
narrative_ontology:measurement_basis(turk_be_t1898, observed).
narrative_ontology:measurement(turk_be_t1908, turkish_graphemic_substrate__ottoman_continuity_reading, base_extractiveness, 1908, 0.18).
narrative_ontology:measurement_basis(turk_be_t1908, observed).
narrative_ontology:measurement(turk_be_t1913, turkish_graphemic_substrate__ottoman_continuity_reading, base_extractiveness, 1913, 0.2).
narrative_ontology:measurement_basis(turk_be_t1913, observed).
narrative_ontology:measurement(turk_be_t1918, turkish_graphemic_substrate__ottoman_continuity_reading, base_extractiveness, 1918, 0.21).
narrative_ontology:measurement_basis(turk_be_t1918, observed).
narrative_ontology:measurement(turk_be_t1923, turkish_graphemic_substrate__ottoman_continuity_reading, base_extractiveness, 1923, 0.24).
narrative_ontology:measurement_basis(turk_be_t1923, observed).
narrative_ontology:measurement(turk_be_t1928, turkish_graphemic_substrate__ottoman_continuity_reading, base_extractiveness, 1928, 0.28).
narrative_ontology:measurement_basis(turk_be_t1928, observed).

% Suppression requirement over time
narrative_ontology:measurement(turk_su_t1898, turkish_graphemic_substrate__ottoman_continuity_reading, suppression_requirement, 1898, 0.58).
narrative_ontology:measurement_basis(turk_su_t1898, observed).
narrative_ontology:measurement(turk_su_t1908, turkish_graphemic_substrate__ottoman_continuity_reading, suppression_requirement, 1908, 0.36).
narrative_ontology:measurement_basis(turk_su_t1908, observed).
narrative_ontology:measurement(turk_su_t1913, turkish_graphemic_substrate__ottoman_continuity_reading, suppression_requirement, 1913, 0.47).
narrative_ontology:measurement_basis(turk_su_t1913, observed).
narrative_ontology:measurement(turk_su_t1918, turkish_graphemic_substrate__ottoman_continuity_reading, suppression_requirement, 1918, 0.33).
narrative_ontology:measurement_basis(turk_su_t1918, observed).
narrative_ontology:measurement(turk_su_t1923, turkish_graphemic_substrate__ottoman_continuity_reading, suppression_requirement, 1923, 0.2).
narrative_ontology:measurement_basis(turk_su_t1923, observed).
narrative_ontology:measurement(turk_su_t1928, turkish_graphemic_substrate__ottoman_continuity_reading, suppression_requirement, 1928, 0.12).
narrative_ontology:measurement_basis(turk_su_t1928, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(turkish_graphemic_substrate__ottoman_continuity_reading, information_standard).
narrative_ontology:affects_constraint(turkish_graphemic_substrate__ottoman_continuity_reading, turkish_graphemic_substrate__secular_nationalist_reading).
narrative_ontology:affects_constraint(turkish_graphemic_substrate__ottoman_continuity_reading, turkish_graphemic_substrate__gradual_transition_reading).

% DUAL FORMULATION NOTE:
% Constraint family decomposition of the colloquial label 'the Turkish alphabet question.' The label conflates three structurally distinct arrangements: terminal Arabic-script legitimacy (this story), terminal Latin-script legitimacy (secular_nationalist_reading), and a managed dual-script transition window (gradual_transition_reading). Each carries its own epsilon, beneficiary/victim structure, and enforcement profile; forcing them into one story would make epsilon observer-dependent in violation of the invariance principle. Family edges run from this story to both siblings: the continuity reading was the upstream standing arrangement whose overthrow defined the nationalist sibling's founding act, and whose veto pressure shaped the gradualist sibling's compromise space.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
