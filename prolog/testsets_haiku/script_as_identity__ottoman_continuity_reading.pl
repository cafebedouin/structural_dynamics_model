% ============================================================================
% CONSTRAINT STORY: script_as_identity__ottoman_continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_script_as_identity__ottoman_continuity_reading, []).

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
 *   constraint_id: script_as_identity__ottoman_continuity_reading
 *   human_readable: Arabic Script as Identity Anchor (Ottoman Continuity Reading)
 *   domain: linguistic_identity/political_authority
 *
 * SUMMARY:
 *   This is the Ottoman continuity reading of the script-as-identity kernel.
 *   The reading claims that Arabic script is constitutive of Turkish-Islamic
 *   identity because it preserves access to Ottoman institutional memory,
 *   religious authority structures, and historical continuity. The constraint
 *   operates as a tangled rope: genuine coordination function (preserving
 *   institutional genealogy) coupled with asymmetric extraction (religious
 *   and state elites benefit from gatekeeping access to texts; lay
 *   populations bear literacy barriers). The reading is contested by two
 *   sibling readings—the Kemalist rupture reading (script as barrier to
 *   modernization) and phonetic instrumentalism (script as neutral
 *   technology). This story instantiates only the Ottoman continuity
 *   reading's structural implications, not the alternatives. The authored
 *   claim/metric divergence is intentional: the reading CLAIMS the constraint
 *   as rope (coordination-centered framing) while the measurements describe
 *   substantially extractive operation with rising suppression costs—the
 *   engine's per-seat computation will show how the beneficiary seats
 *   (religious scholars, state apparatus) experience coordination while
 *   target seats (lay populations, secular modernizers) experience
 *   extraction.
 *
 * KEY AGENTS:
 *   - religious_scholarly_class: institutional beneficiary/agenda-setter (identity-locked exit, high time horizon) — maintains gatekeeping authority over Ottoman texts
 *   - lay_populations_without_advanced_literacy: powerless payer (trapped exit, biographical horizon) — bears literacy barriers, restricted access to documents
 *   - secular_modernization_advocates: powerful payer/challenger (arbitrage exit, biographical horizon) — have state capacity for script reform but face institutional resistance
 *   - state_authority_apparatus: institutional agenda-setter (analytical position but enforcement power) — enforces continuity through educational policy and official standards
 *   - ottoman_institutional_memory_custodians: institutional beneficiary (constrained exit) — legitimacy derives from being custodians of continuity
 *   - rival_latin_script_proponents: excluded powerful actors — technical arguments sidelined by identity framing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(script_as_identity__ottoman_continuity_reading, 0.68).
domain_priors:suppression_score(script_as_identity__ottoman_continuity_reading, 0.76).
domain_priors:theater_ratio(script_as_identity__ottoman_continuity_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(script_as_identity__ottoman_continuity_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(script_as_identity__ottoman_continuity_reading, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(script_as_identity__ottoman_continuity_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(script_as_identity__ottoman_continuity_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(script_as_identity__ottoman_continuity_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(script_as_identity__ottoman_continuity_reading, tangled_rope).
narrative_ontology:human_readable(script_as_identity__ottoman_continuity_reading, "Arabic Script as Identity Anchor (Ottoman Continuity Reading)").
narrative_ontology:topic_domain(script_as_identity__ottoman_continuity_reading, "linguistic_identity/political_authority").

domain_priors:requires_active_enforcement(script_as_identity__ottoman_continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(script_as_identity__ottoman_continuity_reading, '351b3a7e-6527-4a0c-8d45-2bb43945f3e9').
narrative_ontology:cs_kernel_codification('351b3a7e-6527-4a0c-8d45-2bb43945f3e9', formalized).
narrative_ontology:cs_authority_grounding('351b3a7e-6527-4a0c-8d45-2bb43945f3e9', lineage).
narrative_ontology:cs_interpretation_layer_present('351b3a7e-6527-4a0c-8d45-2bb43945f3e9').
narrative_ontology:cs_reading_relation('351b3a7e-6527-4a0c-8d45-2bb43945f3e9', script_as_identity__kemalist_rupture_reading, forecloses).
narrative_ontology:cs_reading_relation('351b3a7e-6527-4a0c-8d45-2bb43945f3e9', script_as_identity__phonetic_instrumentalism_reading, coexists_with).
narrative_ontology:cs_axiom('351b3a7e-6527-4a0c-8d45-2bb43945f3e9', foundational, arabic_script_preserves_ottoman_institutional_memory).
narrative_ontology:cs_axiom_status(arabic_script_preserves_ottoman_institutional_memory, holdable).
narrative_ontology:cs_axiom_grounding('351b3a7e-6527-4a0c-8d45-2bb43945f3e9', arabic_script_preserves_ottoman_institutional_memory, empirically_contingent).
narrative_ontology:cs_axiom('351b3a7e-6527-4a0c-8d45-2bb43945f3e9', foundational, turkish_islamic_identity_constituted_through_script_continuity).
narrative_ontology:cs_axiom_status(turkish_islamic_identity_constituted_through_script_continuity, holdable).
narrative_ontology:cs_axiom_grounding('351b3a7e-6527-4a0c-8d45-2bb43945f3e9', turkish_islamic_identity_constituted_through_script_continuity, deontological).
narrative_ontology:cs_reference_frame('351b3a7e-6527-4a0c-8d45-2bb43945f3e9', ottoman_institutional_continuity).
narrative_ontology:cs_drift_state('351b3a7e-6527-4a0c-8d45-2bb43945f3e9', contemporary_secular_nation_state, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('351b3a7e-6527-4a0c-8d45-2bb43945f3e9', '').
narrative_ontology:cs_kernel_id(script_as_identity__ottoman_continuity_reading, script_as_identity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(script_as_identity__ottoman_continuity_reading, religious_scholarly_class).
narrative_ontology:constraint_beneficiary(script_as_identity__ottoman_continuity_reading, ottoman_institutional_memory_custodians).
narrative_ontology:constraint_victim(script_as_identity__ottoman_continuity_reading, lay_populations_without_advanced_literacy).
narrative_ontology:constraint_victim(script_as_identity__ottoman_continuity_reading, secular_modernization_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(script_as_identity__ottoman_continuity_reading, printing_and_publishing_sector).
narrative_ontology:constraint_beneficiary(script_as_identity__ottoman_continuity_reading, diaspora_ottoman_knowledge_communities).
narrative_ontology:constraint_victim(script_as_identity__ottoman_continuity_reading, printing_and_publishing_sector).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Islamic scholars and religious authorities who maintain interpretive control over Ottoman texts, Quranic commentary, and theological tradition through Arabic script literacy. Their authority derives partly from gatekeeping access to texts written in the script only they can reliably read. Enforces script preservation through religious education institutions (madrasas) and textual transmission, framing script continuity as religious duty. Their professional identity and institutional prestige are constituted through the claim that Islamic knowledge requires Arabic script mastery.
narrative_ontology:constraint_stakeholder(script_as_identity__ottoman_continuity_reading, religious_scholarly_class, beneficiary,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(script_as_identity__ottoman_continuity_reading, religious_scholarly_class, agenda_setter).

% State archivists, historians, and bureaucratic classes who manage Ottoman administrative records, legal documents, and historical continuity claims. Their institutional legitimacy is grounded in the narrative that the Turkish nation-state descends directly from the Ottoman empire and that preserving the script is preserving that legal/political genealogy. Arabic script preservation enables them to claim unbroken institutional authority back to Ottoman governance structures.
narrative_ontology:constraint_stakeholder(script_as_identity__ottoman_continuity_reading, ottoman_institutional_memory_custodians, beneficiary,
    institutional, generational, constrained, national).

% Rural populations, women excluded from formal Islamic education, and ordinary citizens who cannot easily read Ottoman Arabic-script texts. They bear the cost of script preservation through restricted access to historical records, property deeds, and legal documents that affect their lives. Literacy in the script requires years of specialized training they cannot afford or access. Their inability to read what governs them is the condition of the constraint's persistence.
narrative_ontology:constraint_stakeholder(script_as_identity__ottoman_continuity_reading, lay_populations_without_advanced_literacy, payer,
    powerless, biographical, trapped, national).

% Nationalist reformers, intellectuals, and state modernizers (particularly the Kemalist movement in Turkey) who see Arabic script as a barrier to mass literacy and national development. They advocate script replacement as enabling secular governance, vernacular education, and break from Ottoman-Islamic institutional structures. They have substantial state capacity but face resistance from institutional and religious actors who benefit from script continuity. Their exit option is political rupture and forced script reform through state authority.
narrative_ontology:constraint_stakeholder(script_as_identity__ottoman_continuity_reading, secular_modernization_advocates, payer,
    powerful, biographical, arbitrage, national).

% Publishers and printers of religious texts, historical documents, and educational materials. They are constrained to maintain two parallel printing systems (Arabic and Latin script versions) or specialize in one at the cost of market access. Religious text publishers benefit from script continuity (their monopoly on production). Mass-education publishers suffer under it (printing costs, technical complexity, limited typeface options for Arabic in early print era).
narrative_ontology:constraint_stakeholder(script_as_identity__ottoman_continuity_reading, printing_and_publishing_sector, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(script_as_identity__ottoman_continuity_reading, printing_and_publishing_sector, beneficiary).

% Scholars, expatriate intellectuals, and diaspora communities outside the Turkish state who maintain Ottoman-Islamic knowledge traditions and can access Arabic-script materials. They benefit from the national commitment to script preservation because it ensures the historical archive they study remains available and valued. Their exit option is relocation or scholarship in other linguistic/cultural contexts; they can shift intellectual orientation more readily than domestic populations.
narrative_ontology:constraint_stakeholder(script_as_identity__ottoman_continuity_reading, diaspora_ottoman_knowledge_communities, beneficiary,
    moderate, generational, mobile, global).

% The central state authority that can mandate script change or preservation through educational policy, official documentation standards, and cultural decree. In this reading, the state is positioned as enforcer of script continuity through institutional embedding (madrasas, court systems, official records). The state's interest is partly sovereign legitimacy (linking modern Turkish state to Ottoman legal authority) and partly managing religious institutional cooperation.
narrative_ontology:constraint_stakeholder(script_as_identity__ottoman_continuity_reading, state_authority_apparatus, agenda_setter,
    institutional, generational, analytical, national).

% Modernizers and reformers who advocate Latin script adoption would participate in the script-choice decision if structures permitted. They are excluded from formal authority over script standardization; their technical arguments (phonetic transparency, printing efficiency, lower literacy barriers) are systematically sidelined in debates framed as identity/continuity rather than functional linguistics. Their exclusion is maintained by framing script as religious/identity matter rather than technical policy choice.
narrative_ontology:constraint_stakeholder(script_as_identity__ottoman_continuity_reading, rival_latin_script_proponents, excluded,
    powerful, biographical, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(script_as_identity__ottoman_continuity_reading, religious_scholarly_class).
narrative_ontology:fixing_cost_class(script_as_identity__ottoman_continuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves institutional continuity with Ottoman governance, legal authority, and Islamic scholarly tradition by maintaining readable access to historical texts, administrative precedents, and religious sources. Solves the coordination problem: how does a post-Ottoman state claim legitimate inheritance of Ottoman legal/institutional authority? Answer: maintain the script that embodies that authority.
% TRANSFER_FUNCTION: Transfers educational burden and literacy barriers from privileged scholarly classes (who master Arabic script through specialized training) to lay populations (who face restricted access to documents that affect property, law, and historical knowledge). Also transfers opportunity cost from religious institutions (who maintain monopoly on text interpretation) to secular modernizers (who must overcome script barriers to implement alternative educational systems).
% ABSENT_VOICES: Latin-script reformers and modernizers would contest the framing of script as identity-constitutive rather than functionally technical. Diaspora communities and international scholarship communities that use Latin script for Ottoman studies would argue phonetic transparency is more important than script continuity. Village populations who cannot access their own property records would dispute the claim that script preserves their heritage rather than restricting it. These voices are structurally excluded from the identity/continuity framing itself.
% DISAPPEARANCE_RATIONALE: If the reading that 'Arabic script is constitutive of Turkish-Islamic identity' were abandoned — if script became framed as neutral technology rather than identity marker — educational policy would rapidly shift, mass literacy would accelerate under Latin script, Ottoman archives would be transliterated and democratized, and the religious scholarly class would lose gatekeeping authority over historical interpretation. The Ottoman state's institutional legitimacy claim would need to be regrounded in alternative sources (constitutional continuity, territorial sovereignty, bureaucratic succession rather than textual/script continuity).
% FOUNDING_PROBLEM: After Ottoman empire's fragmentation (1920s), the new Turkish state faced delegitimacy crisis: How does a post-imperial state claim authority over Ottoman territories and populations without the Ottoman sultanate and caliphate? The reading answers: declare continuity of Ottoman institutional/legal structures, preserved through the script that contains that institutional memory. Script preservation becomes proof of legitimate succession.
% FOUNDING_PROBLEM_CORROBORATION: Religious scholars and Ottoman historians within the benefiting institutional classes attest the founding problem remains live — without script continuity, Ottoman knowledge would be lost and Turkish state's legitimacy would be severed. Secular historians and modernizers attest the problem is substantially solved through alternative means (constitutional law, territorial treaties, bureaucratic succession) and that script preservation now serves institutional rent-seeking rather than state legitimacy. International scholarship from outside the benefiting parties (Western Ottoman studies, comparative script analysis) supports the view that legitimacy can be regrounded and that script has become a mechanism of gatekeeping rather than necessity.
narrative_ontology:disappearance_verdict(script_as_identity__ottoman_continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(script_as_identity__ottoman_continuity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(script_as_identity__ottoman_continuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(script_as_identity__ottoman_continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(script_as_identity__ottoman_continuity_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(script_as_identity__ottoman_continuity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(script_as_identity__ottoman_continuity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(script_as_identity__ottoman_continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises over the interval (0.38 → 0.68) because the founding coordination problem (state legitimacy via Ottoman continuity) gradually devolves into pure gatekeeping extraction as the state solidifies its authority through alternative means (constitutional law, territorial treaties, bureaucratic institutions). Early on, script preservation genuinely solves a coordination problem. By interval end, the same constraint operates primarily as a mechanism for religious and state elites to maintain interpretive monopoly over Ottoman history. Suppression requirement rises even faster (0.42 → 0.76) because active enforcement is required to maintain the script barrier against rising secular-modernizer pressure and lay-population demand for accessible education. Theater ratio rises moderately (0.18 → 0.42), indicating growing performative overhead: religious authorities increasingly justify the constraint in identity/spiritual terms (performed maintenance of tradition) rather than functional coordination terms. The measurement grid shares one time axis across all three metrics, from the founding problem (t=0, immediately post-Ottoman) through institutional consolidation (t=50) to contemporary entrenchment (t=100). All time points are authored at every shared moment per the alignment rule.
 *
 * PERSPECTIVAL GAP:
 *   The payer-seat perspective and the beneficiary-seat perspective diverge sharply. From the religious scholarly class's position, the constraint is genuine coordination: without script preservation, Ottoman knowledge is lost, Islamic continuity is severed, and their role as knowledge custodians becomes moot. From the lay-population perspective, the same constraint is pure extraction: they have no choice in the matter, bear all the costs (cannot read documents affecting their lives), and benefit-from the coordination minimally (their alternative is not 'lose Ottoman knowledge' but 'access Ottoman knowledge in transliterated form'). The secular modernizers see the constraint as false summitry: presented as identity/continuity necessity but actually a gatekeeping mechanism. From the state apparatus's analytic position, the constraint is transitional (it solved a real legitimacy crisis in 1920s; by 1980s+ it is inertial)—but the state has no incentive to unwind it if it maintains religious institutional cooperation. The engine should compute different types at different seats from this same structural data: beneficiary seats see coordination; target seats see extraction; state seat sees tangled rope (both); this divergence is the measurement the system exists to produce.
 *
 * DIRECTIONALITY LOGIC:
 *   Religious scholars sit near the beneficiary end of the directionality axis (d near 0.1–0.2): they benefit directly (gatekeeping authority), have identity-locked exit (professional identity constituted through text interpretation), and operate at institutional power with generational time horizon. State apparatus sits moderate (d near 0.3–0.4): genuine coordination benefit early on (script as legitimacy mechanism) but growing extraction as alternatives become available; institutional power but analytical-position exit option (state can reground legitimacy). Lay populations sit near the target end (d near 0.8–0.9): trapped exit, powerless position, biographical horizon, bearing costs (literacy barriers, document access) with no decision authority. Secular modernizers sit moderate-to-target (d near 0.5–0.7): have arbitrage-grade exit (can shift to alternative policy frameworks) and powerful institutional position, but face systematic suppression of their technical arguments and exclusion from script-choice framing. The directionality derivation from beneficiary/victim + exit structure should produce this pattern without override; overrides would be unnecessary if the structural declarations are accurate.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (state legitimacy via Ottoman continuity claim) has substantially died out. The Turkish republic is institutionally consolidated, legally grounded in constitutional authority, territorially established by treaty, and administratively continuous through bureaucratic succession. Alternative grounding for state legitimacy are robust and do not require script continuity. However, the constraint persists—enforced through religious educational policy, official documentation standards, and state authority—because no stakeholder with sufficient power has incentive to remove it. Religious elites benefit. State apparatus benefits from religious cooperation. Secular modernizers have arbitrage-exit (can shift policy frameworks) and do not need to destroy the constraint, only compartmentalize it (accept script in religious contexts, mandate Latin in secular education). Lay populations have no power to enforce change. This is the classic piton structure: the cost of maintaining the constraint (suppression_requirement rising, enforcement complexity growing) is distributed diffusely across powerless targets; the benefit is concentrated in institutional elites; no party is hurt badly enough to fix it; the agenda-setter (state + religious authority) could change it but the cost of coordinating that change exceeds their benefit. The constraint classification as tangled_rope at the claim level is the beneficiary-seat reading (coordination-centered narrative); the engine's per-seat computation should detect piton signals (high theater, rising suppression, distributed costs, concentrated benefits, founding problem dead) at the lay-population and some state seats, even as the institutional beneficiaries experience it as rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    identity_constructed_vs_primordial,
    'Is the link between Arabic script and Turkish-Islamic identity an ancient, constitutive fact, or a modern constructed claim that was reified into seeming-naturalness through institutional enforcement?',
    'Historical analysis of pre-Ottoman script use, Ottoman subjects'' script attitudes, and comparative-religion analysis of other Islamic communities'' script relationships. If Arabic script association is recent (Ottoman consolidation era rather than pre-Islamic continuity), the identity claim is institutional construction, not discovered fact.',
    'If constructed and recent, the constraint''s status shifts from rope (preserving genuine continuity) toward snare (enforcing artificial identity-fusion to maintain elite gatekeeping). The ε value would not change (the constraint''s extractiveness is the same either way), but the mandatrophy diagnosis would shift: a constructed identity can be un-constructed more readily than a genuinely constitutive one.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(identity_constructed_vs_primordial, empirical, 'Whether script-identity link is ancient continuity or modern construction reified through enforcement.').

omega_variable(
    suppression_structural_vs_internalized,
    'To what extent is the measured suppression (0.76) structural (external barriers—document standards, literacy training access) versus internalized (populations'' self-beliefs about script necessity, identity-fusion, shame about non-mastery)?',
    'Post-reform natural experiment: If a jurisdiction mandates Latin-script transition for official documents and education, measure whether populations'' suppression-related self-concepts persist (internalized) after structural barriers lift. If suppression stays high after structural removal, internalization is substantial; if it drops, suppression was primarily structural.',
    'High internalization means the constraint persists through self-policing even after external enforcement is removed. The effective extraction remains high even if formal enforcement ceases—targets carry the suppression with them. This would suggest deeper identity-lock than the stakeholder analysis captures and would warrant reclassifying the powerless-payer seat as partially identity-locked.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Composition of suppression mechanism: structural vs. internalized.').

omega_variable(
    founding_problem_persistence_ambiguity,
    'Is the founding problem (state legitimacy via Ottoman continuity) genuinely dead, or does it persist in attenuated form within specific institutional or conservative-identity constituencies?',
    'Survey of state authorities, religious scholars, and lay populations on whether Turkish state legitimacy depends on Ottoman continuity claim. If state/religious elites attest dependence but lay populations do not, the founding problem is partially alive (for institutional elites) but dead for majority—revealing that enforcement serves elite institutional interests, not general state survival.',
    'If the founding problem is alive only for institutional elites, the constraint is clearly piton: maintained by agenda-setters to preserve their own institutional position, not because the general legitimacy problem requires it. This supports mandatrophy diagnosis and would justify reclassification from tangled_rope (genuine coordination) to inertial_piton (performative tradition-maintenance with distributed costs).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_persistence_ambiguity, empirical, 'Whether founding problem (state legitimacy) persists or has been resolved through alternative grounding.').

omega_variable(
    kernels_reading_framing_ambiguity,
    'Is the reading''s framing—that script is identity-constitutive—itself a kernel interpretation that could be contested, or is it a fixed structural fact about Turkish-Islamic society?',
    'Comparison with sibling readings'' core premises: kemalist rupture claims script is contingent tool for modernization (not identity-constitutive); instrumentalism claims script is neutral technology. If these readings can be sustained without logical contradiction in alternate institutional frameworks, the ''identity-constitutive'' reading is one interpretation among coherent alternatives, not inevitable fact. This would mean the constraint''s persistence depends on which reading dominates institutional authority, not on the reading being uniquely true.',
    'If the reading is one among coexisting alternatives rather than the only defensible interpretation, then the reading''s authority depends on institutional power backing it (religious elites'' enforcement), not on inherent correctness. This supports snare diagnosis: the reading is maintained through enforcement, not discovered through evidence. The sibling readings'' exclusion (framed as dangerous modernization or technocratic reductionism) would then be suppression mechanisms rather than legitimate sidelining of false alternatives.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernels_reading_framing_ambiguity, conceptual, 'Whether the ottoman_continuity reading is uniquely defensible or one among coexisting, equally defensible interpretations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(script_as_identity__ottoman_continuity_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(scri_tr_t0, script_as_identity__ottoman_continuity_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(scri_tr_t15, script_as_identity__ottoman_continuity_reading, theater_ratio, 15, 0.24).
narrative_ontology:measurement(scri_tr_t30, script_as_identity__ottoman_continuity_reading, theater_ratio, 30, 0.31).
narrative_ontology:measurement(scri_tr_t50, script_as_identity__ottoman_continuity_reading, theater_ratio, 50, 0.38).
narrative_ontology:measurement(scri_tr_t75, script_as_identity__ottoman_continuity_reading, theater_ratio, 75, 0.4).
narrative_ontology:measurement(scri_tr_t100, script_as_identity__ottoman_continuity_reading, theater_ratio, 100, 0.42).

% Extraction over time
narrative_ontology:measurement(scri_be_t0, script_as_identity__ottoman_continuity_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(scri_be_t15, script_as_identity__ottoman_continuity_reading, base_extractiveness, 15, 0.45).
narrative_ontology:measurement(scri_be_t30, script_as_identity__ottoman_continuity_reading, base_extractiveness, 30, 0.52).
narrative_ontology:measurement(scri_be_t50, script_as_identity__ottoman_continuity_reading, base_extractiveness, 50, 0.62).
narrative_ontology:measurement(scri_be_t75, script_as_identity__ottoman_continuity_reading, base_extractiveness, 75, 0.66).
narrative_ontology:measurement(scri_be_t100, script_as_identity__ottoman_continuity_reading, base_extractiveness, 100, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(scri_su_t0, script_as_identity__ottoman_continuity_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(scri_su_t15, script_as_identity__ottoman_continuity_reading, suppression_requirement, 15, 0.58).
narrative_ontology:measurement(scri_su_t30, script_as_identity__ottoman_continuity_reading, suppression_requirement, 30, 0.65).
narrative_ontology:measurement(scri_su_t50, script_as_identity__ottoman_continuity_reading, suppression_requirement, 50, 0.72).
narrative_ontology:measurement(scri_su_t75, script_as_identity__ottoman_continuity_reading, suppression_requirement, 75, 0.74).
narrative_ontology:measurement(scri_su_t100, script_as_identity__ottoman_continuity_reading, suppression_requirement, 100, 0.76).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(script_as_identity__ottoman_continuity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(script_as_identity__ottoman_continuity_reading, 0.12).
narrative_ontology:affects_constraint(script_as_identity__ottoman_continuity_reading, script_as_identity__kemalist_rupture_reading).
narrative_ontology:affects_constraint(script_as_identity__ottoman_continuity_reading, script_as_identity__phonetic_instrumentalism_reading).

% DUAL FORMULATION NOTE:
% The script-as-identity kernel decomposes into three structurally distinct constraints, each instantiating a different reading with different ε values, beneficiary structures, and suppression mechanisms. The ottoman_continuity_reading (this story) claims script preserves institutional continuity and solves a state-legitimacy coordination problem; ε=0.68 reflects substantial extraction layered on that coordination. The kemalist_rupture_reading frames script-change as enabling secular modernization and treats script preservation as obstacle to progress; its ε and beneficiary/victim structure are inverted relative to this reading (modernizers become beneficiaries, religious conservatives become victims/payers). The phonetic_instrumentalism_reading brackets the identity/continuity questions entirely, treats script as neutral technology, and produces yet a different beneficiary structure (linguists/educators benefit, neither religious nor nationalist elites capture gains in pure form). These are not the same constraint viewed from different angles—they have different ε values, different victim/beneficiary sets, and different suppression mechanisms. The sibling readings are linked because institutional authority selects among them (determines which reading dominates policy), but they are separate constraints in the corpus. See kernel_context in commentary for detailed reading relations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(script_as_identity__ottoman_continuity_reading, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
