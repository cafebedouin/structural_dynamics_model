% ============================================================================
% CONSTRAINT STORY: shinbutsu_coexistence_commitment__syncretic_fusion_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_shinbutsu_syncretic_fusion, []).

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
 *   constraint_id: shinbutsu_coexistence_commitment__syncretic_fusion_reading
 *   human_readable: Shinbutsu Coexistence: Syncretic Fusion Reading (Honji Suijaku Ontology)
 *   domain: religious/philosophical/institutional
 *
 * SUMMARY:
 *   The syncretic fusion reading (honji suijaku) asserts that kami and
 *   Buddhist deities are ontologically unified—kami are local, particularized
 *   manifestations of universal Buddhist truth, specifically the
 *   Buddha-nature accessible to all beings. This reading was the dominant
 *   framework from the 9th century through the Meiji Restoration (1868),
 *   institutionalized in the jinguji system (shrine-temples administered by
 *   Buddhist clergy) and enforced through imperial patronage and clerical
 *   authority. The reading constrains how the kernel (shinbutsu coexistence)
 *   is understood: it demands doctrinal consistency, vests interpretive
 *   authority in the theological elite and Buddhist hierarchy, and
 *   subordinates local shrine autonomy to this unified framework. This story
 *   narrates the syncretic fusion reading as a tangled_rope: it solves a real
 *   coordination problem (managing dual religious systems without
 *   contradiction) while extracting power over ritual autonomy, institutional
 *   control, and doctrinal authority from local practitioners and
 *   shrine-centered traditions. The constraint persists because both the
 *   coordination benefit and the extraction are tightly bound: you cannot
 *   accept the ontological unification without accepting Buddhist clerical
 *   mediation of kami practice.
 *
 * KEY AGENTS:
 *   - Buddhist clerical hierarchy: institutional authority setting the honji suijaku doctrine and controlling its interpretation
 *   - Jinguji institution: the structural embodiment of fusion theology, integrating kami and Buddhist worship under clerical administration
 *   - Kami devotional practitioners: local faithful who experience their kami worship reframed as Buddhist-mediated practice, identity-locked to place and tradition
 *   - Imperial authority: benefits from the fusion as a unifying framework that integrates both religious systems under imperial oversight
 *   - Theological elite: scholarly-clerical interpreters maintaining the coherence and consistency of the fusion doctrine
 *   - Rival religious movements: excluded from the interpretive authority and unable to contest the fusion framework
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_coexistence_commitment__syncretic_fusion_reading, 0.62).
domain_priors:suppression_score(shinbutsu_coexistence_commitment__syncretic_fusion_reading, 0.58).
domain_priors:theater_ratio(shinbutsu_coexistence_commitment__syncretic_fusion_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__syncretic_fusion_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__syncretic_fusion_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__syncretic_fusion_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__syncretic_fusion_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__syncretic_fusion_reading, resistance, 0.67).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_coexistence_commitment__syncretic_fusion_reading, tangled_rope).
narrative_ontology:human_readable(shinbutsu_coexistence_commitment__syncretic_fusion_reading, "Shinbutsu Coexistence: Syncretic Fusion Reading (Honji Suijaku Ontology)").
narrative_ontology:topic_domain(shinbutsu_coexistence_commitment__syncretic_fusion_reading, "religious/philosophical/institutional").

domain_priors:requires_active_enforcement(shinbutsu_coexistence_commitment__syncretic_fusion_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_coexistence_commitment__syncretic_fusion_reading, '4fbb041e-4af8-4767-bbed-5162eea3d7a3').
narrative_ontology:cs_kernel_codification('4fbb041e-4af8-4767-bbed-5162eea3d7a3', fixed_text).
narrative_ontology:cs_authority_grounding('4fbb041e-4af8-4767-bbed-5162eea3d7a3', lineage).
narrative_ontology:cs_interpretation_layer_present('4fbb041e-4af8-4767-bbed-5162eea3d7a3').
narrative_ontology:cs_reading_relation('4fbb041e-4af8-4767-bbed-5162eea3d7a3', shinbutsu_coexistence_commitment__domain_partition_reading, coexists_with).
narrative_ontology:cs_reading_relation('4fbb041e-4af8-4767-bbed-5162eea3d7a3', shinbutsu_coexistence_commitment__incoherent_bundle_reading, coexists_with).
narrative_ontology:cs_axiom('4fbb041e-4af8-4767-bbed-5162eea3d7a3', foundational, universal_buddha_nature_grounds_kami).
narrative_ontology:cs_axiom_status(universal_buddha_nature_grounds_kami, holdable).
narrative_ontology:cs_axiom_grounding('4fbb041e-4af8-4767-bbed-5162eea3d7a3', universal_buddha_nature_grounds_kami, deontological).
narrative_ontology:cs_axiom('4fbb041e-4af8-4767-bbed-5162eea3d7a3', foundational, honji_suijaku_doctrine_coherent_and_true).
narrative_ontology:cs_axiom_status(honji_suijaku_doctrine_coherent_and_true, overridden).
narrative_ontology:cs_axiom_grounding('4fbb041e-4af8-4767-bbed-5162eea3d7a3', honji_suijaku_doctrine_coherent_and_true, conventional).
narrative_ontology:cs_reference_frame('4fbb041e-4af8-4767-bbed-5162eea3d7a3', unified_buddha_nature_cosmos).
narrative_ontology:cs_drift_state('4fbb041e-4af8-4767-bbed-5162eea3d7a3', meiji_restoration_era, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('4fbb041e-4af8-4767-bbed-5162eea3d7a3', '').
narrative_ontology:cs_kernel_id(shinbutsu_coexistence_commitment__syncretic_fusion_reading, shinbutsu_coexistence_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__syncretic_fusion_reading, buddhist_clerical_hierarchy).
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__syncretic_fusion_reading, jinguji_institution).
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__syncretic_fusion_reading, imperial_authority).
narrative_ontology:constraint_victim(shinbutsu_coexistence_commitment__syncretic_fusion_reading, kami_devotional_practitioners).
narrative_ontology:constraint_victim(shinbutsu_coexistence_commitment__syncretic_fusion_reading, localized_shrine_autonomy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets and refines the honji suijaku doctrine, asserting that local kami are manifestations of universal Buddhist truth and that their worship is spiritually continuous with Buddhist practice. This interpretation consolidates doctrinal authority and legitimates Buddhist clergy as the spiritual mediators of kami veneration. The clerical hierarchy gains interpretive control over shrine practice and integrates shrine revenues into Buddhist institutional networks through jinguji (shrine-temples).
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__syncretic_fusion_reading, buddhist_clerical_hierarchy, agenda_setter,
    institutional, generational, constrained, national).

% The structural embodiment of syncretic fusion: shrine-temples that house both kami altars and Buddhist devotional spaces, administered by Buddhist clergy. Jinguji institutions gain administrative control over shrine property, ritual calendar, and pilgrim traffic. They mediate the relationship between kami and Buddhist deities, making them indispensable to the fusion theology and capturing the institutional rents of dual-purpose worship sites.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__syncretic_fusion_reading, jinguji_institution, agenda_setter,
    institutional, generational, mobile, regional).
narrative_ontology:stakeholder_secondary_role(shinbutsu_coexistence_commitment__syncretic_fusion_reading, jinguji_institution, beneficiary).

% The local faithful who venerate kami for harvest, healing, and life-world protection. Under the syncretic fusion reading, their practice is reinterpreted as worship of Buddhist-aligned deities, not autonomous kami. Their devotional autonomy is constrained—their kami are declared to be manifestations of something else (universal Buddhahood), and the ritual form of their devotion is mediated through Buddhist clerical authority rather than directed autonomously. Identity-locked: kami devotion is often constitutive of local identity, ancestral continuity, and place-based belonging, making exit deeply costly.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__syncretic_fusion_reading, kami_devotional_practitioners, payer,
    powerless, biographical, identity_locked, local).

% The abstract principle that local shrines might govern themselves, conduct rituals autonomously, and maintain doctrinal independence from Buddhist interpretation. Under the syncretic fusion reading, this autonomy is systematically subordinated to Buddhist clerical interpretation and institutional integration. The principle persists as a tension, never fully eliminated but consistently overridden by the fusion doctrine.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__syncretic_fusion_reading, localized_shrine_autonomy, payer,
    powerless, generational, trapped, local).
narrative_ontology:stakeholder_non_agent(shinbutsu_coexistence_commitment__syncretic_fusion_reading, localized_shrine_autonomy).

% The imperial court benefits from syncretic fusion as a unifying religious framework that integrates Shinto-derived legitimacy (emperor as Shinto high priest) with Buddhist institutional power. The fusion allows the court to command both religious systems without choosing between them, and the Buddhist clerical hierarchy—bound through the jinguji network—becomes a tool of imperial cultural authority and institutional stability.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__syncretic_fusion_reading, imperial_authority, beneficiary,
    institutional, generational, constrained, national).

% The scholarly-clerical circle interpreting the honji suijaku doctrine and refining its theological coherence. This elite controls what counts as a valid reading of the kernel commitment and produces the doctrinal consistency that legitimates the arrangement. They have high mobility within institutional structures and can move between temples, court service, and scholarly networks.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__syncretic_fusion_reading, theological_elite, agenda_setter,
    institutional, generational, arbitrage, national).

% Non-Buddhist, non-Shinto derived spiritual movements or kami-focused traditions that might claim autonomy from the syncretic fusion framework. They are structurally excluded from the interpretive authority that defines the kernel commitment and cannot directly influence what counts as valid kami-Buddha relationship.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__syncretic_fusion_reading, rival_religious_movements, excluded,
    moderate, biographical, trapped, regional).

% The genealogical transmission of the honji suijaku doctrine from Indian Buddhist philosophy through Chinese Chan/Zen interpretations, into Japanese Buddhist schools. This lineage is the epistemic authority grounding the fusion reading: a claims about ontological continuity that stretch back through centuries of scholarly-clerical transmission.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__syncretic_fusion_reading, committing_lineage, observer,
    institutional, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(shinbutsu_coexistence_commitment__syncretic_fusion_reading, buddhist_clerical_hierarchy).
narrative_ontology:fixing_cost_class(shinbutsu_coexistence_commitment__syncretic_fusion_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single coherent religious ontology that integrates two seemingly distinct spiritual systems (kami veneration and Buddhist practice) into one framework, allowing worshippers, institutions, and the state to navigate both without doctrinal contradiction or competitive tension. Solves the problem of managing dual religious commitments by asserting their deep metaphysical unity.
% TRANSFER_FUNCTION: Moves interpretive authority from locally-rooted kami practitioners to the Buddhist clerical hierarchy; moves institutional control of shrines from autonomous shrine management to jinguji (shrine-temple) administration; moves shrine revenues from independent shrine support to the Buddhist institutional network; moves ritual autonomy from local practitioners to clerical mediation of 'correct' kami-Buddha relationship.
% ABSENT_VOICES: Practitioners whose kami devotion is centered on local autonomy and independence from Buddhist framing; village shrine priests who would resist subordination to clerical hierarchy; kami-focused traditions that reject the claim that kami are manifestations of universal Buddhist truth; any tradition that reads kami as genuinely autonomous entities rather than as expressions of Buddha-nature.
% DISAPPEARANCE_RATIONALE: If the syncretic fusion reading vanished—if the ontological claim that kami are manifestations of universal Buddhist truth were rejected—shrines would immediately reorganize under non-Buddhist administration, ritual autonomy would reassert at the local level, and the jinguji network would collapse as an institutional form. The imperial court would lose the framework unifying both religious systems, and the Buddhist clerical hierarchy would lose its interpretive grip on kami practice. Devotional geography and institutional authority across Japan would be fundamentally redrawn.
% FOUNDING_PROBLEM: In the 8th–9th centuries, Buddhism's rapid spread in Japan created a theological and institutional problem: how to integrate the Buddha Dharma with the indigenous kami system that was already central to Japanese religion and statecraft. A direct replacement was impossible (imperial legitimacy derived from Shinto), and outright coexistence without unification created inconsistency and competition for resources. The fusion framework solved this by declaring kami to be local manifestations of universal Buddhist truth—making both systems part of a single coherent cosmos.
% FOUNDING_PROBLEM_CORROBORATION: The Buddhist clerical hierarchy and imperial court maintained throughout the premodern period that the founding problem—managing dual religious systems without contradiction—remained live and required continuous interpretive maintenance. Historical sources from outside the benefiting parties (local shrine records, folklore, Meiji-era accounts of the shock of Shinbutsu-bunri) confirm that the problem's conditions persisted for more than a millennium, but also document growing tension: local practitioners increasingly experienced the syncretic framework as an imposing doctrine rather than a self-evident truth, and the Meiji rejection of the framework (Shinbutsu-bunri, 1868) demonstrates that the founding problem's legitimacy could be revoked decisively.
narrative_ontology:disappearance_verdict(shinbutsu_coexistence_commitment__syncretic_fusion_reading, world_rearranges).
narrative_ontology:founding_problem_status(shinbutsu_coexistence_commitment__syncretic_fusion_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_coexistence_commitment__syncretic_fusion_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(shinbutsu_coexistence_commitment__syncretic_fusion_reading, 'none', 1).
narrative_ontology:epsilon_provenance(shinbutsu_coexistence_commitment__syncretic_fusion_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shinbutsu_coexistence_commitment__syncretic_fusion_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(shinbutsu_coexistence_commitment__syncretic_fusion_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(shinbutsu_coexistence_commitment__syncretic_fusion_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.62) because the constraint systematically transfers interpretive authority from local practitioners to the Buddhist hierarchy, institutional control from autonomous shrines to jinguji networks, and ritual autonomy from local communities to clerical mediation. However, extraction is not total (0.74+) because the constraint does deliver genuine coordination benefit—worshippers can genuinely navigate both kami and Buddhist practice through a coherent framework, and the doctrine is not purely cover-story. Suppression is substantial (0.58) because the constraint requires active enforcement: local shrine autonomy must be continuously suppressed through jinguji integration, interpretive challenges from rival traditions must be excluded, and local practitioners' identity-locked resistance must be managed (kami devotion is constitutive of local identity, so resistance to the fusion reading is identity-threatening and must be actively negotiated). Theater ratio starts low (0.25) and rises to moderate (0.42), suggesting that as the arrangement matures, an increasing share of enforcement energy goes to maintaining doctrinal consistency narratively (producing theological texts, refining honji suijaku arguments) rather than solving the coordination problem (which stabilizes by the mid-period). Accessibility collapse is high (0.71): once the honji suijaku ontology is asserted, alternatives collapse—local practitioners cannot simply reject the framework without breaking from the national religious establishment and losing access to imperial-sanctioned rituals. Resistance is substantial (0.67): local shrine priests, kami-focused traditions, and practitioners with strong local identity resist the subordination of kami autonomy, creating constant tension that requires enforcement.
 *
 * PERSPECTIVAL GAP:
 *   Why would the clerical hierarchy and practitioners compute differently? Because the hierarchy controls the discourse and has arbitrage options (they can serve the court, move between temples, shift their emphasis between kami and Buddha practice); practitioners have no such options—they are rooted in place, their identity is constituted through kami devotion, and exit means cultural death. The two seats are looking at the same constraint through completely different power and exit conditions. This asymmetry is why one seat reads unification and the other reads imposition.
 *
 * DIRECTIONALITY LOGIC:
 *   Buddhist clerical hierarchy: d near 0.0 (full beneficiary) — they set the interpretation, control jinguji networks, gain authority and resources. Jinguji institution: d near 0.2–0.3 (beneficiary) — gains administrative control, captures revenues, coordinates genuine value but also extracts institutional rents. Kami devotional practitioners: d near 0.8–0.9 (near-total target) — their interpretive autonomy is overridden, their practice is reframed, their exit is identity-locked (kami devotion is constitutive of local being; rejecting it means breaking from ancestral tradition and community identity). Imperial authority: d near 0.1–0.2 (beneficiary) — gains the unifying framework and control of both religious systems without choosing between them. Theological elite: d near 0.0–0.1 (full beneficiary) — produces the doctrine that legitimates the arrangement and has high mobility, arbitrage options to exit if they lose interpretive prestige. The constraint's effectiveness as extraction depends entirely on the identity-lock of kami practitioners: if they could exit costlessly, the suppression would collapse. Instead, the identity-locked relationship means practitioners are trapped even when they experience the arrangement as subordination.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem—managing two religious systems without contradiction—is genuinely live throughout the premodern period (confirmed by continuous theological production, jinguji expansion, and imperial patronage). The syncretic fusion framework solves this problem: it does reduce contradiction, it does provide a unifying ontology, and it does enable dual worship. BUT the solution is tightly bound to extraction: you cannot accept the unification without accepting Buddhist clerical authority over kami practice. This is exactly the tangled_rope pattern—real coordination (genuine problem solved) and real extraction (power transferred to hierarchy) are structurally inseparable in this reading. The constraint is not pure extraction (snare) because the unification is not mere cover—theological texts are serious, the framework is intellectually coherent, and worshippers genuinely benefit from navigating a unified cosmos. But it is not pure coordination (rope) because the hierarchy benefits more than it costs, and local autonomy is subordinated whether or not it makes the coordination work better. The Meiji Shinbutsu-bunri (separation of kami and Buddha, 1868) proves that this was a constructed constraint, not inevitable: once imperial priorities shifted, the unification was violently reversed within a few years, suggesting it persisted through active enforcement rather than natural inevitability or universal benefit. The theater ratio rising from 0.25 to 0.42 suggests that by the late premodern period, increasing energy was going to maintaining the doctrine (producing texts, refining arguments) rather than solving original coordination challenges (those had stabilized)—a pattern consistent with mandatrophy where the original function is solved and only the extraction machinery requires ongoing maintenance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ontological_unification_vs_functional_coordination,
    'Is honji suijaku an ontologically true statement about the relationship between kami and Buddha-nature, or a successful coordination device that created functional unity without metaphysical claim?',
    'Test via counterfactual: if the same coordination function (dual worship, institutional efficiency, absence of contradiction) could be achieved through a domain-partition framework (kami and Buddhas govern different existential domains), then unification is a choice, not a necessity. The Meiji reintroduction of Shinbutsu-bunri suggests this is possible.',
    'If unification is merely functional (coordination device), the constraint reclassifies from tangled_rope (embedded extraction in the doctrine itself) toward snare (the doctrine is cover story for pure institutional extraction). If unification is ontologically true, the extraction component is genuinely entangled with the coordination benefit and cannot be separated.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(ontological_unification_vs_functional_coordination, conceptual, 'Whether syncretic fusion is metaphysical necessity or institutional choice').

omega_variable(
    identity_lock_internalization,
    'How much of the kami practitioners'' suppression is structural (jinguji control, institutional integration) versus internalized (absorbed the fusion doctrine as truth, experience kami worship as authentically Buddhist)?',
    'Historical evidence: post-Meiji trajectories of kami practitioners. If suppression persists after jinguji dissolution (practitioners continue to experience kami as unified with Buddhism despite institutional separation), internalization is substantial. If kami practice rapidly reorganizes under independent shrine administration, internalization was minimal and suppression was primarily structural.',
    'If internalized: kami practitioners carry the suppression with them after institutional separation; restoration of autonomy requires not just structural change but sustained re-education and identity reconstruction. If structural: suppression was enforcement machinery; remove the machinery and practitioners readily reorganize. Historical record shows partial recovery (Meiji Shinto nationalism broke identity-fusion with Buddhism, but decades of education were required), suggesting internalization was substantial.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_internalization, empirical, 'Structural versus internalized suppression in kami devotional practice').

omega_variable(
    theological_elite_gatekeeping,
    'Does the honji suijaku doctrine require sophisticated theological interpretation (justifying theological elite control), or is it simple enough that any practitioner could understand and apply it?',
    'Analyze primary texts: are the core theological claims (kami = manifestations of Buddha-nature, universal Buddhahood accessible through local kami) stated simply and applied straightforwardly, or do they require specialized learning and refinement to maintain coherence?',
    'If simple: the theological elite''s gatekeeping is pure institutional capture (keeping interpretation restricted to maintain authority). If complex: the elite''s role as interpreters is structurally necessary to maintain the doctrine''s consistency and coherence. The measurement data (rising theater_ratio suggesting increasing focus on doctrinal maintenance) suggests complexity requiring expert interpretation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theological_elite_gatekeeping, empirical, 'Whether honji suijaku is simple doctrine or requires specialized theological interpretation').

omega_variable(
    sibling_reading_committer_displacement,
    'Which reading (syncretic_fusion, domain_partition, or incoherent_bundle) represents the actual metaphysical relationship between kami and Buddhist deities, and which are merely institutional framings?',
    'This is non-resolvable by empirical means: it is an irreducibly theological question about which ontological framework is true. Different parties hold different readings as metaphysically true and others as cover stories. The question belongs to the engine''s Omega_C (conceptual irreducibility), not Omega_E (empirical).',
    'If syncretic_fusion is metaphysically true: the constraint embodies real theological insight and extraction is a cost of institutional embodiment, not of imposing a false doctrine. If domain_partition is true: syncretic_fusion is a cover story for institutional capture. If incoherent_bundle is true: none of the readings is metaphysically true; the whole system is maintained through deliberate ambiguity and institutional power. The three readings structure the entire classification landscape differently.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_committer_displacement, conceptual, 'Metaphysical status of syncretic fusion versus sibling readings (irreducibly theological)').

omega_variable(
    imperial_cooptation_vs_genuine_synthesis,
    'Did the imperial court adopt honji suijaku because it genuinely solved the theological problem of managing dual religions, or because it served imperial interests in controlling both religious systems?',
    'Examine imperial patronage patterns: does the court favor the syncretic reading equally with Buddhist and kami institutions, or does it selectively emphasize whichever reading serves current political needs? Do imperial legitimacy claims rest on the fusion doctrine or on independent Shinto legitimacy?',
    'If genuine synthesis: the imperial court''s support reflects real problem-solving. If instrumental: imperial backing is political cooptation that used the fusion doctrine to consolidate power. Historical evidence suggests both: the court genuinely benefited from unifying the two systems AND used that unification as a tool of political consolidation. The two are not mutually exclusive.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(imperial_cooptation_vs_genuine_synthesis, empirical, 'Whether imperial adoption of honji suijaku represents genuine theological synthesis or instrumental political capture').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_coexistence_commitment__syncretic_fusion_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shinbutsu_syncretic_tr_t0, shinbutsu_coexistence_commitment__syncretic_fusion_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(shinbutsu_syncretic_tr_t5, shinbutsu_coexistence_commitment__syncretic_fusion_reading, theater_ratio, 5, 0.3).
narrative_ontology:measurement(shinbutsu_syncretic_tr_t10, shinbutsu_coexistence_commitment__syncretic_fusion_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement(shinbutsu_syncretic_tr_t15, shinbutsu_coexistence_commitment__syncretic_fusion_reading, theater_ratio, 15, 0.38).
narrative_ontology:measurement(shinbutsu_syncretic_tr_t20, shinbutsu_coexistence_commitment__syncretic_fusion_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement(shinbutsu_syncretic_tr_t25, shinbutsu_coexistence_commitment__syncretic_fusion_reading, theater_ratio, 25, 0.41).
narrative_ontology:measurement(shinbutsu_syncretic_tr_t30, shinbutsu_coexistence_commitment__syncretic_fusion_reading, theater_ratio, 30, 0.42).
narrative_ontology:measurement(shinbutsu_syncretic_tr_t35, shinbutsu_coexistence_commitment__syncretic_fusion_reading, theater_ratio, 35, 0.42).

% Extraction over time
narrative_ontology:measurement(shinbutsu_syncretic_be_t0, shinbutsu_coexistence_commitment__syncretic_fusion_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(shinbutsu_syncretic_be_t5, shinbutsu_coexistence_commitment__syncretic_fusion_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(shinbutsu_syncretic_be_t10, shinbutsu_coexistence_commitment__syncretic_fusion_reading, base_extractiveness, 10, 0.56).
narrative_ontology:measurement(shinbutsu_syncretic_be_t15, shinbutsu_coexistence_commitment__syncretic_fusion_reading, base_extractiveness, 15, 0.59).
narrative_ontology:measurement(shinbutsu_syncretic_be_t20, shinbutsu_coexistence_commitment__syncretic_fusion_reading, base_extractiveness, 20, 0.61).
narrative_ontology:measurement(shinbutsu_syncretic_be_t25, shinbutsu_coexistence_commitment__syncretic_fusion_reading, base_extractiveness, 25, 0.62).
narrative_ontology:measurement(shinbutsu_syncretic_be_t30, shinbutsu_coexistence_commitment__syncretic_fusion_reading, base_extractiveness, 30, 0.62).
narrative_ontology:measurement(shinbutsu_syncretic_be_t35, shinbutsu_coexistence_commitment__syncretic_fusion_reading, base_extractiveness, 35, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(shinbutsu_syncretic_su_t0, shinbutsu_coexistence_commitment__syncretic_fusion_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(shinbutsu_syncretic_su_t5, shinbutsu_coexistence_commitment__syncretic_fusion_reading, suppression_requirement, 5, 0.45).
narrative_ontology:measurement(shinbutsu_syncretic_su_t10, shinbutsu_coexistence_commitment__syncretic_fusion_reading, suppression_requirement, 10, 0.5).
narrative_ontology:measurement(shinbutsu_syncretic_su_t15, shinbutsu_coexistence_commitment__syncretic_fusion_reading, suppression_requirement, 15, 0.53).
narrative_ontology:measurement(shinbutsu_syncretic_su_t20, shinbutsu_coexistence_commitment__syncretic_fusion_reading, suppression_requirement, 20, 0.55).
narrative_ontology:measurement(shinbutsu_syncretic_su_t25, shinbutsu_coexistence_commitment__syncretic_fusion_reading, suppression_requirement, 25, 0.57).
narrative_ontology:measurement(shinbutsu_syncretic_su_t30, shinbutsu_coexistence_commitment__syncretic_fusion_reading, suppression_requirement, 30, 0.58).
narrative_ontology:measurement(shinbutsu_syncretic_su_t35, shinbutsu_coexistence_commitment__syncretic_fusion_reading, suppression_requirement, 35, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_coexistence_commitment__syncretic_fusion_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(shinbutsu_coexistence_commitment__syncretic_fusion_reading, 0.12).
narrative_ontology:affects_constraint(shinbutsu_coexistence_commitment__syncretic_fusion_reading, shinbutsu_coexistence_commitment__domain_partition_reading).
narrative_ontology:affects_constraint(shinbutsu_coexistence_commitment__syncretic_fusion_reading, shinbutsu_coexistence_commitment__incoherent_bundle_reading).
narrative_ontology:affects_constraint(shinbutsu_coexistence_commitment__syncretic_fusion_reading, jinguji_institution_administrative_control).
narrative_ontology:affects_constraint(shinbutsu_coexistence_commitment__syncretic_fusion_reading, buddhist_clerical_authority_over_shrine_practice).

% DUAL FORMULATION NOTE:
% The shinbutsu_coexistence_commitment kernel instantiates three structurally distinct constraints, one per reading. This constraint (syncretic_fusion_reading) asserts ontological unification through honji suijaku, vesting interpretive authority in the Buddhist theological elite and institutional control in jinguji networks. The domain_partition_reading asserts functional separation without ontological unification, allowing local shrine autonomy. The incoherent_bundle_reading asserts the whole system was never coherent but maintained through institutional power and deliberate ambiguity. Each reading has different beneficiaries, different victims, different extraction patterns, and different vulnerability to external challenge. The three are related through the shared kernel (shinbutsu coexistence) but are NOT reducible to observational variants—they are different commitments with different structures. See omegas for the theological irreducibility (Omega_C) and empirical tests (Omega_E) distinguishing them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(shinbutsu_coexistence_commitment__syncretic_fusion_reading, powerless, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
