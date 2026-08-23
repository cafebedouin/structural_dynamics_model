% ============================================================================
% CONSTRAINT STORY: orthographic_kernel__modernization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_orthographic_kernel__modernization_reading, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: orthographic_kernel__modernization_reading
 *   human_readable: Latin Script Adoption for Technological Modernization (Modernization Reading)
 *   domain: political_linguistics/state_formation/commitment_systems
 *
 * SUMMARY:
 *   The 1928 Turkish Alphabet Law replaced the Arabic script with a
 *   Latin-based alphabet of 29 letters, enforced through mandatory schooling,
 *   closure of Arabic-script presses, and penalization of public
 *   Arabic-script use. The modernization_reading frames this as a
 *   coordination solution: Latin script's phonological transparency for
 *   Turkish vowel harmony enabled mass literacy and direct scientific
 *   vocabulary import, while the Turkish language itself (vocabulary,
 *   grammar, identity) was preserved. This reading claims the constraint is a
 *   rope or tangled_rope — genuine coordination with unavoidable transition
 *   costs. The authored metrics reveal substantial suppression (0.78) and
 *   accessibility collapse (0.82), indicating the constraint's persistence
 *   depended on active enforcement against the Ottoman textual order. The
 *   claim/metric gap is deliberate: the reading claims coordination; the
 *   metrics show extraction layered on coordination.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(orthographic_kernel__modernization_reading, 0.48).
domain_priors:suppression_score(orthographic_kernel__modernization_reading, 0.78).
domain_priors:theater_ratio(orthographic_kernel__modernization_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(orthographic_kernel__modernization_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(orthographic_kernel__modernization_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(orthographic_kernel__modernization_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(orthographic_kernel__modernization_reading, accessibility_collapse, 0.82).
narrative_ontology:constraint_metric(orthographic_kernel__modernization_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(orthographic_kernel__modernization_reading, tangled_rope).
narrative_ontology:human_readable(orthographic_kernel__modernization_reading, "Latin Script Adoption for Technological Modernization (Modernization Reading)").
narrative_ontology:topic_domain(orthographic_kernel__modernization_reading, "political_linguistics/state_formation/commitment_systems").

domain_priors:requires_active_enforcement(orthographic_kernel__modernization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(orthographic_kernel__modernization_reading, 'bebb6e2a-769c-4605-a6e0-e7cbc89e6c5b').
narrative_ontology:cs_kernel_codification('bebb6e2a-769c-4605-a6e0-e7cbc89e6c5b', formalized).
narrative_ontology:cs_authority_grounding('bebb6e2a-769c-4605-a6e0-e7cbc89e6c5b', extraction).
narrative_ontology:cs_interpretation_layer_present('bebb6e2a-769c-4605-a6e0-e7cbc89e6c5b').
narrative_ontology:cs_reading_relation('bebb6e2a-769c-4605-a6e0-e7cbc89e6c5b', orthographic_kernel__continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('bebb6e2a-769c-4605-a6e0-e7cbc89e6c5b', orthographic_kernel__rupture_reading, coexists_with).
narrative_ontology:cs_axiom('bebb6e2a-769c-4605-a6e0-e7cbc89e6c5b', foundational, latin_script_enables_scientific_access).
narrative_ontology:cs_axiom_status(latin_script_enables_scientific_access, holdable).
narrative_ontology:cs_axiom_grounding('bebb6e2a-769c-4605-a6e0-e7cbc89e6c5b', latin_script_enables_scientific_access, empirically_contingent).
narrative_ontology:cs_axiom('bebb6e2a-769c-4605-a6e0-e7cbc89e6c5b', foundational, turkish_identity_survives_script_change).
narrative_ontology:cs_axiom_status(turkish_identity_survives_script_change, holdable).
narrative_ontology:cs_axiom_grounding('bebb6e2a-769c-4605-a6e0-e7cbc89e6c5b', turkish_identity_survives_script_change, deontological).
narrative_ontology:cs_reference_frame('bebb6e2a-769c-4605-a6e0-e7cbc89e6c5b', ottoman_islamic_textual_order).
narrative_ontology:cs_drift_state('bebb6e2a-769c-4605-a6e0-e7cbc89e6c5b', republican_consolidation_era, gap(codification_collapse, substantial, false)).
narrative_ontology:cs_created_at('bebb6e2a-769c-4605-a6e0-e7cbc89e6c5b', '2026-08-15T14:30:00Z').
narrative_ontology:cs_kernel_id(orthographic_kernel__modernization_reading, orthographic_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(orthographic_kernel__modernization_reading, state_bureaucracy).
narrative_ontology:constraint_beneficiary(orthographic_kernel__modernization_reading, new_literate_urban_class).
narrative_ontology:constraint_beneficiary(orthographic_kernel__modernization_reading, technocratic_military_elite).
narrative_ontology:constraint_victim(orthographic_kernel__modernization_reading, ottoman_educated_ulema_scribes).
narrative_ontology:constraint_victim(orthographic_kernel__modernization_reading, rural_anatolian_populations).
narrative_ontology:constraint_victim(orthographic_kernel__modernization_reading, arabic_script_textual_tradition_bearers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(orthographic_kernel__modernization_reading, new_literate_urban_class).
narrative_ontology:constraint_vindicates(orthographic_kernel__modernization_reading, latin_script_enables_scientific_access).
narrative_ontology:constraint_vindicates(orthographic_kernel__modernization_reading, turkish_identity_survives_script_change).
narrative_ontology:constraint_vindicates(orthographic_kernel__modernization_reading, standardized_orthography_serves_state_building).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Authored and enforced the 1928 Alphabet Law mandating Latin script. Gains standardized administrative communication, simplified record-keeping, and direct access to European technical literature. Controls the Turkish Language Institute (TDK) which manages lexical purification and orthographic standards. Could reverse the policy but bears no cost for maintaining it.
narrative_ontology:constraint_stakeholder(orthographic_kernel__modernization_reading, state_bureaucracy, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(orthographic_kernel__modernization_reading, state_bureaucracy, beneficiary).

% Urban professionals, teachers, journalists, and civil servants who acquired literacy through the new script. Benefits from easier literacy acquisition (Latin script has closer grapheme-phoneme mapping for Turkish), access to modern education, and participation in the new public sphere. Bears indirect costs through taxonomic disruption of inherited cultural references. Can exit by emigrating or retreating into traditional spheres but loses professional standing.
narrative_ontology:constraint_stakeholder(orthographic_kernel__modernization_reading, new_literate_urban_class, beneficiary,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(orthographic_kernel__modernization_reading, new_literate_urban_class, payer).

% Religious scholars, madrasa teachers, calligraphers, and scribes trained in Arabic script and Ottoman Turkish. Lost professional legitimacy, teaching positions, and access to the textual tradition that constituted their authority. The 1924 abolition of the caliphate and 1925 closure of dervish lodges compounded the script change. Exit requires abandoning their entire epistemic formation; most were marginalized or co-opted into the new Diyanet with reduced authority.
narrative_ontology:constraint_stakeholder(orthographic_kernel__modernization_reading, ottoman_educated_ulema_scribes, payer,
    organized, biographical, trapped, national).

% Peasant households where Arabic script literacy (often via Quranic memorization) was the primary literacy. Faced abrupt devaluation of existing literacy, mandatory attendance at new Latin-script schools (1928 Law on Unification of Education), and disruption of intergenerational knowledge transmission. Exit is geographically and economically constrained; compliance was enforced through village institutes and conscription. The literacy gap between generations created a structural rupture in cultural transmission.
narrative_ontology:constraint_stakeholder(orthographic_kernel__modernization_reading, rural_anatolian_populations, payer,
    powerless, generational, constrained, national).

% Scholars, libraries, and institutions preserving Ottoman manuscript traditions, Islamic sciences in Turkish, and the Arabic-script literary heritage (divan poetry, tekke literature, historical chronicles). Their corpus became inaccessible to the new literate generation without specialized training. The TDK's lexical purification campaign (removing Arabic/Persian loanwords) further severed continuity. Their exclusion from the new national canon was structural — the constraint's coordination function required their marginalization.
narrative_ontology:constraint_stakeholder(orthographic_kernel__modernization_reading, arabic_script_textual_tradition_bearers, payer,
    moderate, civilizational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(orthographic_kernel__modernization_reading, arabic_script_textual_tradition_bearers, excluded).

% Armenian, Greek, and Jewish communities maintaining their own scripts and educational institutions under Lausanne Treaty protections. The Latinization created a de facto hierarchy where Turkish in Latin script became the sole language of the public sphere, while minority scripts were confined to communal spaces. They were not consulted on the script change; their objection would have challenged the unitary nation-state model the reform served.
narrative_ontology:constraint_stakeholder(orthographic_kernel__modernization_reading, minority_script_communities, excluded,
    moderate, generational, constrained, national).

% Modern linguists, historians, and sociologists analyzing the reform's structural effects. They see the full coordination-extraction topology: genuine phonological fit of Latin script for Turkish, real literacy expansion, but also the epistemic violence of severing a millennium of textual production. Their seat has no material stake but frames the constraint's classification for subsequent readers.
narrative_ontology:constraint_stakeholder(orthographic_kernel__modernization_reading, contemporary_linguistic_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Standardized a phonologically transparent orthography for Turkish, enabling mass literacy, direct scientific/technical vocabulary adoption from European languages, and a unified written standard for state administration and national education.
% TRANSFER_FUNCTION: Moves literacy acquisition costs and epistemic authority from Ottoman-educated classes (ulema, scribes, traditional scholars) to the state bureaucracy and new urban professional classes. Transfers control of the textual canon from religious/traditional institutions to the secular state apparatus (TDK, Ministry of Education).
% ABSENT_VOICES: Ottoman-educated ulema and scribes (marginalized by 1924-25 reforms before the script law), rural populations with no organized representation, minority script communities (protected by treaty but not consulted), and the Arabic-script textual tradition itself — which cannot speak but whose collapse is the constraint's structural condition.
% DISAPPEARANCE_RATIONALE: If the Latin script mandate vanished overnight, the Turkish state would lose its standardized administrative orthography, the education system would lose its phonological foundation, and the TDK's purist lexical framework would lose its enforcement basis. The world would rearrange: either a return to Arabic script (impossible without the ulema class), a new script debate, or a fragmented orthographic landscape. The constraint is load-bearing for the republican epistemic order.
% FOUNDING_PROBLEM: The Ottoman Empire's multi-script, multi-lingual textual ecology (Arabic script for Turkish/Ottoman, Armenian, Greek, Hebrew scripts for minorities) impeded centralized administration, mass literacy, and direct access to European scientific literature. The Arabic script's poor fit for Turkish vowel harmony and its association with the defeated imperial order made it a target for replacement.
% FOUNDING_PROBLEM_CORROBORATION: The modernization reading's founding problem is corroborated by non-beneficiary sources: European orientalists (e.g., von Mzik, Kellner-Heinkele) documented Arabic script's phonological mismatch for Turkish; UNESCO literacy studies confirm Latin script's learnability advantage; even continuity_reading advocates (e.g., İhsan Doğramacı's memoirs) acknowledge the administrative chaos of the late Ottoman multi-script system. The contestation is whether script change *required* the cultural rupture that accompanied it, not whether the coordination problem existed.
narrative_ontology:disappearance_verdict(orthographic_kernel__modernization_reading, world_rearranges).
narrative_ontology:founding_problem_status(orthographic_kernel__modernization_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(orthographic_kernel__modernization_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(orthographic_kernel__modernization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(orthographic_kernel__modernization_reading, 0.48, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(orthographic_kernel__modernization_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(orthographic_kernel__modernization_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(orthographic_kernel__modernization_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.48) because the literacy costs were real but distributed across a generation — the state bore infrastructure costs while the population bore cognitive transition costs. Suppression is high (0.78) because the constraint required banning Arabic-script printing, closing madrasas, and criminalizing the old script's public use — not mere persuasion. Theater ratio (0.38) reflects genuine coordination function (phonological fit, literacy gains) mixed with performative nation-building (lexical purification, historical narrative control). Accessibility collapse (0.82) is near-mountain level: within a generation, the entire Ottoman textual corpus became opaque to the new literate public. Resistance (0.65) was significant but fragmented — Kurdish rebellions (Sheikh Said 1925, Dersim 1937) had script dimensions, and ulema networks resisted covertly.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat experiences this as a rope (coordination problem solved, costs managed). The payer seats (ulema, rural) experience it as a snare (extraction enforced by coercion, alternatives suppressed). The beneficiary/payer hybrid seat (new urban class) experiences it as a tangled_rope (real gains, real losses). The engine computes this divergence from the structural data — the claimed_type 'tangled_rope' reflects the constraint's hybrid nature across seats, not a single classification.
 *
 * DIRECTIONALITY LOGIC:
 *   State bureaucracy (agenda_setter/beneficiary) sits at d≈0.15: it designed the constraint, controls its enforcement, and collects administrative efficiency gains. New literate urban class (beneficiary/payer) sits at d≈0.35: net beneficiaries of literacy access but pay taxonomic disruption costs. Ottoman ulema/scribes (payer) sit at d≈0.95: identity-locked, trapped, their entire epistemic capital expropriated. Rural populations (payer) sit at d≈0.85: constrained exit, generational literacy rupture. Minority communities (excluded) sit at d≈0.7: not targeted but structurally marginalized by the unitary script-nation equation. Contemporary scholars (observer) sit at d=0.5: analytical symmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (administrative chaos, literacy barrier, scientific access) was live in 1928. By 1950, mass literacy was achieved and scientific vocabulary was established — the coordination function was substantially fulfilled. Yet the constraint persisted with high suppression (0.78) and theater (0.38), shifting from transitional coordination to identity enforcement (lexical purification continued, Ottoman past remained suppressed). This is mandatrophy: the mandate outlived its function. The constraint did not sunset; it became a piton-in-training. The founding_problem_status 'contested' captures that beneficiaries claim the problem persists (new terminology needs), while victims and observers see the function as fulfilled.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    literacy_cost_necessity,
    'Were the literacy transition costs (generational rupture, ulema marginalization) structurally necessary for achieving mass literacy, or could Arabic script reform (vowel diacritics, simplified forms) have achieved similar outcomes with less extraction?',
    'Counterfactual comparison with Arabic-script reform movements in other Turkic contexts (e.g., Tatar Latinization 1920s vs. later Cyrillic; Kazakh current Latinization) and Ottoman-era script reform proposals (Münif Pasha, 1860s). Measure literacy acquisition curves and textual continuity preservation.',
    'If reform was feasible, the constraint''s extraction was discretionary — a snare component layered on coordination. If reform was impossible, the extraction is the price of coordination — tangled_rope stands. Affects claimed_type and mandatrophy assessment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(literacy_cost_necessity, empirical, 'Whether the extraction was necessary for the coordination function or discretionary.').

omega_variable(
    identity_preservation_claim,
    'Does ''preserving Turkish linguistic identity'' under Latin script hold descriptively, or was identity transformed (lexical purification, syntactic Turkification, loss of Ottoman register) such that the claim is a cover for rupture?',
    'Corpus linguistics comparing pre-1928 and post-1950 Turkish: lexical composition (Arabic/Persian loanword retention), syntactic structures, register range, and mutual intelligibility with Ottoman texts. Attitudinal surveys of identity self-description across generations.',
    'If identity was transformed, the modernization_reading''s foundational axiom (turkish_identity_survives_script_change) is empirically falsified — the reading becomes rupture_reading in disguise. If identity was preserved, the axiom holds and the reading is distinct from rupture_reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_preservation_claim, conceptual, 'Whether the identity preservation claim is descriptive or normative cover.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Was the high suppression (0.78) primarily structural (state coercion: bans, penalties, school enforcement) or internalized (population adopting the new script as ''modern,'' stigmatizing the old as ''backward'')?',
    'Post-exit suppression trajectory: after 1950 relaxation (multi-party period, Arabic script permitted in religious contexts), did Arabic script literacy revive spontaneously or remain extinct? If internalized, suppression persists after structural enforcement lifts.',
    'If internalized, the constraint''s effective suppression is higher than structural measures suggest — the target carries the suppression forward. This affects piton detection: a constraint with internalized suppression persists without active enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in the script transition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(orthographic_kernel__modernization_reading, 1928, 1950).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(orth_mod_tr_t1928, orthographic_kernel__modernization_reading, theater_ratio, 1928, 0.25).
narrative_ontology:measurement(orth_mod_tr_t1932, orthographic_kernel__modernization_reading, theater_ratio, 1932, 0.32).
narrative_ontology:measurement(orth_mod_tr_t1936, orthographic_kernel__modernization_reading, theater_ratio, 1936, 0.38).
narrative_ontology:measurement(orth_mod_tr_t1940, orthographic_kernel__modernization_reading, theater_ratio, 1940, 0.41).
narrative_ontology:measurement(orth_mod_tr_t1945, orthographic_kernel__modernization_reading, theater_ratio, 1945, 0.39).
narrative_ontology:measurement(orth_mod_tr_t1950, orthographic_kernel__modernization_reading, theater_ratio, 1950, 0.38).

% Extraction over time
narrative_ontology:measurement(orth_mod_be_t1928, orthographic_kernel__modernization_reading, base_extractiveness, 1928, 0.35).
narrative_ontology:measurement(orth_mod_be_t1932, orthographic_kernel__modernization_reading, base_extractiveness, 1932, 0.42).
narrative_ontology:measurement(orth_mod_be_t1936, orthographic_kernel__modernization_reading, base_extractiveness, 1936, 0.47).
narrative_ontology:measurement(orth_mod_be_t1940, orthographic_kernel__modernization_reading, base_extractiveness, 1940, 0.51).
narrative_ontology:measurement(orth_mod_be_t1945, orthographic_kernel__modernization_reading, base_extractiveness, 1945, 0.49).
narrative_ontology:measurement(orth_mod_be_t1950, orthographic_kernel__modernization_reading, base_extractiveness, 1950, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(orth_mod_su_t1928, orthographic_kernel__modernization_reading, suppression_requirement, 1928, 0.85).
narrative_ontology:measurement(orth_mod_su_t1932, orthographic_kernel__modernization_reading, suppression_requirement, 1932, 0.82).
narrative_ontology:measurement(orth_mod_su_t1936, orthographic_kernel__modernization_reading, suppression_requirement, 1936, 0.78).
narrative_ontology:measurement(orth_mod_su_t1940, orthographic_kernel__modernization_reading, suppression_requirement, 1940, 0.75).
narrative_ontology:measurement(orth_mod_su_t1945, orthographic_kernel__modernization_reading, suppression_requirement, 1945, 0.72).
narrative_ontology:measurement(orth_mod_su_t1950, orthographic_kernel__modernization_reading, suppression_requirement, 1950, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(orthographic_kernel__modernization_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(orthographic_kernel__modernization_reading, 0.08).
narrative_ontology:affects_constraint(orthographic_kernel__modernization_reading, orthographic_kernel__continuity_reading).
narrative_ontology:affects_constraint(orthographic_kernel__modernization_reading, orthographic_kernel__rupture_reading).
narrative_ontology:affects_constraint(orthographic_kernel__modernization_reading, tdk_lexical_purification).
narrative_ontology:affects_constraint(orthographic_kernel__modernization_reading, village_institute_education_system).
narrative_ontology:affects_constraint(orthographic_kernel__modernization_reading, turkish_language_reform_1930s).

% DUAL FORMULATION NOTE:
% This constraint (modernization_reading) decomposes the orthographic_kernel with continuity_reading and rupture_reading. The modernization_reading claims coordination function (literacy, scientific access) with moderate extraction. The continuity_reading claims the kernel is a mountain (Arabic script as natural textual continuity) — a false summit candidate. The rupture_reading claims the kernel is a snare (deliberate cultural rupture). All three share the same referent (1928 Alphabet Law) but author different ε, beneficiaries, and victims. Linked via affects_constraints for contamination propagation analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(orthographic_kernel__modernization_reading, organized, 0.95).
constraint_indexing:directionality_override(orthographic_kernel__modernization_reading, powerless, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
