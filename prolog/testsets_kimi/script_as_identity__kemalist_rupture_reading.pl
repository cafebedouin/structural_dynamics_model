% ============================================================================
% CONSTRAINT STORY: script_as_identity__kemalist_rupture_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_script_as_identity__kemalist_rupture_reading, []).

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
 *   constraint_id: script_as_identity__kemalist_rupture_reading
 *   human_readable: Kemalist Script Reform as Identity Rupture
 *   domain: comparative_linguistics/political_authority/state-building
 *
 * SUMMARY:
 *   In 1928 the Turkish Republic legally replaced the Ottoman Arabic alphabet
 *   with a Latin-based script. The kemalist_rupture_reading treats this not
 *   as a technical orthographic adjustment but as a constitutive act of
 *   secular nation-building: the severance of the Ottoman-Islamic textual
 *   tradition is a feature, not a cost, because it eliminates competing
 *   textual authorities and allows the Republican state to monopolize the
 *   literacy apparatus. This constraint story models that reading as a
 *   structurally enforced arrangement with genuine coordination benefits
 *   (mass phonetic literacy) and asymmetric extraction (disenfranchisement of
 *   Ottoman-literate elites, state control over historical memory, identity
 *   rupture for religious communities).
 *
 * KEY AGENTS:
 *   - kemalist_state_apparatus: Agenda-setter and beneficiary â enforces the reform and captures textual authority
 *   - secular_nationalist_intelligentsia: Beneficiary â cultural capital vested in the new Latin-script canon
 *   - ottoman_literate_elites: Payer â cultural capital in Arabic script liquidated by fiat
 *   - religious_scholars: Payer â textual transmission chain severed, religious authority state-mediated
 *   - rural_conservative_population: Payer â excluded from ancestral textual archive, dependent on state schools
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(script_as_identity__kemalist_rupture_reading, 0.72).
domain_priors:suppression_score(script_as_identity__kemalist_rupture_reading, 0.82).
domain_priors:theater_ratio(script_as_identity__kemalist_rupture_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(script_as_identity__kemalist_rupture_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(script_as_identity__kemalist_rupture_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(script_as_identity__kemalist_rupture_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(script_as_identity__kemalist_rupture_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(script_as_identity__kemalist_rupture_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(script_as_identity__kemalist_rupture_reading, tangled_rope).
narrative_ontology:human_readable(script_as_identity__kemalist_rupture_reading, "Kemalist Script Reform as Identity Rupture").
narrative_ontology:topic_domain(script_as_identity__kemalist_rupture_reading, "comparative_linguistics/political_authority/state-building").

domain_priors:requires_active_enforcement(script_as_identity__kemalist_rupture_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(script_as_identity__kemalist_rupture_reading, '304c5c77-81fe-4915-92ce-e771045b6b0c').
narrative_ontology:cs_kernel_codification('304c5c77-81fe-4915-92ce-e771045b6b0c', formalized).
narrative_ontology:cs_authority_grounding('304c5c77-81fe-4915-92ce-e771045b6b0c', extraction).
narrative_ontology:cs_interpretation_layer_present('304c5c77-81fe-4915-92ce-e771045b6b0c').
narrative_ontology:cs_reading_relation('304c5c77-81fe-4915-92ce-e771045b6b0c', script_as_identity__ottoman_continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('304c5c77-81fe-4915-92ce-e771045b6b0c', script_as_identity__phonetic_instrumentalism_reading, influences).
narrative_ontology:cs_axiom('304c5c77-81fe-4915-92ce-e771045b6b0c', foundational, script_rupture_necessary_for_secular_nation).
narrative_ontology:cs_axiom_status(script_rupture_necessary_for_secular_nation, holdable).
narrative_ontology:cs_axiom_grounding('304c5c77-81fe-4915-92ce-e771045b6b0c', script_rupture_necessary_for_secular_nation, instrumental).
narrative_ontology:cs_axiom('304c5c77-81fe-4915-92ce-e771045b6b0c', foundational, state_sovereignty_over_textual_authority).
narrative_ontology:cs_axiom_status(state_sovereignty_over_textual_authority, holdable).
narrative_ontology:cs_axiom_grounding('304c5c77-81fe-4915-92ce-e771045b6b0c', state_sovereignty_over_textual_authority, conventional).
narrative_ontology:cs_reference_frame('304c5c77-81fe-4915-92ce-e771045b6b0c', republican_nationalist_sovereignty).
narrative_ontology:cs_drift_state('304c5c77-81fe-4915-92ce-e771045b6b0c', post_single_party_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('304c5c77-81fe-4915-92ce-e771045b6b0c', '').
narrative_ontology:cs_kernel_id(script_as_identity__kemalist_rupture_reading, script_as_identity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(script_as_identity__kemalist_rupture_reading, kemalist_state_apparatus).
narrative_ontology:constraint_beneficiary(script_as_identity__kemalist_rupture_reading, secular_nationalist_intelligentsia).
narrative_ontology:constraint_victim(script_as_identity__kemalist_rupture_reading, ottoman_literate_elites).
narrative_ontology:constraint_victim(script_as_identity__kemalist_rupture_reading, religious_scholars).
narrative_ontology:constraint_victim(script_as_identity__kemalist_rupture_reading, rural_conservative_population).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enacts and enforces the 1928 script reform through law, education policy, and press controls; monopolizes the legitimate literacy apparatus; actively excludes Arabic script from public and official life; collects ideological authority and nation-building legitimacy from the rupture.
narrative_ontology:constraint_stakeholder(script_as_identity__kemalist_rupture_reading, kemalist_state_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(script_as_identity__kemalist_rupture_reading, kemalist_state_apparatus, beneficiary).

% Their cultural capital and public authority are vested in the new Latin-script literacy; they produce the textbooks, novels, and journalism that constitute the new national canon; dependent on the state's exclusion of Arabic-script competitors for their elevated status.
narrative_ontology:constraint_stakeholder(script_as_identity__kemalist_rupture_reading, secular_nationalist_intelligentsia, beneficiary,
    powerful, biographical, constrained, national).

% Hold extensive cultural capital in Arabic script; abruptly excluded from official publishing, legal practice, and public education; face the choice of painful relearning or retreat from public intellectual life; their authority derives from a textual tradition the reform delegitimizes overnight.
narrative_ontology:constraint_stakeholder(script_as_identity__kemalist_rupture_reading, ottoman_literate_elites, payer,
    moderate, biographical, identity_locked, national).

% Religious authority depends on direct access to Arabic-script Quran, hadith, and Ottoman jurisprudence; the reform severs the unbroken chain of textual transmission and makes religious education state-dependent through Latin-script mediation.
narrative_ontology:constraint_stakeholder(script_as_identity__kemalist_rupture_reading, religious_scholars, payer,
    organized, generational, identity_locked, national).

% Depend on state schools for literacy; receive a script disconnected from religious practice and ancestral textual memory; bear the identity cost of a literacy that cannot access the theological and historical archive without state mediation.
narrative_ontology:constraint_stakeholder(script_as_identity__kemalist_rupture_reading, rural_conservative_population, payer,
    powerless, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(script_as_identity__kemalist_rupture_reading, kemalist_state_apparatus).
narrative_ontology:fixing_cost_class(script_as_identity__kemalist_rupture_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Standardizing national literacy around a phonetically transparent alphabet, enabling mass education, administrative unification across dialects, and a single national print market.
% TRANSFER_FUNCTION: Moves textual authority and cultural capital from Ottoman-Arabic literate elites to the secular Republican state and its allied intelligentsia; transfers the population's historical and religious textual access to state-controlled mediation.
% ABSENT_VOICES: Ottoman diaspora intellectuals, Arabic-script religious authorities outside the Republic (e.g., Egyptian ulema), and non-Muslim minorities whose pre-existing Latin/Greek/Armenian literacy made them spectators rather than stakeholders in the 'national' standard.
% DISAPPEARANCE_RATIONALE: If the Latin-script requirement vanished overnight, the state's monopoly on textual authority would collapse, the Republican legal and educational archive would require reinterpretation, and the Arabic-script historical and religious textual continuity would immediately reassert alternative access channels; the entire nationalist literacy apparatus would unravel.
% FOUNDING_PROBLEM: Low literacy rates and administrative inefficiency under the Ottoman Arabic script; the need for rapid mass mobilization and nation-building in a post-imperial context; diglossia between high Ottoman and spoken Turkish.
% FOUNDING_PROBLEM_CORROBORATION: Kemalist state historians and educational institutions attest to the literacy crisis. Foreign observers and Ottoman-era educational records from outside the benefiting party corroborate low literacy, but dispute whether script change was the necessary remedy or whether educational investment alone would have sufficed.
narrative_ontology:disappearance_verdict(script_as_identity__kemalist_rupture_reading, world_rearranges).
narrative_ontology:founding_problem_status(script_as_identity__kemalist_rupture_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(script_as_identity__kemalist_rupture_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(script_as_identity__kemalist_rupture_reading, 'none', 1).
narrative_ontology:epsilon_provenance(script_as_identity__kemalist_rupture_reading, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(script_as_identity__kemalist_rupture_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(script_as_identity__kemalist_rupture_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(script_as_identity__kemalist_rupture_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) is high because the constraint does not merely standardize literacy but actively transfers textual authority from existing elites to the state. Suppression (0.82) reflects the legal prohibition on Arabic script in public and official contexts, active press controls, and pedagogical enforcement. Theater_ratio (0.35 at interval end, declining from 0.70) captures the performative revolutionary spectacles (Alphabet Festivals) that accompanied the reform; over time the performance declined as the new script became habitual, but the early peak was substantial. Accessibility_collapse (0.88) is very high because Arabic-script alternatives were legally and socially obliterated in the public sphere within months. Resistance (0.75) reflects sustained conservative, religious, and some Kurdish opposition, though ultimately overcome. The measurement series share a single time grid to prevent misalignment artifacts.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (kemalist_state_apparatus) experiences the constraint as necessary nation-building coordination that delivers modernity and unity; the payer seats (Ottoman elites, religious scholars, rural conservatives) experience the identical arrangement as extractive identity rupture and cultural dispossession. The engine computes this divergence from the structural data â the divergence itself is the measurement.
 *
 * DIRECTIONALITY LOGIC:
 *   The kemalist_state_apparatus and secular_nationalist_intelligentsia are structural beneficiaries: they collect ideological monopoly and cultural capital validation respectively, yielding low directionality. The Ottoman-literate elites and religious scholars are structural targets: their identity is fused with the displaced script and their exit options are identity_locked, yielding high directionality. The rural conservative population bears diffuse costs with constrained exit. The effective extraction is thus amplified for the identity-locked targets and damped for the state beneficiaries.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling by requiring both genuine coordination and asymmetric extraction for tangled_rope. Here, the coordination (mass literacy, phonetic transparency) is real and documented; the extraction (elite dispossession, state monopoly on textual authority) is equally real and structurally distinct. A snare classification would fail because the coordination function is not cover â the literacy gains were substantive. A rope classification would fail because the asymmetric victim set (old elites, religious scholars) is structurally necessary to the state's monopoly, not an accidental side effect.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    zero_cost_claim_ambiguity,
    'Does the Kemalist reading''s claim of zero transition cost hold, or does the displacement of Ottoman-literate elites constitute a substantial hidden cost borne by specific seats?',
    'Comparative analysis of literacy rates, publishing output, and elite career trajectories before and after 1928.',
    'If transition costs were high for incumbents, the constraint is more extractive than the reading admits; if truly zero, the reading''s framing is descriptively accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(zero_cost_claim_ambiguity, empirical, 'Whether the rupture imposed hidden transition costs on displaced elites').

omega_variable(
    rupture_severability,
    'Could the phonetic standardization and mass-literacy coordination have been achieved without the identity rupture and state monopoly components?',
    'Counterfactual analysis of other language reforms and comparison with the phonetic_instrumentalism_reading.',
    'If severable, the rupture is extractive surplus; if inseparable, the extraction is the necessary price of coordination.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(rupture_severability, conceptual, 'Whether coordination and extraction are structurally separable in this reform').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (laws banning Arabic script, press controls) or internalized (shame attached to Ottoman identity and script use in private)?',
    'Post-legal-ban persistence of Arabic-script avoidance in private and religious contexts.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests â the target carries the suppression after formal barriers ease.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(script_as_identity__kemalist_rupture_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(scri_tr_t0, script_as_identity__kemalist_rupture_reading, theater_ratio, 0, 0.7).
narrative_ontology:measurement(scri_tr_t5, script_as_identity__kemalist_rupture_reading, theater_ratio, 5, 0.55).
narrative_ontology:measurement(scri_tr_t10, script_as_identity__kemalist_rupture_reading, theater_ratio, 10, 0.45).
narrative_ontology:measurement(scri_tr_t15, script_as_identity__kemalist_rupture_reading, theater_ratio, 15, 0.4).
narrative_ontology:measurement(scri_tr_t20, script_as_identity__kemalist_rupture_reading, theater_ratio, 20, 0.38).
narrative_ontology:measurement(scri_tr_t25, script_as_identity__kemalist_rupture_reading, theater_ratio, 25, 0.36).
narrative_ontology:measurement(scri_tr_t30, script_as_identity__kemalist_rupture_reading, theater_ratio, 30, 0.35).

% Extraction over time
narrative_ontology:measurement(scri_be_t0, script_as_identity__kemalist_rupture_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(scri_be_t5, script_as_identity__kemalist_rupture_reading, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(scri_be_t10, script_as_identity__kemalist_rupture_reading, base_extractiveness, 10, 0.62).
narrative_ontology:measurement(scri_be_t15, script_as_identity__kemalist_rupture_reading, base_extractiveness, 15, 0.68).
narrative_ontology:measurement(scri_be_t20, script_as_identity__kemalist_rupture_reading, base_extractiveness, 20, 0.7).
narrative_ontology:measurement(scri_be_t25, script_as_identity__kemalist_rupture_reading, base_extractiveness, 25, 0.71).
narrative_ontology:measurement(scri_be_t30, script_as_identity__kemalist_rupture_reading, base_extractiveness, 30, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(scri_su_t0, script_as_identity__kemalist_rupture_reading, suppression_requirement, 0, 0.9).
narrative_ontology:measurement(scri_su_t5, script_as_identity__kemalist_rupture_reading, suppression_requirement, 5, 0.85).
narrative_ontology:measurement(scri_su_t10, script_as_identity__kemalist_rupture_reading, suppression_requirement, 10, 0.8).
narrative_ontology:measurement(scri_su_t15, script_as_identity__kemalist_rupture_reading, suppression_requirement, 15, 0.75).
narrative_ontology:measurement(scri_su_t20, script_as_identity__kemalist_rupture_reading, suppression_requirement, 20, 0.72).
narrative_ontology:measurement(scri_su_t25, script_as_identity__kemalist_rupture_reading, suppression_requirement, 25, 0.7).
narrative_ontology:measurement(scri_su_t30, script_as_identity__kemalist_rupture_reading, suppression_requirement, 30, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(script_as_identity__kemalist_rupture_reading, information_standard).
narrative_ontology:affects_constraint(script_as_identity__kemalist_rupture_reading, ottoman_continuity_reading).
narrative_ontology:affects_constraint(script_as_identity__kemalist_rupture_reading, phonetic_instrumentalism_reading).

% DUAL FORMULATION NOTE:
% This constraint is the kemalist_rupture_reading of the script_as_identity kernel. Sibling readings include the ottoman_continuity_reading (Arabic script as constitutive of Turkish-Islamic identity) and the phonetic_instrumentalism_reading (script as neutral phonetic technology). The kernel decomposes into structurally distinct claims because the epsilon values and victim/beneficiary structures differ: rupture is extractive and state-monopolistic; continuity is identity-preserving but potentially exclusionary; instrumentalism is low-extraction coordination. Each reading is a separate constraint story linked through this network family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
