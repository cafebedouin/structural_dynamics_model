% ============================================================================
% CONSTRAINT STORY: turkish_graphemic_substrate__ottoman_continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    narrative_ontology:affects_constraint/2,
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
 *   human_readable: Ottoman Continuity Graphemic Substrate (Arabic Script Legitimacy)
 *   domain: political_linguistics/state_formation/cultural_engineering
 *
 * SUMMARY:
 *   In the early Turkish Republic, the Kemalist language reform replaced the
 *   Arabic script with a Latin-based alphabet as part of a broader
 *   secularization and Westernization program. The ottoman_continuity_reading
 *   of the turkish_graphemic_substrate kernel resists this reform by
 *   asserting that Turkish linguistic identity is inseparable from its
 *   Ottoman-Islamic past and that the Arabic script is the only legitimate
 *   graphemic substrate. This constraint operates as a counter-hegemonic
 *   commitment system: it coordinates access to a shared sacred and literary
 *   corpus while extracting from youth who need modern Latin-script literacy
 *   for economic and state participation. It requires active
 *   enforcementâparallel religious education, underground manuscript
 *   circulation, and communal identity maintenanceâto persist against the
 *   secular state's Latinization.
 *
 * KEY AGENTS:
 *   - religious_education_establishment: Primary agenda-setter (institutional/constrained) â administers the medrese system and controls the interpretive gate to Ottoman texts.
 *   - ottoman_literacy_elites: Primary beneficiary (organized/constrained) â hold cultural capital through Arabic-script mastery.
 *   - rural_pious_communities: Secondary beneficiary (moderate/identity_locked) â experience the script as constitutive of Islamic and communal identity.
 *   - youth_seeking_modern_education: Primary target (powerless/trapped) â bear the opportunity cost of graphemic bifurcation and blocked modern channels.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(turkish_graphemic_substrate__ottoman_continuity_reading, 0.68).
domain_priors:suppression_score(turkish_graphemic_substrate__ottoman_continuity_reading, 0.75).
domain_priors:theater_ratio(turkish_graphemic_substrate__ottoman_continuity_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(turkish_graphemic_substrate__ottoman_continuity_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(turkish_graphemic_substrate__ottoman_continuity_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(turkish_graphemic_substrate__ottoman_continuity_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(turkish_graphemic_substrate__ottoman_continuity_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(turkish_graphemic_substrate__ottoman_continuity_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(turkish_graphemic_substrate__ottoman_continuity_reading, tangled_rope).
narrative_ontology:human_readable(turkish_graphemic_substrate__ottoman_continuity_reading, "Ottoman Continuity Graphemic Substrate (Arabic Script Legitimacy)").
narrative_ontology:topic_domain(turkish_graphemic_substrate__ottoman_continuity_reading, "political_linguistics/state_formation/cultural_engineering").

domain_priors:requires_active_enforcement(turkish_graphemic_substrate__ottoman_continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(turkish_graphemic_substrate__ottoman_continuity_reading, '6a51265f-cdd7-4d27-8f35-d10fd459e874').
narrative_ontology:cs_kernel_codification('6a51265f-cdd7-4d27-8f35-d10fd459e874', fixed_text).
narrative_ontology:cs_authority_grounding('6a51265f-cdd7-4d27-8f35-d10fd459e874', lineage).
narrative_ontology:cs_interpretation_layer_present('6a51265f-cdd7-4d27-8f35-d10fd459e874').
narrative_ontology:cs_reading_relation('6a51265f-cdd7-4d27-8f35-d10fd459e874', turkish_graphemic_substrate__secular_nationalist_reading, forecloses).
narrative_ontology:cs_reading_relation('6a51265f-cdd7-4d27-8f35-d10fd459e874', turkish_graphemic_substrate__gradual_transition_reading, influences).
narrative_ontology:cs_axiom('6a51265f-cdd7-4d27-8f35-d10fd459e874', foundational, ottoman_turkish_identity_continuity).
narrative_ontology:cs_axiom_status(ottoman_turkish_identity_continuity, holdable).
narrative_ontology:cs_axiom_grounding('6a51265f-cdd7-4d27-8f35-d10fd459e874', ottoman_turkish_identity_continuity, conventional).
narrative_ontology:cs_axiom('6a51265f-cdd7-4d27-8f35-d10fd459e874', foundational, arabic_script_civilizational_legitimacy).
narrative_ontology:cs_axiom_status(arabic_script_civilizational_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('6a51265f-cdd7-4d27-8f35-d10fd459e874', arabic_script_civilizational_legitimacy, deontological).
narrative_ontology:cs_reference_frame('6a51265f-cdd7-4d27-8f35-d10fd459e874', classical_ottoman_literacy_framework).
narrative_ontology:cs_drift_state('6a51265f-cdd7-4d27-8f35-d10fd459e874', early_republican_secularization_era, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('6a51265f-cdd7-4d27-8f35-d10fd459e874', '').
narrative_ontology:cs_kernel_id(turkish_graphemic_substrate__ottoman_continuity_reading, turkish_graphemic_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(turkish_graphemic_substrate__ottoman_continuity_reading, religious_education_establishment).
narrative_ontology:constraint_beneficiary(turkish_graphemic_substrate__ottoman_continuity_reading, ottoman_literacy_elites).
narrative_ontology:constraint_beneficiary(turkish_graphemic_substrate__ottoman_continuity_reading, rural_pious_communities).
narrative_ontology:constraint_victim(turkish_graphemic_substrate__ottoman_continuity_reading, youth_seeking_modern_education).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers medrese curriculum and religious instruction in Arabic script; controls the interpretive gate to the Ottoman textual tradition. Derives institutional authority and student enrollment from the continuity of Arabic-script literacy. Under sustained pressure from the secular state's Latin-script reform, it actively maintains parallel educational infrastructure.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__ottoman_continuity_reading, religious_education_establishment, agenda_setter,
    institutional, generational, constrained, national).

% Hold cultural capital and social status through mastery of Ottoman Turkish in Arabic script. Their interpretive authority, archival access, and professional standing as scribes, jurists, and poets depend on the script's continuity. The Latin reform threatens to obsolete this capital.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__ottoman_continuity_reading, ottoman_literacy_elites, beneficiary,
    organized, generational, constrained, national).

% Experience the Arabic script as the visible and sacred form of Islamic identity and Ottoman continuity. Religious practice, communal belonging, and kinship memory are fused with this graphemic form; switching to Latin script is experienced not merely as inconvenience but as civilizational and spiritual rupture.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__ottoman_continuity_reading, rural_pious_communities, beneficiary,
    moderate, generational, identity_locked, regional).

% Must acquire Arabic-script literacy to access traditional religious education or read Ottoman sources, yet are blocked from Latin-script modern educational and bureaucratic opportunities. Bear the highest opportunity cost: they cannot easily switch between scripts and are funneled into either obsolete Ottoman curricula or exclusion from state modernity.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__ottoman_continuity_reading, youth_seeking_modern_education, payer,
    powerless, biographical, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(turkish_graphemic_substrate__ottoman_continuity_reading, religious_education_establishment).
narrative_ontology:fixing_cost_class(turkish_graphemic_substrate__ottoman_continuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves access to the Ottoman literary corpus and archives; maintains intergenerational religious literacy across Anatolia; coordinates pan-Islamic identity across diverse ethnic groups through a shared sacred graphemic substrate that predates ethnic nationalism.
% TRANSFER_FUNCTION: Moves cultural authority, educational control, and interpretive monopoly from the secular modernizing state to the religious establishment and Ottoman-literacy holders; transfers the opportunity cost of graphemic incompatibility onto younger generations and families excluded from Latin-script modern channels.
% ABSENT_VOICES: Secular republican modernizers, Kemalist educational reformers, and proponents of the Latin script are structurally excluded from the legitimacy framework of this reading; they would argue for graphemic discontinuity and European alignment but are treated as outside Ottoman-Islamic civilizational continuity. Non-Muslim minorities seeking non-Ottoman linguistic identities are similarly absent from the framework.
% DISAPPEARANCE_RATIONALE: If the Arabic-script legitimacy vanished, the medrese system would lose its textual foundation, the Ottoman corpus would become inaccessible without specialized mediation, rural pious communities would experience a rupture in religious identity transmission, and the religious establishment's authority would collapseâliteracy, state identity, and communal belonging would reorganize around the Latin script.
% FOUNDING_PROBLEM: The late Ottoman Empire faced imperial disintegration under Western colonial pressure and internal ethnic nationalism; the Arabic script and Ottoman literary tradition were the primary carriers of Islamic civilizational cohesion across diverse millets and provinces.
% FOUNDING_PROBLEM_CORROBORATION: Republican historians and Kemalist educational reformers attest from outside the beneficiary set that the empire dissolved and the original cohesion problem is obsolete; European diplomatic archives and post-Ottoman nationalist historiography independently confirm the imperial framework no longer exists.
narrative_ontology:disappearance_verdict(turkish_graphemic_substrate__ottoman_continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(turkish_graphemic_substrate__ottoman_continuity_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(turkish_graphemic_substrate__ottoman_continuity_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(turkish_graphemic_substrate__ottoman_continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(turkish_graphemic_substrate__ottoman_continuity_reading, 0.68, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(turkish_graphemic_substrate__ottoman_continuity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(turkish_graphemic_substrate__ottoman_continuity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(turkish_graphemic_substrate__ottoman_continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) is high because the constraint imposes a steep opportunity cost on youth who must choose between religious/Ottoman literacy and modern state economic participation. Suppression (0.75) is higher still: the constraint cannot persist without actively suppressing the Latin script's legitimacy and maintaining parallel institutions against state pressure. Theater_ratio (0.45) is moderateâgenuine coordination (preserving a vast literary corpus) is real, but a growing share of activity is performative civilizational defense rather than textual transmission. Accessibility_collapse (0.70) is high because, within this reading's framework, Latin-script alternatives are not merely inconvenient but illegitimate. Resistance (0.80) is high because the secular republican state and modernizing social forces actively contest the constraint.
 *
 * PERSPECTIVAL GAP:
 *   The religious establishment and rural communities experience the constraint as preserving civilization and sacred identity; the engine should compute a low directionality (subsidy toward preservation) for these seats. Youth seeking modern education experience the same structure as an imposed barrier to economic and social mobility; the engine should compute a high directionality (extraction) for this seat. The divergence is structural, not perspectival error.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (religious_education_establishment, ottoman_literacy_elites, rural_pious_communities) are structurally subsidized by the constraint: it preserves their authority, capital, and identity. Their exit is constrained or identity-locked, but the constraint flows resources and legitimacy toward them (low d). Victims (youth_seeking_modern_education) are structurally targeted: the constraint blocks their access to the modern state's literacy regime and extracts biographical time and opportunity (high d).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâimperial cohesion under Ottoman-Islamic identityâis dead, yet the constraint persists and the world would still rearrange if it vanished because it has been repurposed for religious education and communal identity maintenance. The mismatch between founding_problem_status=dead and disappearance_verdict=world_rearranges signals mandatrophy (zombie/capture). This prevents mislabeling the constraint as a living scaffold or a natural rope: its original justification is obsolete, but it continues to extract and coordinate through institutional inertia and identity lock-in.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint is one reading of the turkish_graphemic_substrate kernel; what would change structurally if the secular_nationalist_reading or gradual_transition_reading were adopted instead?',
    'Comparative analysis of the three generated constraint stories in the family: compare epsilon values, beneficiary/victim sets, and directionality distributions across the readings.',
    'Adopting a sibling reading would invert the beneficiary/victim structure (Latin-script modernizers become beneficiaries, religious establishment becomes excluded/payer) and shift the coordination function from corpus preservation to European integration or managed transition.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Commit frame position within the graphemic substrate kernel').

omega_variable(
    founding_problem_obsolescence,
    'Has the constraint''s founding problem (imperial cohesion) been genuinely replaced by a live problem (religious education preservation), or is its persistence purely inertial?',
    'Historical analysis of medrese enrollment, manuscript circulation, and communal literacy rates independent of the beneficiary institutions'' self-reporting.',
    'If purely inertial, the constraint should compute toward piton; if repurposed for a live religious-education problem, it remains a tangled rope with a contested mandate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_obsolescence, empirical, 'Whether the constraint persists by mandate or by inertia').

omega_variable(
    coordination_extraction_boundary,
    'Is the preservation of the Ottoman literary corpus a genuine coordination function separable from clerical power, or is the corpus used as cover for institutional extraction?',
    'Catalog the actual textual access provided to non-elite communities versus the gatekeeping authority exercised by the religious establishment.',
    'If the corpus is genuinely accessible and widely used, a larger share of extractiveness is coordination cost; if access is elite-monopolized, the constraint is more extractive than coordinative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, conceptual, 'Separability of coordination and extraction in manuscript preservation').

omega_variable(
    suppression_mechanism_ambiguity,
    'Does the constraint''s suppression of Latin-script alternatives operate through active institutional enforcement (medrese gatekeeping, communal sanction) or through internalized cultural framing (Latin script experienced as civilizational betrayal)?',
    'Post-reform ethnographic and autobiographic evidence: if suppression declines when institutional enforcement weakens but cultural stigma persists, the mechanism is partially internalized.',
    'If internalized, the constraint''s effective suppression exceeds the institutional measure because the target carries the suppression after formal exit is technically available.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression of Latin-script alternatives').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(turkish_graphemic_substrate__ottoman_continuity_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ottoman_cont_tr_t0, turkish_graphemic_substrate__ottoman_continuity_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(ottoman_cont_tr_t5, turkish_graphemic_substrate__ottoman_continuity_reading, theater_ratio, 5, 0.3).
narrative_ontology:measurement(ottoman_cont_tr_t10, turkish_graphemic_substrate__ottoman_continuity_reading, theater_ratio, 10, 0.38).
narrative_ontology:measurement(ottoman_cont_tr_t15, turkish_graphemic_substrate__ottoman_continuity_reading, theater_ratio, 15, 0.42).
narrative_ontology:measurement(ottoman_cont_tr_t20, turkish_graphemic_substrate__ottoman_continuity_reading, theater_ratio, 20, 0.44).
narrative_ontology:measurement(ottoman_cont_tr_t25, turkish_graphemic_substrate__ottoman_continuity_reading, theater_ratio, 25, 0.45).
narrative_ontology:measurement(ottoman_cont_tr_t30, turkish_graphemic_substrate__ottoman_continuity_reading, theater_ratio, 30, 0.45).

% Extraction over time
narrative_ontology:measurement(ottoman_cont_be_t0, turkish_graphemic_substrate__ottoman_continuity_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(ottoman_cont_be_t5, turkish_graphemic_substrate__ottoman_continuity_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(ottoman_cont_be_t10, turkish_graphemic_substrate__ottoman_continuity_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(ottoman_cont_be_t15, turkish_graphemic_substrate__ottoman_continuity_reading, base_extractiveness, 15, 0.62).
narrative_ontology:measurement(ottoman_cont_be_t20, turkish_graphemic_substrate__ottoman_continuity_reading, base_extractiveness, 20, 0.65).
narrative_ontology:measurement(ottoman_cont_be_t25, turkish_graphemic_substrate__ottoman_continuity_reading, base_extractiveness, 25, 0.66).
narrative_ontology:measurement(ottoman_cont_be_t30, turkish_graphemic_substrate__ottoman_continuity_reading, base_extractiveness, 30, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(ottoman_cont_su_t0, turkish_graphemic_substrate__ottoman_continuity_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(ottoman_cont_su_t5, turkish_graphemic_substrate__ottoman_continuity_reading, suppression_requirement, 5, 0.58).
narrative_ontology:measurement(ottoman_cont_su_t10, turkish_graphemic_substrate__ottoman_continuity_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(ottoman_cont_su_t15, turkish_graphemic_substrate__ottoman_continuity_reading, suppression_requirement, 15, 0.7).
narrative_ontology:measurement(ottoman_cont_su_t20, turkish_graphemic_substrate__ottoman_continuity_reading, suppression_requirement, 20, 0.72).
narrative_ontology:measurement(ottoman_cont_su_t25, turkish_graphemic_substrate__ottoman_continuity_reading, suppression_requirement, 25, 0.74).
narrative_ontology:measurement(ottoman_cont_su_t30, turkish_graphemic_substrate__ottoman_continuity_reading, suppression_requirement, 30, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(turkish_graphemic_substrate__ottoman_continuity_reading, secular_nationalist_reading).
narrative_ontology:affects_constraint(turkish_graphemic_substrate__ottoman_continuity_reading, gradual_transition_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the turkish_graphemic_substrate kernel, which decomposes into three structurally distinct claims: ottoman_continuity_reading (Arabic script as legitimate substrate, high extraction), secular_nationalist_reading (Latin script as legitimate substrate), and gradual_transition_reading (managed dual-script transition). Each reading has different beneficiaries, victims, and epsilon values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
