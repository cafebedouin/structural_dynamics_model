% ============================================================================
% CONSTRAINT STORY: jewish_territorial_claim__revisionist_zionism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_territorial_claim__revisionist_zionism_reading, []).

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
 *   constraint_id: jewish_territorial_claim__revisionist_zionism_reading
 *   human_readable: Revisionist Zionist Maximalist Territorial Claim ('Iron Wall' Doctrine)
 *   domain: political_history/settler_colonialism/nationalism_studies
 *
 * SUMMARY:
 *   This constraint models the Revisionist Zionist reading of the contested
 *   Jewish territorial claim kernel: Jabotinsky's doctrine of maximalist
 *   sovereignty over both banks of the Jordan, explicitly premised on
 *   rejecting Arab consent as a prerequisite and on building an 'Iron Wall'
 *   of Jewish military strength to compel eventual Arab acceptance of a fait
 *   accompli. This is one of four sibling readings of the same underlying
 *   kernel (cultural, labor, political, and revisionist Zionism); each
 *   reading is authored as its own ε-invariant constraint per the
 *   decomposition principle, since the readings diverge sharply on consent,
 *   territorial scope, and mechanism, not merely on emphasis. This story
 *   covers only the revisionist reading — the others are separate constraint
 *   files linked via network.affects_constraints.
 *
 * KEY AGENTS:
 *   - revisionist_zionist_movement: agenda_setter (organized/mobile) — sets and propagates the maximalist doctrine
 *   - irgun_and_etzel_militants: agenda_setter/beneficiary (organized/identity_locked) — operationalizes the doctrine through armed force
 *   - palestinian_arab_population: primary payer (powerless/trapped) — bears displacement and coercion as the doctrine's mechanism
 *   - transjordan_arab_residents: payer (powerless/trapped) — claimed territory's residents excluded from consent
 *   - labor_zionist_negotiating_faction: payer/excluded (organized/constrained) — rival strategy undercut by the doctrine's intransigence
 *   - british_mandatory_authority: observer/excluded (institutional/constrained) — governing authority whose partition efforts the doctrine treats as illegitimate
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_territorial_claim__revisionist_zionism_reading, 0.82).
domain_priors:suppression_score(jewish_territorial_claim__revisionist_zionism_reading, 0.88).
domain_priors:theater_ratio(jewish_territorial_claim__revisionist_zionism_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_territorial_claim__revisionist_zionism_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(jewish_territorial_claim__revisionist_zionism_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(jewish_territorial_claim__revisionist_zionism_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_territorial_claim__revisionist_zionism_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(jewish_territorial_claim__revisionist_zionism_reading, resistance, 0.9).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_territorial_claim__revisionist_zionism_reading, tangled_rope).
narrative_ontology:human_readable(jewish_territorial_claim__revisionist_zionism_reading, "Revisionist Zionist Maximalist Territorial Claim ('Iron Wall' Doctrine)").
narrative_ontology:topic_domain(jewish_territorial_claim__revisionist_zionism_reading, "political_history/settler_colonialism/nationalism_studies").

domain_priors:requires_active_enforcement(jewish_territorial_claim__revisionist_zionism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_territorial_claim__revisionist_zionism_reading, 'd42c5a4e-b4b5-4c82-ad99-b78ca6a7ee50').
narrative_ontology:cs_kernel_codification('d42c5a4e-b4b5-4c82-ad99-b78ca6a7ee50', distributed).
narrative_ontology:cs_authority_grounding('d42c5a4e-b4b5-4c82-ad99-b78ca6a7ee50', practice).
narrative_ontology:cs_interpretation_layer_present('d42c5a4e-b4b5-4c82-ad99-b78ca6a7ee50').
narrative_ontology:cs_reading_relation('d42c5a4e-b4b5-4c82-ad99-b78ca6a7ee50', jewish_territorial_claim__cultural_zionism_reading, coexists_with).
narrative_ontology:cs_reading_relation('d42c5a4e-b4b5-4c82-ad99-b78ca6a7ee50', jewish_territorial_claim__labor_zionism_reading, influences).
narrative_ontology:cs_reading_relation('d42c5a4e-b4b5-4c82-ad99-b78ca6a7ee50', jewish_territorial_claim__political_zionism_reading, influences).
narrative_ontology:cs_axiom('d42c5a4e-b4b5-4c82-ad99-b78ca6a7ee50', foundational, arab_consent_not_a_precondition).
narrative_ontology:cs_axiom_status(arab_consent_not_a_precondition, holdable).
narrative_ontology:cs_axiom_grounding('d42c5a4e-b4b5-4c82-ad99-b78ca6a7ee50', arab_consent_not_a_precondition, instrumental).
narrative_ontology:cs_axiom('d42c5a4e-b4b5-4c82-ad99-b78ca6a7ee50', foundational, military_force_as_primary_legitimating_mechanism).
narrative_ontology:cs_axiom_status(military_force_as_primary_legitimating_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('d42c5a4e-b4b5-4c82-ad99-b78ca6a7ee50', military_force_as_primary_legitimating_mechanism, instrumental).
narrative_ontology:cs_axiom('d42c5a4e-b4b5-4c82-ad99-b78ca6a7ee50', secondary, territorial_scope_both_banks_non_negotiable).
narrative_ontology:cs_axiom_status(territorial_scope_both_banks_non_negotiable, overridden).
narrative_ontology:cs_axiom_grounding('d42c5a4e-b4b5-4c82-ad99-b78ca6a7ee50', territorial_scope_both_banks_non_negotiable, conventional).
narrative_ontology:cs_reference_frame('d42c5a4e-b4b5-4c82-ad99-b78ca6a7ee50', biblical_and_historical_land_of_israel_boundaries).
narrative_ontology:cs_drift_state('d42c5a4e-b4b5-4c82-ad99-b78ca6a7ee50', post_1948_state_formation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d42c5a4e-b4b5-4c82-ad99-b78ca6a7ee50', '').
narrative_ontology:cs_kernel_id(jewish_territorial_claim__revisionist_zionism_reading, jewish_territorial_claim).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__revisionist_zionism_reading, revisionist_zionist_movement).
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__revisionist_zionism_reading, future_jewish_state_maximalists).
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__revisionist_zionism_reading, irgun_and_etzel_militants).
narrative_ontology:constraint_victim(jewish_territorial_claim__revisionist_zionism_reading, palestinian_arab_population).
narrative_ontology:constraint_victim(jewish_territorial_claim__revisionist_zionism_reading, transjordan_arab_residents).
narrative_ontology:constraint_victim(jewish_territorial_claim__revisionist_zionism_reading, labor_zionist_negotiating_faction).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Led by Ze'ev Jabotinsky and successors, this movement sets the doctrine: sovereignty over both banks of the Jordan is non-negotiable, Arab consent is not a precondition, and an 'Iron Wall' of Jewish military strength must be built to force Arab acceptance of the fait accompli. It organizes militias, publishes the doctrine openly, and treats compromise proposals from other Zionist factions as betrayals of the national claim.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__revisionist_zionism_reading, revisionist_zionist_movement, agenda_setter,
    organized, generational, mobile, regional).

% Settlers and political constituencies who benefit from a doctrine promising immediate, unqualified sovereignty over the full claimed territory rather than a negotiated partition. They gain ideological certainty and territorial ambition at the cost of accepting perpetual armed conflict as the mechanism of achievement.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__revisionist_zionism_reading, future_jewish_state_maximalists, beneficiary,
    moderate, generational, constrained, regional).

% Paramilitary organizations that operationalize the Iron Wall doctrine through armed action, both defending and expanding settlement by force. Their institutional identity and purpose are constituted by the doctrine's premise that force, not negotiation, is the legitimate path to sovereignty; abandoning the doctrine would dissolve their reason for existing.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__revisionist_zionism_reading, irgun_and_etzel_militants, agenda_setter,
    organized, biographical, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(jewish_territorial_claim__revisionist_zionism_reading, irgun_and_etzel_militants, beneficiary).

% The population whose consent the doctrine explicitly declares unnecessary and whose resistance the Iron Wall is designed to break by force rather than address by negotiation. They bear displacement, land loss, and armed suppression as the direct mechanism by which the claim is to be realized; the doctrine offers them no political standing until they capitulate.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__revisionist_zionism_reading, palestinian_arab_population, payer,
    powerless, generational, trapped, regional).

% Residents of the eastern bank of the Jordan explicitly claimed under the maximalist doctrine, which asserts sovereignty over territory they inhabit without any mechanism for their consent or representation. Their situation is more theoretical than the western-bank population's in practice, but the doctrine's textual claim extends coercive intent to them as well.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__revisionist_zionism_reading, transjordan_arab_residents, payer,
    powerless, generational, trapped, regional).

% Rival Zionist faction pursuing gradual settlement and negotiated accommodation with Arab political actors. The Revisionist doctrine's rejection of compromise undercuts their negotiating position by hardening Arab distrust of Zionism as a whole and by making any Jewish concession appear as weakness relative to the maximalist rival within the same movement.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__revisionist_zionism_reading, labor_zionist_negotiating_faction, payer,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(jewish_territorial_claim__revisionist_zionism_reading, labor_zionist_negotiating_faction, excluded).

% The mandate power nominally administering the territory, forced to respond to escalating armed conflict driven partly by the Iron Wall strategy. It attempts partition and restriction policies that the doctrine treats as illegitimate obstacles rather than negotiating partners, and its own authority is progressively undermined by the strategy's success at making the territory ungovernable by compromise.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__revisionist_zionism_reading, british_mandatory_authority, observer,
    institutional, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(jewish_territorial_claim__revisionist_zionism_reading, british_mandatory_authority, excluded).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jewish_territorial_claim__revisionist_zionism_reading, revisionist_zionist_movement).
narrative_ontology:fixing_cost_class(jewish_territorial_claim__revisionist_zionism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a faction of the Jewish national movement around a single, unambiguous territorial and strategic program — removing the ambiguity and internal division that gradualist or negotiation-oriented factions tolerated, and building military capacity to make the program achievable independent of Arab cooperation.
% TRANSFER_FUNCTION: Moves land, political sovereignty, and physical security from the Arab population of Palestine and Transjordan to the future Jewish state, using organized armed force as the transfer mechanism rather than purchase, negotiation, or gradual demographic change.
% ABSENT_VOICES: The Palestinian Arab population and Transjordanian residents are the parties most directly affected and are explicitly and doctrinally excluded from any consent role — the doctrine's own text states their acceptance is to be compelled after the fact by demonstrated Jewish military strength, not sought beforehand.
% DISAPPEARANCE_RATIONALE: If the maximalist doctrine and its associated militias had not existed, the Zionist movement's political strategy would have remained dominated by Labor Zionism's gradualist, negotiation-oriented, and partition-tolerant approach; the territorial scope claimed, the willingness to reject partition compromises, and the normalization of armed force as primary tool would all have been substantially different, altering the trajectory of the 1936-1948 conflict and its territorial outcomes.
% FOUNDING_PROBLEM: The perceived failure of political and cultural Zionism's gradualist, consent-seeking, and philanthropic-settlement strategies to secure a Jewish state quickly enough against Arab demographic and political resistance, combined with the urgency created by rising European antisemitism in the interwar period.
% FOUNDING_PROBLEM_CORROBORATION: Revisionist Zionist historiography and Jabotinsky's own writings attest the founding problem (urgent Jewish statehood against a hostile demographic reality) as permanently live and vindicated by later events. Independent historians of the Mandate period and Labor Zionist contemporaries attest that the doctrine's premise — that Arab consent was categorically unobtainable and thus irrelevant — was a strategic choice rather than a settled empirical fact, and that alternative negotiated outcomes were foreclosed by the doctrine's own refusal to test them.
narrative_ontology:disappearance_verdict(jewish_territorial_claim__revisionist_zionism_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_territorial_claim__revisionist_zionism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_territorial_claim__revisionist_zionism_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(jewish_territorial_claim__revisionist_zionism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_territorial_claim__revisionist_zionism_reading, 0.82, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_territorial_claim__revisionist_zionism_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jewish_territorial_claim__revisionist_zionism_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jewish_territorial_claim__revisionist_zionism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high and rising across the interval (0.55 to 0.82) because the doctrine's core mechanism — territorial acquisition via demonstrated force rather than negotiation or purchase — intensifies from rhetorical maximalism in the 1920s to active paramilitary campaigns by the late Mandate period. Suppression is authored even higher (0.88 at endpoint) because the doctrine's persistence structurally depends on active armed enforcement against both Arab resistance and rival Zionist factions favoring compromise — this is not incidental friction but the doctrine's stated primary mechanism ('Iron Wall'). Theater ratio is kept low (0.1 to 0.2) because the doctrine is unusually candid about its coercive nature; Jabotinsky's writings explicitly reject euphemism, so little performative cover exists relative to the actual function. Accessibility collapse is moderate (0.4) because negotiated alternatives (partition, binationalism, gradualism) remained live and contested throughout the interval — the doctrine did not eliminate alternatives, it refused to pursue them. Resistance is very high (0.9), reflecting sustained Arab political and armed resistance plus internal Zionist opposition from Labor and cultural factions.
 *
 * DIRECTIONALITY LOGIC:
 *   The Revisionist movement and its paramilitary wings are the structural agenda-setters and primary beneficiaries: they author the doctrine, build the enforcement capacity, and stand to gain the claimed territory and sovereignty. Palestinian Arabs and Transjordanian residents sit at the full-target end of directionality — they are the population the doctrine's own text identifies as the object of compulsion, with no consent mechanism offered and no mobility (trapped exit options, given displacement is the mechanism itself). The Labor Zionist faction is a payer of a different kind: not physically targeted, but structurally undercut, since the maximalist doctrine forecloses their negotiating strategy and increases the costs of any compromise position within the broader Jewish national movement. The British Mandatory Authority occupies an ambivalent observer/excluded position — nominally sovereign but functionally sidelined as the doctrine treats mandate-brokered compromises as illegitimate.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (urgent need for Jewish statehood given antisemitism and doubted prospects for gradual accommodation) is authored as contested rather than resolved, because the doctrine's own proponents treat it as permanently and continuously live, while outside historians read the specific claim that consent was categorically unobtainable as a doctrinal choice rather than an empirical finding validated by testing negotiation first. This divergence is the mandatrophy signal: a founding-problem narrative asserted almost exclusively by the doctrine's own adherents, with corroboration from outside parties available only in a qualified, contested form.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    consent_rejection_as_kernel_defining_axiom,
    'Is the explicit rejection of Arab consent as a prerequisite a distinguishing structural feature of this specific reading, or does it reflect an assumption latent in the broader Zionist territorial claim kernel that other readings merely soften rhetorically without truly abandoning?',
    'Comparative textual and policy analysis across the four sibling readings: whether political and labor Zionist leadership ever conditioned territorial acquisition on affirmative Arab consent in practice, versus treating consent as aspirational rhetoric while pursuing demographic and settlement facts on the ground regardless.',
    'If other readings shared the same de facto disregard for consent despite different rhetoric, this reading''s distinctiveness narrows to mechanism (force vs. gradualism) and scope (maximal vs. partition-tolerant) rather than the consent question itself, which would reduce the axiomatic distance between readings and could shift how sharply reading_relations should be drawn.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_rejection_as_kernel_defining_axiom, conceptual, 'Whether consent-rejection is genuinely unique to this reading or a difference of candor rather than substance.').

omega_variable(
    iron_wall_as_coordination_or_pure_extraction,
    'Does the Iron Wall doctrine possess a genuine coordination function (uniting a fractured national movement around an achievable, unambiguous program) sufficient to sustain a tangled_rope classification, or is the coordination story cover for what is structurally closer to a pure extraction/conquest program (snare) directed at the Arab population?',
    'Assess whether the doctrine produced net benefits for a broad Jewish coalition beyond the militant factions (i.e., whether ordinary settlers, not just militia leadership, were net beneficiaries) versus whether the doctrine primarily served the organizational and political interests of the Revisionist leadership and paramilitary command structure.',
    'If benefits accrued narrowly to militant leadership and organizational interests rather than a broad beneficiary coalition, the classification would shift from tangled_rope toward snare, since the coordination function would be insufficiently genuine to satisfy the hybrid gate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(iron_wall_as_coordination_or_pure_extraction, empirical, 'Whether the doctrine''s coordination function is genuine enough to sustain tangled_rope versus reducing to snare.').

omega_variable(
    maximalist_claim_natural_vs_constructed,
    'Is the claim to both banks of the Jordan grounded in a historically continuous claim (biblical/historical boundaries of the Land of Israel) that Revisionist doctrine treats as pre-existing and merely asserted, or is the specific territorial maximalism (as opposed to some other historical boundary conception) itself a constructed, strategically chosen scope selected for its political and military utility in the 1920s-1940s context?',
    'Textual-historical analysis of Jabotinsky''s writings and Revisionist party platforms tracing whether the both-banks claim was treated as an inherited, fixed premise or was itself subject to internal debate and adjustment based on political circumstance (e.g., changing scope after the 1922 Transjordan partition of the Mandate).',
    'If the scope was adjusted for strategic reasons rather than treated as fixed and inherited, this weakens any framing of the claim as natural/given and reinforces its status as a constructed, contestable political program rather than an inherited absolute.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(maximalist_claim_natural_vs_constructed, conceptual, 'Whether the specific both-banks territorial scope is an inherited fixed claim or a strategically constructed one.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_territorial_claim__revisionist_zionism_reading, 1923, 1948).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t1923, jewish_territorial_claim__revisionist_zionism_reading, theater_ratio, 1923, 0.1).
narrative_ontology:measurement(jewi_tr_t1929, jewish_territorial_claim__revisionist_zionism_reading, theater_ratio, 1929, 0.12).
narrative_ontology:measurement(jewi_tr_t1935, jewish_territorial_claim__revisionist_zionism_reading, theater_ratio, 1935, 0.15).
narrative_ontology:measurement(jewi_tr_t1938, jewish_territorial_claim__revisionist_zionism_reading, theater_ratio, 1938, 0.17).
narrative_ontology:measurement(jewi_tr_t1944, jewish_territorial_claim__revisionist_zionism_reading, theater_ratio, 1944, 0.19).
narrative_ontology:measurement(jewi_tr_t1948, jewish_territorial_claim__revisionist_zionism_reading, theater_ratio, 1948, 0.2).

% Extraction over time
narrative_ontology:measurement(jewi_be_t1923, jewish_territorial_claim__revisionist_zionism_reading, base_extractiveness, 1923, 0.55).
narrative_ontology:measurement(jewi_be_t1929, jewish_territorial_claim__revisionist_zionism_reading, base_extractiveness, 1929, 0.62).
narrative_ontology:measurement(jewi_be_t1935, jewish_territorial_claim__revisionist_zionism_reading, base_extractiveness, 1935, 0.68).
narrative_ontology:measurement(jewi_be_t1938, jewish_territorial_claim__revisionist_zionism_reading, base_extractiveness, 1938, 0.74).
narrative_ontology:measurement(jewi_be_t1944, jewish_territorial_claim__revisionist_zionism_reading, base_extractiveness, 1944, 0.79).
narrative_ontology:measurement(jewi_be_t1948, jewish_territorial_claim__revisionist_zionism_reading, base_extractiveness, 1948, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t1923, jewish_territorial_claim__revisionist_zionism_reading, suppression_requirement, 1923, 0.5).
narrative_ontology:measurement(jewi_su_t1929, jewish_territorial_claim__revisionist_zionism_reading, suppression_requirement, 1929, 0.6).
narrative_ontology:measurement(jewi_su_t1935, jewish_territorial_claim__revisionist_zionism_reading, suppression_requirement, 1935, 0.7).
narrative_ontology:measurement(jewi_su_t1938, jewish_territorial_claim__revisionist_zionism_reading, suppression_requirement, 1938, 0.79).
narrative_ontology:measurement(jewi_su_t1944, jewish_territorial_claim__revisionist_zionism_reading, suppression_requirement, 1944, 0.85).
narrative_ontology:measurement(jewi_su_t1948, jewish_territorial_claim__revisionist_zionism_reading, suppression_requirement, 1948, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_territorial_claim__revisionist_zionism_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(jewish_territorial_claim__revisionist_zionism_reading, political_zionism_reading).
narrative_ontology:affects_constraint(jewish_territorial_claim__revisionist_zionism_reading, labor_zionism_reading).
narrative_ontology:affects_constraint(jewish_territorial_claim__revisionist_zionism_reading, cultural_zionism_reading).

% DUAL FORMULATION NOTE:
% This story is one of four sibling readings of the jewish_territorial_claim kernel, each authored as a separate ε-invariant constraint per the decomposition principle: cultural_zionism_reading (spiritual center, no sovereignty requirement, low extraction), labor_zionism_reading (gradual settlement via socialist nation-building, moderate extraction concentrated in land/labor displacement), political_zionism_reading (statehood via sovereignty with Jewish majority, moderate-to-high extraction contingent on demographic and diplomatic strategy), and this revisionist_zionism_reading (maximalist territory via compelled military force, highest extraction and suppression of the four due to explicit rejection of consent and reliance on armed compulsion as primary mechanism). The revisionist reading exerts downstream influence on the political and labor readings by hardening Arab distrust of the broader Zionist project and by shifting the Overton window of acceptable territorial and strategic ambition within the Jewish national movement — hence the influences edges in cs_structure.reading_relations rather than pure coexistence in every case.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
