% ============================================================================
% CONSTRAINT STORY: jewish_territorial_claim__revisionist_zionism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:constraint_vindicates/2,
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
 *   human_readable: Revisionist Zionist Maximalist Territorial Claim (Both Banks) and the Iron Wall Doctrine
 *   domain: political_history/settler_colonialism/nationalism_studies
 *
 * SUMMARY:
 *   Vladimir Jabotinsky's Revisionist Zionism, formalized in his 1923 essay
 *   'The Iron Wall,' broke from mainstream Zionist strategy by declaring that
 *   Arab opposition to Jewish immigration and sovereignty was rational,
 *   permanent, and could never be dissolved through goodwill, economic
 *   partnership, or partial concession. The doctrine held that Zionism must
 *   therefore build an 'iron wall' of Jewish military strength sufficiently
 *   absolute that Arab leadership would abandon resistance not because it
 *   accepted Zionism's legitimacy but because resistance became hopeless. The
 *   territorial program attached to this doctrine was maximalist: sovereignty
 *   over the entirety of Mandate Palestine AND Transjordan (both banks of the
 *   Jordan River), a claim exceeding even the Mandate's own boundaries and
 *   considerably exceeding the territorial scope most Labor and political
 *   Zionist leaders were prepared to pursue. The Irgun and later Lehi
 *   operationalized elements of this program through armed action against
 *   British administration and Arab civilian and political targets; the
 *   doctrine's political heirs (Herut, later Likud) carried the maximalist
 *   territorial claim into Israeli politics after 1948, particularly
 *   regarding the West Bank after 1967.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_territorial_claim__revisionist_zionism_reading, 0.81).
domain_priors:suppression_score(jewish_territorial_claim__revisionist_zionism_reading, 0.87).
domain_priors:theater_ratio(jewish_territorial_claim__revisionist_zionism_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_territorial_claim__revisionist_zionism_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(jewish_territorial_claim__revisionist_zionism_reading, suppression_requirement, 0.87).
narrative_ontology:constraint_metric(jewish_territorial_claim__revisionist_zionism_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_territorial_claim__revisionist_zionism_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(jewish_territorial_claim__revisionist_zionism_reading, resistance, 0.88).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_territorial_claim__revisionist_zionism_reading, tangled_rope).
narrative_ontology:human_readable(jewish_territorial_claim__revisionist_zionism_reading, "Revisionist Zionist Maximalist Territorial Claim (Both Banks) and the Iron Wall Doctrine").
narrative_ontology:topic_domain(jewish_territorial_claim__revisionist_zionism_reading, "political_history/settler_colonialism/nationalism_studies").

domain_priors:requires_active_enforcement(jewish_territorial_claim__revisionist_zionism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_territorial_claim__revisionist_zionism_reading, 'd11d6ddb-4f04-4c8f-956d-26fe5ff80f71').
narrative_ontology:cs_kernel_codification('d11d6ddb-4f04-4c8f-956d-26fe5ff80f71', distributed).
narrative_ontology:cs_authority_grounding('d11d6ddb-4f04-4c8f-956d-26fe5ff80f71', practice).
narrative_ontology:cs_interpretation_layer_present('d11d6ddb-4f04-4c8f-956d-26fe5ff80f71').
narrative_ontology:cs_reading_relation('d11d6ddb-4f04-4c8f-956d-26fe5ff80f71', jewish_territorial_claim__political_zionism_reading, influences).
narrative_ontology:cs_reading_relation('d11d6ddb-4f04-4c8f-956d-26fe5ff80f71', jewish_territorial_claim__labor_zionism_reading, influences).
narrative_ontology:cs_reading_relation('d11d6ddb-4f04-4c8f-956d-26fe5ff80f71', jewish_territorial_claim__cultural_zionism_reading, coexists_with).
narrative_ontology:cs_axiom('d11d6ddb-4f04-4c8f-956d-26fe5ff80f71', foundational, arab_consent_not_a_precondition).
narrative_ontology:cs_axiom_status(arab_consent_not_a_precondition, holdable).
narrative_ontology:cs_axiom_grounding('d11d6ddb-4f04-4c8f-956d-26fe5ff80f71', arab_consent_not_a_precondition, instrumental).
narrative_ontology:cs_axiom('d11d6ddb-4f04-4c8f-956d-26fe5ff80f71', foundational, territorial_maximalism_both_banks_non_negotiable).
narrative_ontology:cs_axiom_status(territorial_maximalism_both_banks_non_negotiable, holdable).
narrative_ontology:cs_axiom_grounding('d11d6ddb-4f04-4c8f-956d-26fe5ff80f71', territorial_maximalism_both_banks_non_negotiable, conventional).
narrative_ontology:cs_axiom('d11d6ddb-4f04-4c8f-956d-26fe5ff80f71', secondary, military_force_as_primary_not_fallback_mechanism).
narrative_ontology:cs_axiom_status(military_force_as_primary_not_fallback_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('d11d6ddb-4f04-4c8f-956d-26fe5ff80f71', military_force_as_primary_not_fallback_mechanism, instrumental).
narrative_ontology:cs_reference_frame('d11d6ddb-4f04-4c8f-956d-26fe5ff80f71', iron_wall_force_primacy_doctrine).
narrative_ontology:cs_drift_state('d11d6ddb-4f04-4c8f-956d-26fe5ff80f71', post_1948_statehood_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('d11d6ddb-4f04-4c8f-956d-26fe5ff80f71', '').
narrative_ontology:cs_kernel_id(jewish_territorial_claim__revisionist_zionism_reading, jewish_territorial_claim).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__revisionist_zionism_reading, revisionist_zionist_movement).
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__revisionist_zionism_reading, irgun_and_successor_militias).
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__revisionist_zionism_reading, future_jewish_state_maximalist_faction).
narrative_ontology:constraint_victim(jewish_territorial_claim__revisionist_zionism_reading, arab_population_of_mandate_palestine).
narrative_ontology:constraint_victim(jewish_territorial_claim__revisionist_zionism_reading, transjordanian_arab_population).
narrative_ontology:constraint_victim(jewish_territorial_claim__revisionist_zionism_reading, moderate_zionist_factions_seeking_partition).
narrative_ontology:constraint_vindicates(jewish_territorial_claim__revisionist_zionism_reading, jewish_national_self_determination_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Articulates the doctrine (Jabotinsky's 'Iron Wall') that Arab acceptance of Jewish sovereignty will never be won by persuasion or partial concession, only by an unbreakable wall of military force that removes the possibility of Arab veto. Sets the political program: sovereignty over the whole of Mandate Palestine and Transjordan as a non-negotiable starting position, not an opening bid. Builds and directs paramilitary capacity (Irgun and successors) to enforce this position against both Arab resistance and rival Zionist factions willing to accept partition.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__revisionist_zionism_reading, revisionist_zionist_movement, agenda_setter,
    organized, generational, mobile, regional).
narrative_ontology:stakeholder_secondary_role(jewish_territorial_claim__revisionist_zionism_reading, revisionist_zionist_movement, beneficiary).

% Operational arm that carries out the Iron Wall doctrine through armed force, targeting British administration and Arab population centers to make partition or compromise politically costly. Members' identities and social standing are constituted through militant commitment to the maximalist program; leaving the movement means abandoning both the cause and the community that gives their action meaning.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__revisionist_zionism_reading, irgun_and_successor_militias, agenda_setter,
    organized, biographical, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(jewish_territorial_claim__revisionist_zionism_reading, irgun_and_successor_militias, beneficiary).

% Majority population of the territory claimed. The doctrine explicitly denies that their consent, demographic weight, or political preferences are a relevant constraint on the territorial claim; the described function of military force is precisely to make their acceptance irrelevant to the outcome. Displacement, conflict, and later dispossession are counted by the doctrine's own logic as necessary costs of erecting the wall, not as failures of it.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__revisionist_zionism_reading, arab_population_of_mandate_palestine, payer,
    powerless, generational, trapped, regional).

% Population east of the Jordan River whose territory is claimed as part of the 'both banks' maximalist program, a claim that goes beyond even the mainstream Zionist territorial ambition of the Mandate-era west bank. They are not party to any negotiation contemplated by the doctrine; their land is claimed as birthright regardless of their presence or political institutions.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__revisionist_zionism_reading, transjordanian_arab_population, payer,
    powerless, generational, trapped, regional).

% Labor Zionist and mainstream political Zionist leadership pursuing negotiated partition, British mediation, or phased settlement toward a Jewish-majority state on part of the territory. The maximalist claim and its armed enforcement undercut their negotiating position with both the British Mandate authority and Arab interlocutors, and its violence is used by opponents to discredit the wider Zionist project. They pay a reputational and strategic cost for a program they did not choose and often actively opposed.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__revisionist_zionism_reading, moderate_zionist_factions_seeking_partition, payer,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(jewish_territorial_claim__revisionist_zionism_reading, moderate_zionist_factions_seeking_partition, excluded).

% Formal administering power whose partition proposals and gradualist policy are treated by the doctrine as obstacles to be overcome by force rather than authorities to be persuaded. Absorbs the violence directed at its administration as a cost of the maximalist program's rejection of any negotiated, incremental path.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__revisionist_zionism_reading, british_mandate_authority, excluded,
    institutional, biographical, constrained, regional).

% Assess the doctrine's stated logic, its operational history through the Irgun and later Herut/Likud successor politics, and its consequences for the shape of the eventual state, the 1948 displacement, and ongoing territorial conflict. Draw on internal movement writings (Jabotinsky's own essays), British administrative records, and Arab political sources external to the Zionist movement.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__revisionist_zionism_reading, historians_of_zionism, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jewish_territorial_claim__revisionist_zionism_reading, revisionist_zionist_movement).
narrative_ontology:fixing_cost_class(jewish_territorial_claim__revisionist_zionism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a faction of the Jewish national movement around a single non-negotiable objective and a matching military strategy, resolving internal Zionist disagreement over strategy (gradualism vs. immediate maximalism) by asserting that only overwhelming, unambiguous force can secure any Jewish sovereignty claim at all — a genuine coordination problem for a minority population seeking self-determination against both an imperial administrator and a numerical majority.
% TRANSFER_FUNCTION: Moves territorial control, political voice, and physical security from the Arab population of Palestine and Transjordan to the future Jewish sovereign entity the doctrine aims to establish, using organized force as the transfer mechanism rather than negotiation, purchase, or demographic majority-building.
% ABSENT_VOICES: The Arab population whose consent the doctrine explicitly declares irrelevant is definitionally excluded from the negotiation the doctrine contemplates — there is no seat for them in the doctrine's own logic, only a wall to be built against their resistance. Labor and political Zionist leaders arguing for partition are present in the historical record but structurally overridden by the doctrine's rejection of compromise.
% DISAPPEARANCE_RATIONALE: Had the maximalist claim and Iron Wall doctrine never taken hold within the Zionist movement, the mainstream trajectory toward negotiated partition (UN 1947 plan lineage) would have faced less internal paramilitary competition and less discrediting violence attributable to the wider movement; the territorial scope ultimately contested (west bank only vs. both banks) and the role of armed force as a stated first-resort strategy would both look different. The doctrine's absence does not make Arab-Jewish conflict disappear, but it removes a specific accelerant that foreclosed compromise as even a nominal starting position for one major faction.
% FOUNDING_PROBLEM: Early Zionist leaders faced a minority population attempting national self-determination in a territory with an Arab majority, under an ambivalent imperial administrator, with no realistic prospect that voluntary Arab consent to Jewish sovereignty would be forthcoming through negotiation alone. Jabotinsky's founding claim was that this asymmetry made moderation self-defeating: only a demonstrated, permanent wall of force could ever produce Arab acceptance of a fait accompli.
% FOUNDING_PROBLEM_CORROBORATION: The Revisionist movement and its Herut/Likud successors continue to attest that force-first maximalism was vindicated by 1948 and subsequent conflicts, and that the founding problem (absence of consensual Arab acceptance) remains live today. Labor Zionist and political Zionist historians, British Mandate administrative records, and Palestinian and broader Arab historiography attest, from outside the movement's own ranks, that the doctrine's rejection of any negotiated ceiling on territorial claims foreclosed viable partition paths and that its stated founding logic served as much to justify an already-desired territorial maximalism as to solve a genuinely intractable consent problem.
narrative_ontology:disappearance_verdict(jewish_territorial_claim__revisionist_zionism_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_territorial_claim__revisionist_zionism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_territorial_claim__revisionist_zionism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(jewish_territorial_claim__revisionist_zionism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_territorial_claim__revisionist_zionism_reading, 0.81, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored high (0.81 by 1948) and rising across the interval because the doctrine's own stated mechanism — force sufficient to make Arab consent irrelevant — is by construction a transfer of political and territorial control achieved against the preference of the affected majority population, not a negotiated allocation. Suppression is authored even higher (0.87) because the doctrine requires not merely defending a position but actively imposing a fait accompli through organized violence and rejecting any negotiated ceiling that would create an exit for Arab or moderate Zionist alternatives. Theater ratio is kept low (0.22) throughout: this is not a performative constraint — the Irgun's operations and the doctrine's political program were substantively pursued, not symbolically maintained. Accessibility collapse is moderate (0.62) rather than near-total because, historically, the doctrine never achieved uncontested dominance within the Zionist movement — Labor Zionism and political Zionism remained live, better-resourced alternatives throughout the Mandate period, and British and later UN partition frameworks persisted as institutional alternatives the doctrine sought to override but never fully closed off. Resistance is authored very high (0.88) reflecting both Arab political and armed resistance and internal Zionist opposition from Labor and mainstream political factions who viewed the maximalist program as strategically catastrophic.
 *
 * DIRECTIONALITY LOGIC:
 *   The Revisionist movement and its paramilitary arm are the clear structural beneficiaries and agenda-setters: they author the doctrine, direct its enforcement, and their political program is what the arrangement exists to advance. The Arab population of both Mandate Palestine and Transjordan are the primary targets — the doctrine's explicit content is that their consent is not a constraint to be satisfied but an obstacle to be overcome, placing them at the full-target end of directionality regardless of any exit option, because the doctrine denies them a negotiating seat in the first place (trapped, not merely constrained). Moderate Zionist factions occupy an unusual position: nominally aligned with the broader Zionist project (a partial beneficiary of eventual statehood) but structurally paying a cost imposed by a rival faction's tactics — their exit options are constrained rather than trapped because they retain political voice within the movement, but that voice is degraded by association with violence they did not choose.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — that Arab consent to Jewish sovereignty would not be forthcoming through negotiation — was genuinely live in the Mandate period and remains contested rather than dead; this is why founding_problem_status is authored 'contested' rather than 'dead.' The classification as tangled_rope (rather than pure snare) reflects that the doctrine did solve a real coordination problem internal to the Zionist movement — resolving strategic paralysis among a minority population with no obvious path to secure self-determination — while simultaneously requiring, by its own explicit terms, asymmetric extraction from the Arab population whose consent it declared irrelevant. Collapsing this into either 'pure coordination' (ignoring the declared victims) or 'pure extraction' (ignoring the genuine internal coordination function for a minority facing real strategic constraints) would mislabel the structure. The tangled_rope classification requires both beneficiary and victim declarations plus active enforcement — all three are present and are the story's central content, not incidental detail.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    iron_wall_necessity_vs_choice,
    'Was the Iron Wall doctrine''s rejection of negotiated consent a necessary response to genuinely irreconcilable national claims, or a strategic choice among live alternatives (partition, federation, binationalism) that were foreclosed by the doctrine''s own maximalism rather than by structural necessity?',
    'Comparative historical analysis of contemporaneous negotiated alternatives (Peel Commission proposals, binational federation proposals from figures like Judah Magnes) and assessment of whether they had genuine political viability that the Revisionist program''s ascendance foreclosed.',
    'If Iron Wall logic reflects genuine irreconcilability, the doctrine is closer to a tragic-necessity reading with lower excess extraction beyond the coordination problem itself. If viable negotiated alternatives existed and were foreclosed specifically by this faction''s maximalism and violence, the extraction is substantially the doctrine''s own contribution rather than an inherited structural condition.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(iron_wall_necessity_vs_choice, conceptual, 'Whether the doctrine''s zero-consent premise reflects necessity or foreclosed alternatives.').

omega_variable(
    kernel_reading_disagreement_location,
    'Where exactly does this reading diverge from the political_zionism_reading, given both ultimately endorse Jewish territorial sovereignty? Is the disagreement about the END STATE (both banks vs. Mandate Palestine with partition acceptable) or about the MEANS (force as primary vs. fallback mechanism), or both simultaneously and inseparably?',
    'Close textual comparison of Jabotinsky''s programmatic writings against Weizmann/Ben-Gurion era political Zionist platforms and negotiating positions at Peel Commission and later UNSCOP hearings.',
    'If the disagreement is primarily about means with convergent ends, the revisionist and political readings might be closer to an ''influences'' relationship (tactical pressure) than fully independent readings. If it is about ends (territorial scope) AND means (force-first vs. negotiation-first) simultaneously, they are more properly independent coexisting readings, as authored here.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_location, conceptual, 'Locating precisely where the revisionist reading structurally diverges from the political Zionist reading.').

omega_variable(
    post_1948_doctrinal_continuity,
    'Does the founding_problem_status ''contested'' rating hold uniformly across the full historical arc, or did the doctrine''s founding problem (absence of Arab consent) transform in character after 1948 statehood was achieved and again after 1967 territorial expansion, such that the ''both banks'' maximalism became a different claim (retention vs. acquisition) requiring separate treatment?',
    'Track the doctrine''s political heirs (Herut, Likud) and whether their post-1948 and post-1967 territorial claims maintain the same structural logic (force substituting for consent) or shift to a different constraint (administered-territory governance) that would warrant its own separate constraint story per the ε-invariance principle.',
    'If the post-1967 West Bank settlement project constitutes a structurally distinct claim (different beneficiary/victim configuration, different enforcement mechanism), it should be decomposed into a separate linked constraint story rather than treated as continuous with the Mandate-era doctrine authored here.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(post_1948_doctrinal_continuity, empirical, 'Whether doctrinal continuity across 1948 and 1967 justifies treating later territorial claims as the same constraint or a distinct family member.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_territorial_claim__revisionist_zionism_reading, 1923, 1948).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t1923, jewish_territorial_claim__revisionist_zionism_reading, theater_ratio, 1923, 0.12).
narrative_ontology:measurement(jewi_tr_t1929, jewish_territorial_claim__revisionist_zionism_reading, theater_ratio, 1929, 0.14).
narrative_ontology:measurement(jewi_tr_t1935, jewish_territorial_claim__revisionist_zionism_reading, theater_ratio, 1935, 0.17).
narrative_ontology:measurement(jewi_tr_t1937, jewish_territorial_claim__revisionist_zionism_reading, theater_ratio, 1937, 0.18).
narrative_ontology:measurement(jewi_tr_t1943, jewish_territorial_claim__revisionist_zionism_reading, theater_ratio, 1943, 0.2).
narrative_ontology:measurement(jewi_tr_t1948, jewish_territorial_claim__revisionist_zionism_reading, theater_ratio, 1948, 0.22).

% Extraction over time
narrative_ontology:measurement(jewi_be_t1923, jewish_territorial_claim__revisionist_zionism_reading, base_extractiveness, 1923, 0.55).
narrative_ontology:measurement(jewi_be_t1929, jewish_territorial_claim__revisionist_zionism_reading, base_extractiveness, 1929, 0.62).
narrative_ontology:measurement(jewi_be_t1935, jewish_territorial_claim__revisionist_zionism_reading, base_extractiveness, 1935, 0.68).
narrative_ontology:measurement(jewi_be_t1937, jewish_territorial_claim__revisionist_zionism_reading, base_extractiveness, 1937, 0.72).
narrative_ontology:measurement(jewi_be_t1943, jewish_territorial_claim__revisionist_zionism_reading, base_extractiveness, 1943, 0.77).
narrative_ontology:measurement(jewi_be_t1948, jewish_territorial_claim__revisionist_zionism_reading, base_extractiveness, 1948, 0.81).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t1923, jewish_territorial_claim__revisionist_zionism_reading, suppression_requirement, 1923, 0.58).
narrative_ontology:measurement(jewi_su_t1929, jewish_territorial_claim__revisionist_zionism_reading, suppression_requirement, 1929, 0.66).
narrative_ontology:measurement(jewi_su_t1935, jewish_territorial_claim__revisionist_zionism_reading, suppression_requirement, 1935, 0.74).
narrative_ontology:measurement(jewi_su_t1937, jewish_territorial_claim__revisionist_zionism_reading, suppression_requirement, 1937, 0.79).
narrative_ontology:measurement(jewi_su_t1943, jewish_territorial_claim__revisionist_zionism_reading, suppression_requirement, 1943, 0.83).
narrative_ontology:measurement(jewi_su_t1948, jewish_territorial_claim__revisionist_zionism_reading, suppression_requirement, 1948, 0.87).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_territorial_claim__revisionist_zionism_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(jewish_territorial_claim__revisionist_zionism_reading, political_zionism_reading).
narrative_ontology:affects_constraint(jewish_territorial_claim__revisionist_zionism_reading, labor_zionism_reading).
narrative_ontology:affects_constraint(jewish_territorial_claim__revisionist_zionism_reading, cultural_zionism_reading).

% DUAL FORMULATION NOTE:
% This story is one of four sibling readings of the jewish_territorial_claim kernel, decomposed per the ε-invariance principle because the natural-language label 'Zionism's territorial claim' conflates structurally distinct positions with different ε values, different beneficiary/victim structures, and different classifications. The revisionist_zionism_reading (this story) is authored as tangled_rope with high ε (0.81) because it explicitly makes force the primary mechanism and rejects consent as a prerequisite. The political_zionism_reading is expected to show a lower ε reflecting its acceptance of negotiated, partial-territory outcomes. The labor_zionism_reading is expected to emphasize settlement and economic 'facts on ground' over declared military maximalism, likely yielding a different beneficiary/victim configuration (land purchase and settlement displacement rather than declared conquest). The cultural_zionism_reading, lacking a sovereignty/territorial claim at all, is expected to classify closer to rope or scaffold with minimal victim structure. All four readings share the same underlying kernel (Jewish national return to Palestine) but diverge sharply on whether consent is required, what territorial scope is claimed, and what mechanism secures the claim — exactly the kind of observable-dependent divergence the ε-invariance principle requires decomposing into separate stories rather than averaging within one.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
