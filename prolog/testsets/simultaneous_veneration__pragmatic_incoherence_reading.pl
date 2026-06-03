% ============================================================================
% CONSTRAINT STORY: simultaneous_veneration__pragmatic_incoherence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_simultaneous_veneration__pragmatic_incoherence_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: simultaneous_veneration__pragmatic_incoherence_reading
 *   human_readable: Simultaneous Veneration as Suppressed Incoherence (Pragmatic Reading)
 *   domain: religious_studies/comparative_religion/japanese_history
 *
 * SUMMARY:
 *   This constraint instantiates the pragmatic-incoherence reading of the
 *   simultaneous-veneration kernel. The reading holds that practitioners of
 *   Edo-period Japanese religion (roughly 1600–1868) maintained contradictory
 *   theological commitments — kami as independent entities versus buddhas as
 *   independent entities, yet both valid simultaneously — without resolution.
 *   The constraint was sustained by the absence of institutional enforcement
 *   pressure: no authority (Buddhist clergy, shrine priests, or political
 *   leadership) required practitioners to choose between frameworks or to
 *   defend their coherence. This lack of enforcement is not a feature of a
 *   stable coordination mechanism (as the domain-partition reading claims)
 *   but rather the suppression mechanism that kept a latent incoherence from
 *   surfacing as an irresolvable problem. When the Meiji state imposed
 *   shinbutsu-bunri (separation of kami and buddhas) in 1868, it did not
 *   rupture a functional compromise but revealed the incoherence that had
 *   been suppressed all along. From this reading's perspective, simultaneous
 *   veneration operated as a Snare: practitioners were trapped in
 *   contradiction, institutional actors (Buddhism, shrine priesthoods, state)
 *   benefited from practitioners' inability to force resolution, and the
 *   constraint was maintained through suppression of critical questioning
 *   rather than through genuine doctrinal reconciliation. The honji-suijaku
 *   theory (each kami is a manifestation of a specific buddha), which
 *   nominally resolved the contradiction, operated as performative theater —
 *   providing narrative cover without requiring actual metaphysical
 *   coherence.
 *
 * KEY AGENTS:
 *   - Practitioners (powerless/trapped): Held contradictory beliefs simultaneously; could not exit without social cost; bore the epistemic burden of incoherence.
 *   - Institutional Buddhism (institutional/arbitrage): Benefited from pragmatic coexistence; honji-suijaku allowed Buddhist expansion into shrine spaces without doctrinal conquest.
 *   - Shrine Priesthoods (institutional/arbitrage): Benefited from coexistence; maintained kami primacy in practice while accessing Buddhist doctrinal resources.
 *   - Political Authorities (institutional/arbitrage): Benefited from both-and legitimacy; could invoke Buddhist and Shinto frames depending on political need.
 *   - Doctrinal Consistency (powerless/trapped): Victim — the very concept of coherence was suppressed; no mechanism existed to resolve contradictions.
 *   - Honji-Suijaku Theory (institutional/arbitrage): Performative apparatus providing narrative reconciliation without genuine metaphysical commitment; persists through inertia.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(simultaneous_veneration__pragmatic_incoherence_reading, 0.62).
domain_priors:suppression_score(simultaneous_veneration__pragmatic_incoherence_reading, 0.72).
domain_priors:theater_ratio(simultaneous_veneration__pragmatic_incoherence_reading, 0.81).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(simultaneous_veneration__pragmatic_incoherence_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(simultaneous_veneration__pragmatic_incoherence_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(simultaneous_veneration__pragmatic_incoherence_reading, theater_ratio, 0.81).

% --- Constraint claim ---
narrative_ontology:constraint_claim(simultaneous_veneration__pragmatic_incoherence_reading, snare).
narrative_ontology:human_readable(simultaneous_veneration__pragmatic_incoherence_reading, "Simultaneous Veneration as Suppressed Incoherence (Pragmatic Reading)").
narrative_ontology:topic_domain(simultaneous_veneration__pragmatic_incoherence_reading, "religious_studies/comparative_religion/japanese_history").

domain_priors:requires_active_enforcement(simultaneous_veneration__pragmatic_incoherence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(simultaneous_veneration__pragmatic_incoherence_reading, '961517cc-af05-47a3-b16c-e136dfbd6372').
narrative_ontology:cs_kernel_codification('961517cc-af05-47a3-b16c-e136dfbd6372', fixed_text).
narrative_ontology:cs_authority_grounding('961517cc-af05-47a3-b16c-e136dfbd6372', extraction).
narrative_ontology:cs_interpretation_layer_present('961517cc-af05-47a3-b16c-e136dfbd6372').
narrative_ontology:cs_reading_relation('961517cc-af05-47a3-b16c-e136dfbd6372', simultaneous_veneration__domain_partition_reading, coexists_with).
narrative_ontology:cs_reading_relation('961517cc-af05-47a3-b16c-e136dfbd6372', simultaneous_veneration__ontological_fusion_reading, coexists_with).
narrative_ontology:cs_axiom('961517cc-af05-47a3-b16c-e136dfbd6372', foundational, incoherence_not_reconcilable_by_honji_suijaku).
narrative_ontology:cs_axiom_status(incoherence_not_reconcilable_by_honji_suijaku, holdable).
narrative_ontology:cs_axiom_grounding('961517cc-af05-47a3-b16c-e136dfbd6372', incoherence_not_reconcilable_by_honji_suijaku, empirically_contingent).
narrative_ontology:cs_axiom('961517cc-af05-47a3-b16c-e136dfbd6372', foundational, suppression_mechanism_is_absence_of_enforcement).
narrative_ontology:cs_axiom_status(suppression_mechanism_is_absence_of_enforcement, holdable).
narrative_ontology:cs_axiom_grounding('961517cc-af05-47a3-b16c-e136dfbd6372', suppression_mechanism_is_absence_of_enforcement, empirically_contingent).
narrative_ontology:cs_reference_frame('961517cc-af05-47a3-b16c-e136dfbd6372', honji_suijaku_as_genuine_reconciliation).
narrative_ontology:cs_drift_state('961517cc-af05-47a3-b16c-e136dfbd6372', meiji_shinbutsu_bunri_enforcement, gap(authority_erosion, severe, true)).
narrative_ontology:cs_created_at('961517cc-af05-47a3-b16c-e136dfbd6372', '2026-02-26T14:32:18Z').
narrative_ontology:cs_kernel_id(simultaneous_veneration__pragmatic_incoherence_reading, simultaneous_veneration).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(simultaneous_veneration__pragmatic_incoherence_reading, institutional_buddhism).
narrative_ontology:constraint_beneficiary(simultaneous_veneration__pragmatic_incoherence_reading, shrine_priesthoods).
narrative_ontology:constraint_beneficiary(simultaneous_veneration__pragmatic_incoherence_reading, political_authorities).
narrative_ontology:constraint_victim(simultaneous_veneration__pragmatic_incoherence_reading, doctrinal_consistency).
narrative_ontology:constraint_victim(simultaneous_veneration__pragmatic_incoherence_reading, practitioner_epistemic_honesty).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PRACTITIONER (SNARE) — Trapped in mutually incompatible belief structures (kami as kami, buddhas as buddhas, yet both simultaneously valid) with no resolution mechanism. Cannot exit without social penalty. Suppression operates through lack of critical questioning — doctrinal contradiction is never openly addressed. Maximum experienced extraction from the necessity to hold incoherent commitments.
constraint_indexing:constraint_classification(simultaneous_veneration__pragmatic_incoherence_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: INSTITUTIONAL BUDDHISM (ROPE) — Benefits from pragmatic incoherence. Simultaneous veneration allows Buddhism to occupy shrine space and ritual function without requiring doctrinal conquest of kami worship. The institutional arrangement is coordination: Buddhism and shrine priesthoods coordinate on shared ritual practice while maintaining nominal doctrinal separation. Institutional Buddhism has arbitrage — they can exit to purely doctrinal Buddhism if needed, but the Japanese hybrid yields greater institutional reach.
constraint_indexing:constraint_classification(simultaneous_veneration__pragmatic_incoherence_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: SHRINE PRIESTHOODS (ROPE) — Benefit from pragmatic incoherence equally. Kami veneration remains privileged in shrine ritual while Buddhist elements provide doctrinal legitimacy and access to Buddhist institutional resources. Shrine priesthoods coordinate with Buddhism on shared narrative (honji-suijaku) while maintaining kami primacy in actual practice. Arbitrage position — can exit to purely Shinto frameworks if needed.
constraint_indexing:constraint_classification(simultaneous_veneration__pragmatic_incoherence_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: POLITICAL AUTHORITIES (ROPE) — Benefit from the coordination function. Pragmatic incoherence allows authorities to derive legitimacy from both Buddhist and Shinto frames without having to choose. The constraint operates as a coordination mechanism for political authority: the emperor can be both Buddhist devotee and kami-descended sovereign. Arbitrage — authorities can emphasize one frame or the other depending on political need.
constraint_indexing:constraint_classification(simultaneous_veneration__pragmatic_incoherence_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: DOCTRINAL CONSISTENCY (SNARE) — The very concept of internal coherence is trapped and suppressed. Intellectual traditions that might generate contradictions (Buddhist logic, Shinto cosmology) are never pressed to their conclusions within the simultaneous veneration framework. Doctrinal contradiction is the victim here — the constraint extracts the cost of never resolving incompatibilities. No exit from this victimhood because the suppression mechanism IS the lack of enforcement pressure — nobody is required to be coherent, so nobody becomes so.
constraint_indexing:constraint_classification(simultaneous_veneration__pragmatic_incoherence_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 6: HONJI-SUIJAKU THEORY (PITON) — The doctrinal apparatus that nominally resolves the contradiction (each kami is a manifestation of a specific buddha) is largely performative. It provides narrative cover for incoherence rather than genuine resolution. The theory persists through institutional inertia — it allows both Buddhist and Shinto authorities to claim compatibility without requiring actual metaphysical commitment. Theater ratio is high because the theory does work (provides narrative legitimacy) but the functional contradiction it claims to resolve persists unchanged.
constraint_indexing:constraint_classification(simultaneous_veneration__pragmatic_incoherence_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / FALSE SUMMIT VIEW (MOUNTAIN) — This perspective treats pragmatic pluralism as a natural law: 'Religious traditions naturally coexist without requiring doctrinal unity; Japan discovered what the West later termed productive ambiguity.' This reading naturalizes contingent institutional arrangements (lack of enforcement pressure, political incentives for both-and rather than either-or) as immutable features of how religion works. The engine's false summit detector identifies this perspective as a false natural law: the 'incoherence as feature' framing is contingent on specific historical conditions (distributed political authority, weak doctrinal gatekeeping), not on laws of nature or logic.
constraint_indexing:constraint_classification(simultaneous_veneration__pragmatic_incoherence_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(simultaneous_veneration__pragmatic_incoherence_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(simultaneous_veneration__pragmatic_incoherence_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(simultaneous_veneration__pragmatic_incoherence_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(simultaneous_veneration__pragmatic_incoherence_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(simultaneous_veneration__pragmatic_incoherence_reading, TR),
    TR >= 0.70.

:- end_tests(simultaneous_veneration__pragmatic_incoherence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62): High-moderate. The constraint extracts significantly from practitioners through the necessity to maintain incoherent commitments without resolution. The extraction is not as severe as pure predatory Snares (which involve explicit deception and coercion) because institutional actors appear to benefit from coordination-like outcomes (Buddhism and shrine systems coexist, political authorities leverage both frames). However, the beneficiary institutions extract real value from practitioners' inability to demand coherence — practitioners cannot force doctrinal resolution without threatening the institutional arrangement. Suppression (0.72): High. The suppression mechanism is specifically the absence of enforcement pressure toward coherence. Institutional actors do not require practitioners to choose or to defend their simultaneous veneration against logical challenge. This lack of critical pressure is what sustains the incoherence. When Meiji authorities imposed shinbutsu-bunri, they created enforcement pressure, and the incoherence that had been suppressed all along surfaced immediately — practitioners largely abandoned simultaneous veneration when forced to choose. Theater ratio (0.81): High and increasing over the interval. The honji-suijaku theory is nominally a metaphysical resolution but functions primarily as narrative cover. The theory allows institutional actors and practitioners to claim coherence without requiring genuine doctrinal engagement. Over the Edo period, as honji-suijaku became institutionalized in Buddhist liturgy and shrine theology, the theater increased — practitioners could invoke the theory to deflect coherence challenges, but the theory did not actually resolve the underlying contradiction. The Meiji measurements would show theater_ratio collapsing to near zero if continued — once shinbutsu-bunri was enforced, the performative function of honji-suijaku vanished.
 *
 * PERSPECTIVAL GAP:
 *   The pragmatic-incoherence reading generates a strong perspectival gap between the practitioner perspective (Snare: trapped in incoherence) and the beneficiary perspectives (Rope: coordinating without coherence). From the institutional perspective, simultaneous veneration is a successful coordination mechanism — Buddhism and shrine systems coexist peacefully, political authorities leverage both frames. From the practitioner perspective, it is an extraction mechanism — they bear the epistemic cost of holding incompatible beliefs without resolution. The honji-suijaku theory bridges this gap performatively but does not eliminate it. When Meiji authorities imposed shinbutsu-bunri, the gap collapsed suddenly — practitioners largely abandoned simultaneous veneration, revealing that the 'coordination' was sustained primarily by the absence of enforcement pressure, not by genuine acceptance of the honji-suijaku reconciliation. The analytical observer (mountain perspective) risks naturalizing this contingent historical arrangement as a law of how religion works, ignoring that the arrangement depended on specific power distributions and enforcement incentives.
 *
 * DIRECTIONALITY LOGIC:
 *   Practitioners occupy the position of victims with trapped exit options — they experience maximum extraction (d ≈ 0.90, f(d) ≈ 1.28). Institutional beneficiaries (Buddhism, shrine priesthoods, political authorities) occupy beneficiary positions with arbitrage exit options — they experience minimal or negative extraction (d ≈ 0.10, f(d) ≈ -0.05). The suppression mechanism (lack of enforcement pressure toward coherence) is what sustains this asymmetry: practitioners cannot organize to demand resolution without disrupting institutional arrangements that benefit them in other ways. The beneficiary institutions have no incentive to raise enforcement pressure because their arbitrage position allows them to maintain both doctrinal frameworks for different constituencies. This directionality distribution is characteristic of Snares: the target (practitioners) is trapped; the extractors (institutional actors) have exit options and use them to avoid commitment to coherence.
 *
 * MANDATROPHY ANALYSIS:
 *   The pragmatic-incoherence reading resolves the mandatrophy by demonstrating that simultaneous veneration operated as Snare + Rope depending on observation position: practitioners in Snare (trapped in suppressed contradiction), institutions in Rope (coordinating without coherence requirement). The reading argues that this is NOT a stable or coherent arrangement but rather an equilibrium maintained by absence of enforcement pressure. The Meiji shinbutsu-bunri is not presented as rupture but as revelation — when enforcement pressure was applied, the suppressed incoherence surfaced, and the constraint collapsed. The honji-suijaku theory, which from the ontological-fusion reading appears as genuine metaphysical reconciliation, appears from the pragmatic-incoherence reading as performative theater that provided cover for incoherence without resolving it. The mandate against mixed types (either Rope or Snare, not both) is satisfied because the reading argues the constraint was fundamentally Snare with performative Rope-like features, not a genuine hybrid. The high theater ratio (0.81) captures the performative function of honji-suijaku.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    incoherence_vs_domain_partition,
    'Is simultaneous veneration incoherent contradiction or legitimate domain specialization that appears contradictory only from monistic theological assumptions?',
    'Textual analysis of practitioners'' own metacognitive reflections; examination of whether practitioners treated kami and buddha realms as genuinely incompatible or as legitimately separate domains. Analysis of Edo-period philosophical texts debating the logical structure of simultaneous veneration.',
    'If domain specialization: constraint shifts from Snare to Tangled Rope (genuine coordination function alongside extraction). If genuine incoherence: Snare classification confirmed — the suppression is exactly the lack of critical pressure to acknowledge contradiction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incoherence_vs_domain_partition, empirical, 'Whether simultaneous veneration was incoherent or domain-partitioned').

omega_variable(
    enforcement_pressure_counterfactual,
    'Would practitioners have abandoned simultaneous veneration if institutional actors (Buddhist clergy, shrine priests, political authorities) had enforced doctrinal choice?',
    'Historical analysis of regions/periods where enforcement was attempted (esp. Meiji shinbutsu-bunri); comparison with periods of looser institutional control. Study of practitioner behavior when forced to choose between Buddhist and Shinto ritual.',
    'If practitioners abandoned it under pressure: suppression mechanism confirmed — the constraint depended on absence of enforcement. If practitioners resisted enforcement: constraint may have been more stable than the pragmatic-incoherence reading suggests, indicating genuine if unacknowledged acceptance of coexistence.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforcement_pressure_counterfactual, empirical, 'Whether practitioners would abandon simultaneous veneration under institutional pressure').

omega_variable(
    meiji_shinbutsu_bunri_causation,
    'Was Meiji shinbutsu-bunri (separation of kami and buddhas) a forcible rupture of a functional compromise, or the revelation of a latent incoherence that had been suppressed all along?',
    'Examination of Meiji state rhetoric and enforcement intensity; comparison with other forced religious separations (e.g., Christian-Shinto separation in Tokugawa); analysis of whether practitioners experienced the separation as loss of a coherent framework vs. resolution of suppressed contradiction.',
    'If rupture: constraint shifts toward Scaffold (coordination maintained until enforcement pressure ended). If revelation: Snare classification and high suppression value confirmed — the incoherence was never resolved, only suppressed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(meiji_shinbutsu_bunri_causation, empirical, 'Whether Meiji shinbutsu-bunri was rupture or revelation of latent incoherence').

omega_variable(
    honji_suijaku_commitment_depth,
    'Did honji-suijaku theory represent a genuine metaphysical commitment by practitioners, or a convenient narrative device used without strong belief?',
    'Analysis of doctrinal writings, sermon records, and practitioner testimonies; examination of whether honji-suijaku was invoked in moments of genuine philosophical dispute or only when coherence was demanded. Study of theological development within the theory — did it deepen or stagnate?',
    'If genuine commitment: removes some of the extractive character of the constraint — practitioners may have believed in the resolution rather than merely tolerating incoherence. If narrative device: high theater ratio and Snare classification confirmed — honji-suijaku is performative cover for suppressed contradiction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(honji_suijaku_commitment_depth, empirical, 'Whether honji-suijaku theory was genuine metaphysical commitment or narrative device').

omega_variable(
    reading_identity_ambiguity,
    'This constraint is the pragmatic-incoherence reading of the simultaneous-veneration kernel. Is this reading itself a stable commitment, or an anachronistic scholarly imposition that imputes incoherence where practitioners felt none?',
    'Genealogy of the ''incoherence'' framing within Japanese religious studies; identification of whether the reading originates in practitioner metacognition or in modern scholarly frameworks (esp. post-Meiji revisionism, Western philosophy of religion). Examination of whether pre-Meiji sources describe simultaneous veneration in incoherence language or in other vocabularies (yugo, gassai, etc.).',
    'If anachronistic imposition: this reading is a false summit itself — scholarly modernism naturalizing what practitioners experienced through different categories. If grounded in practitioner reflection: reading captures real structural tension that practitioners felt but didn''t articulate in Western philosophical language.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_identity_ambiguity, conceptual, 'Whether pragmatic-incoherence reading is practitioner-grounded or scholarly imposition').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(simultaneous_veneration__pragmatic_incoherence_reading, 0, 600).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(simven_tr_t0, simultaneous_veneration__pragmatic_incoherence_reading, theater_ratio, 0, 0.55).
narrative_ontology:measurement(simven_tr_t300, simultaneous_veneration__pragmatic_incoherence_reading, theater_ratio, 300, 0.72).
narrative_ontology:measurement(simven_tr_t600, simultaneous_veneration__pragmatic_incoherence_reading, theater_ratio, 600, 0.81).

% Extraction over time
narrative_ontology:measurement(simven_be_t0, simultaneous_veneration__pragmatic_incoherence_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(simven_be_t300, simultaneous_veneration__pragmatic_incoherence_reading, base_extractiveness, 300, 0.58).
narrative_ontology:measurement(simven_be_t600, simultaneous_veneration__pragmatic_incoherence_reading, base_extractiveness, 600, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(simven_su_t0, simultaneous_veneration__pragmatic_incoherence_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(simven_su_t300, simultaneous_veneration__pragmatic_incoherence_reading, suppression_requirement, 300, 0.7).
narrative_ontology:measurement(simven_su_t600, simultaneous_veneration__pragmatic_incoherence_reading, suppression_requirement, 600, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(simultaneous_veneration__pragmatic_incoherence_reading, identity_coordination).
narrative_ontology:affects_constraint(simultaneous_veneration__pragmatic_incoherence_reading, simultaneous_veneration__domain_partition_reading).
narrative_ontology:affects_constraint(simultaneous_veneration__pragmatic_incoherence_reading, simultaneous_veneration__ontological_fusion_reading).
narrative_ontology:affects_constraint(simultaneous_veneration__pragmatic_incoherence_reading, meiji_shinbutsu_bunri_enforcement).
narrative_ontology:affects_constraint(simultaneous_veneration__pragmatic_incoherence_reading, honji_suijaku_doctrinal_theory).

% DUAL FORMULATION NOTE:
% The simultaneous-veneration kernel decomposes into three structurally distinct constraint stories, each representing a different reading of the same historical phenomenon. The pragmatic-incoherence reading generates a Snare (high ε from suppressed contradiction); the domain-partition reading generates a Tangled Rope (genuine coordination alongside pragmatic specialization); the ontological-fusion reading generates a Rope (genuine metaphysical unity). These are not the same constraint viewed from different angles — they are different structural claims about whether simultaneous veneration was coherent, and each has different implications for Meiji shinbutsu-bunri's historical meaning (rupture vs. revelation). The three readings coexist in contemporary Japanese religious studies scholarship; the pragmatic-incoherence reading is increasingly dominant in post-war historiography.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
