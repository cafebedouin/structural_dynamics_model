% ============================================================================
% CONSTRAINT STORY: homoousios_christology__semi_arian_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_homoousios_christology__semi_arian_reading, []).

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
 *   constraint_id: homoousios_christology__semi_arian_reading
 *   human_readable: Homoiousian (Similar-Substance) Christology as Ecclesial Compromise
 *   domain: religious/historical/political
 *
 * SUMMARY:
 *   This story instantiates the homoiousian ('similar substance') reading of
 *   the fourth-century Christological kernel — the moderate eastern episcopal
 *   center's attempt at c. 357-360 to draft a creedal formula that could hold
 *   together bishops repelled by both Nicene homoousios (feared as Sabellian)
 *   and Arian subordinationism. As the councils of Ancyra, Sirmium, and
 *   Seleucia show, the formula functioned as a real coordination device for a
 *   genuine three-way schism risk, backed by imperial enforcement under
 *   Constantius II, and it extracted communion and see-tenure costs from both
 *   the strict Nicene party and the anomoean radicals it was built to
 *   exclude. It did not survive as a standing position: by 381 its vocabulary
 *   was absorbed into a clarified pro-Nicene settlement at Constantinople.
 *   This is a distinct constraint from the pro-Nicene reading (which claims
 *   permanent, identity-grounding substance-unity and treats homoousios as
 *   non-negotiable orthodoxy) and from the Arian reading (which denies
 *   substance-unity outright); each of those readings has its own ε, its own
 *   victims, and its own persistence profile and is authored as a separate
 *   sibling story.
 *
 * KEY AGENTS:
 *   - eastern_episcopal_moderates: agenda-setting drafters of the compromise formula, institutional power, constrained exit
 *   - constantius_imperial_court: imperial enforcer and chief beneficiary of ecclesiastical peace, institutional power, arbitrage exit
 *   - strict_nicene_bishops: primary payers, exiled and deposed, organized power but constrained exit under imperial pressure
 *   - anomoean_radicals: secondary payers, excluded from both poles of the compromise, powerless and trapped
 *   - western_latin_churches: payers of the communion rupture, organized power, mobile exit via Roman prestige
 *   - later_church_historians: analytical observers assessing the formula's absorption into 381 orthodoxy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(homoousios_christology__semi_arian_reading, 0.42).
domain_priors:suppression_score(homoousios_christology__semi_arian_reading, 0.38).
domain_priors:theater_ratio(homoousios_christology__semi_arian_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(homoousios_christology__semi_arian_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(homoousios_christology__semi_arian_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(homoousios_christology__semi_arian_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(homoousios_christology__semi_arian_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(homoousios_christology__semi_arian_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(homoousios_christology__semi_arian_reading, scaffold).
narrative_ontology:human_readable(homoousios_christology__semi_arian_reading, "Homoiousian (Similar-Substance) Christology as Ecclesial Compromise").
narrative_ontology:topic_domain(homoousios_christology__semi_arian_reading, "religious/historical/political").

domain_priors:requires_active_enforcement(homoousios_christology__semi_arian_reading).
narrative_ontology:has_sunset_clause(homoousios_christology__semi_arian_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(homoousios_christology__semi_arian_reading, '9a5d8746-f081-4fdc-8eb6-194f2389848b').
narrative_ontology:cs_kernel_codification('9a5d8746-f081-4fdc-8eb6-194f2389848b', distributed).
narrative_ontology:cs_authority_grounding('9a5d8746-f081-4fdc-8eb6-194f2389848b', practice).
narrative_ontology:cs_interpretation_layer_present('9a5d8746-f081-4fdc-8eb6-194f2389848b').
narrative_ontology:cs_reading_relation('9a5d8746-f081-4fdc-8eb6-194f2389848b', homoousios_christology__pro_nicene_reading, influences).
narrative_ontology:cs_reading_relation('9a5d8746-f081-4fdc-8eb6-194f2389848b', homoousios_christology__arian_reading, coexists_with).
narrative_ontology:cs_axiom('9a5d8746-f081-4fdc-8eb6-194f2389848b', foundational, substance_similarity_suffices_for_communion).
narrative_ontology:cs_axiom_status(substance_similarity_suffices_for_communion, overridden).
narrative_ontology:cs_axiom_grounding('9a5d8746-f081-4fdc-8eb6-194f2389848b', substance_similarity_suffices_for_communion, conventional).
narrative_ontology:cs_axiom('9a5d8746-f081-4fdc-8eb6-194f2389848b', secondary, creedal_precision_may_be_deferred_for_ecclesial_unity).
narrative_ontology:cs_axiom_status(creedal_precision_may_be_deferred_for_ecclesial_unity, overridden).
narrative_ontology:cs_axiom_grounding('9a5d8746-f081-4fdc-8eb6-194f2389848b', creedal_precision_may_be_deferred_for_ecclesial_unity, instrumental).
narrative_ontology:cs_reference_frame('9a5d8746-f081-4fdc-8eb6-194f2389848b', post_nicene_pre_cappadocian_vocabulary_gap).
narrative_ontology:cs_drift_state('9a5d8746-f081-4fdc-8eb6-194f2389848b', council_of_constantinople_381, gap(axiom_overriding, severe, true)).
narrative_ontology:cs_created_at('9a5d8746-f081-4fdc-8eb6-194f2389848b', '').
narrative_ontology:cs_kernel_id(homoousios_christology__semi_arian_reading, homoousios_christology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(homoousios_christology__semi_arian_reading, eastern_episcopal_moderates).
narrative_ontology:constraint_beneficiary(homoousios_christology__semi_arian_reading, constantius_imperial_court).
narrative_ontology:constraint_beneficiary(homoousios_christology__semi_arian_reading, unity_seeking_laity).
narrative_ontology:constraint_victim(homoousios_christology__semi_arian_reading, strict_nicene_bishops).
narrative_ontology:constraint_victim(homoousios_christology__semi_arian_reading, anomoean_radicals).
narrative_ontology:constraint_victim(homoousios_christology__semi_arian_reading, western_latin_churches).
narrative_ontology:constraint_vindicates(homoousios_christology__semi_arian_reading, conciliar_compromise_can_preserve_communion).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bishops centered in Asia Minor and Syria (Basil of Ancyra, George of Laodicea) who draft and promote the homoiousios formula at councils like Ancyra and Seleucia (357-359) as a middle path between what they see as Arian subordinationism and Nicene modalism-adjacent language. They administer synods, draft creeds, and lobby the imperial court to adopt the formula as imperial policy, hoping to hold the eastern episcopate together under one banner.
narrative_ontology:constraint_stakeholder(homoousios_christology__semi_arian_reading, eastern_episcopal_moderates, agenda_setter,
    institutional, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(homoousios_christology__semi_arian_reading, eastern_episcopal_moderates, beneficiary).

% Emperor Constantius II and his court back homoiousian language at the councils of Sirmium, Nike, and Constantinople (359-360) because a single imperially-endorsed formula that most eastern bishops can sign promises ecclesiastical peace and administrative simplicity across the empire's fractious provinces. The emperor can convene councils, exile dissenting bishops, and rewrite creedal wording by decree; his stake is political unity, not theological precision.
narrative_ontology:constraint_stakeholder(homoousios_christology__semi_arian_reading, constantius_imperial_court, beneficiary,
    institutional, immediate, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(homoousios_christology__semi_arian_reading, constantius_imperial_court, agenda_setter).

% Ordinary congregants and lower clergy who experience repeated local schisms, rival bishops, and shifting orthodoxy tests as disruptive to worship and community life. A workable compromise formula that keeps their bishop in communion with neighboring sees and the imperial church is a genuine relief, even though they have no say in drafting it and must simply adopt whatever creed their see settles on.
narrative_ontology:constraint_stakeholder(homoousios_christology__semi_arian_reading, unity_seeking_laity, beneficiary,
    powerless, biographical, trapped, regional).

% Bishops loyal to the 325 Nicene homoousios formula (Athanasius and allies) who are deposed, exiled, or excommunicated when homoiousian-leaning councils and the imperial court impose the compromise formula as the test of communion. They lose sees, face imperial banishment, and must operate underground or in exile until the political wind shifts back after 381; their exit options are constrained by dependence on episcopal office and imperial favor for any ministry at scale.
narrative_ontology:constraint_stakeholder(homoousios_christology__semi_arian_reading, strict_nicene_bishops, payer,
    organized, generational, constrained, continental).

% Followers of Aetius and Eunomius who hold Christ is fundamentally unlike the Father (anomoios) and are condemned by the homoiousian councils as too radical, losing standing on the opposite flank from the Nicenes. They are squeezed out by a compromise formula built explicitly to exclude both edges of the dispute, with no institutional power to contest their exclusion.
narrative_ontology:constraint_stakeholder(homoousios_christology__semi_arian_reading, anomoean_radicals, payer,
    powerless, biographical, trapped, regional).

% Latin-speaking sees, especially Rome under Pope Liberius (until his own temporary capitulation) and later firmly under Damasus, that largely reject homoiousios as a dodge that fails to affirm real unity of substance. They bear the cost of a fractured universal church and repeated eastern doctrinal reversals, but retain more mobility than eastern bishops because western imperial pressure is weaker and Rome's prestige gives it leverage to simply refuse and wait out the controversy.
narrative_ontology:constraint_stakeholder(homoousios_christology__semi_arian_reading, western_latin_churches, payer,
    organized, generational, mobile, continental).

% Patristic scholars and church historians who, writing after the Council of Constantinople (381) settled the matter in the Nicene direction, assess the homoiousian party as having supplied the vocabulary (ousia/hypostasis distinctions) that later enabled Nicene orthodoxy to be restated more precisely, even though the homoiousian compromise itself did not survive as a standing position.
narrative_ontology:constraint_stakeholder(homoousios_christology__semi_arian_reading, later_church_historians, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a creedal formula ('of similar substance') that a broad center of eastern bishops, uncomfortable with both Nicene homoousios (feared as verging on Sabellian modalism) and Arian/anomoean subordinationism, can sign in common — averting a three-way schism by giving the majority middle a shared banner and a path to continued communion with the imperial church.
% TRANSFER_FUNCTION: Moves ecclesiastical legitimacy, see occupancy, and imperial favor away from strict Nicene bishops and anomoean radicals toward the homoiousian center; moves peace and continuity of worship to ordinary laity at the cost of doctrinal precision and at the cost of temporarily severing communion with western sees that reject the formula.
% ABSENT_VOICES: Rank-and-file believers in contested sees have no vote in the councils that decide which creed their bishop must sign; anomoean radicals are present at some councils but structurally outvoted and then condemned by the very compromise built to exclude their position; western Latin bishops are largely absent from the eastern council rooms (Sirmium, Seleucia) where the formula is hammered out, despite bearing the cost of the resulting communion rupture.
% DISAPPEARANCE_RATIONALE: If the homoiousian formula had never been proposed, the eastern episcopate would have split earlier and more sharply into pro-Nicene and Arian/anomoean camps without a moderate coordination point; imperial policy would have had to choose a side outright rather than sponsor a middle position, and the eventual 381 settlement would have had a different, more adversarial path to consensus, likely with more permanent schism in the interim.
% FOUNDING_PROBLEM: The mid-fourth century eastern church faced a real coordination crisis: Nicene homoousios language was widely suspected (not without some historical basis) of collapsing into modalism, while explicit subordinationist language was suspected of demoting Christ to a creature — and neither extreme commanded majority assent among sitting eastern bishops, threatening open schism absent some formula the center could sign.
% FOUNDING_PROBLEM_CORROBORATION: The Council of Constantinople (381), populated substantially by former homoiousians (the so-called Cappadocian settlement) who came to accept a clarified homoousios once hypostasis/ousia terminology was distinguished, attests that the coordination problem the homoiousian formula addressed was resolved by better vocabulary rather than by the formula's own survival. Western Latin bishops and later historians (both outside the homoiousian party) corroborate that the formula was a transitional way-station, not a permanent settlement — no party still holds it as first-order orthodoxy after 381.
narrative_ontology:disappearance_verdict(homoousios_christology__semi_arian_reading, world_rearranges).
narrative_ontology:founding_problem_status(homoousios_christology__semi_arian_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(homoousios_christology__semi_arian_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(homoousios_christology__semi_arian_reading, 'none', 1).
narrative_ontology:epsilon_provenance(homoousios_christology__semi_arian_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(homoousios_christology__semi_arian_reading_tests).
:- end_tests(homoousios_christology__semi_arian_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction rises through the 350s as the formula moves from theological proposal to imperially enforced test of communion (peaking c.357-362 as sees are stripped from Nicene loyalists), then falls sharply after 381 as the formula is superseded and no longer extracts anything from anyone — there is no longer a standing arrangement to pay into. Theater ratio tracks a modest rise as later councils (Seleucia, Constantinople 360) increasingly restate the formula for political signaling rather than fresh theological work, then eases as the position dissolves. Suppression (enforcement intensity: exiles, deposed sees) spikes with imperial backing under Constantius and collapses after his death and the 381 settlement, consistent with a scaffold whose sunset arrived via absorption rather than formal repeal.
 *
 * DIRECTIONALITY LOGIC:
 *   Eastern moderates and the imperial court sit near the beneficiary end: they set the agenda, gain unity dividends, and bear little personal cost from the formula's operation. Strict Nicene bishops and anomoean radicals sit near the target end: both lose sees, standing, or communion specifically because the compromise is built to exclude their positions — this is genuine asymmetric extraction riding on a genuine coordination function, which is why the claimed type is scaffold (temporary, coordination-justified, sunset via 381) rather than tangled_rope; the enforcement was real but was explicitly aimed at holding a transition together, and the formula itself declares (in retrospect) its own transitional character by not surviving its function.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (avoiding an immediate three-way schism under real doctrinal and political pressure) was live and real in the 350s, which is why this is not classified as pure extraction despite its real victims. But the founding_problem_status is dead by 381 — the vocabulary problem the formula patched over was solved more precisely by the Cappadocian ousia/hypostasis distinction, and the formula itself was absorbed rather than defended, indicating no residual mandatrophy: nothing persisted past its function long enough to become an inertial piton. The sunset was not a formal clause but a historical absorption event, which the has_sunset_clause flag encodes.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    compromise_vs_capitulation,
    'Was the homoiousian formula a genuine good-faith theological middle position, or primarily a face-saving device for bishops under imperial pressure to avoid choosing a side that risked exile?',
    'Comparative analysis of homoiousian bishops'' private correspondence and later conduct after 381 — those who genuinely held the middle position on principle versus those who quickly and without friction accepted the Nicene clarification would evidence different motivations.',
    'If primarily face-saving, the coordination-function claim weakens and the constraint reads closer to a snare imposed by imperial convenience; if genuinely principled, the scaffold/coordination reading is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compromise_vs_capitulation, conceptual, 'Whether the compromise was substantively theological or primarily politically expedient.').

omega_variable(
    kernel_framing_underdetermination,
    'Is the homoiousian position better framed as a distinct third reading of the Christological kernel, or as an unstable intermediate state within the pro-Nicene reading''s own historical development (i.e., proto-Cappadocian Nicene orthodoxy still finding its vocabulary)?',
    'Trace whether later Nicene-Cappadocian theologians (Basil of Caesarea, Gregory of Nyssa) self-identify their mature position as a correction of, or a continuous development from, the homoiousian formula their teachers held.',
    'If continuous development, the semi_arian_reading constraint may be better modeled as an early phase of the pro_nicene_reading rather than a fully independent sibling constraint, changing which network edges are causal versus definitional.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether homoiousianism is a distinct kernel reading or an early phase of the pro-Nicene reading.').

omega_variable(
    enforcement_symmetry_ambiguity,
    'Did the homoiousian party''s own enforcement apparatus (councils, depositions) exceed what was structurally necessary to prevent schism, i.e., did it extract more than the coordination problem required?',
    'Compare the rate and severity of episcopal depositions under homoiousian-backed imperial policy (357-360) against comparable schism-averting church settlements that used lighter-touch mechanisms (e.g., regional toleration rather than universal creedal tests).',
    'If the enforcement was disproportionate to the coordination need, the extractiveness trajectory understates the actual asymmetry against strict Nicenes and anomoeans; if proportionate, the scaffold classification''s enforcement component is well-calibrated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_symmetry_ambiguity, empirical, 'Whether enforcement severity matched the genuine coordination need or exceeded it.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(homoousios_christology__semi_arian_reading, 342, 381).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(homo_tr_t342, homoousios_christology__semi_arian_reading, theater_ratio, 342, 0.15).
narrative_ontology:measurement(homo_tr_t349, homoousios_christology__semi_arian_reading, theater_ratio, 349, 0.2).
narrative_ontology:measurement(homo_tr_t357, homoousios_christology__semi_arian_reading, theater_ratio, 357, 0.28).
narrative_ontology:measurement(homo_tr_t362, homoousios_christology__semi_arian_reading, theater_ratio, 362, 0.32).
narrative_ontology:measurement(homo_tr_t370, homoousios_christology__semi_arian_reading, theater_ratio, 370, 0.35).
narrative_ontology:measurement(homo_tr_t381, homoousios_christology__semi_arian_reading, theater_ratio, 381, 0.3).

% Extraction over time
narrative_ontology:measurement(homo_be_t342, homoousios_christology__semi_arian_reading, base_extractiveness, 342, 0.2).
narrative_ontology:measurement(homo_be_t349, homoousios_christology__semi_arian_reading, base_extractiveness, 349, 0.28).
narrative_ontology:measurement(homo_be_t357, homoousios_christology__semi_arian_reading, base_extractiveness, 357, 0.4).
narrative_ontology:measurement(homo_be_t362, homoousios_christology__semi_arian_reading, base_extractiveness, 362, 0.42).
narrative_ontology:measurement(homo_be_t370, homoousios_christology__semi_arian_reading, base_extractiveness, 370, 0.35).
narrative_ontology:measurement(homo_be_t381, homoousios_christology__semi_arian_reading, base_extractiveness, 381, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(homo_su_t342, homoousios_christology__semi_arian_reading, suppression_requirement, 342, 0.15).
narrative_ontology:measurement(homo_su_t349, homoousios_christology__semi_arian_reading, suppression_requirement, 349, 0.22).
narrative_ontology:measurement(homo_su_t357, homoousios_christology__semi_arian_reading, suppression_requirement, 357, 0.4).
narrative_ontology:measurement(homo_su_t362, homoousios_christology__semi_arian_reading, suppression_requirement, 362, 0.38).
narrative_ontology:measurement(homo_su_t370, homoousios_christology__semi_arian_reading, suppression_requirement, 370, 0.28).
narrative_ontology:measurement(homo_su_t381, homoousios_christology__semi_arian_reading, suppression_requirement, 381, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(homoousios_christology__semi_arian_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(homoousios_christology__semi_arian_reading, 0.1).
narrative_ontology:affects_constraint(homoousios_christology__semi_arian_reading, pro_nicene_reading).
narrative_ontology:affects_constraint(homoousios_christology__semi_arian_reading, arian_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the homoousios_christology kernel (pro_nicene_reading, arian_reading, semi_arian_reading — this file). Each reading is authored as a separate constraint with its own stable epsilon: the pro-Nicene reading claims permanent identity-grounding substance-unity and persists past 381 as standing orthodoxy (higher long-run enforcement ε as a permanent creedal test); the Arian reading denies substance-unity and is progressively suppressed and exiled after 325 and especially after 381 (high suppression, victim-heavy); this semi-Arian reading is distinguished by its explicitly transitional, coordination-first character and its ε trajectory collapsing toward zero by 381 as it is absorbed rather than defended. The three readings are linked because they contest the same kernel (Christ's relationship to the Father's substance) and because the historical outcome of one (pro-Nicene consolidation at Constantinople) directly determines the fate of the other two.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
