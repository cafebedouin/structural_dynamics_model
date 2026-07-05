% ============================================================================
% CONSTRAINT STORY: homoousios_christology__semi_arian_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [RESOLVED MANDATROPHY]
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
 *   human_readable: Homoiousian (Semi-Arian) Christological Compromise Formula
 *   domain: historical_theology/ecclesiastical_politics
 *
 * SUMMARY:
 *   In the decades after Nicaea (325), the eastern Church fractured over how
 *   to describe Christ's relationship to the Father. This story treats the
 *   homoiousios ('of similar substance') formula associated with Basil of
 *   Ancyra and the eastern court-aligned bishops of the 350s-360s as its own
 *   constraint, distinct from both the Nicene homoousios reading and the
 *   Arian subordinationist reading. The homoiousian formula functioned as a
 *   coordination device: a linguistically close-but-doctrinally-hedged
 *   compromise that let the imperial court and a broad swath of eastern
 *   bishops avoid immediate, total schism. It required active synodal
 *   enforcement (condemnation of Anomoean extremists, pressure on Nicene
 *   loyalists to accept ambiguous language) and produced real victims on both
 *   flanks, while its own beneficiary faction dissolved into the Nicene
 *   settlement after 381. Per the ε-invariance principle, this is authored as
 *   a separate constraint from the arian_reading and pro_nicene_reading
 *   siblings — the beneficiary sets, enforcement mechanisms, and eventual
 *   fates of each reading differ sufficiently that averaging them into one
 *   'homoousios controversy' constraint would obscure exactly the
 *   coordination-versus-extraction structure this framework exists to detect.
 *
 * KEY AGENTS:
 *   - basil_of_ancyra_circle: Primary agenda-setter (institutional/constrained) — drafts and promotes the compromise formula
 *   - eastern_court_bishops: Primary beneficiary (institutional/arbitrage) — retains sees and imperial favor via the ambiguous formula
 *   - imperial_unity_faction: Secondary institutional actor (institutional/arbitrage) — values ecclesiastical peace over doctrinal precision
 *   - strict_arian_clergy: Primary target on the subordinationist flank (moderate/constrained) — pressured to concede more divinity than their theology allows
 *   - nicene_loyalist_clergy: Primary target on the consubstantialist flank (moderate/trapped) — deposed and exiled under compromise-era enforcement
 *   - rank_and_file_congregants: Diffuse victim (powerless/trapped) — bears doctrinal whiplash with no voice
 *   - later_church_historians: Analytical observer (analytical/analytical) — reads the episode's absorption into the post-381 settlement
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
narrative_ontology:constraint_metric(homoousios_christology__semi_arian_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(homoousios_christology__semi_arian_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(homoousios_christology__semi_arian_reading, tangled_rope).
narrative_ontology:human_readable(homoousios_christology__semi_arian_reading, "Homoiousian (Semi-Arian) Christological Compromise Formula").
narrative_ontology:topic_domain(homoousios_christology__semi_arian_reading, "historical_theology/ecclesiastical_politics").

domain_priors:requires_active_enforcement(homoousios_christology__semi_arian_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(homoousios_christology__semi_arian_reading, '97d092bf-0a33-484c-97f9-492c7c6c3eb3').
narrative_ontology:cs_kernel_codification('97d092bf-0a33-484c-97f9-492c7c6c3eb3', distributed).
narrative_ontology:cs_authority_grounding('97d092bf-0a33-484c-97f9-492c7c6c3eb3', distributed).
narrative_ontology:cs_reading_relation('97d092bf-0a33-484c-97f9-492c7c6c3eb3', homoousios_christology__pro_nicene_reading, influences).
narrative_ontology:cs_reading_relation('97d092bf-0a33-484c-97f9-492c7c6c3eb3', homoousios_christology__arian_reading, coexists_with).
narrative_ontology:cs_axiom('97d092bf-0a33-484c-97f9-492c7c6c3eb3', foundational, similar_substance_preserves_unity_without_identity).
narrative_ontology:cs_axiom_status(similar_substance_preserves_unity_without_identity, overridden).
narrative_ontology:cs_axiom_grounding('97d092bf-0a33-484c-97f9-492c7c6c3eb3', similar_substance_preserves_unity_without_identity, conventional).
narrative_ontology:cs_axiom('97d092bf-0a33-484c-97f9-492c7c6c3eb3', secondary, ecclesiastical_peace_justifies_terminological_ambiguity).
narrative_ontology:cs_axiom_status(ecclesiastical_peace_justifies_terminological_ambiguity, holdable).
narrative_ontology:cs_axiom_grounding('97d092bf-0a33-484c-97f9-492c7c6c3eb3', ecclesiastical_peace_justifies_terminological_ambiguity, instrumental).
narrative_ontology:cs_reference_frame('97d092bf-0a33-484c-97f9-492c7c6c3eb3', pre_nicene_subordinationist_consensus).
narrative_ontology:cs_drift_state('97d092bf-0a33-484c-97f9-492c7c6c3eb3', post_ancyra_360_synodal_peak, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('97d092bf-0a33-484c-97f9-492c7c6c3eb3', '').
narrative_ontology:cs_kernel_id(homoousios_christology__semi_arian_reading, homoousios_christology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(homoousios_christology__semi_arian_reading, eastern_court_bishops).
narrative_ontology:constraint_beneficiary(homoousios_christology__semi_arian_reading, imperial_unity_faction).
narrative_ontology:constraint_beneficiary(homoousios_christology__semi_arian_reading, basil_of_ancyra_circle).
narrative_ontology:constraint_victim(homoousios_christology__semi_arian_reading, strict_arian_clergy).
narrative_ontology:constraint_victim(homoousios_christology__semi_arian_reading, nicene_loyalist_clergy).
narrative_ontology:constraint_victim(homoousios_christology__semi_arian_reading, rank_and_file_congregants).
narrative_ontology:constraint_vindicates(homoousios_christology__semi_arian_reading, ecclesiastical_peace_as_supreme_good).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bishops led by Basil of Ancyra draft and promote the homoiousios formula at the Synod of Ancyra (358) and successive eastern councils, positioning it as the reasonable middle path between Nicene consubstantiality and Anomoean (radical Arian) subordinationism. They administer the wording, circulate it through eastern sees, and press it on the imperial court as the formula most likely to hold the empire's clergy together.
narrative_ontology:constraint_stakeholder(homoousios_christology__semi_arian_reading, basil_of_ancyra_circle, agenda_setter,
    institutional, biographical, constrained, regional).

% Bishops with access to Emperor Constantius II use the homoiousian compromise to retain sees and imperial favor without committing to either extreme, securing synodal majorities (Seleucia, Constantinople 360 pressure) and continued episcopal authority under a formula that lets them claim orthodoxy from multiple directions at once.
narrative_ontology:constraint_stakeholder(homoousios_christology__semi_arian_reading, eastern_court_bishops, beneficiary,
    institutional, biographical, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(homoousios_christology__semi_arian_reading, eastern_court_bishops, agenda_setter).

% Constantius II and the imperial administration favor homoiousios as a formula that can be imposed empire-wide to end open episcopal warfare without requiring the deposition of the Nicene-sympathetic sees or the full Arian faction. Political stability of the eastern Church is worth more to the court than doctrinal precision.
narrative_ontology:constraint_stakeholder(homoousios_christology__semi_arian_reading, imperial_unity_faction, beneficiary,
    institutional, generational, arbitrage, continental).

% Anomoean and strict subordinationist clergy (Aetius, Eunomius and allies) are pressured to accept a formula that concedes more to Christ's divinity than their theology allows. Refusal risks deposition and exile; the homoiousian synods actively condemn and marginalize the Anomoean position to secure the compromise's dominance.
narrative_ontology:constraint_stakeholder(homoousios_christology__semi_arian_reading, strict_arian_clergy, payer,
    moderate, biographical, constrained, regional).

% Bishops loyal to the Nicene homoousios formula (including exiled figures aligned with Athanasius) are treated as the deviant extreme requiring correction under the compromise's logic. They are deposed, exiled, or forced into ambiguous subscription during the years homoiousian formulas dominate eastern councils, bearing the direct enforcement cost of the compromise's imposition.
narrative_ontology:constraint_stakeholder(homoousios_christology__semi_arian_reading, nicene_loyalist_clergy, payer,
    moderate, biographical, trapped, regional).

% Ordinary believers experience repeated changes of bishop, repeated demands to subscribe to shifting creedal formulas, and repeated local schisms as sees change hands between factions. They have no voice in the formula's drafting and bear the confusion and social rupture of doctrinal whiplash without any of the political benefit.
narrative_ontology:constraint_stakeholder(homoousios_christology__semi_arian_reading, rank_and_file_congregants, payer,
    powerless, biographical, trapped, local).

% Later historians and theologians (from the post-381 Nicene-Constantinopolitan settlement onward) read the homoiousian episode as a transitional compromise that ultimately dissolved into the Nicene camp once its adherents (notably Basil of Caesarea and the Cappadocians) concluded homoousios could be read compatibly with their concerns. They analyze the compromise's function without having a stake in its outcome.
narrative_ontology:constraint_stakeholder(homoousios_christology__semi_arian_reading, later_church_historians, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a creedal formula broad enough that bishops across a spectrum of Christological views could subscribe without immediately triggering deposition or schism, buying time for the eastern Church and the imperial administration to avoid an immediate, total doctrinal rupture in the 350s-360s.
% TRANSFER_FUNCTION: Moves ecclesiastical authority and imperial favor toward bishops willing to subscribe to the ambiguous middle formula, and moves the costs of doctrinal instability (deposition, exile, congregational confusion) onto both extremes — strict Arians and Nicene loyalists — who are treated as the parties that must yield.
% ABSENT_VOICES: Rank-and-file congregants had no representation in the councils that drafted or revised the formula; Western bishops (largely Nicene-aligned) were structurally excluded from the eastern synodal process that produced and enforced homoiousios, and later objected that the formula was negotiated without them.
% DISAPPEARANCE_RATIONALE: Had the homoiousian compromise not existed as a mediating position, the 350s-360s eastern Church would likely have fractured immediately and completely between Nicene and Anomoean camps without the intermediate coalition-building it enabled; the eventual absorption of the homoiousian party into the pro-Nicene settlement at Constantinople 381 depended on the personal and theological relationships this compromise period built (notably the Cappadocian reworking of ousia/hypostasis language).
% FOUNDING_PROBLEM: The Church faced apparent Christological fracture after Nicaea (325) failed to end the controversy: extreme subordinationists (Anomoeans) and strict consubstantialists (Nicenes) could not coexist under one formula, and the imperial court needed ecclesiastical peace to govern effectively.
% FOUNDING_PROBLEM_CORROBORATION: The Cappadocian Fathers (Basil of Caesarea, Gregory of Nazianzus, Gregory of Nyssa) — figures who emerged from the homoiousian milieu but are not beneficiaries of the compromise formula itself — attest that the underlying terminological problem (Greek ousia/hypostasis ambiguity) was substantively resolved by their own conceptual refinement, after which the homoiousian formula's coordination function became unnecessary; the Council of Constantinople (381) records the formula's supersession without adopting its wording, corroborating from outside the original Ancyran faction that the compromise had served its purpose and lapsed.
narrative_ontology:disappearance_verdict(homoousios_christology__semi_arian_reading, world_rearranges).
narrative_ontology:founding_problem_status(homoousios_christology__semi_arian_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(homoousios_christology__semi_arian_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
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
 *   Extractiveness is authored at a moderate 0.42, rising through the mid-interval (peaking near t=10, corresponding to the height of homoiousian synodal dominance c. 358-360) and then declining sharply as the formula's own faction migrates toward the Nicene camp after 381 — this is a coordination mechanism with a real, if temporary, extractive cost on both flanks, not a stable extractive structure. Suppression follows the same arc: enforcement (deposition, forced subscription, condemnation of Anomoeans) intensifies during the formula's period of imperial backing and then collapses as the compromise is absorbed rather than defeated. Theater ratio rises across the interval because, as the underlying terminological dispute is progressively resolved by Cappadocian conceptual work, continued invocation of the homoiousian formula becomes increasingly performative — bishops who had used it as a live theological position increasingly used it as a face-saving label while substantively converging with Nicene theology.
 *
 * DIRECTIONALITY LOGIC:
 *   The Ancyran circle and eastern court bishops are near the beneficiary end: they set the formula's terms and retain institutional position through it. Strict Arian and Nicene loyalist clergy are pushed toward the target end from opposite directions — both are treated as the extreme requiring correction, and both bear real costs (exile, deposition) from the compromise's enforcement apparatus. Rank-and-file congregants are the most trapped: powerless, local, with no drafting voice, bearing the social cost of repeated creedal whiplash. The imperial unity faction has arbitrage-grade exit because its interest is in ecclesiastical peace as an instrument of governance, not in the theological content itself — if homoiousios stopped serving that function, the court's investment would shift accordingly, as history in fact shows.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — immediate post-Nicene fracture risk requiring an interim coordination formula — is authored as dead, corroborated by figures (the Cappadocians) who emerged from within the homoiousian milieu but were not its beneficiaries in the narrow political sense; they resolved the underlying terminological ambiguity and the compromise formula's coordination function lapsed accordingly at Constantinople 381. This prevents mislabeling the entire episode as pure extraction: the formula genuinely solved a coordination problem for a real historical window, and its status as 'resolved mandatrophy' (its function absorbed into a successor settlement, not defeated by force) is distinguishable from a snare whose founding problem is claimed dead by its own beneficiaries with no outside corroboration.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    compromise_or_capitulation,
    'Was homoiousios a genuine theological middle position with independent conceptual content, or was it primarily a political instrument that borrowed theological vocabulary to manage imperial governance needs?',
    'Close textual analysis of Basil of Ancyra''s own writings and the Synod of Ancyra''s synodal letter versus the correspondence of Constantius II''s court, to determine whether the formula''s content was theologically motivated or politically dictated.',
    'If primarily political, the coordination function claimed here is weaker than authored and the constraint tilts further toward tangled_rope or even snare, with the imperial unity faction as the dominant beneficiary rather than a secondary one. If genuinely theological, the coordination function is stronger and the classification is more defensibly rope-adjacent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compromise_or_capitulation, conceptual, 'Whether the compromise formula had independent theological content or was primarily an instrument of imperial ecclesiastical management.').

omega_variable(
    kernel_reading_disagreement_location,
    'Where exactly does the semi_arian_reading''s disagreement with its sibling readings live — is it a genuine difference in Christological content (similar vs. identical substance) or a difference in the acceptable degree of terminological ambiguity for maintaining ecclesiastical unity?',
    'Comparative analysis of how post-381 theologians (Cappadocians) described their own prior homoiousian commitments — did they describe themselves as having changed their theology, or as having found better language for what they always meant?',
    'If the disagreement is substantially terminological, this reading and pro_nicene_reading are closer to a single reading split by vocabulary rather than two structurally distinct constraints, and the ''absorption'' framing in the founding_problem_status is strongly supported. If the disagreement is substantively theological, the two constraints are more genuinely distinct and the absorption should be read as capitulation by the losing side rather than resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_location, conceptual, 'Whether the semi-Arian/pro-Nicene split was primarily terminological or substantively theological — bears on how the sibling readings relate structurally.').

omega_variable(
    post_381_faction_fate,
    'Did the homoiousian faction''s absorption into pro-Nicene orthodoxy represent genuine theological convergence, or did it represent the politically weaker faction being folded into the politically dominant one after imperial favor shifted under Theodosius I?',
    'Track individual bishops'' careers and see whether conversion to Nicene language correlates with imperial patronage timing versus independent theological argument predating the shift in imperial favor.',
    'If patronage-driven, the founding_problem_status of ''dead'' is less securely established — the formula may have been abandoned under duress rather than resolved by genuine conceptual progress, which would push this constraint closer to a snare that lost its enforcement backing rather than a scaffold that completed its transition.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(post_381_faction_fate, empirical, 'Whether the compromise faction''s post-381 absorption into Nicene orthodoxy reflects genuine resolution or political capitulation under a changed imperial patron.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(homoousios_christology__semi_arian_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(homo_tr_t0, homoousios_christology__semi_arian_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(homo_tr_t5, homoousios_christology__semi_arian_reading, theater_ratio, 5, 0.28).
narrative_ontology:measurement(homo_tr_t10, homoousios_christology__semi_arian_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement(homo_tr_t15, homoousios_christology__semi_arian_reading, theater_ratio, 15, 0.4).
narrative_ontology:measurement(homo_tr_t20, homoousios_christology__semi_arian_reading, theater_ratio, 20, 0.45).
narrative_ontology:measurement(homo_tr_t25, homoousios_christology__semi_arian_reading, theater_ratio, 25, 0.5).
narrative_ontology:measurement(homo_tr_t30, homoousios_christology__semi_arian_reading, theater_ratio, 30, 0.55).

% Extraction over time
narrative_ontology:measurement(homo_be_t0, homoousios_christology__semi_arian_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(homo_be_t5, homoousios_christology__semi_arian_reading, base_extractiveness, 5, 0.38).
narrative_ontology:measurement(homo_be_t10, homoousios_christology__semi_arian_reading, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(homo_be_t15, homoousios_christology__semi_arian_reading, base_extractiveness, 15, 0.42).
narrative_ontology:measurement(homo_be_t20, homoousios_christology__semi_arian_reading, base_extractiveness, 20, 0.36).
narrative_ontology:measurement(homo_be_t25, homoousios_christology__semi_arian_reading, base_extractiveness, 25, 0.28).
narrative_ontology:measurement(homo_be_t30, homoousios_christology__semi_arian_reading, base_extractiveness, 30, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(homo_su_t0, homoousios_christology__semi_arian_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(homo_su_t5, homoousios_christology__semi_arian_reading, suppression_requirement, 5, 0.34).
narrative_ontology:measurement(homo_su_t10, homoousios_christology__semi_arian_reading, suppression_requirement, 10, 0.42).
narrative_ontology:measurement(homo_su_t15, homoousios_christology__semi_arian_reading, suppression_requirement, 15, 0.4).
narrative_ontology:measurement(homo_su_t20, homoousios_christology__semi_arian_reading, suppression_requirement, 20, 0.3).
narrative_ontology:measurement(homo_su_t25, homoousios_christology__semi_arian_reading, suppression_requirement, 25, 0.18).
narrative_ontology:measurement(homo_su_t30, homoousios_christology__semi_arian_reading, suppression_requirement, 30, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(homoousios_christology__semi_arian_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(homoousios_christology__semi_arian_reading, 0.1).
narrative_ontology:affects_constraint(homoousios_christology__semi_arian_reading, homoousios_christology__pro_nicene_reading).
narrative_ontology:affects_constraint(homoousios_christology__semi_arian_reading, homoousios_christology__arian_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the homoousios_christology kernel, decomposed per the ε-invariance principle: pro_nicene_reading (homoousios, identical substance — higher enforcement ε post-381, becomes the imperially-backed orthodox settlement), arian_reading (Christ as created/subordinate — the position most thoroughly suppressed after 381), and this semi_arian_reading (homoiousios, similar substance — a transitional coordination formula with lower sustained enforcement ε, whose beneficiary faction is eventually absorbed into the pro-Nicene camp). The three stories share the same historical kernel — the fourth-century Christological controversy — but instantiate structurally distinct constraints with different beneficiary sets, different enforcement trajectories, and different eventual fates. affects_constraints links this story to both siblings because the semi-Arian coalition's dissolution directly fed the pro-Nicene settlement's eventual dominance and directly hastened the Arian reading's marginalization.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
