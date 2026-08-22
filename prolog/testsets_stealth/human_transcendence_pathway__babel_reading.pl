% ============================================================================
% CONSTRAINT STORY: human_transcendence_pathway__babel_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_human_transcendence_pathway__babel_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: human_transcendence_pathway__babel_reading
 *   human_readable: Babel Reading: Enforced Unity of Tongue and Technique as Self-Sufficient Order
 *   domain: political theology/technology ethics/linguistic policy
 *
 * SUMMARY:
 *   The babel_reading instantiates the human_transcendence_pathway kernel as
 *   the claim that collective human power, marshaled through a unified
 *   technological-linguistic system, can secure stability and
 *   self-sufficiency with no reference to transcendent authority. Taken as a
 *   standing arrangement rather than a bare proposition, the claim organizes
 *   a concrete regime: one administrative tongue for law, schooling, and
 *   market; standardized technical practice; a monumental center fed by
 *   provincial labor and grain; and an enforcement corps that treats
 *   surviving vernaculars as obstacles to be cleared. The reading's own
 *   lights assess this arrangement as coercive homogenization: the unity is
 *   real but purchased by erasure, and the architects' name is the lasting
 *   product. Per the epsilon-referent rule, extractiveness is authored for
 *   the standing homogenizing arrangement as this reading sees it - not for
 *   the jerusalem alternative the reading would prefer, and not averaged
 *   across sibling readings. Claimed type and metrics are independent
 *   authored facts: the snare claim states what the reading believes is
 *   structurally true; the metric series states what it believes
 *   descriptively occurs across a representative lifecycle (founding,
 *   consolidation, overreach, fracture). Family links run to both sibling
 *   readings; the decomposition note records why the kernel splits.
 *
 * KEY AGENTS:
 *   - - tower_architects: agenda-setter and principal collector (institutional/arbitrage) - designs and commands the unification program, collects its name, historiography, and command height
 *   - - central_administrative_class: secondary collector (organized/constrained) - staffs the registries, courts, schools, and broadcast organs; careers run lifelong inside the hierarchy
 *   - - dominant_tongue_speakers: coordination gainers (organized/constrained) - inherit the continent-wide market, press, and canon the standard medium enables
 *   - - minority_language_communities: primary target (powerless/trapped) - transmission of the prohibited varieties severed generation by generation
 *   - - local_cultural_tradition_bearers: identity-bound target (powerless/identity_locked) - elders, singers, ritual specialists whose craft exists only in the suppressed varieties
 *   - - provincial_elites_assimilating: dual-positioned contributor (moderate/constrained) - trades ancestral speech for posts, standing, and intercession power
 *   - - exiled_tradition_keepers: excluded voice (powerless/mobile) - poets, clergy, organizers publishing from abroad, admitted to no council
 *   - - doctrinal_theologians: analytical observer (analytical/analytical) - reads the arrangement against the Babel-and-Pentecost typology from outside its organs
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(human_transcendence_pathway__babel_reading, 0.78).
domain_priors:suppression_score(human_transcendence_pathway__babel_reading, 0.7).
domain_priors:theater_ratio(human_transcendence_pathway__babel_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(human_transcendence_pathway__babel_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(human_transcendence_pathway__babel_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(human_transcendence_pathway__babel_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(human_transcendence_pathway__babel_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(human_transcendence_pathway__babel_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(human_transcendence_pathway__babel_reading, snare).
narrative_ontology:human_readable(human_transcendence_pathway__babel_reading, "Babel Reading: Enforced Unity of Tongue and Technique as Self-Sufficient Order").
narrative_ontology:topic_domain(human_transcendence_pathway__babel_reading, "political theology/technology ethics/linguistic policy").

domain_priors:requires_active_enforcement(human_transcendence_pathway__babel_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(human_transcendence_pathway__babel_reading, '87957350-8825-4fe9-8a2b-0da683368381').
narrative_ontology:cs_kernel_codification('87957350-8825-4fe9-8a2b-0da683368381', fixed_text).
narrative_ontology:cs_authority_grounding('87957350-8825-4fe9-8a2b-0da683368381', extraction).
narrative_ontology:cs_interpretation_layer_present('87957350-8825-4fe9-8a2b-0da683368381').
narrative_ontology:cs_reading_relation('87957350-8825-4fe9-8a2b-0da683368381', human_transcendence_pathway__jerusalem_reading, forecloses).
narrative_ontology:cs_reading_relation('87957350-8825-4fe9-8a2b-0da683368381', human_transcendence_pathway__technocratic_vs_incarnational_reading, influences).
narrative_ontology:cs_axiom('87957350-8825-4fe9-8a2b-0da683368381', foundational, collective_power_secures_stability_without_transcendent_reference).
narrative_ontology:cs_axiom_status(collective_power_secures_stability_without_transcendent_reference, holdable).
narrative_ontology:cs_axiom_grounding('87957350-8825-4fe9-8a2b-0da683368381', collective_power_secures_stability_without_transcendent_reference, empirically_contingent).
narrative_ontology:cs_axiom('87957350-8825-4fe9-8a2b-0da683368381', secondary, linguistic_uniformity_is_load_bearing_for_order).
narrative_ontology:cs_axiom_status(linguistic_uniformity_is_load_bearing_for_order, holdable).
narrative_ontology:cs_axiom_grounding('87957350-8825-4fe9-8a2b-0da683368381', linguistic_uniformity_is_load_bearing_for_order, instrumental).
narrative_ontology:cs_reference_frame('87957350-8825-4fe9-8a2b-0da683368381', one_tongue_one_project_authority).
narrative_ontology:cs_drift_state('87957350-8825-4fe9-8a2b-0da683368381', contemporary_homogenization_regimes, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('87957350-8825-4fe9-8a2b-0da683368381', '').
narrative_ontology:cs_kernel_id(human_transcendence_pathway__babel_reading, human_transcendence_pathway).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(human_transcendence_pathway__babel_reading, tower_architects).
narrative_ontology:constraint_beneficiary(human_transcendence_pathway__babel_reading, central_administrative_class).
narrative_ontology:constraint_beneficiary(human_transcendence_pathway__babel_reading, dominant_tongue_speakers).
narrative_ontology:constraint_victim(human_transcendence_pathway__babel_reading, minority_language_communities).
narrative_ontology:constraint_victim(human_transcendence_pathway__babel_reading, local_cultural_tradition_bearers).
narrative_ontology:constraint_victim(human_transcendence_pathway__babel_reading, provincial_elites_assimilating).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(human_transcendence_pathway__babel_reading, provincial_elites_assimilating).
narrative_ontology:constraint_vindicates(human_transcendence_pathway__babel_reading, human_self_sufficiency_doctrine).
narrative_ontology:constraint_vindicates(human_transcendence_pathway__babel_reading, unity_through_standardization_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design the unification program and command the corps that administers it: they set the standard tongue for law, schooling, and commerce, commission the monumental works, and decide which local practices count as obstacles to be cleared. The project's glory, its official histories, and its commanding heights answer to them; they can redirect the program's instruments at will, and no part of the arrangement binds them that they did not write.
narrative_ontology:constraint_stakeholder(human_transcendence_pathway__babel_reading, tower_architects, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(human_transcendence_pathway__babel_reading, tower_architects, beneficiary).

% Staff the registries, courts, schools, and broadcast organs that carry the standard tongue into every province. Rank, salary, and pension flow through the program's hierarchy; leaving the service means surrendering status and livelihood, so careers run lifelong inside it.
narrative_ontology:constraint_stakeholder(human_transcendence_pathway__babel_reading, central_administrative_class, beneficiary,
    organized, biographical, constrained, continental).

% Speak the variety the program elevated to standard. They gain a continent-wide market for their labor, a shared press and literary canon, and frictionless dealings with distant strangers; they fund the monumental works through taxes and conscription and watch provincial customs recede, which costs them little they notice day to day.
narrative_ontology:constraint_stakeholder(human_transcendence_pathway__babel_reading, dominant_tongue_speakers, beneficiary,
    organized, biographical, constrained, continental).

% Raise children schooled in a tongue not their own, fined or mocked for speaking their own in offices and classrooms, and conscripted for the center's works. Their villages sit far from any border; moving away means arriving as strangers everywhere, and staying means watching each generation speak less.
narrative_ontology:constraint_stakeholder(human_transcendence_pathway__babel_reading, minority_language_communities, payer,
    powerless, generational, trapped, regional).

% Elders, singers, and ritual specialists whose craft exists only in the prohibited varieties. Their authority rests on knowledge the program classes as superstition or sedition; renouncing it would unmake who they are, and keeping it marks them for surveillance.
narrative_ontology:constraint_stakeholder(human_transcendence_pathway__babel_reading, local_cultural_tradition_bearers, payer,
    powerless, biographical, identity_locked, local).

% Provincial notables and gentry who traded their grandmother's tongue for posts in the center's administration. They send remittances home and intercede for their districts, gaining standing no provincial life could offer, while their own children no longer learn the old speech.
narrative_ontology:constraint_stakeholder(human_transcendence_pathway__babel_reading, provincial_elites_assimilating, payer,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(human_transcendence_pathway__babel_reading, provincial_elites_assimilating, beneficiary).

% Poets, clergy, and organizers who fled or were expelled for defending the prohibited varieties. They publish from abroad and petition the center's courts, but no seat in the program's councils is open to them; their testimony reaches the provinces as contraband print.
narrative_ontology:constraint_stakeholder(human_transcendence_pathway__babel_reading, exiled_tradition_keepers, excluded,
    powerless, generational, mobile, regional).

% Scholars of the social-doctrine tradition who read the program against the Babel-and-Pentecost typology. They take testimony from every side, compare the program's promises with its outcomes across regimes, and write analyses none of the program's organs are obliged to hear.
narrative_ontology:constraint_stakeholder(human_transcendence_pathway__babel_reading, doctrinal_theologians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(human_transcendence_pathway__babel_reading, tower_architects).
narrative_ontology:fixing_cost_class(human_transcendence_pathway__babel_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: A single administrative language and standardized technical practice solves real coordination problems at scale: long-range trade, mass construction, legal uniformity, and the mobilization of labor that mutually unintelligible communities cannot achieve.
% TRANSFER_FUNCTION: Moves labor, grain, and conscripted work from the provinces to the center's works; moves linguistic allegiance and legal obedience from households to the standard medium; moves prestige, historiographic glory, and command of the apparatus upward to the architects and their administrative corps.
% ABSENT_VOICES: The exiled tradition-keepers hold the fullest objection and sit outside every council; the communities whose tongues have already lost their last speakers cannot testify at all; provincial mothers watching transmission fail lack any organ that hears them. Inside the frame, the theological analysts are received, if at all, as picturesque critics rather than as evidence.
% DISAPPEARANCE_RATIONALE: If the uniformity apparatus vanished overnight, courts and schools would reorganize around plural vernaculars within a generation, provincial markets would re-price around restored local media, the center's monumental works would halt for want of conscripted labor, and the architects' historiography would lose its subject - the dependencies are exactly the institutions the arrangement built.
% FOUNDING_PROBLEM: After a dispersing catastrophe, the founders faced a concrete fragility: scattered kin groups, mutually unintelligible, unable to pool labor, guarantee security, or undertake works that outlive a generation. The program was raised to end that fragility by making one people with one speech - 'lest we be scattered.'
% FOUNDING_PROBLEM_CORROBORATION: Outside the benefiting parties: the social-doctrine tradition attests the founding fear as real and the chosen remedy as self-defeating (Babel-Pentecost typology); comparative-historical linguistics corroborates that vernacular loss tracked policy rather than drift; exile presses and revival-movement archives document the costs from the paying side. No source outside the architects' own organs attests that the arrangement still delivers the founding good - that attestation exists only inside the beneficiary set.
narrative_ontology:disappearance_verdict(human_transcendence_pathway__babel_reading, world_rearranges).
narrative_ontology:founding_problem_status(human_transcendence_pathway__babel_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(human_transcendence_pathway__babel_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(human_transcendence_pathway__babel_reading, 'none', 1).
narrative_ontology:epsilon_provenance(human_transcendence_pathway__babel_reading, 0.78, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(human_transcendence_pathway__babel_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(human_transcendence_pathway__babel_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(human_transcendence_pathway__babel_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.78) because what the arrangement takes is not marginal: labor and grain for the center's works, obedience to a single legal medium, and - irreversibly - the intergenerational transmission of prohibited tongues, a loss no later payment restores. Suppression (0.70) is a raw structural property, unscaled by power or scope: school statutes, office fines, licensing, and stigma enforcement actively hold the uniformity in place; the series shows the enforcement machinery building through mid-interval and straining late as overextension sets in. Theater_ratio (0.48 at close, risen from 0.18) tracks proxy substitution: monumental ceremony, unity festivals, and official historiography grow relative to functional coordination as the founding purpose recedes, crossing toward majority-performance in the fracture phase. Accessibility_collapse (0.58) is moderate: alternatives do not vanish - vernaculars persist in kitchens and contraband print, and exit by migration exists at ruinous cost - so the arrangement nowhere approaches the total closure of a natural limit. Resistance (0.62) is sustained: revival movements, evasion, exile presses, quiet noncompliance. The snare claim rests on the collapse signature: when enforcement failed, communication shattered into mutual unintelligibility rather than degrading gracefully into a voluntary common medium - the coordination story did not survive its own compulsion, which is the mark of cover rather than substance. All three tracked series share one grid (0, 5, 10, 15, 20, 25, 30) so no metric is ever sampled against another's end-state.
 *
 * PERSPECTIVAL GAP:
 *   Seats diverge sharply. From the architects' chair the arrangement is the largest cooperative achievement known: millions acting as one body, raising works no scattered people could lift - the coordination experience, computed from a seat with arbitrage-grade exit and command of the instruments. From the minority-language chair the same structure is a machine for unmaking households: every institution a child enters demands surrender of the mother tongue. Dominant-tongue speakers experience convenience with diffuse cost and should compute near-coordination; provincial elites straddle, buying standing with ancestry; the exiled know the full bill and are seated nowhere. The engine derives these per-seat classifications from power, exit, and declared position; the divergence between the architect seat's achievement-experience and the peripheral seats' erasure-experience is the perspectival fact this corpus exists to measure, and the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations: tower_architects (command the instruments, collect the name), central_administrative_class (rank and livelihood flow through the hierarchy), dominant_tongue_speakers (continent-wide market and canon). Victim declarations: minority_language_communities (transmission severed), local_cultural_tradition_bearers (craft criminalized), provincial_elites_assimilating (ancestral speech surrendered). The derivation places the architects and administrators near the beneficiary pole and the peripheral contributors near the full-target pole, amplified by continental-to-global scope, which makes verification of abuses harder and effective extraction higher. One override: provincial elites are listed among those who bear costs because they do surrender the ancestral speech, but the derived directionality would overshoot - they collect standing, income, and intercession power from the same arrangement, so their true position sits near symmetry; the moderate-power override sets d to 0.55. Suppression stays unscaled in the arithmetic; only extractiveness rides directionality and scope.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem - security against dispersion and fragility after catastrophe, 'lest we be scattered' - retains a live core: every polity fears fragmentation, and the anxiety is real. But the arrangement's operative function has migrated: what it now produces, reliably, is the architects' name and the center's command height, while the promised self-sufficiency is exactly what collapses when enforcement fails. Authored honestly, founding_problem_status is contested (the architects attest the threat permanent; the doctrinal tradition and the exile presses attest the cure was always the disease), and the disappearance verdict is world_rearranges - courts, schools, and supply chains reorganize if the uniformity apparatus vanishes. Contested-status crossed with world_rearranges is the mismatch profile the consumer cross-checks against the theater path; the rising theater series supports the zombie reading. The classification guards both errors: it refuses to read the arrangement as pure coordination (the losses are constitutive - erasure is the mechanism, not a side effect), and it refuses to read it as mere banditry (the coordination gains are real enough that fragments of the common medium outlive particular regimes in voluntary use - the survival omega records this open question).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_instantiation,
    'This story instantiates only the babel_reading of the human_transcendence_pathway kernel; how would the jerusalem_reading and the technocratic_vs_incarnational_reading classify the same aspiration, and at which structural element does the disagreement actually bite?',
    'Author the sibling stories against the same referent and compare computed types and epsilon values; locate the disagreement in the specific element at issue (source of cohesion: enforced uniformity versus gifted communion versus optimized enhancement).',
    'If the siblings compute as rope or scaffold, the kernel''s contest is between extractive and benign instantiations of one aspiration; if all readings compute extractive, the kernel itself is a candidate false summit and the family needs decomposition review.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_instantiation, conceptual, 'Committer structure: this is one of three readings; sibling deltas are routed here rather than folded into this constraint.').

omega_variable(
    homogenization_naturalness,
    'Is enforced linguistic-cultural homogenization an unavoidable stage of polity scale (a structural feature of mass society) or a constructed choice that concentrates advantage in the unification''s architects?',
    'Comparative study of polycentric and homogenizing polities at matched scale and era: if comparably stable multilingual arrangements exist at scale, the naturality claim fails.',
    'If natural-stage, part of the measured burden is coordination cost rather than imposed harm and the reading softens toward coordination-with-overhead; if constructed, the pure-extraction reading firms and the architects'' position as collectors stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(homogenization_naturalness, empirical, 'Whether the homogenization is a law of scale or a policy choice with identifiable gainers.').

omega_variable(
    coordination_survival_without_enforcement,
    'Does the arrangement''s coordination function persist when its enforcement is removed, or do communication and cooperation collapse together with the power that compelled them?',
    'Natural experiments at enforcement collapse or liberalization (imperial breakups, language-policy relaxations): track whether the standard medium continues in voluntary use and whether cross-community cooperation survives the lifting of compulsion.',
    'Voluntary persistence indicates a genuine coordination core riding under the compulsion and would move the classification toward the hybrid boundary; full fragmentation confirms the coordination story as cover and fixes the pure-extraction reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_survival_without_enforcement, empirical, 'The decisive test separating cover-story unity from load-bearing unity.').

omega_variable(
    internalized_linguistic_shame,
    'How much of the observed abandonment of prohibited varieties is compelled by statute and how much is carried internally as stigma that outlasts the statutes?',
    'Post-liberalization intergenerational transmission studies: if transmission does not recover after legal barriers fall, a substantial internalized component is established.',
    'An internalized component raises the arrangement''s effective hold beyond the structural measure and predicts slow recovery even after reform; purely structural compulsion predicts rapid revival once bans lapse.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_linguistic_shame, empirical, 'Structural versus internalized share of the suppression holding the uniformity in place.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(human_transcendence_pathway__babel_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(babel_reading_tr_t0, human_transcendence_pathway__babel_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(babel_reading_tr_t0, observed).
narrative_ontology:measurement(babel_reading_tr_t5, human_transcendence_pathway__babel_reading, theater_ratio, 5, 0.22).
narrative_ontology:measurement_basis(babel_reading_tr_t5, observed).
narrative_ontology:measurement(babel_reading_tr_t10, human_transcendence_pathway__babel_reading, theater_ratio, 10, 0.27).
narrative_ontology:measurement_basis(babel_reading_tr_t10, observed).
narrative_ontology:measurement(babel_reading_tr_t15, human_transcendence_pathway__babel_reading, theater_ratio, 15, 0.33).
narrative_ontology:measurement_basis(babel_reading_tr_t15, observed).
narrative_ontology:measurement(babel_reading_tr_t20, human_transcendence_pathway__babel_reading, theater_ratio, 20, 0.39).
narrative_ontology:measurement_basis(babel_reading_tr_t20, observed).
narrative_ontology:measurement(babel_reading_tr_t25, human_transcendence_pathway__babel_reading, theater_ratio, 25, 0.44).
narrative_ontology:measurement_basis(babel_reading_tr_t25, observed).
narrative_ontology:measurement(babel_reading_tr_t30, human_transcendence_pathway__babel_reading, theater_ratio, 30, 0.48).
narrative_ontology:measurement_basis(babel_reading_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(babel_reading_be_t0, human_transcendence_pathway__babel_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(babel_reading_be_t0, observed).
narrative_ontology:measurement(babel_reading_be_t5, human_transcendence_pathway__babel_reading, base_extractiveness, 5, 0.54).
narrative_ontology:measurement_basis(babel_reading_be_t5, observed).
narrative_ontology:measurement(babel_reading_be_t10, human_transcendence_pathway__babel_reading, base_extractiveness, 10, 0.61).
narrative_ontology:measurement_basis(babel_reading_be_t10, observed).
narrative_ontology:measurement(babel_reading_be_t15, human_transcendence_pathway__babel_reading, base_extractiveness, 15, 0.67).
narrative_ontology:measurement_basis(babel_reading_be_t15, observed).
narrative_ontology:measurement(babel_reading_be_t20, human_transcendence_pathway__babel_reading, base_extractiveness, 20, 0.72).
narrative_ontology:measurement_basis(babel_reading_be_t20, observed).
narrative_ontology:measurement(babel_reading_be_t25, human_transcendence_pathway__babel_reading, base_extractiveness, 25, 0.76).
narrative_ontology:measurement_basis(babel_reading_be_t25, observed).
narrative_ontology:measurement(babel_reading_be_t30, human_transcendence_pathway__babel_reading, base_extractiveness, 30, 0.78).
narrative_ontology:measurement_basis(babel_reading_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(babel_reading_su_t0, human_transcendence_pathway__babel_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement_basis(babel_reading_su_t0, observed).
narrative_ontology:measurement(babel_reading_su_t5, human_transcendence_pathway__babel_reading, suppression_requirement, 5, 0.5).
narrative_ontology:measurement_basis(babel_reading_su_t5, observed).
narrative_ontology:measurement(babel_reading_su_t10, human_transcendence_pathway__babel_reading, suppression_requirement, 10, 0.6).
narrative_ontology:measurement_basis(babel_reading_su_t10, observed).
narrative_ontology:measurement(babel_reading_su_t15, human_transcendence_pathway__babel_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement_basis(babel_reading_su_t15, observed).
narrative_ontology:measurement(babel_reading_su_t20, human_transcendence_pathway__babel_reading, suppression_requirement, 20, 0.73).
narrative_ontology:measurement_basis(babel_reading_su_t20, observed).
narrative_ontology:measurement(babel_reading_su_t25, human_transcendence_pathway__babel_reading, suppression_requirement, 25, 0.72).
narrative_ontology:measurement_basis(babel_reading_su_t25, observed).
narrative_ontology:measurement(babel_reading_su_t30, human_transcendence_pathway__babel_reading, suppression_requirement, 30, 0.7).
narrative_ontology:measurement_basis(babel_reading_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(human_transcendence_pathway__babel_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(human_transcendence_pathway__babel_reading, human_transcendence_pathway__jerusalem_reading).
narrative_ontology:affects_constraint(human_transcendence_pathway__babel_reading, human_transcendence_pathway__technocratic_vs_incarnational_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the human_transcendence_pathway kernel per the epsilon-invariance principle: the colloquial label 'human self-transcendence through collective power' conflates at least three structurally distinct claims - babel (unity by enforced homogenization, self-sufficient), jerusalem (communion by gifted, participatory integration of plurality), and the technocratic-versus-incarnational contrast (limit-elimination versus received grace). Each gets its own file, its own epsilon, its own beneficiary/victim structure; this file authors the babel instantiation with high epsilon and links to both siblings. The upstream/downstream gradient runs babel toward the technocratic reading (template inheritance); jerusalem stands as the typological counter-reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(human_transcendence_pathway__babel_reading, moderate, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
