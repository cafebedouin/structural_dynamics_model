% ============================================================================
% CONSTRAINT STORY: homoousios_nicene__subordinationist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_homoousios_nicene__subordinationist_reading, []).

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
 *   constraint_id: homoousios_nicene__subordinationist_reading
 *   human_readable: Homoousios Read as Compatible with Subordination of the Son
 *   domain: historical_theology/ecclesiastical_history/philosophy_of_religion
 *
 * SUMMARY:
 *   This story models the subordinationist reading of the Nicene term
 *   homoousios: the claim that shared divine essence between Father and Son
 *   is compatible with the Son deriving his being from the Father and
 *   standing in a functionally or ontologically ranked relation beneath him.
 *   This reading was politically ascendant at several points between Nicaea
 *   (325) and Constantinople I (381), particularly under Constantius II, when
 *   subordination-tolerant creeds (Sirmium 357, the 'Blasphemy of Sirmium';
 *   the Homoian settlement at Rimini-Seleucia 359) displaced strict Nicene
 *   equality as imperial orthodoxy. The reading is authored here as its own
 *   constraint, structurally distinct from the metaphysical-equality reading
 *   and the honorific-similarity reading of the same term — each reading
 *   distributes power, victims, and beneficiaries differently and must not be
 *   averaged together.
 *
 * KEY AGENTS:
 *   - subordinationist_clergy: primary beneficiary and agenda-setter, retains sees and imperial favor under this reading
 *   - nicene_orthodox_communities and pro_nicene_bishops_in_exile: primary victims, bear exile and censure when this reading is enforced
 *   - homoian_court_factions: secondary institutional actor, values the reading for administrative unity rather than theological conviction
 *   - constantius_ii_and_valens: excluded political actor whose preference drives enforcement cycles without being counted as theological voice
 *   - later_church_historians: analytical observer reconstructing the contest from primary sources
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(homoousios_nicene__subordinationist_reading, 0.62).
domain_priors:suppression_score(homoousios_nicene__subordinationist_reading, 0.71).
domain_priors:theater_ratio(homoousios_nicene__subordinationist_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(homoousios_nicene__subordinationist_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(homoousios_nicene__subordinationist_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(homoousios_nicene__subordinationist_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(homoousios_nicene__subordinationist_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(homoousios_nicene__subordinationist_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(homoousios_nicene__subordinationist_reading, tangled_rope).
narrative_ontology:human_readable(homoousios_nicene__subordinationist_reading, "Homoousios Read as Compatible with Subordination of the Son").
narrative_ontology:topic_domain(homoousios_nicene__subordinationist_reading, "historical_theology/ecclesiastical_history/philosophy_of_religion").

domain_priors:requires_active_enforcement(homoousios_nicene__subordinationist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(homoousios_nicene__subordinationist_reading, 'a763f27b-e205-4318-8a5c-346506ea29f5').
narrative_ontology:cs_kernel_codification('a763f27b-e205-4318-8a5c-346506ea29f5', fixed_text).
narrative_ontology:cs_authority_grounding('a763f27b-e205-4318-8a5c-346506ea29f5', lineage).
narrative_ontology:cs_interpretation_layer_present('a763f27b-e205-4318-8a5c-346506ea29f5').
narrative_ontology:cs_reading_relation('a763f27b-e205-4318-8a5c-346506ea29f5', homoousios_nicene__metaphysical_equality_reading, forecloses).
narrative_ontology:cs_reading_relation('a763f27b-e205-4318-8a5c-346506ea29f5', homoousios_nicene__honorific_similarity_reading, coexists_with).
narrative_ontology:cs_axiom('a763f27b-e205-4318-8a5c-346506ea29f5', foundational, son_derives_being_from_father).
narrative_ontology:cs_axiom_status(son_derives_being_from_father, overridden).
narrative_ontology:cs_axiom_grounding('a763f27b-e205-4318-8a5c-346506ea29f5', son_derives_being_from_father, theological).
narrative_ontology:cs_axiom('a763f27b-e205-4318-8a5c-346506ea29f5', secondary, scriptural_subordination_texts_govern_ontology).
narrative_ontology:cs_axiom_status(scriptural_subordination_texts_govern_ontology, holdable).
narrative_ontology:cs_axiom_grounding('a763f27b-e205-4318-8a5c-346506ea29f5', scriptural_subordination_texts_govern_ontology, conventional).
narrative_ontology:cs_reference_frame('a763f27b-e205-4318-8a5c-346506ea29f5', nicene_325_creedal_formula).
narrative_ontology:cs_drift_state('a763f27b-e205-4318-8a5c-346506ea29f5', constantinople_381_settlement, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('a763f27b-e205-4318-8a5c-346506ea29f5', '').
narrative_ontology:cs_kernel_id(homoousios_nicene__subordinationist_reading, homoousios_nicene).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(homoousios_nicene__subordinationist_reading, subordinationist_clergy).
narrative_ontology:constraint_beneficiary(homoousios_nicene__subordinationist_reading, semi_arian_remnant_communities).
narrative_ontology:constraint_beneficiary(homoousios_nicene__subordinationist_reading, homoian_court_factions).
narrative_ontology:constraint_victim(homoousios_nicene__subordinationist_reading, nicene_orthodox_communities).
narrative_ontology:constraint_victim(homoousios_nicene__subordinationist_reading, pro_nicene_bishops_in_exile).
narrative_ontology:constraint_victim(homoousios_nicene__subordinationist_reading, trinitarian_catechumens).
narrative_ontology:constraint_vindicates(homoousios_nicene__subordinationist_reading, scriptural_priority_over_conciliar_definition).
narrative_ontology:constraint_vindicates(homoousios_nicene__subordinationist_reading, monarchia_of_the_father).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Reads homoousios as permitting the Son to derive being from the Father while sharing a divine nature without equality of rank. This reading lets them retain sees, imperial patronage, and scriptural warrant (subordinating texts like John 14:28) without being formally branded heretical. They administer synods (Sirmium, Rimini, Constantinople 360) that draft creeds compatible with their reading and depose pro-Nicene rivals from their posts.
narrative_ontology:constraint_stakeholder(homoousios_nicene__subordinationist_reading, subordinationist_clergy, beneficiary,
    institutional, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(homoousios_nicene__subordinationist_reading, subordinationist_clergy, agenda_setter).

% Congregations and clergy who hold a homoiousian-adjacent or subordinationist Christology survive under this reading rather than being forced into exile or recantation. Their continued communion and property depend on imperial and episcopal tolerance of the subordinationist construal of the shared term.
narrative_ontology:constraint_stakeholder(homoousios_nicene__subordinationist_reading, semi_arian_remnant_communities, beneficiary,
    organized, generational, constrained, regional).

% Imperial officials and court-aligned bishops (under Constantius II and later Valens) who prefer a vaguer, subordination-compatible formula because it preserves administrative unity across a doctrinally fractured empire and keeps them from having to enforce a single hard metaphysical line. They move between doctrinal factions as political winds shift, extracting stability and patronage.
narrative_ontology:constraint_stakeholder(homoousios_nicene__subordinationist_reading, homoian_court_factions, beneficiary,
    institutional, biographical, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(homoousios_nicene__subordinationist_reading, homoian_court_factions, agenda_setter).

% Congregations committed to full ontological equality of Father and Son bear exile of their bishops, loss of church buildings, and imperial censure whenever a subordination-tolerant creed becomes the enforced standard. They cannot simply leave the empire or the church structure; their only recourse is doctrinal resistance, often at the cost of physical exile (as with Athanasius).
narrative_ontology:constraint_stakeholder(homoousios_nicene__subordinationist_reading, nicene_orthodox_communities, payer,
    moderate, generational, trapped, continental).

% Figures like Athanasius and later Hilary of Poitiers are repeatedly deposed and exiled when subordination-compatible readings of homoousios gain imperial favor. Their sees are given to subordinationist appointees; return from exile depends entirely on shifts in imperial theology they do not control.
narrative_ontology:constraint_stakeholder(homoousios_nicene__subordinationist_reading, pro_nicene_bishops_in_exile, payer,
    moderate, biographical, trapped, continental).

% Ordinary believers being catechized inherit whichever Christological formula their local bishop currently holds under imperial pressure; under a subordinationist-compatible regime they are taught a graded divinity they may later have to publicly recant if Nicene enforcement returns, with real consequences for standing in their community.
narrative_ontology:constraint_stakeholder(homoousios_nicene__subordinationist_reading, trinitarian_catechumens, payer,
    powerless, biographical, trapped, regional).

% Emperors who favor a subordination-tolerant formula as an administrative tool are not themselves theologians and are absent from the doctrinal reasoning proper, yet their preference for imperial unity determines which reading of homoousios gets enforced as public creed in a given decade — a political voice never accounted as a theological one in later historiography.
narrative_ontology:constraint_stakeholder(homoousios_nicene__subordinationist_reading, constantius_ii_and_valens, excluded,
    institutional, biographical, analytical, continental).

% Reconstruct the fourth-century councils from conciliar acts, letters, and creeds, assessing whether the term homoousios was ever intended or received as compatible with subordination, or whether that reading is a later heresiological caricature or a genuine contemporaneous minority position.
narrative_ontology:constraint_stakeholder(homoousios_nicene__subordinationist_reading, later_church_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(homoousios_nicene__subordinationist_reading, subordinationist_clergy).
narrative_ontology:fixing_cost_class(homoousios_nicene__subordinationist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: A single technical term (homoousios) lets a doctrinally fractured, empire-spanning church body use one creedal vocabulary across councils and provinces, avoiding the need for each local church to litigate Christology from scratch — genuine coordination value for imperial and ecclesiastical unity.
% TRANSFER_FUNCTION: Doctrinal legitimacy, episcopal sees, imperial patronage, and freedom from exile or censure move toward clergy and communities who can construe the shared term compatibly with subordination, and away from Nicene-equality communities whose bishops are deposed and exiled whenever a subordination-tolerant creed is enforced.
% ABSENT_VOICES: The emperors whose administrative preference for a flexible formula drives enforcement cycles are treated by later theology as outside the doctrinal conversation entirely, though their political calculus determined which reading held power in a given decade. Ordinary catechumens taught the shifting formula have no voice in which reading is imposed on their instruction.
% DISAPPEARANCE_RATIONALE: If the subordination-compatible reading of homoousios had not been available as a live option, fourth-century imperial religious policy would have had to choose decisively between full Nicene equality and open Arian subordination much earlier; the decades of shifting councils, exiles, and re-exiles that structured the careers of Athanasius, the Cappadocians, and the homoian bishops would not have occurred in the same form. Sees, creeds, and communities currently organized around the ambiguity would need a different settlement.
% FOUNDING_PROBLEM: The term homoousios was adopted at Nicaea (325) to close off Arian subordinationism, but its philosophical vocabulary (ousia, hypostasis) was imprecise enough in Greek theological usage of the period that subordinationist and semi-Arian theologians could, and did, argue it was compatible with a derived, ranked divinity of the Son — especially given genuine scriptural texts describing the Son as sent by, and obedient to, the Father.
% FOUNDING_PROBLEM_CORROBORATION: Pro-Nicene writers of the period itself (Athanasius, the Cappadocian Fathers) attest that this reading is a distortion sustained by imperial power rather than a live theological possibility, and their testimony is corroborated by the eventual settlement at Constantinople I (381), which formally excludes the subordinationist construal — but that settlement is itself a victory of one faction, so the 'dead' verdict here is read from within the eventual conciliar consensus, not from a source outside all theological factions entirely. No fully external corroboration exists because the question is intrinsically a theological/political dispute; the closest to an outside attestation is the empire's own shifting administrative preference, which corroborates that the ambiguity was politically useful independent of its theological merit.
narrative_ontology:disappearance_verdict(homoousios_nicene__subordinationist_reading, world_rearranges).
narrative_ontology:founding_problem_status(homoousios_nicene__subordinationist_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(homoousios_nicene__subordinationist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(homoousios_nicene__subordinationist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(homoousios_nicene__subordinationist_reading, 0.62, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(homoousios_nicene__subordinationist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(homoousios_nicene__subordinationist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(homoousios_nicene__subordinationist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored moderate-high (0.62 by 381) because the reading transfers real institutional goods — sees, patronage, freedom from exile — toward subordinationist clergy at direct cost to Nicene communities, but the transfer operates through a genuine and contested exegetical/philosophical dispute rather than naked coercion alone. Suppression rises sharply through the Constantius II period (0.30 to 0.78 at 359) reflecting escalating imperial enforcement (forced signatures, depositions, exile of dissenting bishops at Rimini-Seleucia) before relaxing somewhat under Julian's tolerance and settling at 0.71 as Theodosius begins reasserting Nicene enforcement by 381 — the underlying contest is not yet resolved at the story's end. Theater ratio tracks the proliferation of creedal formulae (multiple councils issuing near-identical documents with cosmetic differences) as a symptom of substantive dispute needing continual re-litigation rather than settled coordination.
 *
 * PERSPECTIVAL GAP:
 *   From the subordinationist clergy's seat, homoousios-with-subordination is a defensible reading of a genuinely ambiguous philosophical term, consistent with scriptural texts describing the Son's obedience and sending — a live theological position, not extraction. From the Nicene orthodox seat, the same reading is experienced as an enforced doctrinal capture that costs them their sees, communities, and sometimes freedom. The engine should compute these as structurally different experiences of one arrangement, not resolve them into a single verdict.
 *
 * DIRECTIONALITY LOGIC:
 *   Subordinationist clergy and homoian court factions sit near the beneficiary end: the reading preserves their institutional position and gives them scriptural and philosophical room to avoid metaphysical claims they find untenable or politically costly. Nicene communities and exiled bishops sit near the target end: they are trapped (cannot simply relocate their sees or communities) and bear direct, repeated institutional loss whenever this reading gains imperial enforcement. Trinitarian catechumens are powerless and trapped, absorbing whichever formula is locally enforced without agency over the choice.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (closing off subordinationism at Nicaea) is authored as dead by 381 in the sense that the term's ambiguity has been substantively resolved against this reading at Constantinople I, yet the reading persisted and was actively enforced for decades after Nicaea specifically because the imprecision in the original technical vocabulary was never closed by the 325 council itself — classifying this as tangled_rope rather than pure snare preserves the fact that a genuine coordination function (a shared creedal vocabulary across an empire-wide church) was real throughout, even as it was captured asymmetrically by whichever faction held imperial favor at a given moment.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    subordinationist_reading_authenticity,
    'Was the subordinationist construal of homoousios a genuine, philosophically serious theological position held by its fourth-century proponents, or primarily a politically convenient ambiguity exploited by imperial administrators seeking doctrinal flexibility across a fractured empire?',
    'Close philological and historical-theological analysis of the actual writings of homoian and semi-Arian theologians (e.g., Eudoxius, Acacius of Caesarea) versus the political correspondence and conciliar acts documenting imperial pressure at Sirmium, Rimini, and Seleucia, to distinguish sincere doctrinal commitment from politically instrumental theology.',
    'If primarily politically instrumental, this reading''s coordination function is largely pretextual and the constraint tilts toward snare; if genuinely held as serious theology by a substantial community independent of imperial pressure, the coordination function is real and the tangled_rope classification is well-grounded.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(subordinationist_reading_authenticity, empirical, 'Whether subordinationist theology was sincere doctrine or politically instrumental ambiguity.').

omega_variable(
    kernel_reading_multiplicity,
    'Is homoousios genuinely one contested kernel with three structurally distinct readings (subordinationist, metaphysical-equality, honorific-similarity), or did the term''s meaning shift diachronically such that ''the same term'' at Nicaea (325) and at Constantinople (381) is not really the same kernel at all?',
    'Trace the term''s usage and technical definition across primary conciliar documents from 325 to 381 to determine whether a stable referent persisted or whether the reading itself altered what counted as the kernel.',
    'If the term''s meaning shifted diachronically, this story and its siblings may need further temporal decomposition (e.g., pre-Sirmium vs. post-Rimini subordinationist readings) rather than treating the subordinationist reading as a single stable constraint across the full 325-381 interval.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_multiplicity, conceptual, 'Whether the kernel itself is diachronically stable across the authored interval.').

omega_variable(
    monarchia_grounding_status,
    'Is the monarchia of the Father (the vindicated proposition that the Father is the sole unoriginate source of divinity) an authentically pro-Nicene patristic commitment shared by figures like the Cappadocians, or is it specific to the subordinationist tradition and improperly claimed here as a vindicated proposition of this reading alone?',
    'Compare Cappadocian trinitarian theology (which also affirms a monarchia of the Father while maintaining full ontological equality) against homoian/subordinationist usage of the same term to determine whether monarchia entails subordination or is compatible with equality.',
    'If monarchia is compatible with full equality (as most patristic scholarship on the Cappadocians holds), it should not be listed as uniquely vindicated by the subordinationist reading, and the vindicated_propositions list here may overstate this reading''s exclusive claim on a shared theological concept.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(monarchia_grounding_status, conceptual, 'Whether monarchia of the Father is exclusive to subordinationism or shared across readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(homoousios_nicene__subordinationist_reading, 325, 381).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(homo_tr_t325, homoousios_nicene__subordinationist_reading, theater_ratio, 325, 0.2).
narrative_ontology:measurement(homo_tr_t337, homoousios_nicene__subordinationist_reading, theater_ratio, 337, 0.28).
narrative_ontology:measurement(homo_tr_t350, homoousios_nicene__subordinationist_reading, theater_ratio, 350, 0.35).
narrative_ontology:measurement(homo_tr_t359, homoousios_nicene__subordinationist_reading, theater_ratio, 359, 0.45).
narrative_ontology:measurement(homo_tr_t370, homoousios_nicene__subordinationist_reading, theater_ratio, 370, 0.42).
narrative_ontology:measurement(homo_tr_t381, homoousios_nicene__subordinationist_reading, theater_ratio, 381, 0.4).

% Extraction over time
narrative_ontology:measurement(homo_be_t325, homoousios_nicene__subordinationist_reading, base_extractiveness, 325, 0.35).
narrative_ontology:measurement(homo_be_t337, homoousios_nicene__subordinationist_reading, base_extractiveness, 337, 0.48).
narrative_ontology:measurement(homo_be_t350, homoousios_nicene__subordinationist_reading, base_extractiveness, 350, 0.58).
narrative_ontology:measurement(homo_be_t359, homoousios_nicene__subordinationist_reading, base_extractiveness, 359, 0.65).
narrative_ontology:measurement(homo_be_t370, homoousios_nicene__subordinationist_reading, base_extractiveness, 370, 0.6).
narrative_ontology:measurement(homo_be_t381, homoousios_nicene__subordinationist_reading, base_extractiveness, 381, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(homo_su_t325, homoousios_nicene__subordinationist_reading, suppression_requirement, 325, 0.3).
narrative_ontology:measurement(homo_su_t337, homoousios_nicene__subordinationist_reading, suppression_requirement, 337, 0.45).
narrative_ontology:measurement(homo_su_t350, homoousios_nicene__subordinationist_reading, suppression_requirement, 350, 0.6).
narrative_ontology:measurement(homo_su_t359, homoousios_nicene__subordinationist_reading, suppression_requirement, 359, 0.78).
narrative_ontology:measurement(homo_su_t370, homoousios_nicene__subordinationist_reading, suppression_requirement, 370, 0.72).
narrative_ontology:measurement(homo_su_t381, homoousios_nicene__subordinationist_reading, suppression_requirement, 381, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(homoousios_nicene__subordinationist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(homoousios_nicene__subordinationist_reading, 0.1).
narrative_ontology:affects_constraint(homoousios_nicene__subordinationist_reading, metaphysical_equality_reading).
narrative_ontology:affects_constraint(homoousios_nicene__subordinationist_reading, honorific_similarity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the homoousios_nicene kernel. metaphysical_equality_reading authors the same term as securing full ontological equality with no subordination in being (the reading that prevails institutionally by 381); honorific_similarity_reading authors the term as signifying likeness rather than strict identity (a homoiousian-adjacent construal). Each reading has its own ε, beneficiary/victim structure, and classification; they are linked here as a constraint family rather than merged into one story, per the ε-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
