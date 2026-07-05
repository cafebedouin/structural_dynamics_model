% ============================================================================
% CONSTRAINT STORY: homoousios_nicene__honorific_similarity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_homoousios_nicene__honorific_similarity_reading, []).

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
 *   constraint_id: homoousios_nicene__honorific_similarity_reading
 *   human_readable: Homoousios as Honorific Similarity (Homoiousian-adjacent Reading)
 *   domain: historical_theology/ecclesiastical_history/philosophy_of_religion
 *
 * SUMMARY:
 *   This story instantiates the honorific-similarity reading of the
 *   homoousios kernel: the claim that the Nicene term signifies likeness or
 *   honorific unity between Father and Son rather than strict numerical
 *   identity of essence — the position historically associated with the
 *   homoiousios ('of like substance') faction and the broad semi-Arian center
 *   of the mid-fourth century. This is NOT a story about whether homoousios
 *   'really' means identity or similarity in some timeless sense; it is a
 *   story about the structural function this specific reading served for the
 *   specific parties who held it. The sibling readings —
 *   metaphysical_equality_reading (full ontological identity, the eventual
 *   Cappadocian/Athanasian consolidation) and subordinationist_reading
 *   (compatible with real subordination in being) — are separate constraints
 *   with their own ε values, not alternative measurements of this one.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(homoousios_nicene__honorific_similarity_reading, 0.46).
domain_priors:suppression_score(homoousios_nicene__honorific_similarity_reading, 0.4).
domain_priors:theater_ratio(homoousios_nicene__honorific_similarity_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(homoousios_nicene__honorific_similarity_reading, extractiveness, 0.46).
narrative_ontology:constraint_metric(homoousios_nicene__honorific_similarity_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(homoousios_nicene__honorific_similarity_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(homoousios_nicene__honorific_similarity_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(homoousios_nicene__honorific_similarity_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(homoousios_nicene__honorific_similarity_reading, tangled_rope).
narrative_ontology:human_readable(homoousios_nicene__honorific_similarity_reading, "Homoousios as Honorific Similarity (Homoiousian-adjacent Reading)").
narrative_ontology:topic_domain(homoousios_nicene__honorific_similarity_reading, "historical_theology/ecclesiastical_history/philosophy_of_religion").

domain_priors:requires_active_enforcement(homoousios_nicene__honorific_similarity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(homoousios_nicene__honorific_similarity_reading, 'a8d1fb0e-c53e-414c-a221-21ee05899225').
narrative_ontology:cs_kernel_codification('a8d1fb0e-c53e-414c-a221-21ee05899225', formalized).
narrative_ontology:cs_authority_grounding('a8d1fb0e-c53e-414c-a221-21ee05899225', lineage).
narrative_ontology:cs_interpretation_layer_present('a8d1fb0e-c53e-414c-a221-21ee05899225').
narrative_ontology:cs_reading_relation('a8d1fb0e-c53e-414c-a221-21ee05899225', homoousios_nicene__metaphysical_equality_reading, influences).
narrative_ontology:cs_reading_relation('a8d1fb0e-c53e-414c-a221-21ee05899225', homoousios_nicene__subordinationist_reading, coexists_with).
narrative_ontology:cs_axiom('a8d1fb0e-c53e-414c-a221-21ee05899225', foundational, creedal_terms_admit_honorific_construal).
narrative_ontology:cs_axiom_status(creedal_terms_admit_honorific_construal, holdable).
narrative_ontology:cs_axiom_grounding('a8d1fb0e-c53e-414c-a221-21ee05899225', creedal_terms_admit_honorific_construal, conventional).
narrative_ontology:cs_axiom('a8d1fb0e-c53e-414c-a221-21ee05899225', secondary, local_episcopal_discretion_governs_doctrinal_precision).
narrative_ontology:cs_axiom_status(local_episcopal_discretion_governs_doctrinal_precision, overridden).
narrative_ontology:cs_axiom_grounding('a8d1fb0e-c53e-414c-a221-21ee05899225', local_episcopal_discretion_governs_doctrinal_precision, conventional).
narrative_ontology:cs_reference_frame('a8d1fb0e-c53e-414c-a221-21ee05899225', nicene_325_conciliar_formula).
narrative_ontology:cs_drift_state('a8d1fb0e-c53e-414c-a221-21ee05899225', post_semi_arian_councils_360s, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a8d1fb0e-c53e-414c-a221-21ee05899225', '').
narrative_ontology:cs_kernel_id(homoousios_nicene__honorific_similarity_reading, homoousios_nicene).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(homoousios_nicene__honorific_similarity_reading, semi_arian_moderate_bishops).
narrative_ontology:constraint_beneficiary(homoousios_nicene__honorific_similarity_reading, apophatic_theological_traditions).
narrative_ontology:constraint_beneficiary(homoousios_nicene__honorific_similarity_reading, local_episcopal_authorities).
narrative_ontology:constraint_victim(homoousios_nicene__honorific_similarity_reading, strict_nicene_enforcers).
narrative_ontology:constraint_victim(homoousios_nicene__honorific_similarity_reading, hard_subordinationist_clergy).
narrative_ontology:constraint_victim(homoousios_nicene__honorific_similarity_reading, congregations_seeking_doctrinal_clarity).
narrative_ontology:constraint_vindicates(homoousios_nicene__honorific_similarity_reading, pastoral_discretion_doctrine).
narrative_ontology:constraint_vindicates(homoousios_nicene__honorific_similarity_reading, conciliar_language_as_negotiated_settlement).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold a middle position rejecting both strict identity-of-essence and outright subordinationism. The similarity reading of homoousios lets them affirm the Nicene formula in public while retaining homoiousian-style theological commitments in preaching and pastoral instruction. They gain room to maneuver between factions and avoid forced choice between condemnation as Arian or capitulation to what they see as an overreaching metaphysical claim.
narrative_ontology:constraint_stakeholder(homoousios_nicene__honorific_similarity_reading, semi_arian_moderate_bishops, beneficiary,
    organized, generational, mobile, regional).

% Approach divine essence as fundamentally beyond precise predication. The similarity reading validates their instinct that homoousios functions honorifically, gesturing at unity of glory and will rather than asserting a technical identity claim their epistemology treats as overreach. They benefit from a reading that keeps the term usable without demanding the strict metaphysics they consider unwarranted.
narrative_ontology:constraint_stakeholder(homoousios_nicene__honorific_similarity_reading, apophatic_theological_traditions, beneficiary,
    moderate, civilizational, mobile, regional).

% Administer creedal subscription in their own sees and adjudicate which clergy remain in communion. Under the similarity reading, interpretive discretion shifts toward them rather than toward a centralized council enforcement apparatus — they decide locally how much metaphysical weight the term carries, which expands their pastoral and disciplinary authority relative to Nicaea's centralizing intent.
narrative_ontology:constraint_stakeholder(homoousios_nicene__honorific_similarity_reading, local_episcopal_authorities, agenda_setter,
    institutional, generational, constrained, regional).

% Bishops and theologians (in the Athanasian line) who insist homoousios means numerical identity of essence, no remainder. The similarity reading directly erodes the boundary they built their authority and their anti-Arian polemic on; every local see that adopts the looser reading is a see where their enforcement mechanism (charges of heresy, deposition, appeal to imperial authority) loses its target and its force. They bear the cost of watching the formula they fought for become negotiable.
narrative_ontology:constraint_stakeholder(homoousios_nicene__honorific_similarity_reading, strict_nicene_enforcers, payer,
    powerful, generational, constrained, continental).

% Clergy holding that the Son derives being from the Father in a way that entails real subordination in nature, not merely in role. The similarity reading does not rescue them — it still requires affirming a real, if loosely construed, unity that their position denies. They remain exposed to heresy charges under this reading just as under the strict identity reading, but now also lose the semi-Arian allies who might otherwise have shared their exposure, since those allies have found shelter in the similarity reading instead.
narrative_ontology:constraint_stakeholder(homoousios_nicene__honorific_similarity_reading, hard_subordinationist_clergy, payer,
    moderate, biographical, trapped, regional).

% Ordinary believers and lower clergy who receive the creed as settled doctrine and expect a stable answer to what is confessed about Christ's nature. Under this reading, the content of what they are required to affirm varies by diocese and by which bishop currently holds the see, producing catechetical instability they did not choose and cannot appeal past their own local hierarchy.
narrative_ontology:constraint_stakeholder(homoousios_nicene__honorific_similarity_reading, congregations_seeking_doctrinal_clarity, payer,
    powerless, biographical, trapped, local).

% The emperor and the ecumenical council apparatus intended homoousios as a fixed, empire-wide boundary marker to end the Arian controversy definitively. The similarity reading was never their intent and is not represented in this local settlement; had they been consulted at the point of local implementation, they would object that the term is being hollowed of the very precision it was coined to supply.
narrative_ontology:constraint_stakeholder(homoousios_nicene__honorific_similarity_reading, imperial_conciliar_authority, excluded,
    institutional, generational, analytical, continental).

% Study the fourth-century controversy retrospectively, including the homoousios/homoiousios terminological slippage documented in the Cappadocian settlement. They can trace how the same creedal word carried different metaphysical weight in different sees and periods without needing to adjudicate which reading is theologically correct.
narrative_ontology:constraint_stakeholder(homoousios_nicene__honorific_similarity_reading, later_conciliar_historians, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared confessional vocabulary flexible enough that bishops holding a range of positions on divine unity can remain in communion under one creedal term, avoiding a total schism between Nicene rigorists and the broad center of fourth-century episcopal opinion.
% TRANSFER_FUNCTION: Moves interpretive authority away from the ecumenical council's intended fixed boundary and toward individual sees; moves doctrinal risk away from moderate bishops and onto both the strict enforcers (who lose their enforcement target) and hard subordinationists (who lose moderate allies) and onto ordinary congregants (who absorb the resulting instability in catechesis).
% ABSENT_VOICES: The emperor and the council fathers who coined homoousios specifically to foreclose ambiguity are not present in the local implementation disputes where this reading takes hold; their intended precision is overridden by diocesan practice without their participation in that particular reinterpretation.
% DISAPPEARANCE_RATIONALE: If the similarity reading disappeared and only strict identity or only subordinationist readings remained live, the semi-Arian center would be forced into open schism with one side or the other — moderate bishops and apophatic traditions would lose their accommodation space. Strict enforcers would regard this as the world correcting itself; congregations would regard it as either welcome clarity or renewed conflict, hence contested rather than settled.
% FOUNDING_PROBLEM: The fourth-century church needed a single term that could hold together a fractured episcopate divided over the Son's relationship to the Father, after Nicaea (325) had adopted homoousios but left its precise metaphysical content under-specified relative to the competing homoiousios ('of like substance') formula favored by many moderates.
% FOUNDING_PROBLEM_CORROBORATION: Strict Nicene partisans (in Athanasius's own polemical writings) attest that the term was always meant to secure identity of essence and that the similarity reading is a corruption introduced by those seeking to shelter crypto-Arian positions. Independent historical-critical scholarship on the Cappadocian settlement and the semi-Arian councils of the 350s-360s corroborates, from outside either faction's self-interest, that the term's practical content was genuinely contested and locally negotiated well after 325 — it was not received everywhere as settled identity language until later conciliar consolidation.
narrative_ontology:disappearance_verdict(homoousios_nicene__honorific_similarity_reading, contested).
narrative_ontology:founding_problem_status(homoousios_nicene__honorific_similarity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(homoousios_nicene__honorific_similarity_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(homoousios_nicene__honorific_similarity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(homoousios_nicene__honorific_similarity_reading, 0.46, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(homoousios_nicene__honorific_similarity_reading_tests).
:- end_tests(homoousios_nicene__honorific_similarity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction rises through the 340s-360s (peak ~0.50 at the height of the semi-Arian councils, e.g. Sirmium/Seleucia-Rimini) as the similarity reading's flexibility increasingly serves as a mechanism for moderate bishops and local sees to extract doctrinal latitude and institutional autonomy at the expense of catechetical stability for ordinary congregants and enforcement capacity for strict Nicenes. Extraction eases somewhat after 370 as the Cappadocian settlement begins absorbing and stabilizing the ambiguity rather than leaving it fully open. Theater ratio tracks a similar arc: the more the term is invoked ceremonially in council communiques while its practical content is negotiated locally, the more the invocation becomes performative unity-signaling divorced from settled function. Suppression requirement rises as strict enforcers escalate exile and deposition (Athanasius's repeated exiles) to hold the boundary the similarity reading erodes, then eases as the controversy moves toward the 381 settlement.
 *
 * PERSPECTIVAL GAP:
 *   From the local episcopal authority's seat this reads as legitimate pastoral flexibility preserving the wider communion; from the strict Nicene enforcer's seat the identical structure reads as institutional capture of the creed's intended precision by parties unwilling to be pinned down. The engine should register this as seat divergence rather than resolve it — both seats are looking at the same historical mechanism (local interpretive latitude around a council-fixed term) from opposite ends of the enforcement relationship.
 *
 * DIRECTIONALITY LOGIC:
 *   Semi-Arian moderate bishops and apophatic traditions are declared beneficiaries because the similarity reading is structurally built to serve their position — it lets them retain communion and creedal subscription without conceding the metaphysical claim they reject. Local episcopal authorities benefit as agenda-setters because interpretive discretion devolves to them. Strict Nicene enforcers are victims not because they lose a metaphysical argument but because their entire enforcement apparatus (heresy trials, appeals to imperial power, deposition) loses its clear target when the boundary blurs at the local level. Hard subordinationists are victims because the similarity reading does not actually rescue them — it still requires some real unity claim — while it strips away the semi-Arian allies who might otherwise have diluted their exposure. Ordinary congregations are victims of instability rather than of any doctrinal position: they bear the transaction cost of a floating standard.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (holding a fractured episcopate together under one confessional term after 325) is genuinely contested as live or dead within this reading's own history: the semi-Arian center that benefited from the ambiguity had every institutional incentive to declare the problem still live and the flexible reading still necessary, while strict Nicenes declared the ambiguity itself the problem, not the solution. The Cappadocian resolution (which effectively subsumed the similarity language into a more precise 'one ousia, three hypostaseis' formula by 381) suggests the mandate for open-ended local latitude did eventually expire — the tangled_rope classification captures a period, not a permanent state.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    similarity_vs_slippage,
    'Is the honorific-similarity reading a genuine, coherent alternative construal of homoousios held in good faith by moderate bishops, or is it better described as a terminological slippage/blur between homoousios and homoiousios that historical actors exploited without a stable doctrinal content of its own?',
    'Close philological and conciliar-document analysis of specific fourth-century sees (e.g. correspondence and synodal acts from the 340s-360s) to determine whether ''similarity'' functioned as a stable theological position or as a shifting rhetorical accommodation with no fixed referent.',
    'If the reading is a stable position, it is a genuine sibling reading with its own coherent axioms; if it is pure slippage, the constraint may be better modeled as a degraded/inertial piton riding on the prestige of the Nicene formula rather than a tangled_rope with real coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(similarity_vs_slippage, conceptual, 'Whether the similarity reading is a coherent doctrinal position or an unstable rhetorical blur.').

omega_variable(
    kernel_reading_framing_choice,
    'Is the honorific-similarity reading better framed as a live theological content-reading of homoousios (as authored here) or as a second-order political settlement mechanism whose actual function was ecclesiastical peace-keeping regardless of its stated metaphysical content?',
    'Compare the reading''s operation across sees with different political stakes (imperially favored sees vs. peripheral sees) — if the reading''s content and enforcement track political alignment more than theological conviction, the political-settlement framing is favored.',
    'Under the theological-content framing (adopted here), beneficiaries are theological factions (semi-Arians, apophatic traditions). Under the political-settlement framing, the beneficiary set would shift toward imperial and metropolitan administrative interests seeking to avoid schism, which would change the beneficiary declarations and likely raise measured extraction further.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_framing_choice, conceptual, 'Alternative framing of the reading as theological content vs. political settlement mechanism; documents the Omega_C under-determination in the kernel reading choice.').

omega_variable(
    congregational_awareness,
    'Did ordinary congregations (the powerless payer seat) actually perceive the doctrinal instability, or did the ambiguity operate entirely at the level of episcopal and theological elites while congregational practice remained stable regardless of which reading their bishop held?',
    'Evidence from catechetical materials, sermon records, and lay correspondence (where extant) from sees that shifted between readings, to assess whether the shift produced observable lay confusion or was invisible below the clerical level.',
    'If congregations were largely insulated from the elite-level dispute, the victim status of congregations_seeking_doctrinal_clarity should be weighted lower — most of the true cost fell on the enforcement-and-communion relationships among bishops, not on lay religious life.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(congregational_awareness, empirical, 'Whether lay congregations experienced the doctrinal ambiguity as a real cost or were structurally insulated from it.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(homoousios_nicene__honorific_similarity_reading, 325, 381).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(homo_tr_t325, homoousios_nicene__honorific_similarity_reading, theater_ratio, 325, 0.2).
narrative_ontology:measurement(homo_tr_t336, homoousios_nicene__honorific_similarity_reading, theater_ratio, 336, 0.28).
narrative_ontology:measurement(homo_tr_t350, homoousios_nicene__honorific_similarity_reading, theater_ratio, 350, 0.34).
narrative_ontology:measurement(homo_tr_t359, homoousios_nicene__honorific_similarity_reading, theater_ratio, 359, 0.42).
narrative_ontology:measurement(homo_tr_t370, homoousios_nicene__honorific_similarity_reading, theater_ratio, 370, 0.4).
narrative_ontology:measurement(homo_tr_t381, homoousios_nicene__honorific_similarity_reading, theater_ratio, 381, 0.38).

% Extraction over time
narrative_ontology:measurement(homo_be_t325, homoousios_nicene__honorific_similarity_reading, base_extractiveness, 325, 0.3).
narrative_ontology:measurement(homo_be_t336, homoousios_nicene__honorific_similarity_reading, base_extractiveness, 336, 0.38).
narrative_ontology:measurement(homo_be_t350, homoousios_nicene__honorific_similarity_reading, base_extractiveness, 350, 0.44).
narrative_ontology:measurement(homo_be_t359, homoousios_nicene__honorific_similarity_reading, base_extractiveness, 359, 0.5).
narrative_ontology:measurement(homo_be_t370, homoousios_nicene__honorific_similarity_reading, base_extractiveness, 370, 0.48).
narrative_ontology:measurement(homo_be_t381, homoousios_nicene__honorific_similarity_reading, base_extractiveness, 381, 0.46).

% Suppression requirement over time
narrative_ontology:measurement(homo_su_t325, homoousios_nicene__honorific_similarity_reading, suppression_requirement, 325, 0.25).
narrative_ontology:measurement(homo_su_t336, homoousios_nicene__honorific_similarity_reading, suppression_requirement, 336, 0.35).
narrative_ontology:measurement(homo_su_t350, homoousios_nicene__honorific_similarity_reading, suppression_requirement, 350, 0.45).
narrative_ontology:measurement(homo_su_t359, homoousios_nicene__honorific_similarity_reading, suppression_requirement, 359, 0.55).
narrative_ontology:measurement(homo_su_t370, homoousios_nicene__honorific_similarity_reading, suppression_requirement, 370, 0.48).
narrative_ontology:measurement(homo_su_t381, homoousios_nicene__honorific_similarity_reading, suppression_requirement, 381, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(homoousios_nicene__honorific_similarity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(homoousios_nicene__honorific_similarity_reading, 0.1).
narrative_ontology:affects_constraint(homoousios_nicene__honorific_similarity_reading, homoousios_nicene__metaphysical_equality_reading).
narrative_ontology:affects_constraint(homoousios_nicene__honorific_similarity_reading, homoousios_nicene__subordinationist_reading).

% DUAL FORMULATION NOTE:
% This story is one of three siblings decomposing the natural-language label 'the homoousios controversy' per the ε-invariance principle. The honorific_similarity_reading (this file) has moderate extraction with a genuine coordination function (holding a fractured episcopate together) alongside real asymmetric costs (borne by strict enforcers, hard subordinationists, and congregations). The metaphysical_equality_reading sibling is expected to show a different ε profile: higher accessibility_collapse and lower ambiguity-driven extraction once the Cappadocian settlement fixes the term, but potentially higher suppression as the fixed reading becomes doctrinally mandatory and enforced against dissent. The subordinationist_reading sibling is expected to show high victim concentration (hard subordinationists condemned outright) with different beneficiary structure (those seeking to preserve strict monotheistic hierarchy). All three share the same underlying kernel (the coined term homoousios and the 325/381 conciliar acts) but instantiate structurally distinct constraints with distinct ε values, consistent with the BGS-style decomposition pattern.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
