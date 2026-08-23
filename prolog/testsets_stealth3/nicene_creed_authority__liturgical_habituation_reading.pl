% ============================================================================
% CONSTRAINT STORY: nicene_creed_authority__liturgical_habituation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nicene_creed_liturgical_habituation, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: nicene_creed_authority__liturgical_habituation_reading
 *   human_readable: Nicene Creed as Liturgical Identity Boundary (Habituation Reading)
 *   domain: religious/ecclesial
 *
 * SUMMARY:
 *   Weekly congregational recitation of the Nicene Creed operates, on this
 *   reading, as an identity boundary marker sustained by liturgical
 *   habituation: the few memorized sentences, performed together, mark who is
 *   inside the community independently of whether any performer understands
 *   or privately assents to the metaphysics. The epsilon referent is the
 *   standing arrangement under contest - the embedded liturgical practice of
 *   communal recitation as actually conducted across communions - assessed by
 *   this reading's own lights, not the strict reading's sanction regime or
 *   the symbolic reading's discernment process. This story is one member of a
 *   three-reading constraint family decomposing the colloquial label 'the
 *   creed's authority': the strict_orthodox_reading authors a substantially
 *   extractive arrangement with heresy sanctions, the
 *   symbolic_confessional_reading authors a community-discernment
 *   arrangement, and this reading authors a near-zero-extraction coordination
 *   device. The decomposition follows the epsilon-invariance principle:
 *   measuring the creed's authority through performance yields a different
 *   constraint with a different epsilon than measuring it through enforced
 *   ontology, so they are separate files linked by network edges.
 *
 * KEY AGENTS:
 *   - creed_reciting_laity: primary beneficiary (organized/constrained) - receives portable, instantly recognizable shared identity at the cost of minutes of weekly participation
 *   - catechumens_and_newcomers: secondary beneficiary (moderate/mobile) - receive a low-cost, learnable entry performance into membership
 *   - liturgical_officiants: administering seat (institutional/mobile) - lead the recitation per rubric, bear its labor, change nothing locally
 *   - denominational_hierarchies: agenda-setting seat (institutional/arbitrage) - set rubrics governing use, harvest cross-generational cohesion, can revise practice through synodical process
 *   - nonreciting_attendees: excluded seat (powerless/mobile) - present in the room where the boundary is performed, outside every conversation about the text
 *   - liturgical_historians: analytical observer (analytical/analytical) - attest the arrangement's baptismal origin and persistent function from outside the worshipping communities
 *   - ecumenical_bodies: institutional observer (institutional/analytical) - trade on the creed's common recitation to build agreement between divided communions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nicene_creed_authority__liturgical_habituation_reading, 0.08).
domain_priors:suppression_score(nicene_creed_authority__liturgical_habituation_reading, 0.1).
domain_priors:theater_ratio(nicene_creed_authority__liturgical_habituation_reading, 0.09).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nicene_creed_authority__liturgical_habituation_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(nicene_creed_authority__liturgical_habituation_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(nicene_creed_authority__liturgical_habituation_reading, theater_ratio, 0.09).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nicene_creed_authority__liturgical_habituation_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(nicene_creed_authority__liturgical_habituation_reading, resistance, 0.18).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nicene_creed_authority__liturgical_habituation_reading, rope).
narrative_ontology:human_readable(nicene_creed_authority__liturgical_habituation_reading, "Nicene Creed as Liturgical Identity Boundary (Habituation Reading)").
narrative_ontology:topic_domain(nicene_creed_authority__liturgical_habituation_reading, "religious/ecclesial").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nicene_creed_authority__liturgical_habituation_reading, 'aa7cf2b6-725e-4ad2-ac73-4be5ae822728').
narrative_ontology:cs_kernel_codification('aa7cf2b6-725e-4ad2-ac73-4be5ae822728', fixed_text).
narrative_ontology:cs_authority_grounding('aa7cf2b6-725e-4ad2-ac73-4be5ae822728', lineage).
narrative_ontology:cs_interpretation_layer_present('aa7cf2b6-725e-4ad2-ac73-4be5ae822728').
narrative_ontology:cs_reading_relation('aa7cf2b6-725e-4ad2-ac73-4be5ae822728', nicene_creed_authority__strict_orthodox_reading, influences).
narrative_ontology:cs_reading_relation('aa7cf2b6-725e-4ad2-ac73-4be5ae822728', nicene_creed_authority__symbolic_confessional_reading, influences).
narrative_ontology:cs_axiom('aa7cf2b6-725e-4ad2-ac73-4be5ae822728', foundational, performance_constitutes_membership).
narrative_ontology:cs_axiom_status(performance_constitutes_membership, holdable).
narrative_ontology:cs_axiom_grounding('aa7cf2b6-725e-4ad2-ac73-4be5ae822728', performance_constitutes_membership, conventional).
narrative_ontology:cs_axiom('aa7cf2b6-725e-4ad2-ac73-4be5ae822728', secondary, assent_independence_principle).
narrative_ontology:cs_axiom_status(assent_independence_principle, holdable).
narrative_ontology:cs_axiom_grounding('aa7cf2b6-725e-4ad2-ac73-4be5ae822728', assent_independence_principle, empirically_contingent).
narrative_ontology:cs_reference_frame('aa7cf2b6-725e-4ad2-ac73-4be5ae822728', liturgical_transmission_continuity).
narrative_ontology:cs_drift_state('aa7cf2b6-725e-4ad2-ac73-4be5ae822728', contemporary_pluralist_worship, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('aa7cf2b6-725e-4ad2-ac73-4be5ae822728', '').
narrative_ontology:cs_kernel_id(nicene_creed_authority__liturgical_habituation_reading, nicene_creed_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nicene_creed_authority__liturgical_habituation_reading, creed_reciting_laity).
narrative_ontology:constraint_beneficiary(nicene_creed_authority__liturgical_habituation_reading, catechumens_and_newcomers).
narrative_ontology:constraint_beneficiary(nicene_creed_authority__liturgical_habituation_reading, denominational_hierarchies).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Attend weekly services and recite the creed aloud with the congregation. What flows to them is a shared, instantly recognizable membership - the same few sentences mark them as inside whether they are in Lagos, Minneapolis, or Nairobi, and whether or not they could explain homoousios. What flows from them is a few minutes of voice and presence. Leaving means leaving the community, not merely skipping a line; staying while doubting costs nothing formal, since the recitation does not ask what they privately conclude.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__liturgical_habituation_reading, creed_reciting_laity, beneficiary,
    organized, biographical, constrained, global).

% Are entering the community through baptism preparation or regular attendance. The creed hands them a learnable performance - memorizable in an afternoon - that marks their passage into membership without requiring years of theological formation. At this stage exit is easy: they have invested little and the surrounding society offers many other communities.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__liturgical_habituation_reading, catechumens_and_newcomers, beneficiary,
    moderate, immediate, mobile, local).

% Priests, pastors, and lay readers who lead the recitation according to their tradition's rubric. They carry the weekly labor of liturgical leadership and receive the vocational satisfaction of continuity with the historic church. They did not write the text and cannot alter it locally; they can transfer between parishes readily and between traditions with more friction.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__liturgical_habituation_reading, liturgical_officiants, agenda_setter,
    institutional, biographical, mobile, regional).

% Synods, bishops' conferences, and liturgical commissions that set when and how the creed is said. They inherit the text from the conciliar past and manage its present use - approving translations, permitting or blocking inclusive-language adaptation, deciding whether recitation is weekly or seasonal. Their return is cohesion across congregations and centuries; their lever is the rubric, revisable through slow synodical process.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__liturgical_habituation_reading, denominational_hierarchies, agenda_setter,
    institutional, generational, arbitrage, global).

% Visitors, seekers, and doubting members who stand or sit through the creed without speaking. They are physically present in the room where the boundary is performed but hold no standing in any conversation about the text or its use. Their silence is tolerated in most congregations; their perspective reaches liturgical decision-makers only obliquely, if at all.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__liturgical_habituation_reading, nonreciting_attendees, excluded,
    powerless, immediate, mobile, local).

% Academic specialists in patristic and liturgical history who reconstruct how the creed moved from baptismal interrogation to eucharistic recitation. They attest the practice's origin and function from outside the worshipping communities, publish where the pews do not read, and hold no stake in the practice continuing or ceasing.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__liturgical_habituation_reading, liturgical_historians, observer,
    analytical, civilizational, analytical, global).

% Councils and bilateral dialogues that use the creed as the one text all major communions already share. They build on its common recitation to construct agreements between divided churches; they observe and facilitate rather than govern any congregation's practice.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__liturgical_habituation_reading, ecumenical_bodies, observer,
    institutional, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(nicene_creed_authority__liturgical_habituation_reading, diffuse).
narrative_ontology:fixing_cost_class(nicene_creed_authority__liturgical_habituation_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Synchronizes a geographically dispersed, generationally successive community into one mutually recognizable body: a newcomer in any congregation can perform the same few sentences and be marked as inside. Transmits membership across illiteracy, language shift, and theological ignorance - the performance is learnable in minutes and requires no doctrinal expertise.
% TRANSFER_FUNCTION: Moves a few minutes of communal voice and attention per gathering from every attendee into a shared identity good; moves no money, labor, or material resource. Secondarily transfers inherited legitimacy: each recitation carries the conciliar text's authority forward to the present assembly.
% ABSENT_VOICES: Non-Trinitarian Christians (Unitarians, Latter-day Saints) for whom the creed functions as an exclusionary test regardless of the habituation framing; congregants who object to the text's gendered address and recite under protest or fall silent; visitors and seekers with no standing in liturgical decision-making. Rubrics are set by hierarchies and commissions; the pews are consulted rarely.
% DISAPPEARANCE_RATIONALE: Congregations would lose their cheapest, most portable identity technology: worship would reorganize around hymnody, Eucharist, and lectionary as boundary carriers, catechesis would lengthen, and the visible continuity between a fourth-century council and next Sunday's assembly would thin. Nothing collapses - other rituals partially absorb the load - but the arrangement of Christian self-reproduction visibly rearranges.
% FOUNDING_PROBLEM: How a scattered minority movement defines and transmits who counts as Christian without requiring every member to master theology - originally concretized as the baptismal interrogation of candidates and, after Nicaea and Constantinople, as the boundary against Arian subordinationism.
% FOUNDING_PROBLEM_CORROBORATION: Liturgical historians reconstructing the pre-baptismal interrogatories (the Hippolytan traditio, Cyril of Jerusalem's catechetical lectures) attest the founding problem from outside any benefiting party; sociologists of ritual corroborate the ongoing need for low-cost identity-transmission mechanisms in voluntary communities. No seat inside the liturgy attests it neutrally - the corroboration is scholarly, which is itself signal that the beneficiaries do not supply the genealogy.
narrative_ontology:disappearance_verdict(nicene_creed_authority__liturgical_habituation_reading, world_rearranges).
narrative_ontology:founding_problem_status(nicene_creed_authority__liturgical_habituation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nicene_creed_authority__liturgical_habituation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(nicene_creed_authority__liturgical_habituation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nicene_creed_authority__liturgical_habituation_reading, 0.08, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nicene_creed_authority__liturgical_habituation_reading_tests).
:- end_tests(nicene_creed_authority__liturgical_habituation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.08 because the arrangement takes minutes of voice and presence per week and returns a membership good; nothing material transfers and no seat captures a surplus. Suppression is 0.10: participation is normed but not coerced - silent abstention is tolerated in most congregations, exit to other traditions is legally and socially open, and the reading's own design (assent-independence) removes the lie tax that a propositional test would impose on doubters. Theater_ratio is deliberately low (0.09) despite the practice being literally theatrical: the metric tracks performative activity that has lost its function, and here the performance IS the function - recitation is not a degraded proxy for assent but the identity-transmission mechanism itself. Accessibility_collapse is 0.35: once the constraint is understood as an identity marker, alternatives remain fully available (other rites, hymnody, Eucharist, silence, other communities), so alternatives only partly collapse. Resistance is 0.18: filioque-class disputes, inclusive-language campaigns, conscientious abstention, and the creed-rejecting peace-church traditions meet the practice, but nothing like the resistance an enforced ontology provokes. The temporal series run on one shared grid (381-2025 CE, nine points, all three metrics authored at every point) and show two honest arcs: a modest extractiveness-and-suppression hump peaking around the confessionalization era (circa 1650, when recitation doubled as a loyalty marker between warring confessions) followed by post-Enlightenment relaxation, and a slow theater_ratio creep as recitation persists in low-belief contexts - still far below any piton threshold. Claim and metrics are independent authored facts: rope is what I believe structurally true; the metrics are what I believe descriptively true.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the denominational hierarchy's position the arrangement is an inheritance instrument delivering cohesion across centuries; from the officiant's position it is vocational duty and continuity; from the laity's position it is belonging that costs almost nothing; from the nonreciting attendee's position it is a boundary performed over their heads with no standing offered to them. The same recited sentences are heritage, duty, home, or exclusion depending on the seat - the engine computes this divergence from the power, exit, and role data rather than from the authored claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Every declared party sits at or near the beneficiary end of the directionality axis. The laity and newcomers receive the coordination good directly (low d); the hierarchies receive cohesion and continuity without running a coercive apparatus (low d); the officiants sit nearest symmetric - they bear the recurring labor of leadership against vocational and continuity returns. No victims are declared because no party bears an asymmetric net cost: the costs (minutes, mild conformity visibility) are diffuse and roughly balanced by benefits, which is precisely what distinguishes this reading from the strict reading, whose victim set (sanctioned dissenters) is the structural signature this reading lacks. Excluded and observer seats do not feed the directionality computation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem - transmitting who counts as Christian across generations without requiring universal theological literacy - remains live: voluntary religious communities still face the identity-transmission problem the baptismal interrogatories were built for, and the specific Arian occasion has been dead for sixteen centuries without the function dying with it. This is not mandatrophy: the function is intact, theater is low, and the practice persists because it works, not because inertia alone holds it. The classification guards against two mislabels: calling this a snare would require suppressed exits and captured gains, and neither exists (fixing is cheap - any synod can drop the rubric - yet the practice persists voluntarily); calling it a piton would require atrophied function behind theatrical maintenance, but the performance is the living mechanism. The genuine open risk is the performance-belief feedback loop (see omegas): if habituation manufactures assent, this clean rope gradually becomes the substrate the strict reading enforces through.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_locus_of_binding_force,
    'This story instantiates the liturgical_habituation_reading of the nicene_creed_authority kernel: is the creed''s binding force exhausted by habituated communal performance, or does it additionally require cognitive metaphysical assent (strict_orthodox_reading) or rest wholly in community discernment (symbolic_confessional_reading)?',
    'Comparative analysis across the three sibling stories: locate where each reading places the authority-conferring act (performance, proposition, discernment) and test which placement predicts observed sanction behavior, exit patterns, and reinterpretation latitude in congregations.',
    'If the strict reading captures the operative arrangement, this story''s epsilon understates extraction from doctrinal dissenters and the victim set is non-empty; if the symbolic reading captures it, beneficiaries shrink to discerning communities and the performance substrate becomes instrumental rather than constitutive.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_locus_of_binding_force, conceptual, 'Committer-frame omega: one reading of a three-reading kernel; the disagreement is located in the locus of binding force.').

omega_variable(
    performance_belief_feedback_loop,
    'Does decades of habituated recitation manufacture metaphysical assent, dissolving this reading''s core independence claim (performance without assent) and converting the performance substrate into the strict reading''s enforcement base?',
    'Longitudinal survey correlating reciters'' explicit Christological and metaphysical beliefs with recitation history; compare cohorts formed under mandatory versus optional recitation.',
    'If the feedback loop is strong, the independence axiom fails empirically and effective pressure on silent dissenters rises; if weak, the low-extraction coordination classification holds cleanly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(performance_belief_feedback_loop, empirical, 'Whether habituated performance induces the assent it claims to be independent of.').

omega_variable(
    optional_recitation_boundary_persistence,
    'When congregations or whole traditions make creedal recitation optional or abandon it, does the identity-boundary function survive through substitute performances, or does boundary salience uniquely decay?',
    'Natural experiment across low-church traditions that dropped weekly recitation and high-church traditions that retained it: measure retention, inter-congregational recognizability, and self-reported identity salience.',
    'If the function survives substitution, the creed is one implementable coordination device among several and removal costs are genuinely low; if it decays uniquely, the specific text carries irreplaceable coordination content and the cheap-fixing assessment was underestimated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(optional_recitation_boundary_persistence, empirical, 'Whether the boundary function is text-specific or substitutable by other liturgical performances.').

omega_variable(
    cs_framing_fixed_text_vs_distributed,
    'Is the creed''s kernel better framed as a fixed transmitted text under lineage authority (as authored here) or as a distributed kernel - variant texts (325/381 recensions, filioque divergence), no single adjudicator, multiple traditions reciting different forms?',
    'Test whether any single authority ever successfully adjudicates textual variants: if filioque-class divergences persist unresolved across communions indefinitely, the distributed framing fits practice better than the lineage framing.',
    'Under the distributed framing, interpretation_layer_present loses its warrant and authority_grounding shifts to distributed, weakening the lineage claim that supplies the substrate both sibling readings rely on.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_framing_fixed_text_vs_distributed, conceptual, 'Framing under-determination in the commitment-system classification of the creed kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nicene_creed_authority__liturgical_habituation_reading, 381, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nicene_liturgical_hab_tr_t381, nicene_creed_authority__liturgical_habituation_reading, theater_ratio, 381, 0.04).
narrative_ontology:measurement_basis(nicene_liturgical_hab_tr_t381, observed).
narrative_ontology:measurement(nicene_liturgical_hab_tr_t600, nicene_creed_authority__liturgical_habituation_reading, theater_ratio, 600, 0.03).
narrative_ontology:measurement_basis(nicene_liturgical_hab_tr_t600, observed).
narrative_ontology:measurement(nicene_liturgical_hab_tr_t800, nicene_creed_authority__liturgical_habituation_reading, theater_ratio, 800, 0.03).
narrative_ontology:measurement_basis(nicene_liturgical_hab_tr_t800, observed).
narrative_ontology:measurement(nicene_liturgical_hab_tr_t1054, nicene_creed_authority__liturgical_habituation_reading, theater_ratio, 1054, 0.04).
narrative_ontology:measurement_basis(nicene_liturgical_hab_tr_t1054, observed).
narrative_ontology:measurement(nicene_liturgical_hab_tr_t1200, nicene_creed_authority__liturgical_habituation_reading, theater_ratio, 1200, 0.05).
narrative_ontology:measurement_basis(nicene_liturgical_hab_tr_t1200, observed).
narrative_ontology:measurement(nicene_liturgical_hab_tr_t1500, nicene_creed_authority__liturgical_habituation_reading, theater_ratio, 1500, 0.05).
narrative_ontology:measurement_basis(nicene_liturgical_hab_tr_t1500, observed).
narrative_ontology:measurement(nicene_liturgical_hab_tr_t1650, nicene_creed_authority__liturgical_habituation_reading, theater_ratio, 1650, 0.06).
narrative_ontology:measurement_basis(nicene_liturgical_hab_tr_t1650, observed).
narrative_ontology:measurement(nicene_liturgical_hab_tr_t1900, nicene_creed_authority__liturgical_habituation_reading, theater_ratio, 1900, 0.07).
narrative_ontology:measurement_basis(nicene_liturgical_hab_tr_t1900, observed).
narrative_ontology:measurement(nicene_liturgical_hab_tr_t2025, nicene_creed_authority__liturgical_habituation_reading, theater_ratio, 2025, 0.09).
narrative_ontology:measurement_basis(nicene_liturgical_hab_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(nicene_liturgical_hab_be_t381, nicene_creed_authority__liturgical_habituation_reading, base_extractiveness, 381, 0.1).
narrative_ontology:measurement_basis(nicene_liturgical_hab_be_t381, observed).
narrative_ontology:measurement(nicene_liturgical_hab_be_t600, nicene_creed_authority__liturgical_habituation_reading, base_extractiveness, 600, 0.08).
narrative_ontology:measurement_basis(nicene_liturgical_hab_be_t600, observed).
narrative_ontology:measurement(nicene_liturgical_hab_be_t800, nicene_creed_authority__liturgical_habituation_reading, base_extractiveness, 800, 0.07).
narrative_ontology:measurement_basis(nicene_liturgical_hab_be_t800, observed).
narrative_ontology:measurement(nicene_liturgical_hab_be_t1054, nicene_creed_authority__liturgical_habituation_reading, base_extractiveness, 1054, 0.08).
narrative_ontology:measurement_basis(nicene_liturgical_hab_be_t1054, observed).
narrative_ontology:measurement(nicene_liturgical_hab_be_t1200, nicene_creed_authority__liturgical_habituation_reading, base_extractiveness, 1200, 0.08).
narrative_ontology:measurement_basis(nicene_liturgical_hab_be_t1200, observed).
narrative_ontology:measurement(nicene_liturgical_hab_be_t1500, nicene_creed_authority__liturgical_habituation_reading, base_extractiveness, 1500, 0.09).
narrative_ontology:measurement_basis(nicene_liturgical_hab_be_t1500, observed).
narrative_ontology:measurement(nicene_liturgical_hab_be_t1650, nicene_creed_authority__liturgical_habituation_reading, base_extractiveness, 1650, 0.1).
narrative_ontology:measurement_basis(nicene_liturgical_hab_be_t1650, observed).
narrative_ontology:measurement(nicene_liturgical_hab_be_t1900, nicene_creed_authority__liturgical_habituation_reading, base_extractiveness, 1900, 0.07).
narrative_ontology:measurement_basis(nicene_liturgical_hab_be_t1900, observed).
narrative_ontology:measurement(nicene_liturgical_hab_be_t2025, nicene_creed_authority__liturgical_habituation_reading, base_extractiveness, 2025, 0.08).
narrative_ontology:measurement_basis(nicene_liturgical_hab_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(nicene_liturgical_hab_su_t381, nicene_creed_authority__liturgical_habituation_reading, suppression_requirement, 381, 0.09).
narrative_ontology:measurement_basis(nicene_liturgical_hab_su_t381, observed).
narrative_ontology:measurement(nicene_liturgical_hab_su_t600, nicene_creed_authority__liturgical_habituation_reading, suppression_requirement, 600, 0.07).
narrative_ontology:measurement_basis(nicene_liturgical_hab_su_t600, observed).
narrative_ontology:measurement(nicene_liturgical_hab_su_t800, nicene_creed_authority__liturgical_habituation_reading, suppression_requirement, 800, 0.06).
narrative_ontology:measurement_basis(nicene_liturgical_hab_su_t800, observed).
narrative_ontology:measurement(nicene_liturgical_hab_su_t1054, nicene_creed_authority__liturgical_habituation_reading, suppression_requirement, 1054, 0.07).
narrative_ontology:measurement_basis(nicene_liturgical_hab_su_t1054, observed).
narrative_ontology:measurement(nicene_liturgical_hab_su_t1200, nicene_creed_authority__liturgical_habituation_reading, suppression_requirement, 1200, 0.07).
narrative_ontology:measurement_basis(nicene_liturgical_hab_su_t1200, observed).
narrative_ontology:measurement(nicene_liturgical_hab_su_t1500, nicene_creed_authority__liturgical_habituation_reading, suppression_requirement, 1500, 0.09).
narrative_ontology:measurement_basis(nicene_liturgical_hab_su_t1500, observed).
narrative_ontology:measurement(nicene_liturgical_hab_su_t1650, nicene_creed_authority__liturgical_habituation_reading, suppression_requirement, 1650, 0.13).
narrative_ontology:measurement_basis(nicene_liturgical_hab_su_t1650, observed).
narrative_ontology:measurement(nicene_liturgical_hab_su_t1900, nicene_creed_authority__liturgical_habituation_reading, suppression_requirement, 1900, 0.09).
narrative_ontology:measurement_basis(nicene_liturgical_hab_su_t1900, observed).
narrative_ontology:measurement(nicene_liturgical_hab_su_t2025, nicene_creed_authority__liturgical_habituation_reading, suppression_requirement, 2025, 0.1).
narrative_ontology:measurement_basis(nicene_liturgical_hab_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nicene_creed_authority__liturgical_habituation_reading, identity_coordination).
narrative_ontology:affects_constraint(nicene_creed_authority__liturgical_habituation_reading, nicene_creed_authority__strict_orthodox_reading).
narrative_ontology:affects_constraint(nicene_creed_authority__liturgical_habituation_reading, nicene_creed_authority__symbolic_confessional_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'authority of the Nicene Creed' decomposes into three structurally distinct constraints per the epsilon-invariance principle. This story (liturgical_habituation_reading) authors the performance-substrate arrangement with epsilon 0.08; strict_orthodox_reading authors the enforced-ontology arrangement with a non-empty victim set (sanctioned dissenters) and materially higher epsilon; symbolic_confessional_reading authors the community-discernment arrangement. The upstream/downstream structure runs FROM this reading TO both siblings: the shared performance substrate changes the operating environment of the propositional readings without resolving their dispute. Each family member links the others via affects_constraints; no member averages over the siblings' epsilon values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
